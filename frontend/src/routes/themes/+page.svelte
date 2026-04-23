<script lang="ts">
  import { base } from '$app/paths';
  import { env } from '$env/dynamic/public';
  import { buildThemeStyle } from '$lib/theme';
  import type { ThemeMarketplaceItem } from '$lib/types';
  import { onMount } from 'svelte';

  const backendBaseUrl = env.PUBLIC_YESBLOG_API_BASE_URL || '';
  const headerTemplatePlaceholder = '<header><p>{{author.name}}</p><h1>{{post.title}}</h1></header>';
  const bodyTemplatePlaceholder = "<article class='prose article-prose'>{{post.content}}</article>";
  const footerTemplatePlaceholder = '<footer>Published by {{author.name}}</footer>';
  const customCssPlaceholder = '.theme-template-header { text-align: center; }';

  let items = $state<ThemeMarketplaceItem[]>([]);
  let status = $state('Loading theme marketplace...');
  let loading = $state(true);
  let query = $state('');
  let ownershipFilter = $state<'all' | 'owned' | 'free' | 'paid' | 'remix'>('all');
  let sortMode = $state<'latest' | 'price-asc' | 'price-desc' | 'rating'>('latest');
  let selectedThemeId = $state<number | null>(null);
  let reviewRating = $state('5');
  let reviewText = $state('');
  let reportReason = $state('');
  let reportDetails = $state('');
  let createName = $state('');
  let createSlug = $state('');
  let createDescription = $state('');
  let createBackgroundColor = $state('#f8f0dc');
  let createSurfaceColor = $state('#fffef7');
  let createTextColor = $state('#111111');
  let createAccentColor = $state('#ffe11a');
  let createHeadingFont = $state('Cormorant Garamond');
  let createBodyFont = $state('Space Grotesk');
  let createHeaderTemplate = $state('');
  let createBodyTemplate = $state('');
  let createFooterTemplate = $state('');
  let createCustomCss = $state('');
  let createPriceCents = $state('0');

  const filteredItems = $derived(
    [...items]
      .filter((item) => {
        const normalizedQuery = query.trim().toLowerCase();
        const matchesQuery =
          !normalizedQuery ||
          item.theme.name.toLowerCase().includes(normalizedQuery) ||
          item.theme.slug.toLowerCase().includes(normalizedQuery) ||
          (item.theme.description ?? '').toLowerCase().includes(normalizedQuery);
        const matchesOwnership =
          ownershipFilter === 'all' ||
          (ownershipFilter === 'owned' && item.owned) ||
          (ownershipFilter === 'free' && item.theme.priceCents === 0) ||
          (ownershipFilter === 'paid' && item.theme.priceCents > 0) ||
          (ownershipFilter === 'remix' && Boolean(item.theme.parentId));
        return matchesQuery && matchesOwnership;
      })
      .sort((left, right) => {
        if (sortMode === 'price-asc') return left.theme.priceCents - right.theme.priceCents;
        if (sortMode === 'price-desc') return right.theme.priceCents - left.theme.priceCents;
        if (sortMode === 'rating') return (right.rating?.averageRating ?? 0) - (left.rating?.averageRating ?? 0);
        return new Date(right.theme.updatedAt).getTime() - new Date(left.theme.updatedAt).getTime();
      })
  );

  const selectedItem = $derived(
    filteredItems.find((item) => item.theme.id === selectedThemeId) ?? filteredItems[0] ?? null
  );

  async function readErrorMessage(response: Response, fallback: string) {
    try {
      const payload = await response.json();
      if (typeof payload?.message === 'string' && payload.message.trim()) return payload.message;
    } catch {
      return fallback;
    }
    return fallback;
  }

  async function fetchMarketplace() {
    const response = await fetch(`${backendBaseUrl}/api/themes/marketplace`, {
      credentials: 'include'
    });
    if (!response.ok) throw new Error(await readErrorMessage(response, 'Failed to load themes.'));
    const data: { items: ThemeMarketplaceItem[] } = await response.json();
    items = data.items;
    selectedThemeId = selectedThemeId && data.items.some((item) => item.theme.id === selectedThemeId)
      ? selectedThemeId
      : data.items[0]?.theme.id ?? null;
    status = `${items.length} themes available`;
  }

  async function purchaseTheme(themeId: number) {
    status = 'Creating order...';
    const response = await fetch(`${backendBaseUrl}/api/theme/${themeId}/purchase`, {
      method: 'POST',
      credentials: 'include'
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Purchase failed. Log in first.');
      return;
    }
    const data: { requiresConfirmation?: boolean } = await response.json();
    if (data.requiresConfirmation) {
      const shouldPay = window.confirm('Simulate payment success for this order?');
      const confirmPayload = new URLSearchParams({
        status: shouldPay ? 'paid' : 'failed'
      });
      const confirmResponse = await fetch(`${backendBaseUrl}/api/theme/${themeId}/purchase/confirm`, {
        method: 'POST',
        credentials: 'include',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
        },
        body: confirmPayload.toString()
      });
      if (!confirmResponse.ok) {
        status = await readErrorMessage(confirmResponse, 'Payment confirmation failed.');
        return;
      }
      status = shouldPay ? 'Payment recorded and theme added to your library.' : 'Order marked as failed.';
    }
    await fetchMarketplace();
  }

  async function forkTheme(item: ThemeMarketplaceItem) {
    const name = window.prompt('Name your remix theme', `${item.theme.name} Remix`);
    if (!name) return;
    const slug = window.prompt('Choose a slug for this remix', `${item.theme.slug}-remix`);
    if (!slug) return;
    status = 'Creating remix...';
    const payload = new URLSearchParams({
      name,
      slug,
      priceCents: '0'
    });
    const response = await fetch(`${backendBaseUrl}/api/theme/${item.theme.id}/fork`, {
      method: 'POST',
      credentials: 'include',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      body: payload.toString()
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Remix failed. Purchase or log in first.');
      return;
    }
    await fetchMarketplace();
    status = 'Remix created and sent to review.';
  }

  async function createTheme() {
    status = 'Submitting theme for review...';
    const payload = new URLSearchParams({
      name: createName,
      slug: createSlug,
      description: createDescription,
      backgroundColor: createBackgroundColor,
      surfaceColor: createSurfaceColor,
      textColor: createTextColor,
      accentColor: createAccentColor,
      headingFont: createHeadingFont,
      bodyFont: createBodyFont,
      headerTemplate: createHeaderTemplate,
      bodyTemplate: createBodyTemplate,
      footerTemplate: createFooterTemplate,
      customCss: createCustomCss,
      priceCents: createPriceCents
    });
    const response = await fetch(`${backendBaseUrl}/api/themes/create`, {
      method: 'POST',
      credentials: 'include',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      body: payload.toString()
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Theme submission failed. Log in first.');
      return;
    }
    createName = '';
    createSlug = '';
    createDescription = '';
    createHeaderTemplate = '';
    createBodyTemplate = '';
    createFooterTemplate = '';
    createCustomCss = '';
    await fetchMarketplace();
    status = 'Theme submitted for admin review.';
  }

  async function saveReview() {
    if (!selectedItem) return;
    status = 'Saving review...';
    const payload = new URLSearchParams({
      rating: reviewRating,
      review: reviewText
    });
    const response = await fetch(`${backendBaseUrl}/api/theme/${selectedItem.theme.id}/review`, {
      method: 'POST',
      credentials: 'include',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      body: payload.toString()
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Review save failed.');
      return;
    }
    await fetchMarketplace();
    status = 'Review saved.';
  }

  async function reportTheme() {
    if (!selectedItem) return;
    status = 'Sending report...';
    const payload = new URLSearchParams({
      reason: reportReason,
      details: reportDetails
    });
    const response = await fetch(`${backendBaseUrl}/api/theme/${selectedItem.theme.id}/report`, {
      method: 'POST',
      credentials: 'include',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      body: payload.toString()
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Report failed.');
      return;
    }
    reportReason = '';
    reportDetails = '';
    await fetchMarketplace();
    status = 'Theme reported.';
  }

  onMount(() => {
    void (async () => {
      try {
        await fetchMarketplace();
      } catch (error) {
        status = error instanceof Error ? error.message : 'Failed to load marketplace.';
      } finally {
        loading = false;
      }
    })();
  });

  $effect(() => {
    if (!selectedItem) return;
    reviewRating = String(selectedItem.myReview?.rating ?? 5);
    reviewText = selectedItem.myReview?.review ?? '';
  });
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Theme Marketplace</p>
    <h1 class="hero-title">Buy, remix, and publish blog identities.</h1>
    <p class="lede">
      Themes are reusable writing spaces. Free themes can be used immediately; paid themes are recorded as purchases
      after order confirmation before they can be applied or remixed.
    </p>
    <div class="meta-row">
      <span class="chip">{status}</span>
    </div>
    <div class="action-row">
      <a class="action-link" href={`${base}/themes/mine`}>My themes</a>
      <a class="action-link" href={`${base}/themes/orders`}>Order history</a>
      <a class="action-link" href={`${base}/themes/stats`}>Sales stats</a>
      <a class="action-link" href={`${base}/themes/payouts`}>Payouts</a>
    </div>
  </div>

  {#if loading}
    <section class="panel-card">
      <p class="copy">Loading themes...</p>
    </section>
  {:else}
    <section class="panel-card stack">
      <div class="studio-upload-grid grid">
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Search</span>
          <input class="search-input studio-input" bind:value={query} placeholder="Search name, slug, description" />
        </label>
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Filter</span>
          <select class="studio-select" bind:value={ownershipFilter}>
            <option value="all">All themes</option>
            <option value="owned">Owned</option>
            <option value="free">Free</option>
            <option value="paid">Paid</option>
            <option value="remix">Remix</option>
          </select>
        </label>
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Sort</span>
          <select class="studio-select" bind:value={sortMode}>
            <option value="latest">Latest</option>
            <option value="rating">Top rated</option>
            <option value="price-asc">Price low to high</option>
            <option value="price-desc">Price high to low</option>
          </select>
        </label>
      </div>
      <p class="copy">{filteredItems.length} themes match the current filters.</p>
    </section>

    {#if selectedItem}
      <section class="panel-card stack themed-page" style={buildThemeStyle(selectedItem.theme)}>
        <div class="meta-row">
          <div>
            <p class="eyebrow">Selected theme</p>
            <h2 class="section-title">{selectedItem.theme.name}</h2>
          </div>
          <div class="meta-row">
            <span class="chip">{selectedItem.theme.priceCents === 0 ? 'Free' : `$${(selectedItem.theme.priceCents / 100).toFixed(2)}`}</span>
            <span class="chip">{selectedItem.rating?.averageRating?.toFixed(1) ?? '0.0'} ★</span>
            <span class="chip">{selectedItem.rating?.ratingCount ?? 0} reviews</span>
          </div>
        </div>
        <p class="lede">{selectedItem.theme.description ?? selectedItem.theme.slug}</p>
        <div class="action-row">
          {#if selectedItem.owned}
            <button class="action-link" type="button" onclick={() => forkTheme(selectedItem)}>Remix</button>
          {:else}
            <button class="action-link studio-primary-action" type="button" onclick={() => purchaseTheme(selectedItem.theme.id)}>
              {selectedItem.theme.priceCents === 0 ? 'Get theme' : 'Buy theme'}
            </button>
          {/if}
        </div>

        <div class="studio-upload-grid grid">
          <label class="stack" style="gap: 8px;">
            <span class="eyebrow">Your rating</span>
            <select class="studio-select" bind:value={reviewRating}>
              <option value="5">5</option>
              <option value="4">4</option>
              <option value="3">3</option>
              <option value="2">2</option>
              <option value="1">1</option>
            </select>
          </label>
          <label class="stack theme-submit-wide" style="gap: 8px;">
            <span class="eyebrow">Review</span>
            <textarea class="studio-image-note" bind:value={reviewText} placeholder="What worked well in this theme?"></textarea>
          </label>
        </div>
        <div class="action-row">
          <button class="action-link" type="button" onclick={saveReview}>Save review</button>
        </div>

        <div class="studio-upload-grid grid">
          <label class="stack" style="gap: 8px;">
            <span class="eyebrow">Report reason</span>
            <input class="search-input studio-input" bind:value={reportReason} placeholder="copyright, abuse, malware..." />
          </label>
          <label class="stack theme-submit-wide" style="gap: 8px;">
            <span class="eyebrow">Details</span>
            <textarea class="studio-image-note" bind:value={reportDetails} placeholder="Add context for the admin team"></textarea>
          </label>
        </div>
        <div class="action-row">
          <button class="action-link" type="button" onclick={reportTheme} disabled={selectedItem.reportedByMe}>
            {selectedItem.reportedByMe ? 'Already reported' : 'Report theme'}
          </button>
        </div>
      </section>
    {/if}

    <section class="panel-card stack">
      <div>
        <p class="eyebrow">Sell your theme</p>
        <h2 class="section-title">Submit a theme to the marketplace.</h2>
        <p class="copy">New themes are created in review status. An admin can publish them from the theme admin page.</p>
      </div>
      <div class="theme-submit-grid">
        <label>
          <span>Name</span>
          <input class="search-input studio-input" bind:value={createName} placeholder="Midnight Notes" />
        </label>
        <label>
          <span>Slug</span>
          <input class="search-input studio-input" bind:value={createSlug} placeholder="midnight-notes" />
        </label>
        <label class="theme-submit-wide">
          <span>Description</span>
          <textarea class="studio-image-note" bind:value={createDescription}></textarea>
        </label>
        <label>
          <span>Background</span>
          <input class="search-input studio-input" bind:value={createBackgroundColor} />
        </label>
        <label>
          <span>Surface</span>
          <input class="search-input studio-input" bind:value={createSurfaceColor} />
        </label>
        <label>
          <span>Text</span>
          <input class="search-input studio-input" bind:value={createTextColor} />
        </label>
        <label>
          <span>Accent</span>
          <input class="search-input studio-input" bind:value={createAccentColor} />
        </label>
        <label>
          <span>Heading font</span>
          <input class="search-input studio-input" bind:value={createHeadingFont} />
        </label>
        <label>
          <span>Body font</span>
          <input class="search-input studio-input" bind:value={createBodyFont} />
        </label>
        <label>
          <span>Price cents</span>
          <input class="search-input studio-input" bind:value={createPriceCents} />
        </label>
        <label class="theme-submit-wide">
          <span>Header template</span>
          <textarea
            class="studio-image-note"
            bind:value={createHeaderTemplate}
            placeholder={headerTemplatePlaceholder}
          ></textarea>
        </label>
        <label class="theme-submit-wide">
          <span>Body template</span>
          <textarea
            class="studio-image-note"
            bind:value={createBodyTemplate}
            placeholder={bodyTemplatePlaceholder}
          ></textarea>
        </label>
        <label class="theme-submit-wide">
          <span>Footer template</span>
          <textarea
            class="studio-image-note"
            bind:value={createFooterTemplate}
            placeholder={footerTemplatePlaceholder}
          ></textarea>
        </label>
        <label class="theme-submit-wide">
          <span>Custom CSS</span>
          <textarea
            class="studio-image-note"
            bind:value={createCustomCss}
            placeholder={customCssPlaceholder}
          ></textarea>
        </label>
      </div>
      <div class="action-row">
        <button class="action-link studio-primary-action" type="button" onclick={createTheme}>Submit theme</button>
      </div>
    </section>

    <div class="theme-market-grid">
      {#each filteredItems as item}
        <article class="theme-market-card themed-page" style={buildThemeStyle(item.theme)}>
          <div class="theme-market-preview">
            <p class="eyebrow">{item.theme.status}</p>
            <h2 class="post-title">{item.theme.name}</h2>
            <p class="lede">{item.theme.description ?? item.theme.slug}</p>
            <div class="meta-row">
              <span class="chip">{item.theme.priceCents === 0 ? 'Free' : `$${(item.theme.priceCents / 100).toFixed(2)}`}</span>
              <span class="chip">{item.rating?.averageRating?.toFixed(1) ?? '0.0'} ★</span>
              {#if item.theme.parentId}
                <span class="chip">Remix</span>
              {/if}
              {#if item.owned}
                <span class="chip chip-live">Owned</span>
              {/if}
            </div>
          </div>
          <div class="action-row">
            <button class="action-link" type="button" onclick={() => (selectedThemeId = item.theme.id)}>Preview</button>
            {#if item.owned}
              <button class="action-link" type="button" onclick={() => forkTheme(item)}>Remix</button>
            {:else}
              <button class="action-link studio-primary-action" type="button" onclick={() => purchaseTheme(item.theme.id)}>
                {item.theme.priceCents === 0 ? 'Get theme' : 'Buy theme'}
              </button>
            {/if}
          </div>
        </article>
      {/each}
    </div>
  {/if}
</section>
