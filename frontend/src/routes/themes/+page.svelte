<script lang="ts">
  import { base } from '$app/paths';
  import { apiFormPost, apiGet } from '$lib/api';
  import { buildThemeStyle } from '$lib/theme';
  import type { ApiSession, ThemeMarketplaceItem } from '$lib/types';
  import { onMount } from 'svelte';
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
  let currentThemeId = $state<number | null>(null);
  let authenticated = $state(false);
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

  async function fetchMarketplace() {
    const data = await apiGet<{ items: ThemeMarketplaceItem[] }>('/api/themes/marketplace');
    items = data.items;
    selectedThemeId = selectedThemeId && data.items.some((item) => item.theme.id === selectedThemeId)
      ? selectedThemeId
      : data.items[0]?.theme.id ?? null;
    status = `${items.length} themes available`;
  }

  async function refreshSession() {
    const session = await apiGet<ApiSession>('/api/session');
    authenticated = session.authenticated;
    currentThemeId = typeof session.user?.themeId === 'number' ? session.user.themeId : null;
  }

  async function applyTheme(themeId: number | null) {
    status = themeId ? 'Applying theme...' : 'Clearing theme...';
    try {
      const payload = new URLSearchParams({ themeId: themeId ? String(themeId) : '' });
      await apiFormPost('/api/me/theme', payload);
      currentThemeId = themeId;
      status = themeId ? 'Theme applied to your blog.' : 'Theme cleared.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Theme apply failed.';
    }
  }

  async function purchaseTheme(themeId: number) {
    status = 'Creating order...';
    try {
      const data = await apiFormPost<{ requiresConfirmation?: boolean }>(
        `/api/theme/${themeId}/purchase`,
        new URLSearchParams()
      );
      if (data.requiresConfirmation) {
        status = 'Order created. Payment must be verified by an administrator before the theme is unlocked.';
      } else {
        await applyTheme(themeId);
      }
      await fetchMarketplace();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Purchase failed. Log in first.';
    }
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
    try {
      await apiFormPost(`/api/theme/${item.theme.id}/fork`, payload);
      await fetchMarketplace();
      status = 'Remix created and sent to review.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Remix failed. Purchase or log in first.';
    }
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
    try {
      await apiFormPost('/api/themes/create', payload);
      createName = '';
      createSlug = '';
      createDescription = '';
      createHeaderTemplate = '';
      createBodyTemplate = '';
      createFooterTemplate = '';
      createCustomCss = '';
      await fetchMarketplace();
      status = 'Theme submitted for admin review.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Theme submission failed. Log in first.';
    }
  }

  async function saveReview() {
    if (!selectedItem) return;
    status = 'Saving review...';
    const payload = new URLSearchParams({
      rating: reviewRating,
      review: reviewText
    });
    try {
      await apiFormPost(`/api/theme/${selectedItem.theme.id}/review`, payload);
      await fetchMarketplace();
      status = 'Review saved.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Review save failed.';
    }
  }

  async function reportTheme() {
    if (!selectedItem) return;
    status = 'Sending report...';
    const payload = new URLSearchParams({
      reason: reportReason,
      details: reportDetails
    });
    try {
      await apiFormPost(`/api/theme/${selectedItem.theme.id}/report`, payload);
      reportReason = '';
      reportDetails = '';
      await fetchMarketplace();
      status = 'Theme reported.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Report failed.';
    }
  }

  onMount(() => {
    void (async () => {
      try {
        await Promise.all([fetchMarketplace(), refreshSession()]);
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
      Themes are reusable writing spaces. Free themes can be applied immediately, while paid themes can be applied after purchase confirmation.
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
          {#if selectedItem.isAuthor}
            <button class="action-link" type="button" onclick={() => forkTheme(selectedItem)}>Remix</button>
          {:else if selectedItem.owned}
            <button class="action-link studio-primary-action" type="button" onclick={() => applyTheme(selectedItem.theme.id)}>
              {currentThemeId === selectedItem.theme.id ? 'Applied' : 'Apply to blog'}
            </button>
          {:else}
            <button class="action-link studio-primary-action" type="button" onclick={() => purchaseTheme(selectedItem.theme.id)}>
              {selectedItem.theme.priceCents === 0 ? 'Apply free theme' : 'Buy theme'}
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
              {#if currentThemeId === item.theme.id}
                <span class="chip chip-live">Applied</span>
              {/if}
            </div>
          </div>
          <div class="action-row">
            <button class="action-link" type="button" onclick={() => (selectedThemeId = item.theme.id)}>Preview</button>
            {#if item.isAuthor}
              <button class="action-link" type="button" onclick={() => forkTheme(item)}>Remix</button>
            {:else if item.owned}
              <button class="action-link studio-primary-action" type="button" onclick={() => applyTheme(item.theme.id)}>
                {currentThemeId === item.theme.id ? 'Applied' : 'Apply'}
              </button>
            {:else}
              <button class="action-link studio-primary-action" type="button" onclick={() => purchaseTheme(item.theme.id)}>
                {item.theme.priceCents === 0 ? 'Apply free theme' : 'Buy theme'}
              </button>
            {/if}
          </div>
        </article>
      {/each}
    </div>
  {/if}
</section>
