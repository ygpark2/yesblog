<script lang="ts">
  import { base } from '$app/paths';
  import { onMount } from 'svelte';
  import { apiFetch } from '$lib/api';
  import { buildThemeStyle } from '$lib/theme';
  import type { ApiTheme, ThemeMarketplaceItem } from '$lib/types';

  let items = $state<ThemeMarketplaceItem[]>([]);
  let status = $state('Loading your theme library...');
  const headerTemplatePlaceholder = '<header><p>{{author.name}}</p><h1>{{post.title}}</h1></header>';
  const bodyTemplatePlaceholder = "<article class='prose article-prose'>{{post.content}}</article>";
  const footerTemplatePlaceholder = '<footer>Published by {{author.name}}</footer>';
  const customCssPlaceholder = '.theme-template-header { text-align: center; }';

  let editingThemeId = $state<number | null>(null);
  let saving = $state(false);
  let themeName = $state('');
  let themeSlug = $state('');
  let themeDescription = $state('');
  let themeBackgroundColor = $state('#f8f0dc');
  let themeSurfaceColor = $state('#fffef7');
  let themeTextColor = $state('#111111');
  let themeAccentColor = $state('#ffe11a');
  let themeHeadingFont = $state('Cormorant Garamond');
  let themeBodyFont = $state('Space Grotesk');
  let themeHeaderTemplate = $state('');
  let themeBodyTemplate = $state('');
  let themeFooterTemplate = $state('');
  let themeCustomCss = $state('');
  let themePriceCents = $state('0');

  const authored = $derived(items.filter((item) => item.isAuthor));
  const owned = $derived(items.filter((item) => item.owned && !item.isAuthor));
  const remixes = $derived(items.filter((item) => item.isAuthor && item.theme.parentId));

  async function readErrorMessage(response: Response, fallback: string) {
    try {
      const payload = await response.json();
      if (typeof payload?.message === 'string' && payload.message.trim()) return payload.message;
    } catch {
      return fallback;
    }
    return fallback;
  }

  async function fetchThemes() {
    const data = await apiFetch<{ items: ThemeMarketplaceItem[] }>(fetch, '/api/me/themes');
    items = data.items;
    status = `${data.items.filter((item) => item.isAuthor).length} authored · ${data.items.filter((item) => item.owned && !item.isAuthor).length} owned`;
  }

  onMount(() => {
    void (async () => {
      try {
        await fetchThemes();
      } catch (_error) {
        status = 'Log in first to see your theme library.';
      }
    })();
  });

  function resetForm() {
    editingThemeId = null;
    themeName = '';
    themeSlug = '';
    themeDescription = '';
    themeBackgroundColor = '#f8f0dc';
    themeSurfaceColor = '#fffef7';
    themeTextColor = '#111111';
    themeAccentColor = '#ffe11a';
    themeHeadingFont = 'Cormorant Garamond';
    themeBodyFont = 'Space Grotesk';
    themeHeaderTemplate = '';
    themeBodyTemplate = '';
    themeFooterTemplate = '';
    themeCustomCss = '';
    themePriceCents = '0';
  }

  function startEdit(theme: ApiTheme) {
    editingThemeId = theme.id;
    themeName = theme.name;
    themeSlug = theme.slug;
    themeDescription = theme.description ?? '';
    themeBackgroundColor = theme.backgroundColor;
    themeSurfaceColor = theme.surfaceColor;
    themeTextColor = theme.textColor;
    themeAccentColor = theme.accentColor;
    themeHeadingFont = theme.headingFont ?? '';
    themeBodyFont = theme.bodyFont ?? '';
    themeHeaderTemplate = theme.headerTemplate ?? '';
    themeBodyTemplate = theme.bodyTemplate ?? '';
    themeFooterTemplate = theme.footerTemplate ?? '';
    themeCustomCss = theme.customCss ?? '';
    themePriceCents = String(theme.priceCents ?? 0);
    status = `Editing ${theme.name}. Saving will resubmit it for review.`;
  }

  async function saveTheme() {
    if (!editingThemeId) return;
    saving = true;
    status = 'Saving theme and resubmitting for review...';
    const payload = new URLSearchParams({
      name: themeName,
      slug: themeSlug,
      description: themeDescription,
      backgroundColor: themeBackgroundColor,
      surfaceColor: themeSurfaceColor,
      textColor: themeTextColor,
      accentColor: themeAccentColor,
      headingFont: themeHeadingFont,
      bodyFont: themeBodyFont,
      headerTemplate: themeHeaderTemplate,
      bodyTemplate: themeBodyTemplate,
      footerTemplate: themeFooterTemplate,
      customCss: themeCustomCss,
      priceCents: themePriceCents
    });
    const response = await fetch(`/api/theme/${editingThemeId}/update`, {
      method: 'POST',
      credentials: 'include',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      body: payload.toString()
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Theme update failed.');
      saving = false;
      return;
    }
    resetForm();
    await fetchThemes();
    saving = false;
    status = 'Theme updated and moved back to review.';
  }

  async function deleteTheme(theme: ApiTheme) {
    if (!window.confirm(`Delete ${theme.name}? This cannot be undone.`)) return;
    status = 'Deleting theme...';
    const response = await fetch(`/api/theme/${theme.id}/delete`, {
      method: 'POST',
      credentials: 'include'
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Theme delete failed.');
      return;
    }
    if (editingThemeId === theme.id) resetForm();
    await fetchThemes();
    status = 'Theme deleted.';
  }
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">My Themes</p>
    <h1 class="hero-title">Your authored, owned, and remixed themes.</h1>
    <p class="lede">{status}</p>
    <div class="action-row">
      <a class="action-link" href={`${base}/themes`}>Open marketplace</a>
      <a class="action-link" href={`${base}/themes/orders`}>Order history</a>
      <a class="action-link" href={`${base}/themes/stats`}>Sales stats</a>
      <a class="action-link" href={`${base}/themes/payouts`}>Payouts</a>
    </div>
  </div>

  <section class="panel-card stack">
    <div class="meta-row">
      <div>
        <p class="eyebrow">Studio</p>
        <h2 class="section-title">Authored themes</h2>
      </div>
      <span class="chip">{authored.length}</span>
    </div>
    <div class="theme-market-grid">
      {#each authored as item}
        <article class="theme-market-card themed-page" style={buildThemeStyle(item.theme)}>
          <p class="eyebrow">{item.theme.status}</p>
          <h3 class="post-title">{item.theme.name}</h3>
          <p class="lede">{item.theme.description ?? item.theme.slug}</p>
          <div class="meta-row">
            <span class="chip">{item.theme.priceCents === 0 ? 'Free' : `$${(item.theme.priceCents / 100).toFixed(2)}`}</span>
            {#if item.theme.parentId}
              <span class="chip">Remix</span>
            {/if}
          </div>
          <div class="action-row">
            <button class="action-link" type="button" onclick={() => startEdit(item.theme)}>Edit</button>
            <button class="action-link" type="button" onclick={() => deleteTheme(item.theme)}>Delete</button>
          </div>
        </article>
      {/each}
    </div>
  </section>

  {#if editingThemeId}
    <section class="panel-card stack">
      <div class="meta-row">
        <div>
          <p class="eyebrow">Editor</p>
          <h2 class="section-title">Edit authored theme</h2>
        </div>
        <span class="chip">Resubmits for review</span>
      </div>

      <div class="theme-submit-grid">
        <label>
          <span>Name</span>
          <input class="search-input studio-input" bind:value={themeName} />
        </label>
        <label>
          <span>Slug</span>
          <input class="search-input studio-input" bind:value={themeSlug} />
        </label>
        <label class="theme-submit-wide">
          <span>Description</span>
          <textarea class="studio-image-note" bind:value={themeDescription}></textarea>
        </label>
        <label>
          <span>Background</span>
          <input class="search-input studio-input" bind:value={themeBackgroundColor} />
        </label>
        <label>
          <span>Surface</span>
          <input class="search-input studio-input" bind:value={themeSurfaceColor} />
        </label>
        <label>
          <span>Text</span>
          <input class="search-input studio-input" bind:value={themeTextColor} />
        </label>
        <label>
          <span>Accent</span>
          <input class="search-input studio-input" bind:value={themeAccentColor} />
        </label>
        <label>
          <span>Heading font</span>
          <input class="search-input studio-input" bind:value={themeHeadingFont} />
        </label>
        <label>
          <span>Body font</span>
          <input class="search-input studio-input" bind:value={themeBodyFont} />
        </label>
        <label>
          <span>Price cents</span>
          <input class="search-input studio-input" bind:value={themePriceCents} />
        </label>
        <label class="theme-submit-wide">
          <span>Header template</span>
          <textarea class="studio-image-note" bind:value={themeHeaderTemplate} placeholder={headerTemplatePlaceholder}></textarea>
        </label>
        <label class="theme-submit-wide">
          <span>Body template</span>
          <textarea class="studio-image-note" bind:value={themeBodyTemplate} placeholder={bodyTemplatePlaceholder}></textarea>
        </label>
        <label class="theme-submit-wide">
          <span>Footer template</span>
          <textarea class="studio-image-note" bind:value={themeFooterTemplate} placeholder={footerTemplatePlaceholder}></textarea>
        </label>
        <label class="theme-submit-wide">
          <span>Custom CSS</span>
          <textarea class="studio-image-note" bind:value={themeCustomCss} placeholder={customCssPlaceholder}></textarea>
        </label>
      </div>

      <div class="action-row">
        <button class="action-link studio-primary-action" type="button" disabled={saving} onclick={saveTheme}>
          {saving ? 'Saving...' : 'Save and resubmit'}
        </button>
        <button class="action-link" type="button" onclick={resetForm}>Cancel</button>
      </div>
    </section>
  {/if}

  <section class="panel-card stack">
    <div class="meta-row">
      <div>
        <p class="eyebrow">Collection</p>
        <h2 class="section-title">Purchased or free themes</h2>
      </div>
      <span class="chip">{owned.length}</span>
    </div>
    <div class="theme-market-grid">
      {#each owned as item}
        <article class="theme-market-card themed-page" style={buildThemeStyle(item.theme)}>
          <p class="eyebrow">Owned</p>
          <h3 class="post-title">{item.theme.name}</h3>
          <p class="lede">{item.theme.description ?? item.theme.slug}</p>
        </article>
      {/each}
    </div>
  </section>

  <section class="panel-card stack">
    <div class="meta-row">
      <div>
        <p class="eyebrow">Remix chain</p>
        <h2 class="section-title">Your remixed themes</h2>
      </div>
      <span class="chip">{remixes.length}</span>
    </div>
    <div class="theme-market-grid">
      {#each remixes as item}
        <article class="theme-market-card themed-page" style={buildThemeStyle(item.theme)}>
          <p class="eyebrow">Remix</p>
          <h3 class="post-title">{item.theme.name}</h3>
          <p class="lede">{item.theme.description ?? item.theme.slug}</p>
        </article>
      {/each}
    </div>
  </section>
</section>
