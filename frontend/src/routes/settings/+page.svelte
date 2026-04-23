<script lang="ts">
  import { onMount } from 'svelte';
  import { apiFetch, apiFormPost } from '$lib/api';
  import { buildThemeStyle, parseThemeOverrides, stringifyThemeOverrides } from '$lib/theme';
  import type { ApiMe, ApiTheme, ApiUser, CustomDomainItem } from '$lib/types';

  let ident = $state('');
  let displayName = $state('');
  let bio = $state('');
  let status = $state('Loading settings…');
  let saving = $state(false);
  let themeStatus = $state('Loading themes...');
  let savingTheme = $state(false);
  let themes = $state<ApiTheme[]>([]);
  let plan = $state('free');
  let planStatus = $state('Choose a plan for advanced publishing tools.');
  let savingPlan = $state(false);
  let domains = $state<CustomDomainItem[]>([]);
  let customDomain = $state('');
  let domainStatus = $state('Writer Pro unlocks custom domains.');
  let savingDomain = $state(false);
  let selectedThemeId = $state('');
  let backgroundColor = $state('');
  let surfaceColor = $state('');
  let textColor = $state('');
  let accentColor = $state('');
  let headingFont = $state('');
  let bodyFont = $state('');

  const selectedTheme = $derived(themes.find((theme) => String(theme.id) === selectedThemeId) ?? null);
  const previewThemeStyle = $derived(
    buildThemeStyle(selectedTheme, stringifyThemeOverrides({
      backgroundColor,
      surfaceColor,
      textColor,
      accentColor,
      headingFont,
      bodyFont
    }))
  );

  onMount(() => {
    void (async () => {
      try {
        const [me, themeData] = await Promise.all([
          apiFetch<ApiMe>(fetch, '/api/me'),
          apiFetch<{ items: ApiTheme[] }>(fetch, '/api/themes')
        ]);
        themes = themeData.items;
        ident = me.user.ident;
        displayName = me.user.displayName;
        bio = me.user.bio ?? '';
        plan = me.user.plan ?? 'free';
        domains = me.domains ?? [];
        customDomain = domains[0]?.domain ?? '';
        selectedThemeId = me.user.themeId ? String(me.user.themeId) : '';
        hydrateThemeFields(me.user);
        status = 'Update your profile details';
        themeStatus = themes.length ? 'Choose a base theme, then override the parts you want.' : 'No active themes are available.';
        planStatus = plan === 'free' ? 'Upgrade to unlock scheduling, private visibility, and custom domains.' : 'Pro publishing features are active.';
        domainStatus = domains.length
          ? `Custom domain ${domains[0].domain} is ${domains[0].status}.`
          : 'No custom domain connected.';
      } catch (_error) {
        status = 'Log in first to edit settings.';
        themeStatus = 'Log in first to edit themes.';
        planStatus = 'Log in first to manage plans.';
        domainStatus = 'Log in first to manage domains.';
      }
    })();
  });

  function hydrateThemeFields(user: ApiUser) {
    const overrides = parseThemeOverrides(user.themeOverrides);
    backgroundColor = overrides.backgroundColor ?? user.theme?.backgroundColor ?? '';
    surfaceColor = overrides.surfaceColor ?? user.theme?.surfaceColor ?? '';
    textColor = overrides.textColor ?? user.theme?.textColor ?? '';
    accentColor = overrides.accentColor ?? user.theme?.accentColor ?? '';
    headingFont = overrides.headingFont ?? user.theme?.headingFont ?? '';
    bodyFont = overrides.bodyFont ?? user.theme?.bodyFont ?? '';
  }

  function useSelectedThemeDefaults() {
    if (!selectedTheme) return;
    backgroundColor = selectedTheme.backgroundColor;
    surfaceColor = selectedTheme.surfaceColor;
    textColor = selectedTheme.textColor;
    accentColor = selectedTheme.accentColor;
    headingFont = selectedTheme.headingFont ?? '';
    bodyFont = selectedTheme.bodyFont ?? '';
  }

  async function saveSettings() {
    saving = true;
    status = 'Saving settings…';
    try {
      const payload = new URLSearchParams({
        ident,
        displayName,
        bio
      });
      const data = await apiFormPost<{ user: ApiUser }>('/api/me/update', payload);
      ident = data.user.ident;
      displayName = data.user.displayName;
      bio = data.user.bio ?? '';
      status = 'Settings saved';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Settings update failed';
    } finally {
      saving = false;
    }
  }

  async function saveThemeSettings() {
    savingTheme = true;
    themeStatus = 'Saving theme...';
    try {
      const payload = new URLSearchParams({
        themeId: selectedThemeId,
        themeOverrides: stringifyThemeOverrides({
          backgroundColor,
          surfaceColor,
          textColor,
          accentColor,
          headingFont,
          bodyFont
        })
      });
      const data = await apiFormPost<{ user: ApiUser }>('/api/me/theme', payload);
      selectedThemeId = data.user.themeId ? String(data.user.themeId) : selectedThemeId;
      hydrateThemeFields(data.user);
      themeStatus = 'Theme saved';
    } catch (error) {
      themeStatus = error instanceof Error ? error.message : 'Theme update failed';
    } finally {
      savingTheme = false;
    }
  }

  async function savePlan() {
    savingPlan = true;
    planStatus = 'Updating plan...';
    try {
      const payload = new URLSearchParams({ plan });
      const data = await apiFormPost<{ user: ApiUser }>('/api/me/plan', payload);
      plan = data.user.plan ?? plan;
      planStatus = plan === 'free' ? 'Plan set to free.' : `${plan} activated.`;
    } catch (error) {
      planStatus = error instanceof Error ? error.message : 'Plan update failed';
    } finally {
      savingPlan = false;
    }
  }

  async function saveDomain(mode: 'save' | 'delete' = 'save') {
    savingDomain = true;
    domainStatus = mode === 'delete' ? 'Removing custom domain...' : 'Saving custom domain...';
    try {
      const payload = new URLSearchParams({
        mode,
        domain: mode === 'delete' ? '' : customDomain
      });
      const data = await apiFormPost<{ items: CustomDomainItem[] }>('/api/me/domains', payload);
      domains = data.items;
      customDomain = data.items[0]?.domain ?? '';
      domainStatus =
        data.items.length > 0
          ? `Custom domain ${data.items[0].domain} is ${data.items[0].status}.`
          : 'Custom domain removed.';
    } catch (error) {
      domainStatus = error instanceof Error ? error.message : 'Domain update failed';
    } finally {
      savingDomain = false;
    }
  }
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Settings</p>
    <h1 class="hero-title">Update your writer profile.</h1>
    <p class="lede">{status}</p>
  </div>

  <section class="panel-card stack">
    <label class="stack" style="gap: 8px;">
      <span class="eyebrow">Username</span>
      <input class="search-input studio-input" bind:value={ident} />
    </label>

    <label class="stack" style="gap: 8px;">
      <span class="eyebrow">Display name</span>
      <input class="search-input studio-input" bind:value={displayName} />
    </label>

    <label class="stack" style="gap: 8px;">
      <span class="eyebrow">Bio</span>
      <textarea class="studio-textarea" bind:value={bio}></textarea>
    </label>

    <div class="action-row">
      <button class="action-link studio-primary-action" type="button" disabled={saving} onclick={saveSettings}>
        {saving ? 'Saving…' : 'Save settings'}
      </button>
    </div>
  </section>

  <section class="panel-card stack">
    <div class="meta-row">
      <div>
        <p class="eyebrow">Plan</p>
        <h2 class="section-title">Choose your publishing access.</h2>
      </div>
      <span class="chip">{planStatus}</span>
    </div>

    <label class="stack" style="gap: 8px;">
      <span class="eyebrow">Current plan</span>
      <select class="studio-select" bind:value={plan}>
        <option value="free">Free</option>
        <option value="writer-pro">Writer Pro</option>
        <option value="designer-pro">Designer Pro</option>
      </select>
    </label>

    <div class="action-row">
      <button class="action-link studio-primary-action" type="button" disabled={savingPlan} onclick={savePlan}>
        {savingPlan ? 'Updating…' : 'Update plan'}
      </button>
    </div>
  </section>

  <section class="panel-card stack">
    <div class="meta-row">
      <div>
        <p class="eyebrow">Custom domain</p>
        <h2 class="section-title">Point your own domain to the blog.</h2>
      </div>
      <span class="chip">{domainStatus}</span>
    </div>

    <label class="stack" style="gap: 8px;">
      <span class="eyebrow">Domain</span>
      <input class="search-input studio-input" bind:value={customDomain} placeholder="blog.example.com" />
    </label>

    {#if domains[0]}
      <article class="hero-card stack">
        <p class="eyebrow">Verification</p>
        <h3 class="post-title">{domains[0].domain}</h3>
        <p class="lede">Create a TXT record with this token to verify ownership.</p>
        <div class="meta-row">
          <span class="chip">{domains[0].verificationToken}</span>
          <span class="chip">{domains[0].status}</span>
        </div>
      </article>
    {/if}

    <div class="action-row">
      <button class="action-link studio-primary-action" type="button" disabled={savingDomain} onclick={() => saveDomain('save')}>
        {savingDomain ? 'Saving…' : 'Save domain'}
      </button>
      {#if domains.length}
        <button class="action-link" type="button" disabled={savingDomain} onclick={() => saveDomain('delete')}>
          Remove domain
        </button>
      {/if}
    </div>
  </section>

  <section class="panel-card stack">
    <div class="meta-row">
      <div>
        <p class="eyebrow">Theme</p>
        <h2 class="section-title">Personalize your blog theme.</h2>
      </div>
      <span class="chip">{themeStatus}</span>
    </div>

    <label class="stack" style="gap: 8px;">
      <span class="eyebrow">Base theme</span>
      <select class="studio-select" bind:value={selectedThemeId} onchange={useSelectedThemeDefaults}>
        <option value="">Use site default</option>
        {#each themes as theme}
          <option value={theme.id}>{theme.name}</option>
        {/each}
      </select>
    </label>

    <div class="studio-upload-grid grid">
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Background color</span>
        <input class="search-input studio-input" bind:value={backgroundColor} placeholder="#f8f0dc" />
      </label>
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Surface color</span>
        <input class="search-input studio-input" bind:value={surfaceColor} placeholder="#fffef7" />
      </label>
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Text color</span>
        <input class="search-input studio-input" bind:value={textColor} placeholder="#111111" />
      </label>
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Accent color</span>
        <input class="search-input studio-input" bind:value={accentColor} placeholder="#ffe11a" />
      </label>
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Heading font</span>
        <input class="search-input studio-input" bind:value={headingFont} placeholder="Cormorant Garamond" />
      </label>
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Body font</span>
        <input class="search-input studio-input" bind:value={bodyFont} placeholder="Space Grotesk" />
      </label>
    </div>

    <article class="hero-card stack themed-page" style={previewThemeStyle}>
      <p class="eyebrow">Preview</p>
      <h3 class="post-title">Your themed writing surface</h3>
      <p class="lede">This preview uses your selected base theme plus custom overrides.</p>
      <div class="meta-row">
        <span class="chip">Accent chip</span>
        <span class="chip">Readable text</span>
      </div>
    </article>

    <div class="action-row">
      <button class="action-link studio-primary-action" type="button" disabled={savingTheme} onclick={saveThemeSettings}>
        {savingTheme ? 'Saving...' : 'Save theme'}
      </button>
    </div>
  </section>
</section>
