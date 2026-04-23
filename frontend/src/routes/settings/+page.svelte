<script lang="ts">
  import { onMount } from 'svelte';
  import { apiFetch, apiFormPost } from '$lib/api';
  import { buildThemeStyle, parseThemeOverrides, stringifyThemeOverrides } from '$lib/theme';
  import type { ApiMe, ApiMeMemberships, ApiTheme, ApiUser, CustomDomainItem, MembershipItem, MembershipOrderItem } from '$lib/types';

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
  let membershipPriceCents = $state(0);
  let membershipStatus = $state('Writer Pro unlocks paid memberships.');
  let receivedMemberships = $state<MembershipItem[]>([]);
  let outgoingMemberships = $state<MembershipItem[]>([]);
  let membershipOrders = $state<MembershipOrderItem[]>([]);
  let membershipActionId = $state<number | null>(null);
  let membershipOrderActionId = $state<number | null>(null);
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
        const [me, themeData, memberships, membershipOrdersData] = await Promise.all([
          apiFetch<ApiMe>(fetch, '/api/me'),
          apiFetch<{ items: ApiTheme[] }>(fetch, '/api/themes'),
          apiFetch<ApiMeMemberships>(fetch, '/api/me/memberships'),
          apiFetch<{ items: MembershipOrderItem[] }>(fetch, '/api/me/membership/orders')
        ]);
        themes = themeData.items;
        ident = me.user.ident;
        displayName = me.user.displayName;
        bio = me.user.bio ?? '';
        plan = me.user.plan ?? 'free';
        membershipPriceCents = me.user.membershipPriceCents ?? 0;
        domains = me.domains ?? [];
        receivedMemberships = memberships.received ?? [];
        outgoingMemberships = memberships.outgoing ?? [];
        membershipOrders = membershipOrdersData.items ?? [];
        customDomain = domains[0]?.domain ?? '';
        selectedThemeId = me.user.themeId ? String(me.user.themeId) : '';
        hydrateThemeFields(me.user);
        status = 'Update your profile details';
        themeStatus = themes.length ? 'Choose a base theme, then override the parts you want.' : 'No active themes are available.';
        planStatus = plan === 'free' ? 'Upgrade to unlock scheduling, private visibility, and custom domains.' : 'Pro publishing features are active.';
        membershipStatus =
          membershipPriceCents > 0
            ? `Paid membership is live at ${(membershipPriceCents / 100).toLocaleString()} KRW per 30 days.`
            : 'Set a monthly membership price to unlock member-only posts.';
        domainStatus = domains.length
          ? `Custom domain ${domains[0].domain} is ${domains[0].status}.`
          : 'No custom domain connected.';
      } catch (_error) {
        status = 'Log in first to edit settings.';
        themeStatus = 'Log in first to edit themes.';
        planStatus = 'Log in first to manage plans.';
        membershipStatus = 'Log in first to manage memberships.';
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
        bio,
        membershipPriceCents: String(Math.max(0, membershipPriceCents))
      });
      const data = await apiFormPost<{ user: ApiUser }>('/api/me/update', payload);
      ident = data.user.ident;
      displayName = data.user.displayName;
      bio = data.user.bio ?? '';
      membershipPriceCents = data.user.membershipPriceCents ?? membershipPriceCents;
      status = 'Settings saved';
      membershipStatus =
        membershipPriceCents > 0
          ? `Paid membership is live at ${(membershipPriceCents / 100).toLocaleString()} KRW per 30 days.`
          : 'Paid membership is disabled.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Settings update failed';
    } finally {
      saving = false;
    }
  }

  async function updateMembershipStatus(membershipId: number, nextStatus: 'active' | 'cancelled' | 'expired') {
    membershipActionId = membershipId;
    membershipStatus = `Updating membership to ${nextStatus}...`;
    try {
      const payload = new URLSearchParams({ status: nextStatus });
      const data = await apiFormPost<{ membership: MembershipItem }>(`/api/me/membership/${membershipId}/update`, payload);
      receivedMemberships = receivedMemberships.map((item) => (item.id === membershipId ? data.membership : item));
      membershipStatus = `Membership marked ${nextStatus}.`;
    } catch (error) {
      membershipStatus = error instanceof Error ? error.message : 'Membership update failed.';
    } finally {
      membershipActionId = null;
    }
  }

  async function updateOutgoingAutoRenew(membershipId: number, autoRenew: boolean) {
    membershipActionId = membershipId;
    membershipStatus = autoRenew ? 'Enabling auto-renew…' : 'Disabling auto-renew…';
    try {
      const payload = new URLSearchParams({ autoRenew: autoRenew ? 'true' : 'false' });
      const data = await apiFormPost<{ membership: MembershipItem }>(`/api/me/membership/${membershipId}/update`, payload);
      outgoingMemberships = outgoingMemberships.map((item) => (item.id === membershipId ? data.membership : item));
      membershipStatus = autoRenew ? 'Auto-renew enabled.' : 'Auto-renew disabled.';
    } catch (error) {
      membershipStatus = error instanceof Error ? error.message : 'Auto-renew update failed.';
    } finally {
      membershipActionId = null;
    }
  }

  async function updateMembershipOrder(orderId: number, mode: 'cancel' | 'retry') {
    membershipOrderActionId = orderId;
    membershipStatus = mode === 'cancel' ? 'Cancelling renewal order…' : 'Retrying renewal order…';
    try {
      const payload = new URLSearchParams({ mode });
      const data = await apiFormPost<{ order: MembershipOrderItem; membership: MembershipItem }>(`/api/me/membership/order/${orderId}/update`, payload);
      membershipOrders = membershipOrders.map((item) => (item.id === orderId ? data.order : item));
      outgoingMemberships = outgoingMemberships.map((item) => (item.id === data.membership.id ? data.membership : item));
      receivedMemberships = receivedMemberships.map((item) => (item.id === data.membership.id ? data.membership : item));
      membershipStatus = mode === 'cancel' ? 'Renewal order cancelled.' : 'Renewal order moved back to pending.';
    } catch (error) {
      membershipStatus = error instanceof Error ? error.message : 'Membership order update failed.';
    } finally {
      membershipOrderActionId = null;
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
    if (plan !== 'free') {
      planStatus = 'Plan upgrades are not self-service yet.';
      return;
    }
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
        <h2 class="section-title">Your publishing access.</h2>
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

    <p class="copy">Upgrades are not self-service yet. Billing or admin assignment is required for Writer Pro and Designer Pro.</p>

    <div class="action-row">
      <button class="action-link studio-primary-action" type="button" disabled={savingPlan || plan !== 'free'} onclick={savePlan}>
        {savingPlan ? 'Updating…' : 'Switch to free'}
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
        <p class="eyebrow">Membership</p>
        <h2 class="section-title">Sell access to member-only posts.</h2>
      </div>
      <span class="chip">{membershipStatus}</span>
    </div>

    <label class="stack" style="gap: 8px;">
      <span class="eyebrow">30-day membership price</span>
      <input class="search-input studio-input" bind:value={membershipPriceCents} type="number" min="0" step="100" />
    </label>

        <p class="copy">Writer Pro is required to enable paid memberships. Members-only posts stay locked until an admin marks the related payment as paid.</p>

    <div class="action-row">
      <button class="action-link studio-primary-action" type="button" disabled={saving} onclick={saveSettings}>
        {saving ? 'Saving…' : 'Save membership pricing'}
      </button>
    </div>

    <div class="studio-upload-grid grid">
      <article class="hero-card stack">
        <p class="eyebrow">Pending and active subscribers</p>
        {#if receivedMemberships.length}
          {#each receivedMemberships as membership}
            <div class="panel-card stack">
              <div class="meta-row">
                <strong>{membership.member?.displayName ?? membership.member?.ident ?? 'Unknown member'}</strong>
                <span class="chip">{membership.status}</span>
              </div>
              <p class="copy">{((membership.priceCents ?? 0) / 100).toLocaleString()} KRW / 30 days</p>
              {#if membership.expiresAt}
                <p class="copy">Expires {new Date(membership.expiresAt).toLocaleDateString()}</p>
              {/if}
              <div class="action-row">
                <button class="action-link" type="button" disabled={membershipActionId === membership.id} onclick={() => updateMembershipStatus(membership.id, 'expired')}>
                  Expire
                </button>
                <button class="action-link" type="button" disabled={membershipActionId === membership.id} onclick={() => updateMembershipStatus(membership.id, 'cancelled')}>
                  Cancel
                </button>
              </div>
            </div>
          {/each}
        {:else}
          <p class="copy">No subscriber requests yet.</p>
        {/if}
      </article>

      <article class="hero-card stack">
        <p class="eyebrow">Memberships you joined</p>
        {#if outgoingMemberships.length}
          {#each outgoingMemberships as membership}
            <div class="panel-card stack">
              <div class="meta-row">
                <strong>{membership.creator?.displayName ?? membership.creator?.ident ?? 'Unknown writer'}</strong>
                <span class="chip">{membership.status}</span>
              </div>
              <p class="copy">{((membership.priceCents ?? 0) / 100).toLocaleString()} KRW / 30 days</p>
              <p class="copy">Auto renew: {membership.autoRenew ? 'on' : 'off'}</p>
              {#if membership.expiresAt}
                <p class="copy">Expires {new Date(membership.expiresAt).toLocaleDateString()}</p>
              {/if}
              <div class="action-row">
                <button
                  class="action-link"
                  type="button"
                  disabled={membershipActionId === membership.id}
                  onclick={() => updateOutgoingAutoRenew(membership.id, !membership.autoRenew)}
                >
                  {membership.autoRenew ? 'Disable auto-renew' : 'Enable auto-renew'}
                </button>
              </div>
            </div>
          {/each}
        {:else}
          <p class="copy">You have not joined any writer memberships yet.</p>
        {/if}
      </article>
    </div>

    <article class="hero-card stack">
      <p class="eyebrow">Membership payment orders</p>
      {#if membershipOrders.length}
        {#each membershipOrders as order}
          <div class="panel-card stack">
            <div class="meta-row">
              <strong>{order.creator?.displayName ?? order.creator?.ident ?? 'Unknown writer'}</strong>
              <span class="chip">{order.status}</span>
            </div>
            <p class="copy">
              {order.provider === 'membership-renewal' ? 'Renewal billing attempt' : 'Initial membership payment'}
            </p>
            <p class="copy">{((order.amountCents ?? 0) / 100).toLocaleString()} KRW requested</p>
            <p class="copy">Created {new Date(order.createdAt).toLocaleDateString()}</p>
            {#if order.paidAt}
              <p class="copy">Paid {new Date(order.paidAt).toLocaleDateString()}</p>
            {/if}
            {#if order.adminNote}
              <p class="copy">Admin note: {order.adminNote}</p>
            {/if}
            {#if order.membership?.creator?.ident}
              <p class="copy">Membership with {order.membership.creator.displayName ?? order.membership.creator.ident}</p>
            {/if}
            <div class="action-row">
              {#if order.status === 'pending'}
                <button
                  class="action-link"
                  type="button"
                  disabled={membershipOrderActionId === order.id}
                  onclick={() => updateMembershipOrder(order.id, 'cancel')}
                >
                  Cancel order
                </button>
              {/if}
              {#if order.status === 'failed' || order.status === 'cancelled'}
                <button
                  class="action-link"
                  type="button"
                  disabled={membershipOrderActionId === order.id}
                  onclick={() => updateMembershipOrder(order.id, 'retry')}
                >
                  Retry payment
                </button>
              {/if}
            </div>
          </div>
        {/each}
      {:else}
        <p class="copy">No membership payment orders yet.</p>
      {/if}
    </article>
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
