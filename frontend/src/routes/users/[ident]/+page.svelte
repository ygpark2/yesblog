<script lang="ts">
  import PostCard from '$lib/components/PostCard.svelte';
  import { apiFormPost } from '$lib/api';
  import { buildUserThemeStyle, escapeThemeValue, renderThemeTemplate, sanitizeThemeCss } from '$lib/theme';
  import type { ApiPostSummary, ApiUser, MembershipAccess } from '$lib/types';

  interface Props {
    data: {
      user: ApiUser;
      items: ApiPostSummary[];
      meta: { publishedCount: number };
      membership: MembershipAccess;
    };
  }

  let { data }: Props = $props();
  let membership = $state<MembershipAccess>({ enabled: false, priceCents: 0, active: false });
  let membershipStatus = $state('Loading membership…');
  let membershipSaving = $state(false);

  const templateContext = $derived({
    'site.title': 'YesBlog',
    'author.name': escapeThemeValue(data.user.displayName),
    'author.ident': escapeThemeValue(data.user.ident),
    'author.bio': escapeThemeValue(data.user.bio ?? ''),
    'author.publishedCount': data.meta.publishedCount
  });
  const themeHeaderHtml = $derived(renderThemeTemplate(data.user.theme?.headerTemplate, templateContext));
  const themeFooterHtml = $derived(renderThemeTemplate(data.user.theme?.footerTemplate, templateContext));
  const themeCustomCss = $derived(sanitizeThemeCss(data.user.theme?.customCss));

  function describeMembershipStatus(target: MembershipAccess) {
    return target.enabled
      ? target.active
        ? 'Membership active'
        : target.viewerStatus === 'pending'
          ? 'Membership request pending'
          : 'Request this membership to unlock member-only posts.'
      : 'This writer has not enabled paid memberships.';
  }

  $effect(() => {
    membership = data.membership;
    if (!membershipSaving) {
      membershipStatus = describeMembershipStatus(data.membership);
    }
  });

  async function submitMembership(mode: 'request' | 'cancel') {
    membershipSaving = true;
    membershipStatus = mode === 'cancel' ? 'Cancelling membership…' : 'Submitting membership request…';
    try {
      const payload = new URLSearchParams({ mode });
      const response = await apiFormPost<{ membership: { status: string; priceCents: number; startedAt?: string | null; expiresAt?: string | null } }>(
        `/api/user/${data.user.ident}/membership`,
        payload
      );
      membership = {
        ...membership,
        viewerStatus: response.membership.status,
        priceCents: response.membership.priceCents,
        active: response.membership.status === 'active',
        startedAt: response.membership.startedAt ?? null,
        expiresAt: response.membership.expiresAt ?? null
      };
      membershipStatus =
        response.membership.status === 'active'
          ? 'Membership active'
          : response.membership.status === 'pending'
            ? 'Membership request submitted. Wait for activation.'
            : 'Membership cancelled.';
    } catch (error) {
      membershipStatus = error instanceof Error ? error.message : 'Membership request failed.';
    } finally {
      membershipSaving = false;
    }
  }
</script>

<svelte:head>
  {#if themeCustomCss}
    <style>{themeCustomCss}</style>
  {/if}
</svelte:head>

<section class="stack themed-page" style={buildUserThemeStyle(data.user)}>
  {#if themeHeaderHtml}
    <div class="theme-template-slot theme-template-header">{@html themeHeaderHtml}</div>
  {/if}

  <div class="hero-card stack">
    <p class="eyebrow">Writer page</p>
    <h1 class="hero-title">{data.user.displayName}</h1>
    <div class="meta-row">
      <span class="chip">@{data.user.ident}</span>
      <span class="chip">{data.meta.publishedCount} published posts</span>
      {#if membership.enabled}
        <span class="chip">Members {Math.floor((membership.priceCents ?? 0) / 100).toLocaleString()} KRW / 30 days</span>
      {/if}
    </div>
    {#if data.user.bio}
      <p class="lede">{data.user.bio}</p>
    {/if}

    {#if membership.enabled}
      <div class="panel-card stack">
        <p class="eyebrow">Paid membership</p>
        <p class="copy">{membershipStatus}</p>
        <div class="meta-row">
          <span class="chip">{membership.active ? 'active' : membership.viewerStatus ?? 'not joined'}</span>
          {#if membership.expiresAt}
            <span class="chip">Until {new Date(membership.expiresAt).toLocaleDateString()}</span>
          {/if}
        </div>
        <div class="action-row">
          {#if membership.active || membership.viewerStatus === 'pending'}
            <button class="action-link" type="button" disabled={membershipSaving} onclick={() => submitMembership('cancel')}>
              {membershipSaving ? 'Saving…' : 'Cancel request'}
            </button>
          {:else}
            <button class="action-link studio-primary-action" type="button" disabled={membershipSaving} onclick={() => submitMembership('request')}>
              {membershipSaving ? 'Requesting…' : 'Request membership'}
            </button>
          {/if}
        </div>
      </div>
    {/if}
  </div>

  <div class="stack">
    {#each data.items as post}
      <PostCard {post} />
    {/each}
  </div>

  {#if themeFooterHtml}
    <div class="theme-template-slot theme-template-footer">{@html themeFooterHtml}</div>
  {/if}
</section>
