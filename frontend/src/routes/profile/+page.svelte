<script lang="ts">
  import { goto } from '$app/navigation';
  import { onMount } from 'svelte';
  import { base } from '$app/paths';
  import { apiFetch, apiFormPost } from '$lib/api';
  import type { ApiMe } from '$lib/types';

  let me = $state<ApiMe | null>(null);
  let status = $state('Loading profile…');
  let loggingOut = $state(false);

  onMount(() => {
    void (async () => {
      try {
        me = await apiFetch<ApiMe>(fetch, '/api/me');
        status = 'Profile loaded';
      } catch (_error) {
        status = 'Log in first to view your profile.';
      }
    })();
  });

  async function logout() {
    loggingOut = true;
    status = 'Logging out…';
    try {
      await apiFormPost('/api/auth/logout', new URLSearchParams());
      await goto(`${base}/`);
    } catch (error) {
      status = error instanceof Error ? error.message : 'Logout failed.';
    } finally {
      loggingOut = false;
    }
  }
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Profile</p>
    <h1 class="hero-title">{me?.user.displayName ?? 'Your profile'}</h1>
    <p class="lede">{me?.user.bio ?? status}</p>
    {#if me}
      <div class="meta-row">
        <span class="chip">@{me.user.ident}</span>
        <span class="chip">{me.meta.totalCount} total posts</span>
        <span class="chip">{me.meta.publishedCount} published</span>
        <span class="chip">{me.meta.draftCount} drafts</span>
      </div>
    {/if}
  </div>

  <div class="action-row">
    <a class="action-link" href={`${base}/settings`}>Edit settings</a>
    <a class="action-link" href={`${base}/me/posts`}>My posts</a>
    <a class="action-link studio-primary-action" href={`${base}/studio`}>Open studio</a>
    {#if me?.user.isAdmin}
      <a class="action-link" href={`${base}/admin`}>Admin</a>
    {/if}
    <button class="action-link" type="button" disabled={loggingOut} onclick={logout}>
      {loggingOut ? 'Logging out…' : 'Log out'}
    </button>
  </div>
</section>
