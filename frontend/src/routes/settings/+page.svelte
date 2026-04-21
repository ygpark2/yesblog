<script lang="ts">
  import { onMount } from 'svelte';
  import { apiFetch, apiFormPost } from '$lib/api';
  import type { ApiMe, ApiUser } from '$lib/types';

  let ident = $state('');
  let displayName = $state('');
  let bio = $state('');
  let status = $state('Loading settings…');
  let saving = $state(false);

  onMount(() => {
    void (async () => {
      try {
        const me = await apiFetch<ApiMe>(fetch, '/api/me');
        ident = me.user.ident;
        displayName = me.user.displayName;
        bio = me.user.bio ?? '';
        status = 'Update your profile details';
      } catch (_error) {
        status = 'Log in first to edit settings.';
      }
    })();
  });

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
</section>
