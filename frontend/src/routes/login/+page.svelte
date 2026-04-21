<script lang="ts">
  import { goto } from '$app/navigation';
  import { base } from '$app/paths';
  import { apiFormPost } from '$lib/api';

  let ident = $state('');
  let password = $state('');
  let status = $state('');
  let submitting = $state(false);

  async function submitLogin() {
    status = '';
    submitting = true;
    try {
      await apiFormPost('/api/auth/login', new URLSearchParams({ ident, password }));
      await goto(`${base}/profile`);
    } catch (error) {
      status = error instanceof Error ? error.message : 'Login failed.';
    } finally {
      submitting = false;
    }
  }
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Login</p>
    <h1 class="hero-title">Sign in to write and manage your blog.</h1>
    <p class="lede">Authentication is still handled by Yesod, but the sign-in screen now lives in the frontend app.</p>
  </div>

  <section class="panel-card stack">
    <form
      class="stack"
      style="gap: 16px;"
      onsubmit={(event) => {
        event.preventDefault();
        void submitLogin();
      }}
    >
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Username</span>
        <input class="search-input studio-input" type="text" bind:value={ident} autocomplete="username" required />
      </label>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Password</span>
        <input class="search-input studio-input" type="password" bind:value={password} autocomplete="current-password" required />
      </label>

      {#if status}
        <p class="copy" style="margin: 0; color: #9d1c1c;">{status}</p>
      {/if}

      <div class="action-row">
        <button class="action-link studio-primary-action" type="submit" disabled={submitting}>
          {submitting ? 'Logging in…' : 'Log in'}
        </button>
        <a class="action-link" href={`${base}/register`}>Create account</a>
      </div>
    </form>
  </section>
</section>
