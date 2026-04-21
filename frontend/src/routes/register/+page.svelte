<script lang="ts">
  import { goto } from '$app/navigation';
  import { base } from '$app/paths';
  import { apiFormPost } from '$lib/api';

  let error = $state('');
  let ident = $state('');
  let password = $state('');
  let passwordConfirm = $state('');
  let displayName = $state('');
  let bio = $state('');
  let submitting = $state(false);

  async function submitRegister() {
    error = '';
    submitting = true;
    try {
      await apiFormPost('/api/auth/register', new URLSearchParams({
        ident,
        password,
        passwordConfirm,
        displayName,
        bio
      }));
      await goto(`${base}/profile`);
    } catch (caughtError) {
      error = caughtError instanceof Error ? caughtError.message : 'Registration failed.';
    } finally {
      submitting = false;
    }
  }
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Register</p>
    <h1 class="hero-title">Create a writer account.</h1>
    <p class="lede">Join the multi-user blog and start publishing from the studio.</p>
  </div>

  <section class="panel-card stack">
    {#if error}
      <p class="copy" style="margin: 0; color: #9d1c1c;">{error}</p>
    {/if}

    <form
      class="stack"
      style="gap: 16px;"
      onsubmit={(event) => {
        event.preventDefault();
        void submitRegister();
      }}
    >
      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Username</span>
        <input class="search-input studio-input" type="text" bind:value={ident} required />
      </label>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Password</span>
        <input class="search-input studio-input" type="password" bind:value={password} minlength="8" required />
      </label>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Confirm password</span>
        <input class="search-input studio-input" type="password" bind:value={passwordConfirm} minlength="8" required />
      </label>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Display name</span>
        <input class="search-input studio-input" type="text" bind:value={displayName} />
      </label>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Bio</span>
        <textarea class="studio-textarea" bind:value={bio}></textarea>
      </label>

      <div class="action-row">
        <button class="action-link studio-primary-action" type="submit" disabled={submitting}>
          {submitting ? 'Creating account…' : 'Create account'}
        </button>
        <a class="action-link" href={`${base}/login`}>Already have an account?</a>
      </div>
    </form>
  </section>
</section>
