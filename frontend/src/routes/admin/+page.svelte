<script lang="ts">
  import { base } from '$app/paths';
  import { env } from '$env/dynamic/public';
  import { onMount } from 'svelte';
  import type { AdminDashboard } from '$lib/types';

  const backendBaseUrl = env.PUBLIC_YESBLOG_API_BASE_URL || '';

  let dashboard = $state<AdminDashboard | null>(null);
  let status = $state('Loading admin dashboard…');
  let loading = $state(true);

  async function readErrorMessage(response: Response, fallback: string) {
    try {
      const payload = await response.json();
      if (typeof payload?.message === 'string' && payload.message.trim()) {
        return payload.message;
      }
    } catch {
      return fallback;
    }
    return fallback;
  }

  async function fetchDashboard() {
    const response = await fetch(`${backendBaseUrl}/api/admin/dashboard`, {
      credentials: 'include'
    });

    if (!response.ok) {
      throw new Error(await readErrorMessage(response, 'Failed to load admin dashboard.'));
    }

    dashboard = await response.json();
    status = 'Admin overview is up to date.';
  }

  async function deleteArticle(articleId: number) {
    if (!window.confirm('Delete this article and its comments permanently?')) return;
    status = 'Deleting article…';
    const response = await fetch(`${backendBaseUrl}/api/admin/article/${articleId}/delete`, {
      method: 'POST',
      credentials: 'include'
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Article delete failed.');
      return;
    }
    await fetchDashboard();
  }

  async function deleteComment(commentId: number) {
    if (!window.confirm('Delete this comment permanently?')) return;
    status = 'Deleting comment…';
    const response = await fetch(`${backendBaseUrl}/api/admin/comment/${commentId}/delete`, {
      method: 'POST',
      credentials: 'include'
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Comment delete failed.');
      return;
    }
    await fetchDashboard();
  }

  async function deleteUser(userId: number, ident: string) {
    if (!window.confirm(`Delete user ${ident} and all authored articles permanently?`)) return;
    status = 'Deleting user…';
    const response = await fetch(`${backendBaseUrl}/api/admin/user/${userId}/delete`, {
      method: 'POST',
      credentials: 'include'
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'User delete failed.');
      return;
    }
    await fetchDashboard();
  }

  onMount(() => {
    void (async () => {
      try {
        await fetchDashboard();
      } catch (error) {
        status = error instanceof Error ? error.message : 'Open an admin session first.';
      } finally {
        loading = false;
      }
    })();
  });
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Admin</p>
    <h1 class="hero-title">Frontend moderation dashboard.</h1>
    <p class="lede">
      Yesod no longer renders an admin HTML screen. Moderation, cleanup, and content oversight now live in the same
      frontend surface.
    </p>
    <div class="meta-row">
      <span class="chip">{dashboard?.meta.articleCount ?? 0} articles</span>
      <span class="chip">{dashboard?.meta.commentCount ?? 0} comments</span>
      <span class="chip">{dashboard?.meta.userCount ?? 0} users</span>
      <span class="chip">{dashboard?.meta.imageCount ?? 0} images</span>
      <a class="action-link" href={`${base}/studio`}>Open studio</a>
      <a class="action-link" href={`${base}/profile`}>Profile</a>
    </div>
    <p class="copy">{status}</p>
  </div>

  {#if loading}
    <section class="panel-card stack">
      <p class="copy">Loading dashboard…</p>
    </section>
  {:else if dashboard}
    <div class="stack">
      <section class="panel-card stack">
        <div class="meta-row">
          <p class="eyebrow">Recent articles</p>
          <span class="chip">{dashboard.articles.length} items</span>
        </div>
        {#each dashboard.articles as article}
          <article class="post-card stack">
            <div class="meta-row">
              <strong>{article.title}</strong>
              <span class:chip-live={!article.draft} class="chip">{article.draft ? 'Draft' : 'Published'}</span>
            </div>
            <p class="copy">
              {article.author?.displayName ?? article.author?.ident ?? 'Unknown author'} · {new Date(article.updatedAt).toLocaleString()} · {article.commentCount} comments
            </p>
            <div class="tag-row">
              {#each article.tags as tag}
                <span class="chip">{tag}</span>
              {/each}
            </div>
            <div class="action-row">
              {#if article.draft}
                <span class="copy">Draft posts are managed from the writer studio.</span>
              {:else}
                <a class="action-link" href={`${base}/posts/${article.slug}`}>View post</a>
              {/if}
              <button class="action-link action-link-danger" type="button" onclick={() => deleteArticle(article.id)}>Delete</button>
            </div>
          </article>
        {/each}
      </section>

      <section class="panel-card stack">
        <div class="meta-row">
          <p class="eyebrow">Recent comments</p>
          <span class="chip">{dashboard.comments.length} items</span>
        </div>
        {#each dashboard.comments as comment}
          <article class="post-card stack">
            <strong>{comment.name}</strong>
            <p class="copy">{comment.content}</p>
            <p class="copy">
              {new Date(comment.posted).toLocaleString()}
              {#if comment.article}
                · {comment.article.title}
              {/if}
            </p>
            <div class="action-row">
              {#if comment.article && !comment.article.draft}
                <a class="action-link" href={`${base}/posts/${comment.article.slug}`}>Open post</a>
              {/if}
              <button class="action-link action-link-danger" type="button" onclick={() => deleteComment(comment.id)}>Delete</button>
            </div>
          </article>
        {/each}
      </section>

      <section class="panel-card stack">
        <div class="meta-row">
          <p class="eyebrow">Recent users</p>
          <span class="chip">{dashboard.users.length} items</span>
        </div>
        {#each dashboard.users as user}
          <article class="post-card stack">
            <div class="meta-row">
              <strong>{user.displayName}</strong>
              {#if user.isAdmin}
                <span class="chip chip-live">Admin</span>
              {/if}
            </div>
            <p class="copy">@{user.ident}</p>
            {#if user.bio}
              <p class="copy">{user.bio}</p>
            {/if}
            <p class="copy">{user.publishedCount} published · {user.draftCount} drafts</p>
            <div class="action-row">
              <a class="action-link" href={`${base}/users/${user.ident}`}>Open blog</a>
              <button class="action-link action-link-danger" type="button" onclick={() => deleteUser(user.id, user.ident)}>Delete</button>
            </div>
          </article>
        {/each}
      </section>
    </div>
  {/if}
</section>
