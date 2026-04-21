<script lang="ts">
  import { onMount } from 'svelte';
  import { base } from '$app/paths';
  import PostCard from '$lib/components/PostCard.svelte';
  import { apiFetch } from '$lib/api';
  import type { EditorMine } from '$lib/types';

  let mine = $state<EditorMine | null>(null);
  let status = $state('Loading your posts…');

  onMount(() => {
    void (async () => {
      try {
        mine = await apiFetch<EditorMine>(fetch, '/api/editor/mine');
        status = 'Posts loaded';
      } catch (_error) {
        status = 'Log in first to view your posts.';
      }
    })();
  });
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">My posts</p>
    <h1 class="hero-title">Manage drafts and published posts.</h1>
    <p class="lede">{status}</p>
  </div>

  <div class="action-row">
    <a class="action-link studio-primary-action" href={`${base}/studio`}>New post</a>
  </div>

  <section class="stack">
    <p class="eyebrow">Drafts</p>
    {#if mine?.drafts?.length}
      {#each mine.drafts as post}
        <div class="stack" style="gap: 12px;">
          <PostCard {post} />
          <div class="action-row">
            <a class="action-link" href={`${base}/studio?articleId=${post.id}`}>Edit in studio</a>
          </div>
        </div>
      {/each}
    {:else}
      <p class="copy">No drafts yet.</p>
    {/if}
  </section>

  <section class="stack">
    <p class="eyebrow">Published</p>
    {#if mine?.published?.length}
      {#each mine.published as post}
        <PostCard {post} />
      {/each}
    {:else}
      <p class="copy">No published posts yet.</p>
    {/if}
  </section>
</section>
