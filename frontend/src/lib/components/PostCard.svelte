<script lang="ts">
  import type { ApiPostSummary } from '$lib/types';
  import { base } from '$app/paths';

  let { post, live = true }: { post: ApiPostSummary; live?: boolean } = $props();
</script>

<article class="post-card stack">
  <div class="stack" style="gap: 14px;">
    <p class="eyebrow">Post</p>
    <h2 class="post-title">
      {#if live}
        <a href={`${base}/posts/${post.slug}`}>{post.title}</a>
      {:else}
        {post.title}
      {/if}
    </h2>
    <div class="meta-row">
      <span class="chip">{post.readingMinutes} min read</span>
      <span class="chip">{new Date(post.createdAt).toLocaleDateString()}</span>
      {#if !live}
        <span class="chip">Draft</span>
      {/if}
      {#if post.author}
        <a class="chip" href={`${base}/users/${post.author.ident}`}>{post.author.displayName}</a>
      {/if}
    </div>
  </div>

  <p class="excerpt">{post.excerpt}</p>

  {#if post.tags.length}
    <div class="tag-row">
      {#each post.tags as tag}
        <a class="chip" href={`${base}/tags/${tag}`}>{tag}</a>
      {/each}
    </div>
  {/if}

  {#if live}
    <div class="action-row">
      <a class="action-link" href={`${base}/posts/${post.slug}`}>Read post</a>
    </div>
  {/if}
</article>
