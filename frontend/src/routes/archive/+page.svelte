<script lang="ts">
  import { base } from '$app/paths';
  import PostCard from '$lib/components/PostCard.svelte';
  import type { ApiPostSummary } from '$lib/types';

  interface Props {
    data: {
      posts: ApiPostSummary[];
      meta: {
        page: number;
        total: number;
        hasNext: boolean;
      };
      page: number;
    };
  }

  let { data }: Props = $props();
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Archive</p>
    <h1 class="hero-title">Published archive for the shared blog.</h1>
    <p class="lede">
      Browse all published writing in a single stream, with the newer Svelte reading experience kept as the
      default destination.
    </p>
    <div class="meta-row">
      <span class="chip">{data.meta.total} published posts</span>
      <span class="chip">Page {data.page}</span>
    </div>
  </div>

  <div class="post-grid">
    {#each data.posts as post}
      <PostCard {post} />
    {/each}
  </div>

  <div class="action-row">
    {#if data.page > 1}
      <a class="action-link" href={`${base}/archive?page=${data.page - 1}`}>Newer posts</a>
    {/if}
    {#if data.meta.hasNext}
      <a class="action-link" href={`${base}/archive?page=${data.page + 1}`}>Older posts</a>
    {/if}
  </div>
</section>
