<script lang="ts">
  import PostCard from '$lib/components/PostCard.svelte';
  import type { ApiPostSummary } from '$lib/types';

  interface Props {
    data: {
      posts: ApiPostSummary[];
      meta: { total: number };
      tag: string;
    };
  }

  let { data }: Props = $props();
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Tag view</p>
    <h1 class="hero-title">#{data.tag}</h1>
    <p class="lede">{data.meta.total} published posts in this tag.</p>
  </div>

  <div class="stack">
    {#each data.posts as post}
      <PostCard {post} />
    {/each}
  </div>
</section>
