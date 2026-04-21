<script lang="ts">
  import PostCard from '$lib/components/PostCard.svelte';
  import type { ApiPostSummary, ApiUser } from '$lib/types';

  interface Props {
    data: {
      user: ApiUser;
      items: ApiPostSummary[];
      meta: { publishedCount: number };
    };
  }

  let { data }: Props = $props();
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Writer page</p>
    <h1 class="hero-title">{data.user.displayName}</h1>
    <div class="meta-row">
      <span class="chip">@{data.user.ident}</span>
      <span class="chip">{data.meta.publishedCount} published posts</span>
    </div>
    {#if data.user.bio}
      <p class="lede">{data.user.bio}</p>
    {/if}
  </div>

  <div class="stack">
    {#each data.items as post}
      <PostCard {post} />
    {/each}
  </div>
</section>
