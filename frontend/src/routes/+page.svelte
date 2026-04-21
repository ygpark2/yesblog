<script lang="ts">
  import { base } from '$app/paths';
  import PostCard from '$lib/components/PostCard.svelte';
  import SearchBar from '$lib/components/SearchBar.svelte';
  import type { ApiPostSummary } from '$lib/types';
  import { env } from '$env/dynamic/public';

  const backendBaseUrl = env.PUBLIC_YESBLOG_API_BASE_URL || '';

  interface Props {
    data: {
      posts: ApiPostSummary[];
      meta: { total: number; hasNext: boolean };
      page: number;
    };
  }

  let { data }: Props = $props();
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">YesBlog app</p>
    <h1 class="hero-title">Read in the split frontend, write in the same-origin studio.</h1>
    <p class="lede">
      This frontend is built into the Yesod static folder and served at `/app`, while the backend
      still owns auth, publishing rules, autosave, and permissions.
    </p>
    <SearchBar />
    <div class="meta-row">
      <span class="chip">{data.meta.total} published posts</span>
      <a class="action-link" href={`${base}/studio`}>Open writer studio</a>
      <a class="action-link" href={`${base}/studio`}>Split studio</a>
    </div>
  </div>

  <div class="post-grid">
    {#each data.posts as post}
      <PostCard {post} />
    {/each}
  </div>

  <div class="action-row">
    {#if data.page > 1}
      <a class="action-link" href={`${base}/?page=${data.page - 1}`}>Newer posts</a>
    {/if}
    {#if data.meta.hasNext}
      <a class="action-link" href={`${base}/?page=${data.page + 1}`}>Older posts</a>
    {/if}
  </div>
</section>
