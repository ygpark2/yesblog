<script lang="ts">
  import PostCard from '$lib/components/PostCard.svelte';
  import SearchBar from '$lib/components/SearchBar.svelte';
  import type { ApiPostSummary } from '$lib/types';

  interface Props {
    data: {
      posts: ApiPostSummary[];
      meta: { total: number; hasNext: boolean };
      q: string;
      page: number;
    };
  }

  let { data }: Props = $props();
</script>

<section class="stack">
  <div class="panel-card stack">
    <p class="eyebrow">Search</p>
    <h1 class="section-title">Find a post</h1>
    <SearchBar initialValue={data.q} />
    <p class="copy">
      {#if data.q}
        {data.meta.total} results for "{data.q}".
      {:else}
        Enter a keyword to search published posts.
      {/if}
    </p>
  </div>

  {#if data.posts.length}
    <div class="stack">
      {#each data.posts as post}
        <PostCard {post} />
      {/each}
    </div>
  {/if}

  {#if data.q}
    <div class="action-row">
      {#if data.page > 1}
        <a class="action-link" href={`?q=${encodeURIComponent(data.q)}&page=${data.page - 1}`}>Previous</a>
      {/if}
      {#if data.meta.hasNext}
        <a class="action-link" href={`?q=${encodeURIComponent(data.q)}&page=${data.page + 1}`}>Next</a>
      {/if}
    </div>
  {/if}
</section>
