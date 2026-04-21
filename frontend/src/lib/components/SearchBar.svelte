<script lang="ts">
  import { base } from '$app/paths';

  let { initialValue = '' }: { initialValue?: string } = $props();
</script>

<form class="search-row" action={`${base}/search`} method="GET">
  <input class="search-input" type="search" name="q" value={initialValue} placeholder="Search by title or content" />
  <button class="action-link" type="submit">Search</button>
</form>
