import { apiFetch } from '$lib/api';
import type { ApiPostSummary } from '$lib/types';

type PostsResponse = {
  items: ApiPostSummary[];
  meta: {
    page: number;
    limit: number;
    total: number;
    hasNext: boolean;
    q?: string | null;
  };
};

export async function load({ fetch, url }) {
  const page = Math.max(1, Number(url.searchParams.get('page') ?? '1') || 1);
  const data = await apiFetch<PostsResponse>(fetch, `/api/posts?limit=12&page=${page}`);
  return { posts: data.items, meta: data.meta, page };
}
