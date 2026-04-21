import { apiFetch } from '$lib/api';
import type { ApiPostSummary } from '$lib/types';

type PostsResponse = {
  items: ApiPostSummary[];
  meta: {
    page: number;
    total: number;
    hasNext: boolean;
    q?: string | null;
  };
};

export async function load({ fetch, url }) {
  const q = url.searchParams.get('q') ?? '';
  const page = Math.max(1, Number(url.searchParams.get('page') ?? '1') || 1);
  const suffix = q
    ? `?q=${encodeURIComponent(q)}&limit=20&page=${page}`
    : `?limit=20&page=${page}`;
  const data = await apiFetch<PostsResponse>(fetch, `/api/posts${suffix}`);
  return { posts: data.items, meta: data.meta, q, page };
}
