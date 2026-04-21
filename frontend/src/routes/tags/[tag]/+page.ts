import { apiFetch } from '$lib/api';
import type { ApiPostSummary } from '$lib/types';

type PostsResponse = {
  items: ApiPostSummary[];
  meta: {
    total: number;
    tag?: string | null;
  };
};

export async function load({ fetch, params }) {
  const data = await apiFetch<PostsResponse>(fetch, `/api/posts?tag=${encodeURIComponent(params.tag)}&limit=20`);
  return { posts: data.items, meta: data.meta, tag: params.tag };
}
