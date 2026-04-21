import { apiFetch } from '$lib/api';
import type { ApiPostDetail, ApiPostSummary } from '$lib/types';

type PostResponse = {
  item: ApiPostDetail;
  related: ApiPostSummary[];
};

export async function load({ fetch, params }) {
  const data = await apiFetch<PostResponse>(fetch, `/api/post/${params.slug}`);
  return data;
}
