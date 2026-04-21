import { apiFetch } from '$lib/api';
import type { ApiPostSummary, ApiUser } from '$lib/types';

type UserResponse = {
  user: ApiUser;
  items: ApiPostSummary[];
  meta: {
    publishedCount: number;
  };
};

export async function load({ fetch, params }) {
  return apiFetch<UserResponse>(fetch, `/api/user/${params.ident}`);
}
