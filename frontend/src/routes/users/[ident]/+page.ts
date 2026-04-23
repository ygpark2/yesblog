import { apiFetch } from '$lib/api';
import type { ApiPostSummary, ApiUser, MembershipAccess } from '$lib/types';

type UserResponse = {
  user: ApiUser;
  items: ApiPostSummary[];
  meta: {
    publishedCount: number;
  };
  membership: MembershipAccess;
};

export async function load({ fetch, params }) {
  return apiFetch<UserResponse>(fetch, `/api/user/${params.ident}`);
}
