import { apiFormPost, apiGet } from '$lib/api';
import type { ApiSession, ThemeMarketplaceItem } from '$lib/types';

export type ThemeOwnershipFilter = 'all' | 'owned' | 'free' | 'paid' | 'remix';
export type ThemeSortMode = 'latest' | 'price-asc' | 'price-desc' | 'rating';

export type ThemeDraftForm = {
  name: string;
  slug: string;
  description: string;
  backgroundColor: string;
  surfaceColor: string;
  textColor: string;
  accentColor: string;
  headingFont: string;
  bodyFont: string;
  headerTemplate: string;
  bodyTemplate: string;
  footerTemplate: string;
  customCss: string;
  priceCents: string;
};

type ThemeMarketplaceFilters = {
  query: string;
  ownershipFilter: ThemeOwnershipFilter;
  sortMode: ThemeSortMode;
};

export function createThemeDraft(): ThemeDraftForm {
  return {
    name: '',
    slug: '',
    description: '',
    backgroundColor: '#f8f0dc',
    surfaceColor: '#fffef7',
    textColor: '#111111',
    accentColor: '#ffe11a',
    headingFont: 'Cormorant Garamond',
    bodyFont: 'Space Grotesk',
    headerTemplate: '',
    bodyTemplate: '',
    footerTemplate: '',
    customCss: '',
    priceCents: '0'
  };
}

export function filterThemeMarketplaceItems(
  items: ThemeMarketplaceItem[],
  filters: ThemeMarketplaceFilters
): ThemeMarketplaceItem[] {
  const normalizedQuery = filters.query.trim().toLowerCase();

  return [...items]
    .filter((item) => {
      const matchesQuery =
        !normalizedQuery ||
        item.theme.name.toLowerCase().includes(normalizedQuery) ||
        item.theme.slug.toLowerCase().includes(normalizedQuery) ||
        (item.theme.description ?? '').toLowerCase().includes(normalizedQuery);
      const matchesOwnership =
        filters.ownershipFilter === 'all' ||
        (filters.ownershipFilter === 'owned' && item.owned) ||
        (filters.ownershipFilter === 'free' && item.theme.priceCents === 0) ||
        (filters.ownershipFilter === 'paid' && item.theme.priceCents > 0) ||
        (filters.ownershipFilter === 'remix' && Boolean(item.theme.parentId));
      return matchesQuery && matchesOwnership;
    })
    .sort((left, right) => {
      if (filters.sortMode === 'price-asc') return left.theme.priceCents - right.theme.priceCents;
      if (filters.sortMode === 'price-desc') return right.theme.priceCents - left.theme.priceCents;
      if (filters.sortMode === 'rating') {
        return (right.rating?.averageRating ?? 0) - (left.rating?.averageRating ?? 0);
      }
      return new Date(right.theme.updatedAt).getTime() - new Date(left.theme.updatedAt).getTime();
    });
}

export function resolveSelectedThemeId(
  items: ThemeMarketplaceItem[],
  selectedThemeId: number | null
): number | null {
  return selectedThemeId && items.some((item) => item.theme.id === selectedThemeId)
    ? selectedThemeId
    : items[0]?.theme.id ?? null;
}

export function primaryThemeActionLabel(
  item: ThemeMarketplaceItem,
  currentThemeId: number | null
): string {
  if (item.isAuthor) return 'Remix';
  if (item.owned) return currentThemeId === item.theme.id ? 'Applied' : 'Apply to blog';
  return item.theme.priceCents === 0 ? 'Apply free theme' : 'Buy theme';
}

export function cardThemeActionLabel(
  item: ThemeMarketplaceItem,
  currentThemeId: number | null
): string {
  if (item.isAuthor) return 'Remix';
  if (item.owned) return currentThemeId === item.theme.id ? 'Applied' : 'Apply';
  return item.theme.priceCents === 0 ? 'Apply free theme' : 'Buy theme';
}

export async function fetchThemeMarketplaceItems(): Promise<ThemeMarketplaceItem[]> {
  const data = await apiGet<{ items: ThemeMarketplaceItem[] }>('/api/themes/marketplace');
  return data.items;
}

export async function fetchThemeMarketplaceSession(): Promise<ApiSession> {
  return apiGet<ApiSession>('/api/session');
}

export async function applyMarketplaceTheme(themeId: number | null): Promise<void> {
  await apiFormPost(
    '/api/me/theme',
    new URLSearchParams({ themeId: themeId ? String(themeId) : '' })
  );
}

export async function purchaseMarketplaceTheme(
  themeId: number
): Promise<{ requiresConfirmation?: boolean }> {
  return apiFormPost<{ requiresConfirmation?: boolean }>(
    `/api/theme/${themeId}/purchase`,
    new URLSearchParams()
  );
}

export async function forkMarketplaceTheme(
  themeId: number,
  payload: { name: string; slug: string }
): Promise<void> {
  await apiFormPost(
    `/api/theme/${themeId}/fork`,
    new URLSearchParams({
      name: payload.name,
      slug: payload.slug,
      priceCents: '0'
    })
  );
}

export async function submitThemeDraft(draft: ThemeDraftForm): Promise<void> {
  await apiFormPost('/api/themes/create', new URLSearchParams({ ...draft }));
}

export async function saveThemeReview(themeId: number, rating: string, review: string): Promise<void> {
  await apiFormPost(
    `/api/theme/${themeId}/review`,
    new URLSearchParams({
      rating,
      review
    })
  );
}

export async function reportThemeIssue(
  themeId: number,
  reason: string,
  details: string
): Promise<void> {
  await apiFormPost(
    `/api/theme/${themeId}/report`,
    new URLSearchParams({
      reason,
      details
    })
  );
}
