export type ApiTheme = {
  id: number;
  name: string;
  slug: string;
  description?: string | null;
  backgroundColor: string;
  surfaceColor: string;
  textColor: string;
  accentColor: string;
  headingFont?: string | null;
  bodyFont?: string | null;
  headerTemplate?: string | null;
  bodyTemplate?: string | null;
  footerTemplate?: string | null;
  customCss?: string | null;
  authorId?: number | null;
  parentId?: number | null;
  priceCents: number;
  status: string;
  license?: string | null;
  active: boolean;
  createdAt: string;
  updatedAt: string;
};

export type ThemeMarketplaceItem = {
  theme: ApiTheme;
  owned: boolean;
  canUse: boolean;
  canFork: boolean;
  isAuthor: boolean;
  canEdit: boolean;
  canDelete: boolean;
  rating?: {
    averageRating: number;
    ratingCount: number;
  };
  myReview?: {
    rating: number;
    review?: string | null;
    updatedAt: string;
  } | null;
  reportedByMe?: boolean;
};

export type ThemeOrderItem = {
  id: number;
  amountCents: number;
  status: string;
  provider?: string | null;
  providerOrderId?: string | null;
  createdAt: string;
  paidAt?: string | null;
  theme?: ApiTheme | null;
  user?: ApiUser | null;
};

export type ThemeReviewItem = {
  id: number;
  status: string;
  note?: string | null;
  createdAt: string;
  updatedAt: string;
  theme?: ApiTheme | null;
  reviewer?: ApiUser | null;
};

export type ThemeSalesStat = {
  theme: ApiTheme;
  orderCount: number;
  pendingCount: number;
  paidCount: number;
  failedCount: number;
  revenueCents: number;
};

export type ThemeStatsResponse = {
  summary: {
    themeCount: number;
    orderCount: number;
    pendingCount: number;
    paidCount: number;
    failedCount: number;
    revenueCents: number;
  };
  items: ThemeSalesStat[];
};

export type ThemePayoutItem = {
  theme: ApiTheme;
  paidCount: number;
  grossRevenueCents: number;
  platformFeeCents: number;
  parentRoyaltyCents: number;
  upstreamRoyaltyEarnedCents: number;
  childOrderCount: number;
  netRevenueCents: number;
  payoutReadyCents: number;
};

export type ThemePayoutRequestItem = {
  id: number;
  amountCents: number;
  status: string;
  sellerNote?: string | null;
  adminNote?: string | null;
  requestedAt: string;
  processedAt?: string | null;
  user?: ApiUser | null;
};

export type AdminThemePayoutSeller = {
  user: ApiUser | null;
  requestCount: number;
  requestedCents: number;
  paidCents: number;
  rejectedCents: number;
  openRequestedCount: number;
};

export type AdminThemePayoutsResponse = {
  summary: {
    requestCount: number;
    requestedCents: number;
    paidCents: number;
    rejectedCents: number;
    openRequestedCount: number;
  };
  sellers: AdminThemePayoutSeller[];
  items: ThemePayoutRequestItem[];
};

export type ThemePayoutsResponse = {
  summary: {
    themeCount: number;
    paidCount: number;
    grossRevenueCents: number;
    platformFeeCents: number;
    downstreamRoyaltyCents: number;
    upstreamRoyaltyEarnedCents: number;
    netRevenueCents: number;
    payoutReadyCents: number;
    requestedCents: number;
    paidOutCents: number;
    availableCents: number;
  };
  items: ThemePayoutItem[];
  requests: ThemePayoutRequestItem[];
};

export type ThemeOverrides = Partial<
  Pick<ApiTheme, 'backgroundColor' | 'surfaceColor' | 'textColor' | 'accentColor' | 'headingFont' | 'bodyFont'>
>;

export type ApiUser = {
  ident: string;
  displayName: string;
  bio?: string | null;
  isAdmin?: boolean;
  plan?: string;
  planExpiresAt?: string | null;
  themeId?: number | null;
  themeOverrides?: string | null;
  theme?: ApiTheme | null;
};

export type CustomDomainItem = {
  id: number;
  domain: string;
  verificationToken: string;
  status: string;
  createdAt: string;
  updatedAt: string;
};

export type ApiSession = {
  authenticated: boolean;
  user?: ApiUser | null;
};

export type ApiMe = {
  user: ApiUser;
  domains?: CustomDomainItem[];
  meta: {
    totalCount: number;
    draftCount: number;
    publishedCount: number;
  };
};

export type AdminArticle = {
  id: number;
  title: string;
  slug: string;
  draft: boolean;
  tags: string[];
  createdAt: string;
  updatedAt: string;
  commentCount: number;
  author: ApiUser | null;
};

export type AdminComment = {
  id: number;
  name: string;
  content: string;
  posted: string;
  article?: {
    title: string;
    slug: string;
    draft: boolean;
  } | null;
};

export type AdminUser = {
  id: number;
  ident: string;
  displayName: string;
  bio?: string | null;
  isAdmin: boolean;
  publishedCount: number;
  draftCount: number;
};

export type AdminDashboard = {
  meta: {
    articleCount: number;
    commentCount: number;
    userCount: number;
    imageCount: number;
    themeCount: number;
  };
  articles: AdminArticle[];
  comments: AdminComment[];
  users: AdminUser[];
  themes: ApiTheme[];
};

export type ApiPostSummary = {
  id: number;
  title: string;
  slug: string;
  excerpt: string;
  content: string;
  tags: string[];
  createdAt: string;
  updatedAt: string;
  readingMinutes: number;
  visibility?: string;
  publishAt?: string | null;
  author: ApiUser | null;
};

export type ApiComment = {
  id: number;
  name: string;
  content: string;
  posted: string;
  canManage: boolean;
};

export type ApiPostDetail = ApiPostSummary & {
  viewer?: ApiUser | null;
  comments: ApiComment[];
};

export type EditorImage = {
  id: number;
  filename: string;
  publicUrl: string;
  markdown: string;
  description?: string | null;
  previewable: boolean;
};

export type EditorBootstrap = {
  user: ApiUser;
  meta: {
    draftCount: number;
    publishedCount: number;
  };
  tagSuggestions: string[];
  images: EditorImage[];
};

export type EditorMine = {
  drafts: ApiPostSummary[];
  published: ApiPostSummary[];
};

export type EditorArticle = {
  id: number;
  title: string;
  content: string;
  slug: string;
  draft: boolean;
  visibility?: string;
  publishAt?: string | null;
  tags: string[];
  createdAt: string;
  updatedAt: string;
};

export type EditorSaveResponse = {
  articleId: string;
  slug: string;
  draft: boolean;
  permalink?: string;
};
