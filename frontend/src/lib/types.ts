export type ApiUser = {
  ident: string;
  displayName: string;
  bio?: string | null;
  isAdmin?: boolean;
};

export type ApiSession = {
  authenticated: boolean;
  user?: ApiUser | null;
};

export type ApiMe = {
  user: ApiUser;
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
  };
  articles: AdminArticle[];
  comments: AdminComment[];
  users: AdminUser[];
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
