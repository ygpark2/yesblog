<script lang="ts">
  import { base } from '$app/paths';
  import { apiFetch, apiFormPost, apiGet } from '$lib/api';
  import { onMount, tick } from 'svelte';
  import type {
    AdminDashboard,
    AdminThemePayoutSeller,
    AdminThemePayoutsResponse,
    ApiTheme,
    MembershipOrderItem,
    ThemeOrderItem,
    ThemePayoutRequestItem,
    ThemeReviewItem
  } from '$lib/types';

  type AdminSection = 'overview' | 'articles' | 'comments' | 'users' | 'themes' | 'theme-review' | 'theme-orders' | 'theme-payouts' | 'membership-orders';

  interface Props {
    section: AdminSection;
  }

  let { section }: Props = $props();

  const titles: Record<AdminSection, string> = {
    overview: 'Moderation dashboard',
    articles: 'Article management',
    comments: 'Comment moderation',
    users: 'User management',
    themes: 'Theme management',
    'theme-review': 'Theme review queue',
    'theme-orders': 'Theme order management',
    'theme-payouts': 'Theme payout management',
    'membership-orders': 'Membership order management'
  };
  const managedAdminQueryKeys = ['theme', 'review', 'order', 'membershipOrder', 'payout'] as const;
  const sectionQueryKeys: Record<AdminSection, string[]> = {
    overview: ['theme'],
    articles: [],
    comments: [],
    users: [],
    themes: ['theme'],
    'theme-review': ['review'],
    'theme-orders': ['order'],
    'theme-payouts': ['payout'],
    'membership-orders': ['membershipOrder']
  };
  const headerTemplatePlaceholder = '<header><h1>{{post.title}}</h1></header>';
  const bodyTemplatePlaceholder = '<article>{{post.content}}</article>';
  const footerTemplatePlaceholder = '<footer>By {{author.name}}</footer>';
  const customCssPlaceholder = '.theme-template-header { text-align: center; }';

  let dashboard = $state<AdminDashboard | null>(null);
  let selectedThemeId = $state<number | null>(null);
  let reviews = $state<ThemeReviewItem[]>([]);
  let orders = $state<ThemeOrderItem[]>([]);
  let membershipOrders = $state<MembershipOrderItem[]>([]);
  let membershipOrderFilter = $state<'all' | 'initial' | 'renewal'>('all');
  let payouts = $state<ThemePayoutRequestItem[]>([]);
  let payoutSellers = $state<AdminThemePayoutSeller[]>([]);
  let selectedReviewId = $state<number | null>(null);
  let selectedOrderId = $state<number | null>(null);
  let selectedMembershipOrderId = $state<number | null>(null);
  let selectedPayoutId = $state<number | null>(null);
  let payoutSummary = $state({
    requestCount: 0,
    requestedCents: 0,
    paidCents: 0,
    rejectedCents: 0,
    openRequestedCount: 0
  });
  let status = $state('Loading admin dashboard…');
  let loading = $state(true);
  let editingThemeId = $state<number | null>(null);
  let themeName = $state('');
  let themeSlug = $state('');
  let themeDescription = $state('');
  let themeBackgroundColor = $state('#f8f0dc');
  let themeSurfaceColor = $state('#fffef7');
  let themeTextColor = $state('#111111');
  let themeAccentColor = $state('#ffe11a');
  let themeHeadingFont = $state('Cormorant Garamond');
  let themeBodyFont = $state('Space Grotesk');
  let themeHeaderTemplate = $state('');
  let themeBodyTemplate = $state('');
  let themeFooterTemplate = $state('');
  let themeCustomCss = $state('');
  let themePriceCents = $state('0');
  let themeStatus = $state('published');
  let themeLicense = $state('free-remix');
  let themeActive = $state(true);
  let themeFormEl = $state<HTMLElement | undefined>();

  async function fetchDashboard() {
    dashboard = await apiGet<AdminDashboard>('/api/admin/dashboard');
    syncSelectedThemeFromUrl();
    status = 'Admin overview is up to date.';
  }

  function updateSelectedThemeInUrl(themeId: number | null) {
    const url = new URL(window.location.href);
    if (themeId === null) {
      url.searchParams.delete('theme');
    } else {
      url.searchParams.set('theme', String(themeId));
    }
    window.history.replaceState(window.history.state, '', url);
  }

  function setSelectedTheme(themeId: number | null) {
    selectedThemeId = themeId;
    updateSelectedThemeInUrl(themeId);
  }

  function syncSelectedThemeFromUrl() {
    const themes = dashboard?.themes ?? [];
    const themeParam = new URL(window.location.href).searchParams.get('theme');
    const requestedId = themeParam ? Number(themeParam) : null;
    const matchedTheme =
      requestedId !== null && Number.isFinite(requestedId)
        ? themes.find((theme) => theme.id === requestedId)
        : null;
    if (matchedTheme) {
      selectedThemeId = matchedTheme.id;
    } else if (themes.length > 0) {
      setSelectedTheme(themes[0].id);
    } else {
      setSelectedTheme(null);
    }
  }

  function selectedTheme() {
    return dashboard?.themes.find((theme) => theme.id === selectedThemeId) ?? null;
  }

  function isSelectedTheme(themeId: number) {
    return selectedThemeId === themeId;
  }

  function selectedThemeIndex() {
    return (dashboard?.themes ?? []).findIndex((theme) => theme.id === selectedThemeId);
  }

  function selectAdjacentTheme(direction: -1 | 1) {
    const themes = dashboard?.themes ?? [];
    const nextIndex = selectedThemeIndex() + direction;
    if (nextIndex >= 0 && nextIndex < themes.length) {
      setSelectedTheme(themes[nextIndex].id);
    }
  }

  async function copySelectedThemeLink() {
    if (!selectedThemeId) return;
    try {
      await navigator.clipboard.writeText(window.location.href);
      status = 'Theme link copied.';
    } catch {
      status = 'Could not copy the theme link.';
    }
  }

  async function fetchThemeReviews() {
    const data = await apiGet<{ items: ThemeReviewItem[] }>('/api/admin/themes/review');
    reviews = data.items;
    syncSelectedReviewFromUrl();
    status = `${reviews.length} themes waiting for review.`;
  }

  async function fetchThemeOrders() {
    const data = await apiGet<{ items: ThemeOrderItem[] }>('/api/admin/theme/orders');
    orders = data.items;
    syncSelectedOrderFromUrl();
    status = `${orders.length} theme orders loaded.`;
  }

  async function fetchMembershipOrders() {
    const data = await apiGet<{ items: MembershipOrderItem[] }>('/api/admin/membership/orders');
    membershipOrders = data.items;
    syncSelectedMembershipOrderFromUrl();
    status = `${membershipOrders.length} membership orders loaded.`;
  }

  function updateSelectedMembershipOrderInUrl(orderId: number | null) {
    const url = new URL(window.location.href);
    if (orderId === null) {
      url.searchParams.delete('membershipOrder');
    } else {
      url.searchParams.set('membershipOrder', String(orderId));
    }
    window.history.replaceState(window.history.state, '', url);
  }

  function setSelectedMembershipOrder(orderId: number | null) {
    selectedMembershipOrderId = orderId;
    updateSelectedMembershipOrderInUrl(orderId);
  }

  function updateSelectedReviewInUrl(reviewId: number | null) {
    const url = new URL(window.location.href);
    if (reviewId === null) {
      url.searchParams.delete('review');
    } else {
      url.searchParams.set('review', String(reviewId));
    }
    window.history.replaceState(window.history.state, '', url);
  }

  function setSelectedReview(reviewId: number | null) {
    selectedReviewId = reviewId;
    updateSelectedReviewInUrl(reviewId);
  }

  function syncSelectedReviewFromUrl() {
    const reviewParam = new URL(window.location.href).searchParams.get('review');
    const requestedId = reviewParam ? Number(reviewParam) : null;
    const matchedReview =
      requestedId !== null && Number.isFinite(requestedId)
        ? reviews.find((review) => review.id === requestedId)
        : null;
    if (matchedReview) {
      selectedReviewId = matchedReview.id;
    } else if (reviews.length > 0) {
      setSelectedReview(reviews[0].id);
    } else {
      setSelectedReview(null);
    }
  }

  function selectedReview() {
    return reviews.find((review) => review.id === selectedReviewId) ?? null;
  }

  function isSelectedReview(reviewId: number) {
    return selectedReviewId === reviewId;
  }

  function selectedReviewIndex() {
    return reviews.findIndex((review) => review.id === selectedReviewId);
  }

  function selectAdjacentReview(direction: -1 | 1) {
    const nextIndex = selectedReviewIndex() + direction;
    if (nextIndex >= 0 && nextIndex < reviews.length) {
      setSelectedReview(reviews[nextIndex].id);
    }
  }

  async function copySelectedReviewLink() {
    if (!selectedReviewId) return;
    try {
      await navigator.clipboard.writeText(window.location.href);
      status = 'Theme review link copied.';
    } catch {
      status = 'Could not copy the theme review link.';
    }
  }

  function updateSelectedOrderInUrl(orderId: number | null) {
    const url = new URL(window.location.href);
    if (orderId === null) {
      url.searchParams.delete('order');
    } else {
      url.searchParams.set('order', String(orderId));
    }
    window.history.replaceState(window.history.state, '', url);
  }

  function setSelectedOrder(orderId: number | null) {
    selectedOrderId = orderId;
    updateSelectedOrderInUrl(orderId);
  }

  function syncSelectedOrderFromUrl() {
    const orderParam = new URL(window.location.href).searchParams.get('order');
    const requestedId = orderParam ? Number(orderParam) : null;
    const matchedOrder =
      requestedId !== null && Number.isFinite(requestedId)
        ? orders.find((order) => order.id === requestedId)
        : null;
    if (matchedOrder) {
      selectedOrderId = matchedOrder.id;
    } else if (orders.length > 0) {
      setSelectedOrder(orders[0].id);
    } else {
      setSelectedOrder(null);
    }
  }

  function selectedOrder() {
    return orders.find((order) => order.id === selectedOrderId) ?? null;
  }

  function isSelectedOrder(orderId: number) {
    return selectedOrderId === orderId;
  }

  function selectedOrderIndex() {
    return orders.findIndex((order) => order.id === selectedOrderId);
  }

  function selectAdjacentOrder(direction: -1 | 1) {
    const nextIndex = selectedOrderIndex() + direction;
    if (nextIndex >= 0 && nextIndex < orders.length) {
      setSelectedOrder(orders[nextIndex].id);
    }
  }

  async function copySelectedOrderLink() {
    if (!selectedOrderId) return;
    try {
      await navigator.clipboard.writeText(window.location.href);
      status = 'Theme order link copied.';
    } catch {
      status = 'Could not copy the theme order link.';
    }
  }

  async function fetchThemePayouts() {
    const data = await apiGet<AdminThemePayoutsResponse>('/api/admin/theme/payouts');
    payouts = data.items;
    payoutSellers = data.sellers;
    payoutSummary = data.summary;
    syncSelectedPayoutFromUrl();
    status = `${payouts.length} payout requests loaded.`;
  }

  function updateSelectedPayoutInUrl(payoutId: number | null) {
    const url = new URL(window.location.href);
    if (payoutId === null) {
      url.searchParams.delete('payout');
    } else {
      url.searchParams.set('payout', String(payoutId));
    }
    window.history.replaceState(window.history.state, '', url);
  }

  function setSelectedPayout(payoutId: number | null) {
    selectedPayoutId = payoutId;
    updateSelectedPayoutInUrl(payoutId);
  }

  function syncSelectedPayoutFromUrl() {
    const payoutParam = new URL(window.location.href).searchParams.get('payout');
    const requestedId = payoutParam ? Number(payoutParam) : null;
    const matchedPayout =
      requestedId !== null && Number.isFinite(requestedId)
        ? payouts.find((payout) => payout.id === requestedId)
        : null;
    if (matchedPayout) {
      selectedPayoutId = matchedPayout.id;
    } else if (payouts.length > 0) {
      setSelectedPayout(payouts[0].id);
    } else {
      setSelectedPayout(null);
    }
  }

  function selectedPayout() {
    return payouts.find((payout) => payout.id === selectedPayoutId) ?? null;
  }

  function isSelectedPayout(payoutId: number) {
    return selectedPayoutId === payoutId;
  }

  function selectedPayoutIndex() {
    return payouts.findIndex((payout) => payout.id === selectedPayoutId);
  }

  function selectAdjacentPayout(direction: -1 | 1) {
    const nextIndex = selectedPayoutIndex() + direction;
    if (nextIndex >= 0 && nextIndex < payouts.length) {
      setSelectedPayout(payouts[nextIndex].id);
    }
  }

  async function copySelectedPayoutLink() {
    if (!selectedPayoutId) return;
    try {
      await navigator.clipboard.writeText(window.location.href);
      status = 'Payout request link copied.';
    } catch {
      status = 'Could not copy the payout request link.';
    }
  }

  async function deleteArticle(articleId: number) {
    if (!window.confirm('Delete this article and its comments permanently?')) return;
    status = 'Deleting article…';
    try {
      await apiFormPost(`/api/admin/article/${articleId}/delete`, new URLSearchParams());
      await fetchDashboard();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Article delete failed.';
    }
  }

  async function deleteComment(commentId: number) {
    if (!window.confirm('Delete this comment permanently?')) return;
    status = 'Deleting comment…';
    try {
      await apiFormPost(`/api/admin/comment/${commentId}/delete`, new URLSearchParams());
      await fetchDashboard();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Comment delete failed.';
    }
  }

  async function deleteUser(userId: number, ident: string) {
    if (!window.confirm(`Delete user ${ident} and all authored articles permanently?`)) return;
    status = 'Deleting user…';
    try {
      await apiFormPost(`/api/admin/user/${userId}/delete`, new URLSearchParams());
      await fetchDashboard();
    } catch (error) {
      status = error instanceof Error ? error.message : 'User delete failed.';
    }
  }

  async function editTheme(theme: ApiTheme) {
    setSelectedTheme(theme.id);
    editingThemeId = theme.id;
    themeName = theme.name;
    themeSlug = theme.slug;
    themeDescription = theme.description ?? '';
    themeBackgroundColor = theme.backgroundColor;
    themeSurfaceColor = theme.surfaceColor;
    themeTextColor = theme.textColor;
    themeAccentColor = theme.accentColor;
    themeHeadingFont = theme.headingFont ?? '';
    themeBodyFont = theme.bodyFont ?? '';
    themeHeaderTemplate = theme.headerTemplate ?? '';
    themeBodyTemplate = theme.bodyTemplate ?? '';
    themeFooterTemplate = theme.footerTemplate ?? '';
    themeCustomCss = theme.customCss ?? '';
    themePriceCents = String(theme.priceCents ?? 0);
    themeStatus = theme.status ?? 'published';
    themeLicense = theme.license ?? '';
    themeActive = theme.active;
    status = `Editing theme: ${theme.name}`;
    await tick();
    themeFormEl?.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }

  function resetThemeForm() {
    editingThemeId = null;
    themeName = '';
    themeSlug = '';
    themeDescription = '';
    themeBackgroundColor = '#f8f0dc';
    themeSurfaceColor = '#fffef7';
    themeTextColor = '#111111';
    themeAccentColor = '#ffe11a';
    themeHeadingFont = 'Cormorant Garamond';
    themeBodyFont = 'Space Grotesk';
    themeHeaderTemplate = '';
    themeBodyTemplate = '';
    themeFooterTemplate = '';
    themeCustomCss = '';
    themePriceCents = '0';
    themeStatus = 'published';
    themeLicense = 'free-remix';
    themeActive = true;
  }

  async function saveTheme() {
    status = editingThemeId ? 'Updating theme...' : 'Creating theme...';
    const payload = new URLSearchParams({
      name: themeName,
      slug: themeSlug,
      description: themeDescription,
      backgroundColor: themeBackgroundColor,
      surfaceColor: themeSurfaceColor,
      textColor: themeTextColor,
      accentColor: themeAccentColor,
      headingFont: themeHeadingFont,
      bodyFont: themeBodyFont,
      headerTemplate: themeHeaderTemplate,
      bodyTemplate: themeBodyTemplate,
      footerTemplate: themeFooterTemplate,
      customCss: themeCustomCss,
      priceCents: themePriceCents,
      status: themeStatus,
      license: themeLicense,
      active: String(themeActive)
    });
    const path = editingThemeId ? `/api/admin/theme/${editingThemeId}/update` : '/api/admin/themes';
    try {
      await apiFormPost(path, payload);
      resetThemeForm();
      await fetchDashboard();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Theme save failed.';
    }
  }

  async function deleteTheme(themeId: number, name: string) {
    if (!window.confirm(`Delete theme ${name}? Users using it will return to the default theme.`)) return;
    status = 'Deleting theme...';
    try {
      await apiFormPost(`/api/admin/theme/${themeId}/delete`, new URLSearchParams());
      if (editingThemeId === themeId) resetThemeForm();
      await fetchDashboard();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Theme delete failed.';
    }
  }

  async function approveTheme(themeId: number) {
    status = 'Approving theme...';
    try {
      await apiFormPost(`/api/admin/theme/${themeId}/approve`, new URLSearchParams());
      await Promise.all([fetchDashboard(), fetchThemeReviews()]);
    } catch (error) {
      status = error instanceof Error ? error.message : 'Theme approval failed.';
    }
  }

  async function rejectTheme(themeId: number) {
    const note = (window.prompt('Rejection note (required)', '') ?? '').trim();
    if (!note) {
      status = 'Rejection note is required.';
      return;
    }
    status = 'Rejecting theme...';
    const payload = new URLSearchParams({ note });
    try {
      await apiFormPost(`/api/admin/theme/${themeId}/reject`, payload);
      await Promise.all([fetchDashboard(), fetchThemeReviews()]);
    } catch (error) {
      status = error instanceof Error ? error.message : 'Theme rejection failed.';
    }
  }

  async function updateOrder(orderId: number, nextStatus: string) {
    status = `Updating order to ${nextStatus}...`;
    const payload = new URLSearchParams({ status: nextStatus });
    try {
      await apiFormPost(`/api/admin/theme/order/${orderId}/update`, payload);
      await fetchThemeOrders();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Order update failed.';
    }
  }

  async function updateMembershipOrder(orderId: number, nextStatus: string) {
    status = `Updating membership order to ${nextStatus}...`;
    const promptLabel =
      nextStatus === 'failed' || nextStatus === 'cancelled'
        ? 'Failure or cancellation note (required)'
        : 'Admin note (optional)';
    const note = (window.prompt(promptLabel, '') ?? '').trim();
    if ((nextStatus === 'failed' || nextStatus === 'cancelled') && !note) {
      status = 'Failure or cancellation note is required.';
      return;
    }
    const payload = new URLSearchParams({ status: nextStatus, note });
    try {
      await apiFormPost(`/api/admin/membership/order/${orderId}/update`, payload);
      await fetchMembershipOrders();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Membership order update failed.';
    }
  }

  function filteredMembershipOrders() {
    if (membershipOrderFilter === 'all') {
      return membershipOrders;
    }
    if (membershipOrderFilter === 'renewal') {
      return membershipOrders.filter((order) => order.provider === 'membership-renewal');
    }
    return membershipOrders.filter((order) => order.provider !== 'membership-renewal');
  }

  function syncSelectedMembershipOrderFromUrl() {
    const visibleOrders = filteredMembershipOrders();
    const orderParam = new URL(window.location.href).searchParams.get('membershipOrder');
    const requestedId = orderParam ? Number(orderParam) : null;
    const matchedOrder =
      requestedId !== null && Number.isFinite(requestedId)
        ? visibleOrders.find((order) => order.id === requestedId)
        : null;
    if (matchedOrder) {
      selectedMembershipOrderId = matchedOrder.id;
    } else if (visibleOrders.length > 0) {
      setSelectedMembershipOrder(visibleOrders[0].id);
    } else {
      setSelectedMembershipOrder(null);
    }
  }

  function selectedMembershipOrder() {
    return filteredMembershipOrders().find((order) => order.id === selectedMembershipOrderId) ?? null;
  }

  function isSelectedMembershipOrder(orderId: number) {
    return selectedMembershipOrderId === orderId;
  }

  function selectedMembershipOrderIndex() {
    return filteredMembershipOrders().findIndex((order) => order.id === selectedMembershipOrderId);
  }

  function selectAdjacentMembershipOrder(direction: -1 | 1) {
    const visibleOrders = filteredMembershipOrders();
    const nextIndex = selectedMembershipOrderIndex() + direction;
    if (nextIndex >= 0 && nextIndex < visibleOrders.length) {
      setSelectedMembershipOrder(visibleOrders[nextIndex].id);
    }
  }

  async function copySelectedMembershipOrderLink() {
    if (!selectedMembershipOrderId) return;
    try {
      await navigator.clipboard.writeText(window.location.href);
      status = 'Membership order link copied.';
    } catch {
      status = 'Could not copy the membership order link.';
    }
  }

  async function updatePayout(payoutId: number, nextStatus: string) {
    status = `Updating payout to ${nextStatus}...`;
    const promptLabel = nextStatus === 'rejected' ? 'Rejection note (required)' : 'Admin note (optional)';
    const note = (window.prompt(promptLabel, '') ?? '').trim();
    if (nextStatus === 'rejected' && !note) {
      status = 'Rejection note is required.';
      return;
    }
    const payload = new URLSearchParams({ status: nextStatus, note });
    try {
      await apiFormPost(`/api/admin/theme/payout/${payoutId}/update`, payload);
      await fetchThemePayouts();
    } catch (error) {
      status = error instanceof Error ? error.message : 'Payout update failed.';
    }
  }

  function normalizeAdminQueryForSection() {
    if (typeof window === 'undefined') return;
    const allowedKeys = new Set(sectionQueryKeys[section]);
    const url = new URL(window.location.href);
    let changed = false;
    for (const key of managedAdminQueryKeys) {
      if (!allowedKeys.has(key) && url.searchParams.has(key)) {
        url.searchParams.delete(key);
        changed = true;
      }
    }
    if (changed) {
      window.history.replaceState(window.history.state, '', url);
    }
  }

  function syncSelectionForCurrentSection() {
    if ((section === 'overview' || section === 'themes') && dashboard) {
      syncSelectedThemeFromUrl();
    }
    if (section === 'theme-review' && reviews.length > 0) {
      syncSelectedReviewFromUrl();
    }
    if (section === 'theme-orders' && orders.length > 0) {
      syncSelectedOrderFromUrl();
    }
    if (section === 'membership-orders') {
      syncSelectedMembershipOrderFromUrl();
    }
    if (section === 'theme-payouts' && payouts.length > 0) {
      syncSelectedPayoutFromUrl();
    }
  }

  $effect(() => {
    section;
    normalizeAdminQueryForSection();
    syncSelectionForCurrentSection();
  });

  $effect(() => {
    membershipOrderFilter;
    if (section === 'membership-orders') {
      syncSelectedMembershipOrderFromUrl();
    }
  });

  onMount(() => {
    const handlePopState = () => {
      if (section === 'overview' || section === 'themes') {
        syncSelectedThemeFromUrl();
      }
      if (section === 'theme-review') {
        syncSelectedReviewFromUrl();
      }
      if (section === 'theme-orders') {
        syncSelectedOrderFromUrl();
      }
      if (section === 'membership-orders') {
        syncSelectedMembershipOrderFromUrl();
      }
      if (section === 'theme-payouts') {
        syncSelectedPayoutFromUrl();
      }
    };
    window.addEventListener('popstate', handlePopState);
    void (async () => {
      try {
        await fetchDashboard();
        if (section === 'theme-review') {
          await fetchThemeReviews();
        }
        if (section === 'theme-orders') {
          await fetchThemeOrders();
        }
      if (section === 'theme-payouts') {
        await fetchThemePayouts();
      }
      if (section === 'membership-orders') {
        await fetchMembershipOrders();
      }
    } catch (error) {
        status = error instanceof Error ? error.message : 'Open an admin session first.';
      } finally {
        loading = false;
      }
    })();

    return () => {
      window.removeEventListener('popstate', handlePopState);
    };
  });
</script>

<div class="admin-app">
  <aside class="admin-sidebar">
    <a class="admin-brand" href={`${base}/admin`}>
      <span class="admin-brand-mark">YB</span>
      <span>
        <strong>YesBlog</strong>
        <small>Admin Console</small>
      </span>
    </a>

    <nav class="admin-menu" aria-label="Admin menu">
      <a class:admin-menu-item-active={section === 'overview'} class="admin-menu-item" href={`${base}/admin`}>
        <span>Overview</span>
        <small>Dashboard</small>
      </a>
      <a class:admin-menu-item-active={section === 'articles'} class="admin-menu-item" href={`${base}/admin/articles`}>
        <span>Articles</span>
        <small>{dashboard?.meta.articleCount ?? 0}</small>
      </a>
      <a class:admin-menu-item-active={section === 'comments'} class="admin-menu-item" href={`${base}/admin/comments`}>
        <span>Comments</span>
        <small>{dashboard?.meta.commentCount ?? 0}</small>
      </a>
      <a class:admin-menu-item-active={section === 'users'} class="admin-menu-item" href={`${base}/admin/users`}>
        <span>Users</span>
        <small>{dashboard?.meta.userCount ?? 0}</small>
      </a>
      <a class:admin-menu-item-active={section === 'themes'} class="admin-menu-item" href={`${base}/admin/themes`}>
        <span>Themes</span>
        <small>{dashboard?.meta.themeCount ?? 0}</small>
      </a>
      <a class:admin-menu-item-active={section === 'theme-review'} class="admin-menu-item" href={`${base}/admin/themes/review`}>
        <span>Theme Review</span>
        <small>{reviews.length}</small>
      </a>
      <a class:admin-menu-item-active={section === 'theme-orders'} class="admin-menu-item" href={`${base}/admin/themes/orders`}>
        <span>Theme Orders</span>
        <small>{orders.length}</small>
      </a>
      <a class:admin-menu-item-active={section === 'theme-payouts'} class="admin-menu-item" href={`${base}/admin/themes/payouts`}>
        <span>Theme Payouts</span>
        <small>{payouts.length}</small>
      </a>
      <a class:admin-menu-item-active={section === 'membership-orders'} class="admin-menu-item" href={`${base}/admin/memberships/orders`}>
        <span>Membership Orders</span>
        <small>{membershipOrders.length}</small>
      </a>
    </nav>

    <div class="admin-sidebar-footer">
      <a href={`${base}/`}>View site</a>
      <a href={`${base}/studio`}>Open studio</a>
      <a href={`${base}/profile`}>Profile</a>
    </div>
  </aside>

  <main class="admin-main">
    <section class="admin-topbar">
      <div>
        <p class="admin-kicker">Administration</p>
        <h1>{titles[section]}</h1>
      </div>
      <p class="admin-status">{status}</p>
    </section>

    {#if loading}
      <section class="admin-panel">
        <p class="admin-copy">Loading dashboard...</p>
      </section>
    {:else if dashboard}
      {#if section === 'overview'}
        <section class="admin-metrics" aria-label="Dashboard metrics">
          <article>
            <span>{dashboard.meta.articleCount}</span>
            <p>Articles</p>
          </article>
          <article>
            <span>{dashboard.meta.commentCount}</span>
            <p>Comments</p>
          </article>
          <article>
            <span>{dashboard.meta.userCount}</span>
            <p>Users</p>
          </article>
          <article>
            <span>{dashboard.meta.imageCount}</span>
            <p>Images</p>
          </article>
          <article>
            <span>{dashboard.meta.themeCount}</span>
            <p>Themes</p>
          </article>
        </section>
      {/if}

      <div class:admin-content-grid={section === 'overview'} class="admin-section-page">
        {#if section === 'overview' || section === 'articles'}
        <section class="admin-panel admin-panel-wide">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Content</p>
              <h2>Recent articles</h2>
            </div>
            <span>{dashboard.articles.length} items</span>
          </div>
          <div class="admin-list">
            {#each dashboard.articles as article}
              <article class="admin-list-card">
                <div class="admin-list-title">
                  <strong>{article.title}</strong>
                  <span class:admin-badge-live={!article.draft} class="admin-badge">{article.draft ? 'Draft' : 'Published'}</span>
                </div>
                <p class="admin-copy">
                  {article.author?.displayName ?? article.author?.ident ?? 'Unknown author'} · {new Date(article.updatedAt).toLocaleString()} · {article.commentCount} comments
                </p>
                <div class="admin-tag-row">
                  {#each article.tags as tag}
                    <span>{tag}</span>
                  {/each}
                </div>
                <div class="admin-actions">
                  {#if article.draft}
                    <span>Draft posts are managed from the writer studio.</span>
                  {:else}
                    <a href={`${base}/posts/${article.slug}`}>View post</a>
                  {/if}
                  <button type="button" onclick={() => deleteArticle(article.id)}>Delete</button>
                </div>
              </article>
            {/each}
          </div>
        </section>
        {/if}

        {#if section === 'overview' || section === 'comments'}
        <section class="admin-panel">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Moderation</p>
              <h2>Recent comments</h2>
            </div>
            <span>{dashboard.comments.length}</span>
          </div>
          <div class="admin-list">
            {#each dashboard.comments as comment}
              <article class="admin-list-card">
                <strong>{comment.name}</strong>
                <p class="admin-copy">{comment.content}</p>
                <p class="admin-copy">
                  {new Date(comment.posted).toLocaleString()}
                  {#if comment.article}
                    · {comment.article.title}
                  {/if}
                </p>
                <div class="admin-actions">
                  {#if comment.article && !comment.article.draft}
                    <a href={`${base}/posts/${comment.article.slug}`}>Open post</a>
                  {/if}
                  <button type="button" onclick={() => deleteComment(comment.id)}>Delete</button>
                </div>
              </article>
            {/each}
          </div>
        </section>
        {/if}

        {#if section === 'overview' || section === 'users'}
        <section class="admin-panel">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Accounts</p>
              <h2>Recent users</h2>
            </div>
            <span>{dashboard.users.length}</span>
          </div>
          <div class="admin-list">
            {#each dashboard.users as user}
              <article class="admin-list-card">
                <div class="admin-list-title">
                  <strong>{user.displayName}</strong>
                  {#if user.isAdmin}
                    <span class="admin-badge admin-badge-live">Admin</span>
                  {/if}
                </div>
                <p class="admin-copy">@{user.ident}</p>
                {#if user.bio}
                  <p class="admin-copy">{user.bio}</p>
                {/if}
                <p class="admin-copy">{user.publishedCount} published · {user.draftCount} drafts</p>
                <div class="admin-actions">
                  <a href={`${base}/users/${user.ident}`}>Open blog</a>
                  <button type="button" onclick={() => deleteUser(user.id, user.ident)}>Delete</button>
                </div>
              </article>
            {/each}
          </div>
        </section>
        {/if}

        {#if section === 'theme-review'}
        <section class="admin-panel admin-panel-wide">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Marketplace moderation</p>
              <h2>Pending review queue</h2>
            </div>
            <span>{reviews.length} waiting</span>
          </div>

          <div class="admin-list admin-theme-list">
            {#each reviews as review}
              <article
                class:admin-list-card-selected={isSelectedReview(review.id)}
                class="admin-list-card"
                style={`background: ${review.theme?.surfaceColor ?? '#ffffff'}; color: ${review.theme?.textColor ?? '#111111'};`}
              >
                <div class="admin-list-title">
                  <strong>{review.theme?.name ?? 'Unknown theme'}</strong>
                  <span class="admin-badge">{review.status}</span>
                </div>
                <p class="admin-copy">{review.theme?.description ?? review.theme?.slug ?? 'Missing theme details'}</p>
                <p class="admin-copy">
                  Submitted {new Date(review.createdAt).toLocaleString()}
                  {#if review.theme}
                    · {review.theme.priceCents === 0 ? 'Free' : `$${(review.theme.priceCents / 100).toFixed(2)}`}
                  {/if}
                </p>
                <div class="admin-tag-row">
                  <span>{review.theme?.license ?? 'No license'}</span>
                  {#if review.theme?.parentId}
                    <span>Remix</span>
                  {/if}
                </div>
                <div class="admin-actions">
                  {#if review.theme}
                    <button
                      type="button"
                      class:admin-action-active={isSelectedReview(review.id)}
                      aria-pressed={isSelectedReview(review.id)}
                      onclick={() => setSelectedReview(review.id)}
                    >
                      View details
                    </button>
                    <button type="button" onclick={() => review.theme && approveTheme(review.theme.id)}>Approve</button>
                    <button type="button" onclick={() => review.theme && rejectTheme(review.theme.id)}>Reject</button>
                    <button type="button" onclick={() => review.theme && editTheme(review.theme)}>Open in editor</button>
                  {/if}
                </div>
              </article>
            {/each}
          </div>

          {#if selectedReview()}
            <div class="admin-section-head">
              <div>
                <p class="admin-kicker">Details</p>
                <h2>Selected theme review</h2>
              </div>
              <div class="admin-actions">
                <span>{selectedReview()?.status}</span>
                <button type="button" onclick={() => selectAdjacentReview(-1)} disabled={selectedReviewIndex() <= 0}>Prev</button>
                <button
                  type="button"
                  onclick={() => selectAdjacentReview(1)}
                  disabled={selectedReviewIndex() === -1 || selectedReviewIndex() >= reviews.length - 1}
                >
                  Next
                </button>
                <button type="button" onclick={() => copySelectedReviewLink()}>Copy link</button>
              </div>
            </div>

            <div class="admin-metrics" aria-label="Selected theme review">
              <article>
                <span>{selectedReview()?.theme?.name ?? 'Unknown theme'}</span>
                <p>Theme</p>
              </article>
              <article>
                <span>{selectedReview()?.theme?.priceCents === 0 ? 'Free' : `$${(((selectedReview()?.theme?.priceCents ?? 0) / 100).toFixed(2))}`}</span>
                <p>Price</p>
              </article>
              <article>
                <span>{selectedReview()?.theme?.license ?? 'No license'}</span>
                <p>License</p>
              </article>
              <article>
                <span>{selectedReview()?.createdAt ? new Date(selectedReview()!.createdAt).toLocaleDateString() : '-'}</span>
                <p>Submitted</p>
              </article>
            </div>

            {#if selectedReview()?.theme?.description}
              <article class="admin-list-card">
                <strong>Description</strong>
                <p class="admin-copy">{selectedReview()?.theme?.description}</p>
              </article>
            {/if}
          {/if}
        </section>
        {/if}

        {#if section === 'theme-orders'}
        <section class="admin-panel admin-panel-wide">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Marketplace orders</p>
              <h2>Theme orders</h2>
            </div>
            <span>{orders.length} orders</span>
          </div>

          <div class="admin-list">
            {#each orders as order}
              <article class:admin-list-card-selected={isSelectedOrder(order.id)} class="admin-list-card">
                <div class="admin-list-title">
                  <strong>{order.theme?.name ?? 'Unknown theme'}</strong>
                  <span class:admin-badge-live={order.status === 'paid'} class="admin-badge">{order.status}</span>
                </div>
                <p class="admin-copy">
                  {(order.user?.displayName ?? order.user?.ident ?? 'Unknown buyer')} · {order.amountCents === 0 ? 'Free' : `$${(order.amountCents / 100).toFixed(2)}`}
                </p>
                <p class="admin-copy">{new Date(order.createdAt).toLocaleString()}</p>
                <div class="admin-actions">
                  <button
                    type="button"
                    class:admin-action-active={isSelectedOrder(order.id)}
                    aria-pressed={isSelectedOrder(order.id)}
                    onclick={() => setSelectedOrder(order.id)}
                  >
                    View details
                  </button>
                  <button type="button" onclick={() => updateOrder(order.id, 'pending')}>Mark pending</button>
                  <button type="button" onclick={() => updateOrder(order.id, 'paid')}>Mark paid</button>
                  <button type="button" onclick={() => updateOrder(order.id, 'failed')}>Mark failed</button>
                  <button type="button" onclick={() => updateOrder(order.id, 'cancelled')}>Cancel</button>
                </div>
              </article>
            {/each}
          </div>

          {#if selectedOrder()}
            <div class="admin-section-head">
              <div>
                <p class="admin-kicker">Details</p>
                <h2>Selected theme order</h2>
              </div>
              <div class="admin-actions">
                <span>{selectedOrder()?.status}</span>
                <button type="button" onclick={() => selectAdjacentOrder(-1)} disabled={selectedOrderIndex() <= 0}>Prev</button>
                <button
                  type="button"
                  onclick={() => selectAdjacentOrder(1)}
                  disabled={selectedOrderIndex() === -1 || selectedOrderIndex() >= orders.length - 1}
                >
                  Next
                </button>
                <button type="button" onclick={() => copySelectedOrderLink()}>Copy link</button>
              </div>
            </div>

            <div class="admin-metrics" aria-label="Selected theme order">
              <article>
                <span>{selectedOrder()?.theme?.name ?? 'Unknown theme'}</span>
                <p>Theme</p>
              </article>
              <article>
                <span>{selectedOrder()?.user?.displayName ?? selectedOrder()?.user?.ident ?? 'Unknown buyer'}</span>
                <p>Buyer</p>
              </article>
              <article>
                <span>{selectedOrder()?.amountCents === 0 ? 'Free' : `$${(((selectedOrder()?.amountCents ?? 0) / 100).toFixed(2))}`}</span>
                <p>Amount</p>
              </article>
              <article>
                <span>{selectedOrder()?.createdAt ? new Date(selectedOrder()!.createdAt).toLocaleDateString() : '-'}</span>
                <p>Created</p>
              </article>
            </div>
          {/if}
        </section>
        {/if}

        {#if section === 'membership-orders'}
        <section class="admin-panel admin-panel-wide">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Membership billing</p>
              <h2>Membership payment orders</h2>
            </div>
            <div class="admin-actions">
              <label>
                <span class="sr-only">Membership order filter</span>
                <select bind:value={membershipOrderFilter}>
                  <option value="all">All</option>
                  <option value="initial">Initial only</option>
                  <option value="renewal">Renewals only</option>
                </select>
              </label>
              <span>{filteredMembershipOrders().length} orders</span>
            </div>
          </div>

          <div class="admin-list">
            {#each filteredMembershipOrders() as order}
              <article class:admin-list-card-selected={isSelectedMembershipOrder(order.id)} class="admin-list-card">
                <div class="admin-list-title">
                  <strong>{order.creator?.displayName ?? order.creator?.ident ?? 'Unknown writer'}</strong>
                  <span class:admin-badge-live={order.status === 'paid'} class="admin-badge">{order.status}</span>
                </div>
                <p class="admin-copy">
                  {(order.member?.displayName ?? order.member?.ident ?? 'Unknown member')} · ${(order.amountCents / 100).toFixed(2)}
                </p>
                <p class="admin-copy">{order.provider === 'membership-renewal' ? 'Renewal billing' : 'Initial membership payment'}</p>
                <p class="admin-copy">{new Date(order.createdAt).toLocaleString()}</p>
                {#if order.membership}
                  <p class="admin-copy">Membership status: {order.membership.status}</p>
                {/if}
                {#if order.adminNote}
                  <p class="admin-copy">Admin note: {order.adminNote}</p>
                {/if}
                <div class="admin-actions">
                  <button
                    type="button"
                    class:admin-action-active={isSelectedMembershipOrder(order.id)}
                    aria-pressed={isSelectedMembershipOrder(order.id)}
                    onclick={() => setSelectedMembershipOrder(order.id)}
                  >
                    View details
                  </button>
                  <button type="button" onclick={() => updateMembershipOrder(order.id, 'pending')}>Mark pending</button>
                  <button type="button" onclick={() => updateMembershipOrder(order.id, 'paid')}>Mark paid</button>
                  <button type="button" onclick={() => updateMembershipOrder(order.id, 'failed')}>Mark failed</button>
                  <button type="button" onclick={() => updateMembershipOrder(order.id, 'cancelled')}>Cancel</button>
                </div>
              </article>
            {/each}
          </div>

          {#if selectedMembershipOrder()}
            <div class="admin-section-head">
              <div>
                <p class="admin-kicker">Details</p>
                <h2>Selected membership order</h2>
              </div>
              <div class="admin-actions">
                <span>{selectedMembershipOrder()?.status}</span>
                <button
                  type="button"
                  onclick={() => selectAdjacentMembershipOrder(-1)}
                  disabled={selectedMembershipOrderIndex() <= 0}
                >
                  Prev
                </button>
                <button
                  type="button"
                  onclick={() => selectAdjacentMembershipOrder(1)}
                  disabled={selectedMembershipOrderIndex() === -1 || selectedMembershipOrderIndex() >= filteredMembershipOrders().length - 1}
                >
                  Next
                </button>
                <button type="button" onclick={() => copySelectedMembershipOrderLink()}>Copy link</button>
              </div>
            </div>

            <div class="admin-metrics" aria-label="Selected membership order">
              <article>
                <span>{selectedMembershipOrder()?.creator?.displayName ?? selectedMembershipOrder()?.creator?.ident ?? 'Unknown writer'}</span>
                <p>Writer</p>
              </article>
              <article>
                <span>{selectedMembershipOrder()?.member?.displayName ?? selectedMembershipOrder()?.member?.ident ?? 'Unknown member'}</span>
                <p>Member</p>
              </article>
              <article>
                <span>{selectedMembershipOrder()?.provider === 'membership-renewal' ? 'Renewal' : 'Initial'}</span>
                <p>Billing type</p>
              </article>
              <article>
                <span>${(((selectedMembershipOrder()?.amountCents ?? 0) / 100).toFixed(2))}</span>
                <p>Amount</p>
              </article>
              <article>
                <span>{selectedMembershipOrder()?.createdAt ? new Date(selectedMembershipOrder()!.createdAt).toLocaleDateString() : '-'}</span>
                <p>Created</p>
              </article>
            </div>

            {#if selectedMembershipOrder()?.membership || selectedMembershipOrder()?.adminNote}
              <article class="admin-list-card">
                <strong>Order context</strong>
                {#if selectedMembershipOrder()?.membership}
                  <p class="admin-copy">Membership status: {selectedMembershipOrder()?.membership?.status}</p>
                  {#if selectedMembershipOrder()?.membership?.expiresAt}
                    <p class="admin-copy">Expires: {new Date(selectedMembershipOrder()!.membership!.expiresAt!).toLocaleString()}</p>
                  {/if}
                  <p class="admin-copy">Auto renew: {selectedMembershipOrder()?.membership?.autoRenew ? 'on' : 'off'}</p>
                {/if}
                {#if selectedMembershipOrder()?.adminNote}
                  <p class="admin-copy">Admin note: {selectedMembershipOrder()?.adminNote}</p>
                {/if}
              </article>
            {/if}
          {/if}
        </section>
        {/if}

        {#if section === 'theme-payouts'}
        <section class="admin-panel admin-panel-wide">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Marketplace payouts</p>
              <h2>Theme payout requests</h2>
            </div>
            <span>{payouts.length} requests</span>
          </div>

          <div class="admin-metrics" aria-label="Theme payout summary">
            <article>
              <span>{payoutSummary.requestCount}</span>
              <p>Requests</p>
            </article>
            <article>
              <span>{payoutSummary.openRequestedCount}</span>
              <p>Open</p>
            </article>
            <article>
              <span>${(payoutSummary.requestedCents / 100).toFixed(2)}</span>
              <p>Requested</p>
            </article>
            <article>
              <span>${(payoutSummary.paidCents / 100).toFixed(2)}</span>
              <p>Paid</p>
            </article>
            <article>
              <span>${(payoutSummary.rejectedCents / 100).toFixed(2)}</span>
              <p>Rejected</p>
            </article>
          </div>

          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Sellers</p>
              <h2>Seller payout totals</h2>
            </div>
            <span>{payoutSellers.length} sellers</span>
          </div>

          <div class="admin-list">
            {#each payoutSellers as seller}
              <article class="admin-list-card">
                <div class="admin-list-title">
                  <strong>{seller.user?.displayName ?? seller.user?.ident ?? 'Unknown seller'}</strong>
                  <span class="admin-badge">{seller.openRequestedCount} open</span>
                </div>
                <div class="admin-tag-row">
                  <span>{seller.requestCount} requests</span>
                  <span>${(seller.requestedCents / 100).toFixed(2)} requested</span>
                  <span>${(seller.paidCents / 100).toFixed(2)} paid</span>
                  <span>${(seller.rejectedCents / 100).toFixed(2)} rejected</span>
                </div>
              </article>
            {/each}
          </div>

          <div class="admin-list">
            {#each payouts as payout}
              <article class:admin-list-card-selected={isSelectedPayout(payout.id)} class="admin-list-card">
                <div class="admin-list-title">
                  <strong>{payout.user?.displayName ?? payout.user?.ident ?? 'Unknown seller'}</strong>
                  <span class:admin-badge-live={payout.status === 'paid'} class="admin-badge">{payout.status}</span>
                </div>
                <p class="admin-copy">
                  ${(payout.amountCents / 100).toFixed(2)} · requested {new Date(payout.requestedAt).toLocaleString()}
                </p>
                {#if payout.processedAt}
                  <p class="admin-copy">Processed {new Date(payout.processedAt).toLocaleString()}</p>
                {/if}
                {#if payout.sellerNote}
                  <p class="admin-copy">Seller note: {payout.sellerNote}</p>
                {/if}
                {#if payout.adminNote}
                  <p class="admin-copy">Admin note: {payout.adminNote}</p>
                {/if}
                <div class="admin-actions">
                  <button
                    type="button"
                    class:admin-action-active={isSelectedPayout(payout.id)}
                    aria-pressed={isSelectedPayout(payout.id)}
                    onclick={() => setSelectedPayout(payout.id)}
                  >
                    View details
                  </button>
                  <button type="button" onclick={() => updatePayout(payout.id, 'requested')}>Mark requested</button>
                  <button type="button" onclick={() => updatePayout(payout.id, 'paid')}>Mark paid</button>
                  <button type="button" onclick={() => updatePayout(payout.id, 'rejected')}>Reject</button>
                </div>
              </article>
            {/each}
          </div>

          {#if selectedPayout()}
            <div class="admin-section-head">
              <div>
                <p class="admin-kicker">Details</p>
                <h2>Selected payout request</h2>
              </div>
              <div class="admin-actions">
                <span>{selectedPayout()?.status}</span>
                <button type="button" onclick={() => selectAdjacentPayout(-1)} disabled={selectedPayoutIndex() <= 0}>Prev</button>
                <button
                  type="button"
                  onclick={() => selectAdjacentPayout(1)}
                  disabled={selectedPayoutIndex() === -1 || selectedPayoutIndex() >= payouts.length - 1}
                >
                  Next
                </button>
                <button type="button" onclick={() => copySelectedPayoutLink()}>Copy link</button>
              </div>
            </div>

            <div class="admin-metrics" aria-label="Selected payout request">
              <article>
                <span>${((selectedPayout()?.amountCents ?? 0) / 100).toFixed(2)}</span>
                <p>Amount</p>
              </article>
              <article>
                <span>{selectedPayout()?.user?.displayName ?? selectedPayout()?.user?.ident ?? 'Unknown'}</span>
                <p>Seller</p>
              </article>
              <article>
                <span>{selectedPayout()?.requestedAt ? new Date(selectedPayout()!.requestedAt).toLocaleDateString() : '-'}</span>
                <p>Requested</p>
              </article>
              <article>
                <span>{selectedPayout()?.processedAt ? new Date(selectedPayout()?.processedAt ?? '').toLocaleDateString() : '-'}</span>
                <p>Processed</p>
              </article>
            </div>

            {#if selectedPayout()?.sellerNote}
              <article class="admin-list-card">
                <strong>Seller note</strong>
                <p class="admin-copy">{selectedPayout()?.sellerNote}</p>
              </article>
            {/if}
            {#if selectedPayout()?.adminNote}
              <article class="admin-list-card">
                <strong>Admin note</strong>
                <p class="admin-copy">{selectedPayout()?.adminNote}</p>
              </article>
            {/if}
          {/if}
        </section>
        {/if}

        {#if section === 'overview' || section === 'themes'}
        <section class="admin-panel admin-panel-wide">
          <div class="admin-section-head">
            <div>
              <p class="admin-kicker">Design system</p>
              <h2>Themes</h2>
            </div>
            <span>{dashboard.themes.length} items</span>
          </div>

          <div class="admin-list admin-theme-list">
            {#each dashboard.themes as theme}
              <article
                class:admin-list-card-selected={isSelectedTheme(theme.id)}
                class="admin-list-card"
                style={`background: ${theme.surfaceColor}; color: ${theme.textColor};`}
              >
                <div class="admin-list-title">
                  <strong>{theme.name}</strong>
                  <span class:admin-badge-live={theme.active} class="admin-badge">{theme.active ? 'Active' : 'Hidden'}</span>
                </div>
                <p class="admin-copy">{theme.description ?? theme.slug}</p>
                <div class="admin-tag-row">
                  <span style={`background: ${theme.backgroundColor}; color: ${theme.textColor};`}>Background</span>
                  <span style={`background: ${theme.accentColor}; color: ${theme.textColor};`}>Accent</span>
                  <span>{theme.headingFont ?? 'Default heading'}</span>
                  <span>{theme.priceCents === 0 ? 'Free' : `$${(theme.priceCents / 100).toFixed(2)}`}</span>
                  <span>{theme.status}</span>
                  {#if theme.parentId}
                    <span>Remix</span>
                  {/if}
                </div>
                <div class="admin-actions">
                  <button
                    type="button"
                    class:admin-action-active={isSelectedTheme(theme.id)}
                    aria-pressed={isSelectedTheme(theme.id)}
                    onclick={() => setSelectedTheme(theme.id)}
                  >
                    View details
                  </button>
                  <button type="button" onclick={() => editTheme(theme)}>Edit this theme</button>
                  <button type="button" onclick={() => deleteTheme(theme.id, theme.name)}>Delete this theme</button>
                </div>
              </article>
            {/each}
          </div>

          {#if selectedTheme()}
            <div class="admin-section-head">
              <div>
                <p class="admin-kicker">Details</p>
                <h2>Selected theme</h2>
              </div>
              <div class="admin-actions">
                <span>{selectedTheme()?.status}</span>
                <button type="button" onclick={() => selectAdjacentTheme(-1)} disabled={selectedThemeIndex() <= 0}>Prev</button>
                <button
                  type="button"
                  onclick={() => selectAdjacentTheme(1)}
                  disabled={selectedThemeIndex() === -1 || selectedThemeIndex() >= (dashboard?.themes.length ?? 0) - 1}
                >
                  Next
                </button>
                <button type="button" onclick={() => copySelectedThemeLink()}>Copy link</button>
                <button type="button" onclick={() => selectedTheme() && editTheme(selectedTheme()!)}>Open in editor</button>
              </div>
            </div>

            <div class="admin-metrics" aria-label="Selected theme">
              <article>
                <span>{selectedTheme()?.name ?? 'Unknown theme'}</span>
                <p>Name</p>
              </article>
              <article>
                <span>{selectedTheme()?.priceCents === 0 ? 'Free' : `$${(((selectedTheme()?.priceCents ?? 0) / 100).toFixed(2))}`}</span>
                <p>Price</p>
              </article>
              <article>
                <span>{selectedTheme()?.license ?? 'No license'}</span>
                <p>License</p>
              </article>
              <article>
                <span>{selectedTheme()?.active ? 'Visible' : 'Hidden'}</span>
                <p>Visibility</p>
              </article>
            </div>

            <article class="admin-list-card" style={`background: ${selectedTheme()?.surfaceColor ?? '#ffffff'}; color: ${selectedTheme()?.textColor ?? '#111111'};`}>
              <strong>{selectedTheme()?.description ?? selectedTheme()?.slug ?? 'No theme description'}</strong>
              <div class="admin-tag-row">
                <span style={`background: ${selectedTheme()?.backgroundColor ?? '#ffffff'}; color: ${selectedTheme()?.textColor ?? '#111111'};`}>Background</span>
                <span style={`background: ${selectedTheme()?.accentColor ?? '#ffffff'}; color: ${selectedTheme()?.textColor ?? '#111111'};`}>Accent</span>
                <span>{selectedTheme()?.headingFont ?? 'Default heading'}</span>
                <span>{selectedTheme()?.bodyFont ?? 'Default body'}</span>
                {#if selectedTheme()?.parentId}
                  <span>Remix</span>
                {/if}
              </div>
            </article>
          {/if}

          <div class="admin-theme-form" bind:this={themeFormEl}>
            <div class="admin-theme-form-wide admin-theme-edit-banner">
              <strong>{editingThemeId ? `Editing: ${themeName}` : 'Create a new theme'}</strong>
              <span>{editingThemeId ? 'Update saves changes to the selected registered theme.' : 'Fill the form to register another theme.'}</span>
            </div>
            <label>
              <span>Name</span>
              <input bind:value={themeName} placeholder="Editorial Ink" />
            </label>
            <label>
              <span>Slug</span>
              <input bind:value={themeSlug} placeholder="editorial-ink" />
            </label>
            <label class="admin-theme-form-wide">
              <span>Description</span>
              <textarea bind:value={themeDescription} placeholder="Short theme description"></textarea>
            </label>
            <label>
              <span>Background</span>
              <input bind:value={themeBackgroundColor} placeholder="#f8f0dc" />
            </label>
            <label>
              <span>Surface</span>
              <input bind:value={themeSurfaceColor} placeholder="#fffef7" />
            </label>
            <label>
              <span>Text</span>
              <input bind:value={themeTextColor} placeholder="#111111" />
            </label>
            <label>
              <span>Accent</span>
              <input bind:value={themeAccentColor} placeholder="#ffe11a" />
            </label>
            <label>
              <span>Heading font</span>
              <input bind:value={themeHeadingFont} placeholder="Cormorant Garamond" />
            </label>
            <label>
              <span>Body font</span>
              <input bind:value={themeBodyFont} placeholder="Space Grotesk" />
            </label>
            <label>
              <span>Price cents</span>
              <input bind:value={themePriceCents} placeholder="0" />
            </label>
            <label>
              <span>Status</span>
              <select bind:value={themeStatus}>
                <option value="draft">Draft</option>
                <option value="review">Review</option>
                <option value="published">Published</option>
                <option value="suspended">Suspended</option>
              </select>
            </label>
            <label class="admin-theme-form-wide">
              <span>License</span>
              <input bind:value={themeLicense} placeholder="free-remix" />
            </label>
            <label class="admin-theme-form-wide">
              <span>Header template</span>
              <textarea bind:value={themeHeaderTemplate} placeholder={headerTemplatePlaceholder}></textarea>
            </label>
            <label class="admin-theme-form-wide">
              <span>Body template</span>
              <textarea bind:value={themeBodyTemplate} placeholder={bodyTemplatePlaceholder}></textarea>
            </label>
            <label class="admin-theme-form-wide">
              <span>Footer template</span>
              <textarea bind:value={themeFooterTemplate} placeholder={footerTemplatePlaceholder}></textarea>
            </label>
            <label class="admin-theme-form-wide">
              <span>Custom CSS</span>
              <textarea bind:value={themeCustomCss} placeholder={customCssPlaceholder}></textarea>
            </label>
            <label class="admin-theme-check">
              <input type="checkbox" bind:checked={themeActive} />
              <span>Active theme</span>
            </label>
            <div class="admin-actions admin-theme-form-wide">
              <button type="button" onclick={saveTheme}>{editingThemeId ? 'Update theme' : 'Create theme'}</button>
              <button type="button" onclick={resetThemeForm}>{editingThemeId ? 'Cancel editing' : 'Reset'}</button>
            </div>
          </div>
        </section>
        {/if}
      </div>
    {/if}
  </main>
</div>
