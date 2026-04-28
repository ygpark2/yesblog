<script lang="ts">
  import { base } from '$app/paths';
  import { env } from '$env/dynamic/public';
  import { onMount } from 'svelte';
  import { apiFetch, apiFormPost } from '$lib/api';
  import { renderMarkdown } from '$lib/markdown';
  import type { ApiPostSummary, EditorArticle, EditorBootstrap, EditorMine, EditorSaveResponse } from '$lib/types';

  const backendBaseUrl = env.PUBLIC_YESBLOG_API_BASE_URL || '';
  const LOCAL_DRAFT_KEY = 'yesblog-studio-local-draft';

  let bootstrap = $state<EditorBootstrap | null>(null);
  let mine = $state<EditorMine | null>(null);
  let title = $state('');
  let slug = $state('');
  let content = $state('');
  let tags = $state('');
  let articleId = $state('');
  let draft = $state(true);
  let visibility = $state<'public' | 'private' | 'members'>('public');
  let publishAt = $state('');
  let status = $state('Loading editor…');
  let previewOpen = $state(false);
  let slugTouched = $state(false);
  let autosaveTimer: ReturnType<typeof setTimeout> | null = null;
  let loading = $state(true);
  let loginRequired = $state(false);
  let selectedArticleId = $state('');
  let permalink = $state('');
  let uploadFile = $state<File | null>(null);
  let uploadDescription = $state('');
  let uploadInsertAfter = $state(true);
  let uploadDropActive = $state(false);
  let imageQuery = $state('');
  let imageFilter = $state<'all' | 'previewable'>('all');
  let imageDraftDescriptions = $state<Record<number, string>>({});
  let libraryQuery = $state('');
  let libraryStatus = $state<'all' | 'drafts' | 'published'>('all');
  let libraryTag = $state('all');
  let librarySort = $state<'updated-desc' | 'updated-asc' | 'title-asc'>('updated-desc');
  let editorTextarea = $state<HTMLTextAreaElement | null>(null);
  let previewDrawer = $state<HTMLElement | null>(null);
  let lastSavedSignature = $state('');
  let isHydrated = $state(false);
  let showRestoreBanner = $state(false);
  let pendingRestore = $state<{
    articleId: string;
    title: string;
    slug: string;
    content: string;
    tags: string;
    draft: boolean;
    visibility: 'public' | 'private' | 'members';
    publishAt: string;
    slugTouched: boolean;
  } | null>(null);

  const availableLibraryTags = $derived([
    'all',
    ...Array.from(new Set((mine ? [...mine.drafts, ...mine.published] : []).flatMap((post) => post.tags))).sort()
  ]);

  const filteredDrafts = $derived(
    libraryStatus === 'published' ? [] : filterLibraryPosts(mine?.drafts ?? [])
  );

  const filteredPublished = $derived(
    libraryStatus === 'drafts' ? [] : filterLibraryPosts(mine?.published ?? [])
  );

  const filteredImages = $derived(
    (bootstrap?.images ?? []).filter((image) => {
      const query = imageQuery.trim().toLowerCase();
      const textMatch =
        !query ||
        image.filename.toLowerCase().includes(query) ||
        (image.description ?? '').toLowerCase().includes(query);
      const typeMatch = imageFilter === 'all' || image.previewable;
      return textMatch && typeMatch;
    })
  );

  const slugFeedback = $derived(
    !slug.trim()
      ? 'A clean slug will be generated from the title.'
      : slugTouched
        ? 'Manual slug mode. Save once to confirm the final unique URL.'
        : 'Slug follows the title automatically until you edit it.'
  );

  const maxUploadBytes = 8 * 1024 * 1024;
  const allowedUploadTypes = ['image/jpeg', 'image/png', 'image/gif', 'image/webp', 'image/svg+xml'];

  function slugify(text: string): string {
    return text
      .toLowerCase()
      .trim()
      .replace(/[^a-z0-9가-힣\s-]/g, '')
      .replace(/\s+/g, '-')
      .replace(/-+/g, '-')
      .replace(/^-|-$/g, '');
  }

  function wordCount(text: string): number {
    return text.trim() ? text.trim().split(/\s+/).filter(Boolean).length : 0;
  }

  function snapshotSignature() {
    return JSON.stringify({
      articleId,
      title,
      slug,
      content,
      tags,
      draft,
      visibility,
      publishAt
    });
  }

  function currentLocalDraft() {
    return {
      articleId,
      title,
      slug,
      content,
      tags,
      draft,
      visibility,
      publishAt,
      slugTouched
    };
  }

  function hasMeaningfulContent() {
    return Boolean(title.trim() || slug.trim() || content.trim() || tags.trim());
  }

  function hasUnsavedChanges() {
    return isHydrated && snapshotSignature() !== lastSavedSignature;
  }

  function persistLocalDraft() {
    if (typeof localStorage === 'undefined') return;
    try {
      if (!hasMeaningfulContent()) {
        localStorage.removeItem(LOCAL_DRAFT_KEY);
        return;
      }
      localStorage.setItem(LOCAL_DRAFT_KEY, JSON.stringify(currentLocalDraft()));
    } catch {
      // Ignore storage failures so editor actions still work.
    }
  }

  function clearLocalDraft() {
    if (typeof localStorage === 'undefined') return;
    try {
      localStorage.removeItem(LOCAL_DRAFT_KEY);
    } catch {
      // Ignore storage failures so editor actions still work.
    }
  }

  function markSavedState() {
    lastSavedSignature = snapshotSignature();
    persistLocalDraft();
  }

  function restoreLocalDraft() {
    if (!pendingRestore) return;
    articleId = pendingRestore.articleId;
    selectedArticleId = pendingRestore.articleId;
    title = pendingRestore.title;
    slug = pendingRestore.slug;
    content = pendingRestore.content;
    tags = pendingRestore.tags;
    draft = pendingRestore.draft;
    visibility = pendingRestore.visibility ?? 'public';
    publishAt = pendingRestore.publishAt ?? '';
    slugTouched = pendingRestore.slugTouched;
    permalink = '';
    showRestoreBanner = false;
    status = 'Local draft restored';
  }

  function dismissRestoreDraft() {
    showRestoreBanner = false;
    pendingRestore = null;
    clearLocalDraft();
  }

  function closePreview() {
    previewOpen = false;
  }

  function replaceStudioHistory(nextPath: string) {
    if (typeof window === 'undefined') return;
    try {
      window.history.replaceState({}, '', nextPath);
    } catch {
      // Ignore history API failures and keep the editor usable.
    }
  }

  function redirectToLogin() {
    loginRequired = true;
    status = 'Log in first. Redirecting to the login page…';
    if (typeof window !== 'undefined') {
      window.location.assign(`${base}/login`);
    }
  }

  function syncImageDraftDescriptions() {
    if (!bootstrap) return;
    imageDraftDescriptions = Object.fromEntries(
      bootstrap.images.map((image) => [image.id, image.description ?? ''])
    );
  }

  async function fetchBootstrap() {
    bootstrap = await apiFetch<EditorBootstrap>(fetch, '/api/editor/bootstrap');
    syncImageDraftDescriptions();
  }

  async function fetchMine() {
    mine = await apiFetch<EditorMine>(fetch, '/api/editor/mine');
  }

  async function loadArticle(nextArticleId: string) {
    try {
      const data = await apiFetch<{ item: EditorArticle }>(fetch, `/api/editor/article/${nextArticleId}`);
      const item = data.item;
      selectedArticleId = String(item.id);
      articleId = String(item.id);
      title = item.title;
      slug = item.slug;
      content = item.content;
      tags = item.tags.join(' ');
      draft = item.draft;
      visibility = (item.visibility as 'public' | 'private' | 'members') ?? 'public';
      publishAt = item.publishAt ? item.publishAt.slice(0, 16) : '';
      permalink = item.draft ? '' : `${base}/posts/${item.slug}`;
      slugTouched = true;
      status = item.draft ? 'Draft loaded' : 'Published article loaded';
      loginRequired = false;
      replaceStudioHistory(`${base}/studio?articleId=${item.id}`);
      markSavedState();
    } catch {
      status = 'Failed to load article';
    }
  }

  function startFreshDraft() {
    selectedArticleId = '';
    articleId = '';
    title = '';
    slug = '';
    content = '';
    tags = '';
    draft = true;
    visibility = 'public';
    publishAt = '';
    permalink = '';
    slugTouched = false;
    loginRequired = false;
    status = 'New draft';
    replaceStudioHistory(`${base}/studio`);
    markSavedState();
  }

  async function saveArticle(nextDraft: boolean) {
    if (loginRequired) {
      redirectToLogin();
      return;
    }

    status = nextDraft ? 'Saving draft…' : 'Publishing…';
    const payload = new URLSearchParams({
      title,
      slug,
      content,
      tags,
      draft: String(nextDraft),
      visibility,
      publishAt
    });

    if (articleId) {
      payload.set('articleId', articleId);
    }

    let data: EditorSaveResponse;
    try {
      data = await apiFormPost<EditorSaveResponse>('/api/editor/save', payload);
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Save failed.';
      if (/login|auth|401|403/i.test(message)) {
        redirectToLogin();
        return;
      }
      status = message;
      return;
    }

    articleId = data.articleId || articleId;
    selectedArticleId = data.articleId || selectedArticleId;
    slug = data.slug || slug;
    draft = data.draft;
    permalink = data.draft || !data.permalink ? '' : `${backendBaseUrl}${data.permalink}`;
    status = data.draft ? 'Draft saved' : 'Published';
    loginRequired = false;
    await Promise.all([fetchBootstrap(), fetchMine()]);
    if (articleId) {
      replaceStudioHistory(`${base}/studio?articleId=${articleId}`);
    }
    markSavedState();
  }

  async function autosave() {
    if (loginRequired) return;
    if (!title.trim() && !content.trim() && !tags.trim()) return;
    const payload = new URLSearchParams({
      title,
      slug,
      content,
      tags,
      visibility,
      publishAt
    });

    if (articleId) {
      payload.set('articleId', articleId);
    }

    let data: { articleId?: string; slug?: string };
    try {
      data = await apiFormPost<{ articleId?: string; slug?: string }>('/api/editor/autosave', payload);
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Autosave failed.';
      if (/login|auth|401|403/i.test(message)) {
        loginRequired = true;
        status = 'Log in to re-enable autosave.';
        return;
      }
      status = message;
      return;
    }

    articleId = data.articleId || articleId;
    selectedArticleId = articleId;
    if (!slugTouched && data.slug) {
      slug = data.slug;
    }
    draft = true;
    loginRequired = false;
    status = `Draft autosaved at ${new Date().toLocaleTimeString()}`;
    markSavedState();
  }

  function scheduleAutosave() {
    if (autosaveTimer) clearTimeout(autosaveTimer);
    if (!draft || loginRequired) {
      autosaveTimer = null;
      return;
    }
    autosaveTimer = setTimeout(() => {
      autosave();
    }, 1500);
  }

  function applyTag(tag: string) {
    const nextTags = tags.trim() ? tags.trim().split(/\s+/) : [];
    if (!nextTags.includes(tag)) {
      tags = [...nextTags, tag].join(' ');
      scheduleAutosave();
    }
  }

  function insertMarkdown(snippet: string) {
    content = `${content}${content.endsWith('\n') || !content ? '' : '\n'}${snippet}\n`;
    scheduleAutosave();
  }

  function handleEditorScroll() {
    if (!editorTextarea || !previewDrawer) return;
    const maxEditorScroll = editorTextarea.scrollHeight - editorTextarea.clientHeight;
    const maxPreviewScroll = previewDrawer.scrollHeight - previewDrawer.clientHeight;
    if (maxEditorScroll <= 0 || maxPreviewScroll <= 0) return;
    previewDrawer.scrollTop = (editorTextarea.scrollTop / maxEditorScroll) * maxPreviewScroll;
  }

  function syncPreviewToCaret() {
    if (!editorTextarea || !previewDrawer) return;
    const selectionEnd = editorTextarea.selectionEnd ?? 0;
    const beforeCaret = content.slice(0, selectionEnd);
    const totalLines = Math.max(1, content.split('\n').length);
    const currentLine = Math.max(1, beforeCaret.split('\n').length);
    const ratio = currentLine / totalLines;
    const maxPreviewScroll = previewDrawer.scrollHeight - previewDrawer.clientHeight;
    if (maxPreviewScroll <= 0) return;
    previewDrawer.scrollTop = Math.max(0, maxPreviewScroll * ratio - previewDrawer.clientHeight * 0.22);
  }

  function handleUploadSelection(event: Event) {
    const input = event.currentTarget as HTMLInputElement | null;
    uploadFile = input?.files?.[0] ?? null;
  }

  function pickFirstImageFile(files: FileList | File[] | null | undefined): File | null {
    if (!files) return null;
    for (const file of Array.from(files)) {
      if (file.type.startsWith('image/')) {
        return file;
      }
    }
    return null;
  }

  function validateUploadFile(file: File) {
    if (!allowedUploadTypes.includes(file.type)) {
      status = 'Use jpg, png, gif, webp, or svg images only.';
      return false;
    }

    if (file.size > maxUploadBytes) {
      status = 'Images must be 8 MB or smaller.';
      return false;
    }

    return true;
  }

  async function uploadImageFile(file: File, sourceLabel = 'image') {
    if (!validateUploadFile(file)) {
      return;
    }
    uploadFile = file;
    status = `Uploading ${sourceLabel}…`;
    const payload = new FormData();
    payload.set('image', file);
    if (uploadDescription.trim()) {
      payload.set('description', uploadDescription.trim());
    }

    try {
      const data = await apiFormPost<{ image: EditorBootstrap['images'][number] }>('/api/editor/upload', payload);
      const image = data.image;
      bootstrap = bootstrap
        ? {
            ...bootstrap,
            images: [image, ...bootstrap.images].slice(0, 12)
          }
        : bootstrap;
      syncImageDraftDescriptions();

      if (uploadInsertAfter && image?.markdown) {
        insertMarkdown(image.markdown);
        status = `${sourceLabel} uploaded and inserted`;
      } else {
        status = `${sourceLabel} uploaded`;
      }

      uploadFile = null;
      uploadDescription = '';
    } catch (error) {
      status = error instanceof Error ? error.message : `${sourceLabel} upload failed`;
    }
  }

  async function saveImageDescription(imageId: number) {
    status = 'Saving image note…';
    const payload = new URLSearchParams({
      description: imageDraftDescriptions[imageId] ?? ''
    });
    try {
      const data = await apiFormPost<{ image: EditorBootstrap['images'][number] }>(
        `/api/editor/image/${imageId}/update`,
        payload
      );
      if (bootstrap) {
        bootstrap = {
          ...bootstrap,
          images: bootstrap.images.map((image) => (image.id === imageId ? data.image : image))
        };
        syncImageDraftDescriptions();
      }
      status = 'Image note saved';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Image note save failed';
    }
  }

  async function deleteImage(imageId: number) {
    const confirmed = window.confirm('Delete this image from the shared library?');
    if (!confirmed) return;
    status = 'Deleting image…';
    try {
      await apiFormPost(`/api/editor/image/${imageId}/delete`, new URLSearchParams());
      if (bootstrap) {
        bootstrap = {
          ...bootstrap,
          images: bootstrap.images.filter((image) => image.id !== imageId)
        };
        syncImageDraftDescriptions();
      }
      status = 'Image deleted';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Image delete failed';
    }
  }

  function handleUploadDragOver(event: DragEvent) {
    event.preventDefault();
    uploadDropActive = true;
  }

  function handleUploadDragEnter(event: DragEvent) {
    event.preventDefault();
    uploadDropActive = true;
  }

  function handleUploadDragLeave() {
    uploadDropActive = false;
  }

  async function handleUploadDrop(event: DragEvent) {
    event.preventDefault();
    uploadDropActive = false;
    const file = pickFirstImageFile(event.dataTransfer?.files);
    if (!file) {
      status = 'Drop an image file to upload';
      return;
    }
    await uploadImageFile(file, 'Dropped image');
  }

  async function handleEditorPaste(event: ClipboardEvent) {
    const file = pickFirstImageFile(event.clipboardData?.files);
    if (!file) return;
    event.preventDefault();
    await uploadImageFile(file, 'Pasted image');
  }

  function matchesLibraryQuery(post: ApiPostSummary): boolean {
    const query = libraryQuery.trim().toLowerCase();
    if (!query) return true;
    const haystack = [post.title, post.slug, post.excerpt, post.tags.join(' ')]
      .join(' ')
      .toLowerCase();
    return haystack.includes(query);
  }

  function matchesLibraryTag(post: ApiPostSummary): boolean {
    return libraryTag === 'all' || post.tags.includes(libraryTag);
  }

  function sortLibraryPosts(posts: ApiPostSummary[]): ApiPostSummary[] {
    return [...posts].sort((left, right) => {
      if (librarySort === 'updated-asc') {
        return new Date(left.updatedAt).getTime() - new Date(right.updatedAt).getTime();
      }
      if (librarySort === 'title-asc') {
        return left.title.localeCompare(right.title);
      }
      return new Date(right.updatedAt).getTime() - new Date(left.updatedAt).getTime();
    });
  }

  function filterLibraryPosts(posts: ApiPostSummary[]): ApiPostSummary[] {
    return sortLibraryPosts(posts.filter((post) => matchesLibraryQuery(post) && matchesLibraryTag(post)));
  }

  async function uploadImage() {
    if (!uploadFile) {
      status = 'Choose an image first';
      return;
    }
    await uploadImageFile(uploadFile);
  }

  onMount(() => {
    const handleBeforeUnload = (event: BeforeUnloadEvent) => {
      if (!hasUnsavedChanges()) return;
      event.preventDefault();
      event.returnValue = '';
    };

    const handleKeydown = (event: KeyboardEvent) => {
      if (!(event.metaKey || event.ctrlKey) || event.key.toLowerCase() !== 's') return;
      event.preventDefault();
      void saveArticle(true);
    };

    window.addEventListener('beforeunload', handleBeforeUnload);
    window.addEventListener('keydown', handleKeydown);

    void (async () => {
      try {
        await fetchBootstrap();
        await fetchMine();
        loginRequired = false;
        const queryParams = new URLSearchParams(window.location.search);
        const requestedArticleId = queryParams.get('articleId');
        const storedDraftRaw = localStorage.getItem(LOCAL_DRAFT_KEY);
        if (storedDraftRaw) {
          try {
            pendingRestore = JSON.parse(storedDraftRaw);
          } catch {
            clearLocalDraft();
          }
        }
        if (requestedArticleId) {
          await loadArticle(requestedArticleId);
        } else if (pendingRestore && !hasMeaningfulContent()) {
          showRestoreBanner = true;
        } else {
          status = 'Start writing';
          markSavedState();
        }
      } catch (_error) {
        loginRequired = true;
        redirectToLogin();
      } finally {
        loading = false;
        isHydrated = true;
      }
    })();

    return () => {
      window.removeEventListener('beforeunload', handleBeforeUnload);
      window.removeEventListener('keydown', handleKeydown);
    };
  });

  $effect(() => {
    if (!slugTouched && title.trim()) {
      const nextSlug = slugify(title);
      if (slug !== nextSlug) {
        slug = nextSlug;
      }
    }
  });

  $effect(() => {
    if (!loading && draft) {
      title;
      slug;
      content;
      tags;
      draft;
      visibility;
      publishAt;
      scheduleAutosave();
    }
  });

  $effect(() => {
    if (!draft || loginRequired) {
      if (autosaveTimer) {
        clearTimeout(autosaveTimer);
        autosaveTimer = null;
      }
    }
  });

  $effect(() => {
    if (isHydrated) {
      title;
      slug;
      content;
      tags;
      draft;
      visibility;
      publishAt;
      slugTouched;
      articleId;
      persistLocalDraft();
    }
  });
</script>

<section class="stack studio-shell">
  <section class="panel-card studio-publish-bar">
    <div class="studio-publish-bar-copy">
      <p class="eyebrow">Writing actions</p>
      <strong>{draft ? 'Draft in progress' : 'Live article loaded'}</strong>
      <span class="copy">{status}</span>
    </div>
    <div class="studio-publish-bar-actions">
      <button class="action-link" type="button" onclick={startFreshDraft}>New draft</button>
      <button class="action-link studio-primary-action" type="button" onclick={() => saveArticle(true)}>
        {draft ? 'Save draft' : 'Move to draft'}
      </button>
      <button class="action-link studio-primary-action" type="button" onclick={() => saveArticle(false)}>
        {draft ? 'Publish now' : 'Update live post'}
      </button>
      <button class="action-link" type="button" onclick={() => (previewOpen = !previewOpen)}>
        {previewOpen ? 'Hide preview' : 'Open preview'}
      </button>
      {#if permalink}
        <a class="action-link" href={permalink}>View live post</a>
      {/if}
    </div>
  </section>

  {#if showRestoreBanner}
    <section class="panel-card stack studio-restore-card">
      <div class="meta-row">
        <p class="eyebrow">Restore draft</p>
        <span class="chip">Local recovery</span>
      </div>
      <p class="copy">A local draft from your previous session is available. Restore it or discard it.</p>
      <div class="action-row">
        <button class="action-link" type="button" onclick={restoreLocalDraft}>Restore draft</button>
        <button class="action-link" type="button" onclick={dismissRestoreDraft}>Discard local draft</button>
      </div>
    </section>
  {/if}

  <div class="studio-layout">
    <section class="panel-card stack studio-editor-surface">
      <div class="meta-row">
        <span class="chip">Status</span>
        <strong>{status}</strong>
        <span class:chip-live={!draft && !!permalink} class="chip">{draft ? 'Draft mode' : 'Published mode'}</span>
        <span class="chip">{wordCount(content)} words</span>
        <span class="chip">⌘/Ctrl+S saves draft</span>
      </div>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Title</span>
        <input class="search-input studio-input" bind:value={title} placeholder="Write a strong title" />
      </label>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Slug</span>
        <input
          class="search-input studio-input"
          bind:value={slug}
          placeholder="auto-generated-from-title"
          oninput={() => {
            slugTouched = true;
          }}
        />
        <p class="copy studio-field-note">{slugFeedback}</p>
        <p class="copy studio-field-note">Preview URL: /app/posts/{slug || 'untitled-draft'}</p>
      </label>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Tags</span>
        <input class="search-input studio-input" bind:value={tags} placeholder="essay devlog notes" />
      </label>

      <div class="grid studio-library-controls">
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Visibility</span>
          <select class="studio-select" bind:value={visibility}>
            <option value="public">Public</option>
            <option value="private">Private</option>
            <option value="members">Members</option>
          </select>
        </label>

        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Publish at</span>
          <input class="search-input studio-input" type="datetime-local" bind:value={publishAt} />
        </label>
      </div>
      <p class="copy studio-field-note">Private, members-only visibility, and scheduled publishing require Writer Pro.</p>

      <label class="stack" style="gap: 8px;">
        <span class="eyebrow">Content</span>
        <textarea
          bind:this={editorTextarea}
          class="studio-textarea"
          bind:value={content}
          placeholder="Start writing..."
          onclick={syncPreviewToCaret}
          onkeyup={syncPreviewToCaret}
          onpaste={handleEditorPaste}
          onscroll={handleEditorScroll}
        ></textarea>
      </label>

      <div class="tag-row">
        {#each bootstrap?.tagSuggestions ?? [] as tag}
          <button class="chip studio-chip-button" type="button" onclick={() => applyTag(tag)}>{tag}</button>
        {/each}
      </div>

    </section>

    <aside class="panel-card stack studio-side-library">
      <div class="stack" style="gap: 14px;">
        <div class="meta-row">
          <p class="eyebrow">Library</p>
          <span class="chip">{filteredDrafts.length + filteredPublished.length} visible</span>
        </div>

        <input
          class="search-input studio-input studio-library-search"
          bind:value={libraryQuery}
          placeholder="Search title, slug, excerpt, tag"
        />

        <div class="tag-row">
          <button
            class:studio-filter-active={libraryStatus === 'all'}
            class="chip studio-chip-button"
            type="button"
            onclick={() => (libraryStatus = 'all')}
          >
            All
          </button>
          <button
            class:studio-filter-active={libraryStatus === 'drafts'}
            class="chip studio-chip-button"
            type="button"
            onclick={() => (libraryStatus = 'drafts')}
          >
            Drafts
          </button>
          <button
            class:studio-filter-active={libraryStatus === 'published'}
            class="chip studio-chip-button"
            type="button"
            onclick={() => (libraryStatus = 'published')}
          >
            Published
          </button>
        </div>

        <div class="grid studio-library-controls">
          <label class="stack" style="gap: 8px;">
            <span class="eyebrow">Tag</span>
            <select class="studio-select" bind:value={libraryTag}>
              {#each availableLibraryTags as tagOption}
                <option value={tagOption}>{tagOption === 'all' ? 'All tags' : tagOption}</option>
              {/each}
            </select>
          </label>

          <label class="stack" style="gap: 8px;">
            <span class="eyebrow">Sort</span>
            <select class="studio-select" bind:value={librarySort}>
              <option value="updated-desc">Latest updated</option>
              <option value="updated-asc">Oldest updated</option>
              <option value="title-asc">Title A-Z</option>
            </select>
          </label>
        </div>
      </div>

      <div class="stack" style="gap: 16px;">
        <p class="eyebrow">My drafts</p>
        {#if filteredDrafts.length}
          {#each filteredDrafts as post}
            <button
              class:selected-entry={selectedArticleId === String(post.id)}
              class="studio-entry-button"
              type="button"
              onclick={() => loadArticle(String(post.id))}
            >
              <strong>{post.title}</strong>
              <span>{new Date(post.updatedAt).toLocaleDateString()} · {post.tags.join(', ') || 'No tags'}</span>
            </button>
          {/each}
        {:else}
          <p class="copy">No drafts match the current filter.</p>
        {/if}
      </div>

      <div class="stack" style="gap: 16px;">
        <p class="eyebrow">Published</p>
        {#if filteredPublished.length}
          {#each filteredPublished as post}
            <button
              class:selected-entry={selectedArticleId === String(post.id)}
              class="studio-entry-button"
              type="button"
              onclick={() => loadArticle(String(post.id))}
            >
              <strong>{post.title}</strong>
              <span>{new Date(post.updatedAt).toLocaleDateString()} · {post.tags.join(', ') || 'No tags'}</span>
            </button>
          {/each}
        {:else}
          <p class="copy">No published posts match the current filter.</p>
        {/if}
      </div>
    </aside>

  </div>

  {#if bootstrap}
    <div
      aria-label="Image upload dropzone"
      class:studio-drop-active={uploadDropActive}
      class="panel-card stack studio-upload-panel"
      ondragenter={handleUploadDragEnter}
      ondragover={handleUploadDragOver}
      ondragleave={handleUploadDragLeave}
      ondrop={handleUploadDrop}
      role="group"
    >
      <div class="meta-row">
        <p class="eyebrow">Upload image</p>
        <span class="chip">Studio upload</span>
        <span class="chip">Drag, drop, or paste</span>
      </div>
      <p class="copy studio-field-note">
        Drop an image anywhere in this panel or paste directly into the editor to upload and insert faster.
      </p>
      <div class="grid studio-upload-grid">
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">File</span>
          <input
            class="studio-file-input"
            type="file"
            accept="image/*"
            onchange={handleUploadSelection}
          />
        </label>

        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Caption</span>
          <input class="search-input studio-input" bind:value={uploadDescription} placeholder="Optional image note" />
        </label>
      </div>

      <div class="action-row">
        <label class="studio-checkbox">
          <input type="checkbox" bind:checked={uploadInsertAfter} />
          <span>Insert markdown after upload</span>
        </label>
        <button class="action-link" type="button" onclick={uploadImage}>Upload image</button>
      </div>
    </div>
  {/if}

  {#if bootstrap?.images?.length}
    <section class="panel-card stack">
      <div class="meta-row">
        <p class="eyebrow">Image library</p>
        <span class="chip">{filteredImages.length} visible</span>
      </div>
      <div class="grid studio-library-controls">
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Search images</span>
          <input class="search-input studio-input studio-library-search" bind:value={imageQuery} placeholder="Search filename or note" />
        </label>
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Filter</span>
          <select class="studio-select" bind:value={imageFilter}>
            <option value="all">All images</option>
            <option value="previewable">Previewable only</option>
          </select>
        </label>
      </div>
      <div class="studio-image-grid">
        {#each filteredImages as image}
          <article class="post-card stack">
            <strong>{image.filename}</strong>
            {#if image.previewable}
              <img src={`${backendBaseUrl}${image.publicUrl}`} alt={image.filename} />
            {/if}
            <textarea class="studio-image-note" bind:value={imageDraftDescriptions[image.id]} placeholder="Image note"></textarea>
            <div class="action-row">
              <button class="action-link" type="button" onclick={() => insertMarkdown(image.markdown)}>Insert markdown</button>
              <button class="action-link" type="button" onclick={() => saveImageDescription(image.id)}>Save note</button>
              <button class="action-link action-link-danger" type="button" onclick={() => deleteImage(image.id)}>Delete</button>
            </div>
          </article>
        {/each}
      </div>
    </section>
  {/if}

  <div class:preview-open={previewOpen} class="studio-preview-shell" aria-hidden={!previewOpen}>
    <button class="studio-preview-backdrop" type="button" aria-label="Close preview" onclick={closePreview}></button>
    <aside bind:this={previewDrawer} class="studio-preview-drawer panel-card stack">
      <div class="meta-row">
        <span class="chip">Preview</span>
        <button class="action-link" type="button" onclick={closePreview}>Close</button>
      </div>
      <h2 class="section-title">{title || 'Untitled draft'}</h2>
      <div class="meta-row">
        <span class="chip">{wordCount(content)} words</span>
        <span class="chip">/app/posts/{slug || 'untitled-draft'}</span>
        <a class="chip" href={`${base}/search`}>Search posts</a>
      </div>
      <div class="tag-row">
        {#if tags.trim()}
          {#each tags.trim().split(/\s+/) as tag}
            <span class="chip">{tag}</span>
          {/each}
        {:else}
          <span class="chip">No tags</span>
        {/if}
      </div>
      <div class="prose">{@html renderMarkdown(content || 'Start writing to see the rendered preview.')}</div>
    </aside>
  </div>
</section>
