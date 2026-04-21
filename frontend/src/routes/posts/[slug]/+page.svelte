<script lang="ts">
  import { base } from '$app/paths';
  import { env } from '$env/dynamic/public';
  import PostCard from '$lib/components/PostCard.svelte';
  import { renderMarkdown } from '$lib/markdown';
  import type { ApiComment, ApiPostDetail, ApiPostSummary } from '$lib/types';

  const backendBaseUrl = env.PUBLIC_YESBLOG_API_BASE_URL || '';

  interface Props {
    data: {
      item: ApiPostDetail;
      related: ApiPostSummary[];
    };
  }

  let { data }: Props = $props();

  const post = $derived(data.item);
  const publishedDate = $derived(new Date(post.createdAt).toLocaleDateString());
  const updatedDate = $derived(new Date(post.updatedAt).toLocaleDateString());
  let hydratedPostSlug = $state('');
  let comments = $state<ApiComment[]>([]);
  let commentName = $state('');
  let commentContent = $state('');
  let commentStatus = $state('');
  let isSubmittingComment = $state(false);
  let editingCommentId = $state<number | null>(null);
  let editingCommentContent = $state('');

  $effect(() => {
    if (hydratedPostSlug === post.slug) return;
    hydratedPostSlug = post.slug;
    comments = [...post.comments];
    commentName = post.viewer?.ident ?? '';
    commentContent = '';
    commentStatus = '';
    editingCommentId = null;
    editingCommentContent = '';
    isSubmittingComment = false;
  });

  async function readErrorMessage(response: Response, fallback: string) {
    try {
      const payload = await response.json();
      if (typeof payload?.message === 'string' && payload.message.trim()) {
        return payload.message;
      }
    } catch {
      try {
        const text = await response.text();
        if (text.trim()) {
          return text.trim();
        }
      } catch {
        return fallback;
      }
    }
    return fallback;
  }

  async function submitComment() {
    if (!commentContent.trim()) {
      commentStatus = 'Write a comment first.';
      return;
    }

    isSubmittingComment = true;
    commentStatus = 'Posting comment…';
    const payload = new URLSearchParams({
      name: commentName,
      content: commentContent
    });

    const response = await fetch(`${backendBaseUrl}/api/post/${post.slug}/comment`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      body: payload.toString(),
      credentials: 'include'
    });

    if (!response.ok) {
      commentStatus = await readErrorMessage(response, 'Comment failed to post.');
      isSubmittingComment = false;
      return;
    }

    const data: { comment: ApiComment } = await response.json();
    comments = [...comments, data.comment];
    commentContent = '';
    commentStatus = 'Comment posted.';
    isSubmittingComment = false;
  }

  function startEditingComment(comment: ApiComment) {
    editingCommentId = comment.id;
    editingCommentContent = comment.content;
    commentStatus = '';
  }

  function cancelEditingComment() {
    editingCommentId = null;
    editingCommentContent = '';
  }

  async function saveEditedComment(commentId: number) {
    if (!editingCommentContent.trim()) {
      commentStatus = 'Comment cannot be empty.';
      return;
    }

    commentStatus = 'Saving comment…';
    const payload = new URLSearchParams({
      content: editingCommentContent
    });
    const response = await fetch(`${backendBaseUrl}/api/comment/${commentId}/update`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      credentials: 'include',
      body: payload.toString()
    });

    if (!response.ok) {
      commentStatus = await readErrorMessage(response, 'Comment update failed.');
      return;
    }

    const data: { comment: ApiComment } = await response.json();
    comments = comments.map((comment) => (comment.id === commentId ? data.comment : comment));
    editingCommentId = null;
    editingCommentContent = '';
    commentStatus = 'Comment updated.';
  }

  async function deleteComment(commentId: number) {
    const confirmed = window.confirm('Delete this comment?');
    if (!confirmed) return;

    commentStatus = 'Deleting comment…';
    const response = await fetch(`${backendBaseUrl}/api/comment/${commentId}/delete`, {
      method: 'POST',
      credentials: 'include'
    });

    if (!response.ok) {
      commentStatus = await readErrorMessage(response, 'Comment delete failed.');
      return;
    }

    comments = comments.filter((comment) => comment.id !== commentId);
    if (editingCommentId === commentId) {
      cancelEditingComment();
    }
    commentStatus = 'Comment deleted.';
  }
</script>

<div class="article-layout">
  <article class="stack article-main">
    <section class="hero-card stack article-hero">
      <div class="meta-row">
        <a class="ghost-link" href={`${base}/`}>Back to archive</a>
        <span class="chip">Article</span>
      </div>
      <div class="stack article-headline-block">
        <p class="eyebrow">Long-form reading</p>
        <h1 class="post-title article-title">{post.title}</h1>
        <p class="lede article-lede">
          A focused reading view with generous spacing, strong typography, and minimal interruption.
        </p>
      </div>

      <div class="article-meta-band">
        <div class="article-meta-group">
          <span class="chip">{post.readingMinutes} min read</span>
          <span class="chip">Published {publishedDate}</span>
          <span class="chip">Updated {updatedDate}</span>
        </div>
        {#if post.author}
          <a class="chip" href={`${base}/users/${post.author.ident}`}>{post.author.displayName}</a>
        {/if}
      </div>

      {#if post.tags.length}
        <div class="tag-row">
          {#each post.tags as tag}
            <a class="chip" href={`${base}/tags/${tag}`}>{tag}</a>
          {/each}
        </div>
      {/if}
    </section>

    <section class="panel-card article-body-shell">
      <div class="prose article-prose">{@html renderMarkdown(post.content)}</div>
    </section>
  </article>

  <aside class="stack article-sidebar">
    <section class="panel-card stack article-sidebar-card article-sidebar-sticky">
      <p class="eyebrow">Reading panel</p>
      <div class="stack" style="gap: 14px;">
        <div class="meta-row">
          <span class="chip">{post.readingMinutes} min</span>
          <span class="chip">{post.tags.length} tags</span>
        </div>
        {#if post.author}
          <div class="stack article-author-block" style="gap: 10px;">
            <strong>{post.author.displayName}</strong>
            <p class="copy article-sidebar-copy">@{post.author.ident}</p>
            {#if post.author.bio}
              <p class="copy article-sidebar-copy">{post.author.bio}</p>
            {/if}
          </div>
        {/if}
        <div class="stack" style="gap: 10px;">
          <a class="action-link" href={`${base}/search?q=${encodeURIComponent(post.title)}`}>Find similar posts</a>
          {#if post.author}
            <a class="action-link" href={`${base}/users/${post.author.ident}`}>More from writer</a>
          {/if}
        </div>
      </div>
    </section>

    <section class="panel-card stack article-sidebar-card">
      <p class="eyebrow">Discussion</p>
      <h2 class="section-title article-subtitle">Comments</h2>
      <div class="stack article-comment-form" style="gap: 12px;">
        <div class="meta-row">
          <span class="chip">{post.viewer ? `Signed in as ${post.viewer.ident}` : 'Posting as guest'}</span>
          {#if post.viewer}
            <span class="chip">You can edit or delete your own named comments.</span>
          {/if}
        </div>
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Name</span>
          <input
            class="search-input article-comment-input"
            bind:value={commentName}
            placeholder="Anonymous"
            readonly={Boolean(post.viewer)}
          />
        </label>
        <label class="stack" style="gap: 8px;">
          <span class="eyebrow">Comment</span>
          <textarea
            class="article-comment-textarea"
            bind:value={commentContent}
            placeholder="Join the discussion"
          ></textarea>
        </label>
        <div class="action-row">
          <button class="action-link" type="button" disabled={isSubmittingComment} onclick={submitComment}>
            {isSubmittingComment ? 'Posting…' : 'Post comment'}
          </button>
          {#if commentStatus}
            <span class="chip">{commentStatus}</span>
          {/if}
        </div>
      </div>

      {#if comments.length === 0}
        <p class="copy article-sidebar-copy">No comments yet.</p>
      {:else}
        {#each comments as comment}
          <article class="article-comment-card stack">
            <div class="meta-row">
              <strong>{comment.name}</strong>
              <span class="chip">{new Date(comment.posted).toLocaleDateString()}</span>
            </div>
            {#if editingCommentId === comment.id}
              <textarea class="article-comment-textarea" bind:value={editingCommentContent}></textarea>
              <div class="action-row">
                <button class="action-link" type="button" onclick={() => saveEditedComment(comment.id)}>Save</button>
                <button class="action-link" type="button" onclick={cancelEditingComment}>Cancel</button>
              </div>
            {:else}
              <p class="copy article-sidebar-copy">{comment.content}</p>
            {/if}
            {#if comment.canManage}
              <div class="action-row">
                {#if editingCommentId !== comment.id}
                  <button class="action-link" type="button" onclick={() => startEditingComment(comment)}>Edit</button>
                {/if}
                <button class="action-link action-link-danger" type="button" onclick={() => deleteComment(comment.id)}>Delete</button>
              </div>
            {/if}
          </article>
        {/each}
      {/if}
    </section>

    {#if data.related.length}
      <section class="stack article-related-section">
        <p class="eyebrow">More from this writer</p>
        {#each data.related as related}
          <PostCard post={related} />
        {/each}
      </section>
    {/if}
  </aside>
</div>
