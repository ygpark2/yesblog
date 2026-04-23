<script lang="ts">
  import PostCard from '$lib/components/PostCard.svelte';
  import { buildUserThemeStyle, escapeThemeValue, renderThemeTemplate, sanitizeThemeCss } from '$lib/theme';
  import type { ApiPostSummary, ApiUser } from '$lib/types';

  interface Props {
    data: {
      user: ApiUser;
      items: ApiPostSummary[];
      meta: { publishedCount: number };
    };
  }

  let { data }: Props = $props();

  const templateContext = $derived({
    'site.title': 'YesBlog',
    'author.name': escapeThemeValue(data.user.displayName),
    'author.ident': escapeThemeValue(data.user.ident),
    'author.bio': escapeThemeValue(data.user.bio ?? ''),
    'author.publishedCount': data.meta.publishedCount
  });
  const themeHeaderHtml = $derived(renderThemeTemplate(data.user.theme?.headerTemplate, templateContext));
  const themeFooterHtml = $derived(renderThemeTemplate(data.user.theme?.footerTemplate, templateContext));
  const themeCustomCss = $derived(sanitizeThemeCss(data.user.theme?.customCss));
</script>

<svelte:head>
  {#if themeCustomCss}
    <style>{themeCustomCss}</style>
  {/if}
</svelte:head>

<section class="stack themed-page" style={buildUserThemeStyle(data.user)}>
  {#if themeHeaderHtml}
    <div class="theme-template-slot theme-template-header">{@html themeHeaderHtml}</div>
  {/if}

  <div class="hero-card stack">
    <p class="eyebrow">Writer page</p>
    <h1 class="hero-title">{data.user.displayName}</h1>
    <div class="meta-row">
      <span class="chip">@{data.user.ident}</span>
      <span class="chip">{data.meta.publishedCount} published posts</span>
    </div>
    {#if data.user.bio}
      <p class="lede">{data.user.bio}</p>
    {/if}
  </div>

  <div class="stack">
    {#each data.items as post}
      <PostCard {post} />
    {/each}
  </div>

  {#if themeFooterHtml}
    <div class="theme-template-slot theme-template-footer">{@html themeFooterHtml}</div>
  {/if}
</section>
