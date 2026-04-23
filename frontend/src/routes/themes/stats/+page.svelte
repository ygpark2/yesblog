<script lang="ts">
  import { base } from '$app/paths';
  import { onMount } from 'svelte';
  import { apiFetch } from '$lib/api';
  import type { ThemeStatsResponse } from '$lib/types';

  let stats = $state<ThemeStatsResponse | null>(null);
  let status = $state('Loading sales stats...');

  onMount(() => {
    void (async () => {
      try {
        stats = await apiFetch<ThemeStatsResponse>(fetch, '/api/me/theme/stats');
        status = `${stats.summary.themeCount} themes · ${stats.summary.paidCount} paid orders`;
      } catch (_error) {
        status = 'Log in first to see theme sales stats.';
      }
    })();
  });
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Theme Stats</p>
    <h1 class="hero-title">Track orders, conversions, and revenue by theme.</h1>
    <p class="lede">{status}</p>
    <div class="action-row">
      <a class="action-link" href={`${base}/themes/mine`}>My themes</a>
      <a class="action-link" href={`${base}/themes/orders`}>Order history</a>
      <a class="action-link" href={`${base}/themes/payouts`}>Payouts</a>
    </div>
  </div>

  {#if stats}
    <section class="panel-card stack">
      <div class="meta-row">
        <div>
          <p class="eyebrow">Summary</p>
          <h2 class="section-title">Marketplace performance</h2>
        </div>
      </div>
      <div class="admin-metrics" aria-label="Theme sales summary">
        <article>
          <span>{stats.summary.themeCount}</span>
          <p>Themes</p>
        </article>
        <article>
          <span>{stats.summary.orderCount}</span>
          <p>Orders</p>
        </article>
        <article>
          <span>{stats.summary.paidCount}</span>
          <p>Paid</p>
        </article>
        <article>
          <span>{stats.summary.failedCount}</span>
          <p>Failed</p>
        </article>
        <article>
          <span>${(stats.summary.revenueCents / 100).toFixed(2)}</span>
          <p>Revenue</p>
        </article>
      </div>
    </section>

    <section class="panel-card stack">
      <div class="meta-row">
        <div>
          <p class="eyebrow">Breakdown</p>
          <h2 class="section-title">Per-theme stats</h2>
        </div>
        <span class="chip">{stats.items.length} themes</span>
      </div>

      <div class="admin-list">
        {#each stats.items as item}
          <article class="admin-list-card">
            <div class="admin-list-title">
              <strong>{item.theme.name}</strong>
              <span class="admin-badge admin-badge-live">${(item.revenueCents / 100).toFixed(2)}</span>
            </div>
            <p class="admin-copy">{item.theme.description ?? item.theme.slug}</p>
            <div class="admin-tag-row">
              <span>{item.orderCount} orders</span>
              <span>{item.paidCount} paid</span>
              <span>{item.pendingCount} pending</span>
              <span>{item.failedCount} failed</span>
              <span>{item.theme.priceCents === 0 ? 'Free theme' : `$${(item.theme.priceCents / 100).toFixed(2)}`}</span>
            </div>
          </article>
        {/each}
      </div>
    </section>
  {/if}
</section>
