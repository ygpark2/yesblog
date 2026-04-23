<script lang="ts">
  import { base } from '$app/paths';
  import { onMount } from 'svelte';
  import { apiFetch } from '$lib/api';
  import type { ThemeOrderItem } from '$lib/types';

  let items = $state<ThemeOrderItem[]>([]);
  let status = $state('Loading order history...');

  async function readErrorMessage(response: Response, fallback: string) {
    try {
      const payload = await response.json();
      if (typeof payload?.message === 'string' && payload.message.trim()) return payload.message;
    } catch {
      return fallback;
    }
    return fallback;
  }

  async function fetchOrders() {
    const data = await apiFetch<{ items: ThemeOrderItem[] }>(fetch, '/api/me/theme/orders');
    items = data.items;
    status = `${items.length} theme orders`;
  }

  onMount(() => {
    void (async () => {
      try {
        await fetchOrders();
      } catch (_error) {
        status = 'Log in first to see your order history.';
      }
    })();
  });

  async function confirmOrder(themeId: number, nextStatus: 'paid' | 'cancelled') {
    status = nextStatus === 'paid' ? 'Confirming payment...' : 'Cancelling order...';
    const payload = new URLSearchParams({ status: nextStatus });
    const response = await fetch(`/api/theme/${themeId}/purchase/confirm`, {
      method: 'POST',
      credentials: 'include',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
      },
      body: payload.toString()
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Order update failed.');
      return;
    }
    await fetchOrders();
  }

  async function retryOrder(themeId: number) {
    status = 'Creating retry order...';
    const response = await fetch(`/api/theme/${themeId}/purchase`, {
      method: 'POST',
      credentials: 'include'
    });
    if (!response.ok) {
      status = await readErrorMessage(response, 'Retry order failed.');
      return;
    }
    const shouldPay = window.confirm('Simulate payment success for this retry order?');
    await confirmOrder(themeId, shouldPay ? 'paid' : 'cancelled');
  }
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Theme Orders</p>
    <h1 class="hero-title">Recorded purchases for your theme library.</h1>
    <p class="lede">{status}</p>
    <div class="action-row">
      <a class="action-link" href={`${base}/themes`}>Open marketplace</a>
      <a class="action-link" href={`${base}/themes/mine`}>My themes</a>
      <a class="action-link" href={`${base}/themes/stats`}>Sales stats</a>
      <a class="action-link" href={`${base}/themes/payouts`}>Payouts</a>
    </div>
  </div>

  <section class="panel-card stack">
    <div class="admin-list">
      {#each items as item}
        <article class="admin-list-card">
          <div class="admin-list-title">
            <strong>{item.theme?.name ?? 'Unknown theme'}</strong>
            <span class:admin-badge-live={item.status === 'paid'} class="admin-badge">{item.status}</span>
          </div>
          <p class="admin-copy">
            {item.amountCents === 0 ? 'Free' : `$${(item.amountCents / 100).toFixed(2)}`} · {new Date(item.createdAt).toLocaleString()}
          </p>
          {#if item.status === 'pending'}
            <p class="admin-copy">Waiting for payment confirmation.</p>
            {#if item.theme}
              <div class="admin-actions">
                <button type="button" onclick={() => confirmOrder(item.theme!.id, 'paid')}>Complete payment</button>
                <button type="button" onclick={() => confirmOrder(item.theme!.id, 'cancelled')}>Cancel order</button>
              </div>
            {/if}
          {:else if item.status === 'failed'}
            <p class="admin-copy">Payment was not completed for this order.</p>
            {#if item.theme}
              <div class="admin-actions">
                <button type="button" onclick={() => retryOrder(item.theme!.id)}>Retry payment</button>
              </div>
            {/if}
          {:else if item.status === 'cancelled'}
            <p class="admin-copy">This order was cancelled before payment completed.</p>
            {#if item.theme}
              <div class="admin-actions">
                <button type="button" onclick={() => retryOrder(item.theme!.id)}>Retry payment</button>
              </div>
            {/if}
          {:else if item.paidAt}
            <p class="admin-copy">Paid at {new Date(item.paidAt).toLocaleString()}</p>
          {/if}
          <p class="admin-copy">{item.theme?.description ?? item.theme?.slug ?? 'Theme details unavailable'}</p>
        </article>
      {/each}
    </div>
  </section>
</section>
