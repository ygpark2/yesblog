<script lang="ts">
  import { base } from '$app/paths';
  import { onMount } from 'svelte';
  import { apiFetch, apiFormPost } from '$lib/api';
  import type { ThemePayoutsResponse } from '$lib/types';

  let payouts = $state<ThemePayoutsResponse | null>(null);
  let status = $state('Loading payout estimates...');
  let selectedRequestId = $state<number | null>(null);

  function updateSelectedRequestInUrl(requestId: number | null) {
    const url = new URL(window.location.href);
    if (requestId === null) {
      url.searchParams.delete('request');
    } else {
      url.searchParams.set('request', String(requestId));
    }
    window.history.replaceState(window.history.state, '', url);
  }

  function setSelectedRequest(requestId: number | null) {
    selectedRequestId = requestId;
    updateSelectedRequestInUrl(requestId);
  }

  function syncSelectedRequestFromUrl() {
    if (!payouts) return;
    const requestParam = new URL(window.location.href).searchParams.get('request');
    const requestedId = requestParam ? Number(requestParam) : null;
    const matchedRequest =
      requestedId !== null && Number.isFinite(requestedId)
        ? payouts.requests.find((request) => request.id === requestedId)
        : null;
    if (matchedRequest) {
      selectedRequestId = matchedRequest.id;
    } else if (payouts.requests.length > 0) {
      setSelectedRequest(payouts.requests[0].id);
    } else {
      setSelectedRequest(null);
    }
  }

  function hasOpenRequest() {
    return payouts?.requests.some((request) => request.status === 'requested') ?? false;
  }

  function selectedRequest() {
    return payouts?.requests.find((request) => request.id === selectedRequestId) ?? null;
  }

  function isSelectedRequest(requestId: number) {
    return selectedRequestId === requestId;
  }

  async function copySelectedRequestLink() {
    if (!selectedRequestId) return;
    try {
      await navigator.clipboard.writeText(window.location.href);
      status = 'Payout request link copied.';
    } catch {
      status = 'Could not copy the payout request link.';
    }
  }

  async function loadPayouts() {
    payouts = await apiFetch<ThemePayoutsResponse>(fetch, '/api/me/theme/payouts');
    syncSelectedRequestFromUrl();
    status = hasOpenRequest()
      ? `${payouts.summary.themeCount} themes · payout request pending`
      : `${payouts.summary.themeCount} themes · $${(payouts.summary.availableCents / 100).toFixed(2)} available`;
  }

  async function requestPayout(initialNote = '') {
    if (!payouts || payouts.summary.availableCents <= 0) {
      status = 'There is no available payout balance yet.';
      return;
    }
    if (hasOpenRequest()) {
      status = 'There is already an open payout request.';
      return;
    }
    const note = window.prompt('Payout request note (optional)', initialNote) ?? '';
    try {
      await apiFormPost('/api/me/theme/payout-request', new URLSearchParams({ note }));
      await loadPayouts();
      status = 'Payout request submitted.';
    } catch (error) {
      status = error instanceof Error ? error.message : 'Payout request failed.';
    }
  }

  onMount(() => {
    const handlePopState = () => {
      syncSelectedRequestFromUrl();
    };
    window.addEventListener('popstate', handlePopState);
    void (async () => {
      try {
        await loadPayouts();
      } catch (_error) {
        status = 'Log in first to see payout estimates.';
      }
    })();

    return () => {
      window.removeEventListener('popstate', handlePopState);
    };
  });
</script>

<section class="stack">
  <div class="hero-card stack">
    <p class="eyebrow">Theme Payouts</p>
    <h1 class="hero-title">See what is left after platform fees and remix royalties.</h1>
    <p class="lede">{status}</p>
    <div class="action-row">
      <a class="action-link" href={`${base}/themes/mine`}>My themes</a>
      <a class="action-link" href={`${base}/themes/orders`}>Order history</a>
      <a class="action-link" href={`${base}/themes/stats`}>Sales stats</a>
    </div>
  </div>

  {#if payouts}
    <section class="panel-card stack">
      <div class="meta-row">
        <div>
          <p class="eyebrow">Summary</p>
          <h2 class="section-title">Estimated payout split</h2>
        </div>
        <button class="action-link" type="button" onclick={() => requestPayout()} disabled={hasOpenRequest()}>
          {hasOpenRequest() ? 'Request pending' : 'Request payout'}
        </button>
      </div>
      <div class="admin-metrics" aria-label="Theme payout summary">
        <article>
          <span>${(payouts.summary.grossRevenueCents / 100).toFixed(2)}</span>
          <p>Gross</p>
        </article>
        <article>
          <span>${(payouts.summary.platformFeeCents / 100).toFixed(2)}</span>
          <p>Platform fee</p>
        </article>
        <article>
          <span>${(payouts.summary.downstreamRoyaltyCents / 100).toFixed(2)}</span>
          <p>Royalty paid</p>
        </article>
        <article>
          <span>${(payouts.summary.upstreamRoyaltyEarnedCents / 100).toFixed(2)}</span>
          <p>Royalty earned</p>
        </article>
        <article>
          <span>${(payouts.summary.payoutReadyCents / 100).toFixed(2)}</span>
          <p>Ready</p>
        </article>
        <article>
          <span>${(payouts.summary.requestedCents / 100).toFixed(2)}</span>
          <p>Requested</p>
        </article>
        <article>
          <span>${(payouts.summary.paidOutCents / 100).toFixed(2)}</span>
          <p>Paid out</p>
        </article>
        <article>
          <span>${(payouts.summary.availableCents / 100).toFixed(2)}</span>
          <p>Available</p>
        </article>
      </div>
    </section>

    <section class="panel-card stack">
      <div class="meta-row">
        <div>
          <p class="eyebrow">Requests</p>
          <h2 class="section-title">Payout request history</h2>
        </div>
        <span class="chip">{payouts.requests.length} requests</span>
      </div>
      <div class="admin-list">
        {#each payouts.requests as request}
          <article class:admin-list-card-selected={isSelectedRequest(request.id)} class="admin-list-card">
            <div class="admin-list-title">
              <strong>${(request.amountCents / 100).toFixed(2)}</strong>
              <span class:admin-badge-live={request.status === 'paid'} class="admin-badge">{request.status}</span>
            </div>
            <p class="admin-copy">Requested {new Date(request.requestedAt).toLocaleString()}</p>
            {#if request.processedAt}
              <p class="admin-copy">Processed {new Date(request.processedAt).toLocaleString()}</p>
            {/if}
            {#if request.sellerNote}
              <p class="admin-copy">Your note: {request.sellerNote}</p>
            {/if}
            {#if request.adminNote}
              <p class="admin-copy">{request.status === 'rejected' ? `Admin rejection: ${request.adminNote}` : `Admin note: ${request.adminNote}`}</p>
            {/if}
            {#if request.status === 'rejected'}
              <div class="admin-actions">
                <button type="button" onclick={() => requestPayout(request.sellerNote ?? '')} disabled={hasOpenRequest()}>
                  Request again
                </button>
                <button
                  type="button"
                  class:admin-action-active={isSelectedRequest(request.id)}
                  aria-pressed={isSelectedRequest(request.id)}
                  onclick={() => setSelectedRequest(request.id)}
                >
                  View details
                </button>
              </div>
            {:else}
              <div class="admin-actions">
                <button
                  type="button"
                  class:admin-action-active={isSelectedRequest(request.id)}
                  aria-pressed={isSelectedRequest(request.id)}
                  onclick={() => setSelectedRequest(request.id)}
                >
                  View details
                </button>
              </div>
            {/if}
          </article>
        {/each}
      </div>
    </section>

    {#if selectedRequest()}
      <section class="panel-card stack">
        <div class="meta-row">
          <div>
            <p class="eyebrow">Details</p>
            <h2 class="section-title">Selected payout request</h2>
          </div>
          <div class="action-row">
            <span class="chip">{selectedRequest()?.status}</span>
            <button class="action-link" type="button" onclick={() => copySelectedRequestLink()}>
              Copy link
            </button>
          </div>
        </div>
        <div class="admin-metrics" aria-label="Selected payout request">
          <article>
            <span>${((selectedRequest()?.amountCents ?? 0) / 100).toFixed(2)}</span>
            <p>Amount</p>
          </article>
          <article>
            <span>{selectedRequest()?.requestedAt ? new Date(selectedRequest()!.requestedAt).toLocaleDateString() : '-'}</span>
            <p>Requested</p>
          </article>
          <article>
            <span>{selectedRequest()?.processedAt ? new Date(selectedRequest()?.processedAt ?? '').toLocaleDateString() : '-'}</span>
            <p>Processed</p>
          </article>
        </div>
        {#if selectedRequest()?.sellerNote}
          <div class="admin-list-card">
            <strong>Your note</strong>
            <p class="admin-copy">{selectedRequest()?.sellerNote}</p>
          </div>
        {/if}
        {#if selectedRequest()?.adminNote}
          <div class="admin-list-card">
            <strong>{selectedRequest()?.status === 'rejected' ? 'Admin rejection note' : 'Admin note'}</strong>
            <p class="admin-copy">{selectedRequest()?.adminNote}</p>
          </div>
        {/if}
      </section>
    {/if}

    <section class="panel-card stack">
      <div class="meta-row">
        <div>
          <p class="eyebrow">Breakdown</p>
          <h2 class="section-title">Per-theme payout ledger</h2>
        </div>
        <span class="chip">{payouts.items.length} themes</span>
      </div>

      <div class="admin-list">
        {#each payouts.items as item}
          <article class="admin-list-card">
            <div class="admin-list-title">
              <strong>{item.theme.name}</strong>
              <span class="admin-badge admin-badge-live">${(item.payoutReadyCents / 100).toFixed(2)}</span>
            </div>
            <p class="admin-copy">{item.theme.description ?? item.theme.slug}</p>
            <div class="admin-tag-row">
              <span>{item.paidCount} paid orders</span>
              <span>${(item.grossRevenueCents / 100).toFixed(2)} gross</span>
              <span>${(item.platformFeeCents / 100).toFixed(2)} fee</span>
              <span>${(item.parentRoyaltyCents / 100).toFixed(2)} royalty paid</span>
              <span>${(item.upstreamRoyaltyEarnedCents / 100).toFixed(2)} royalty earned</span>
              <span>{item.childOrderCount} remix sales</span>
            </div>
          </article>
        {/each}
      </div>
    </section>
  {/if}
</section>
