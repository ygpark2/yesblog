<script lang="ts">
  import '../app.css';
  import { base } from '$app/paths';
</script>

<svelte:head>
  <link rel="preconnect" href="https://fonts.googleapis.com" />
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin="anonymous" />
  <link
    href="https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@500;600;700&family=Space+Grotesk:wght@400;500;700&display=swap"
    rel="stylesheet"
  />
</svelte:head>

<header class="site-header">
  <div class="app-shell site-header-inner">
    <a class="brand" href={`${base}/`}>YesBlog</a>
    <nav class="nav-row">
      <a class="nav-link" href={`${base}/`}>Home</a>
      <a class="nav-link" href={`${base}/archive`}>Archive</a>
      <a class="nav-link" href={`${base}/search`}>Search</a>
      <a class="nav-link" href={`${base}/studio`}>Studio</a>
    </nav>
  </div>
</header>

<main class="page-shell">
  <div class="app-shell">
    <slot />
  </div>
</main>

<footer class="site-footer">
  <div class="app-shell site-footer-inner">
    <strong>YesBlog Frontend</strong>
    <div class="nav-row">
      <a class="nav-link" href={`${base}/`}>Posts</a>
      <a class="nav-link" href={`${base}/archive`}>Archive</a>
      <a class="nav-link" href={`${base}/search`}>Search</a>
      <a class="nav-link" href={`${base}/studio`}>Studio</a>
    </div>
  </div>
</footer>
