import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

const config = {
  preprocess: vitePreprocess(),
  kit: {
    adapter: adapter({
      pages: '../static/app',
      assets: '../static/app',
      fallback: 'index.html',
      precompress: false,
      strict: false
    }),
    paths: {
      base: '/app'
    }
  }
};

export default config;
