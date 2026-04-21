# Frontend

This directory is the incremental SvelteKit frontend for YesBlog.

## Build Into Yesod Static

The frontend is built into `../static/app` and is served by Yesod at:

- `/app`
- `/app/*`

After changing frontend code:

```bash
npm install
npm run build
```

Then open the app through Yesod, not a separate frontend origin.

## Dev

```bash
npm install
npm run dev
```

The dev server runs on port `3900`.

Set the API origin when Yesod is running on another host or port:

```bash
cp .env.example .env
```

By default the frontend uses same-origin requests. Set `PUBLIC_YESBLOG_API_BASE_URL` only if you intentionally want a different backend origin.

## Current Pages

- `/` homepage backed by `/api/posts`
- `/posts/[slug]` post detail backed by `/api/post/:slug`
- `/users/[ident]` user blog backed by `/api/user/:ident`
