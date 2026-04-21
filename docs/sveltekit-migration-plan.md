# YesBlog SvelteKit Migration Plan

## Goal

Keep Yesod as the backend of record for:

- authentication and session management
- database access and authorization
- image upload and admin flows
- draft/publish business rules

Move the reader-facing and writer-facing UI to SvelteKit incrementally.

## Recommended Architecture

- `Yesod`
  - `/api/*` JSON endpoints
  - auth/session/cookies
  - admin pages and existing SSR pages kept as fallback during migration
- `frontend/` `SvelteKit`
  - homepage
  - post list and post detail
  - user blog pages
  - search/tag surfaces
  - writer studio after public pages stabilize

## Why This Split

- Reading and writing UX will keep changing. SvelteKit is a better fit for fast UI iteration.
- The current Yesod app already owns the database, auth, and permissions. Throwing that away would slow the project down.
- Incremental migration lets the existing app remain deployable while the frontend is rebuilt page by page.

## API Boundary

Backend business rules must remain on the Yesod side:

- slug normalization and collision handling
- draft vs published behavior
- ownership checks for editing
- image upload permissions
- comment creation rules

Frontend responsibilities:

- presentation
- navigation and transitions
- local editor state
- optimistic UI where safe

## Page Migration Priority

1. `Home`
Reason: high-visibility, mostly read-only, easy to re-implement with fetched data.

2. `Published post detail`
Reason: this is the highest-value reading surface and benefits most from richer frontend typography and navigation.

3. `User blog page`
Reason: naturally reuses post-card and pagination primitives from the first two steps.

4. `Tag and search pages`
Reason: depend on the same listing components and query-state handling.

5. `Writer studio`
Reason: more interactive and valuable in SvelteKit, but it should move only after the public API and auth boundaries are stable.

6. `Profile/admin`
Reason: lowest leverage for frontend polish and most tightly coupled to current backend flows.

## Phased Rollout

### Phase 1

- add stable JSON endpoints under `/api`
- scaffold `frontend/`
- render home, post detail, and user blog from API data

### Phase 2

- add tag/search listing pages
- add shared design tokens and typography system in SvelteKit

### Phase 3

- move writer studio into SvelteKit
- keep publish/autosave rules backed by Yesod endpoints

### Phase 4

- decide whether to keep Yesod SSR pages as fallback or remove them

## Risks To Watch

- duplicated business logic between Yesod and SvelteKit
- session/cookie handling when frontend runs on another origin in development
- SQLite locking under heavier interactive usage
- slug and autosave correctness if frontend starts making parallel writes

## Recommended Next Step

Build the public SvelteKit reading experience first and keep the current Yesod pages as a safety net.
