import { env } from '$env/dynamic/public';

export const baseUrl = env.PUBLIC_YESBLOG_API_BASE_URL || '';

const CSRF_COOKIE_NAME = 'XSRF-TOKEN';
const CSRF_HEADER_NAME = 'X-XSRF-TOKEN';

let cachedCsrfToken: string | null = null;

function readCookie(name: string): string | null {
  if (typeof document === 'undefined') return null;
  const escapedName = name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const match = document.cookie.match(new RegExp(`(?:^|; )${escapedName}=([^;]*)`));
  return match ? decodeURIComponent(match[1]) : null;
}

function isUnsafeMethod(method?: string): boolean {
  const normalized = (method ?? 'GET').toUpperCase();
  return !['GET', 'HEAD', 'OPTIONS'].includes(normalized);
}

async function readApiError(response: Response): Promise<string> {
  let fallback = `API request failed: ${response.status}`;

  try {
    const data = await response.json();
    if (typeof data?.message === 'string' && data.message.trim()) {
      return data.message;
    }
  } catch {
    try {
      const text = await response.text();
      if (text.trim()) {
        fallback = text.trim();
      }
    } catch {
      // Ignore body parsing failures.
    }
  }

  return fallback;
}

async function ensureCsrfToken(fetcher: typeof fetch): Promise<string> {
  const cookieToken = readCookie(CSRF_COOKIE_NAME);
  if (cookieToken) {
    cachedCsrfToken = cookieToken;
    return cookieToken;
  }

  if (cachedCsrfToken) {
    return cachedCsrfToken;
  }

  const response = await fetcher(`${baseUrl}/api/session`, {
    credentials: 'include'
  });

  if (!response.ok) {
    throw new Error(await readApiError(response));
  }

  const refreshedToken = readCookie(CSRF_COOKIE_NAME);
  if (!refreshedToken) {
    throw new Error('Could not initialize the CSRF token.');
  }

  cachedCsrfToken = refreshedToken;
  return refreshedToken;
}

async function requestJson<T>(
  fetcher: typeof fetch,
  path: string,
  init: RequestInit = {},
  allowRetry = true
): Promise<T> {
  const headers = new Headers(init.headers);

  if (isUnsafeMethod(init.method)) {
    const csrfToken = await ensureCsrfToken(fetcher);
    headers.set(CSRF_HEADER_NAME, csrfToken);
  }

  const response = await fetcher(`${baseUrl}${path}`, {
    ...init,
    credentials: 'include',
    headers
  });

  if (!response.ok) {
    if (allowRetry && response.status === 403 && isUnsafeMethod(init.method)) {
      cachedCsrfToken = null;
      return requestJson<T>(fetcher, path, init, false);
    }

    throw new Error(await readApiError(response));
  }

  return response.json() as Promise<T>;
}

export async function apiFetch<T>(fetcher: typeof fetch, path: string): Promise<T> {
  return requestJson<T>(fetcher, path);
}

export async function apiGet<T>(path: string, fetcher: typeof fetch = fetch): Promise<T> {
  return requestJson<T>(fetcher, path);
}

export async function apiPost<T>(
  path: string,
  fetcher: typeof fetch = fetch,
  payload?: URLSearchParams | FormData
): Promise<T> {
  return apiFormPost(path, payload ?? new URLSearchParams(), fetcher);
}

export async function apiFormPost<T>(
  path: string,
  payload: URLSearchParams | FormData,
  fetcher: typeof fetch = fetch
): Promise<T> {
  const isFormData = payload instanceof FormData;

  return requestJson<T>(fetcher, path, {
    method: 'POST',
    headers: isFormData
      ? undefined
      : {
          'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
        },
    body: isFormData ? payload : payload.toString()
  });
}
