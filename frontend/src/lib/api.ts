import { env } from '$env/dynamic/public';

export const baseUrl = env.PUBLIC_YESBLOG_API_BASE_URL || '';

export async function apiFetch<T>(fetcher: typeof fetch, path: string): Promise<T> {
  const response = await fetcher(`${baseUrl}${path}`, {
    credentials: 'include'
  });

  if (!response.ok) {
    throw new Error(`API request failed: ${response.status}`);
  }

  return response.json() as Promise<T>;
}

export async function apiFormPost<T>(path: string, payload: URLSearchParams | FormData): Promise<T> {
  const isFormData = payload instanceof FormData;
  const response = await fetch(`${baseUrl}${path}`, {
    method: 'POST',
    credentials: 'include',
    headers: isFormData
      ? undefined
      : {
          'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
        },
    body: isFormData ? payload : payload.toString()
  });

  if (!response.ok) {
    let message = `API request failed: ${response.status}`;
    try {
      const data = await response.json();
      if (typeof data?.message === 'string' && data.message.trim()) {
        message = data.message;
      }
    } catch {
      // ignore body parse errors
    }
    throw new Error(message);
  }

  return response.json() as Promise<T>;
}
