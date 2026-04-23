import type { ApiTheme, ApiUser, ThemeOverrides } from './types';

const themeKeys = ['backgroundColor', 'surfaceColor', 'textColor', 'accentColor', 'headingFont', 'bodyFont'] as const;
const blockedTags = /<\/?(script|iframe|object|embed|form|input|button|textarea|select|option|link|meta|base)[^>]*>/gi;
const eventHandlers = /\s+on[a-z]+\s*=\s*(".*?"|'.*?'|[^\s>]+)/gi;
const dangerousUrls = /\s+(href|src)\s*=\s*(['"])\s*(javascript:|data:text\/html)[\s\S]*?\2/gi;

export function parseThemeOverrides(rawOverrides?: string | null): ThemeOverrides {
  if (!rawOverrides) return {};
  try {
    const parsed = JSON.parse(rawOverrides) as Record<string, unknown>;
    return themeKeys.reduce<ThemeOverrides>((overrides, key) => {
      if (typeof parsed[key] === 'string' && parsed[key].trim()) {
        overrides[key] = parsed[key].trim();
      }
      return overrides;
    }, {});
  } catch {
    return {};
  }
}

export function buildThemeStyle(theme?: ApiTheme | null, rawOverrides?: string | null): string {
  if (!theme) return '';
  const overrides = parseThemeOverrides(rawOverrides);
  const backgroundColor = overrides.backgroundColor ?? theme.backgroundColor;
  const surfaceColor = overrides.surfaceColor ?? theme.surfaceColor;
  const textColor = overrides.textColor ?? theme.textColor;
  const accentColor = overrides.accentColor ?? theme.accentColor;
  const headingFont = overrides.headingFont ?? theme.headingFont;
  const bodyFont = overrides.bodyFont ?? theme.bodyFont;
  const rules = [
    ['--paper', backgroundColor],
    ['--ink', textColor],
    ['--yellow', accentColor],
    ['--theme-surface', surfaceColor],
    ['--theme-accent', accentColor],
    ['--theme-text', textColor]
  ];
  if (headingFont) rules.push(['--theme-heading-font', quoteFont(headingFont)]);
  if (bodyFont) rules.push(['--theme-body-font', quoteFont(bodyFont)]);
  return rules.map(([key, value]) => `${key}: ${value}`).join('; ');
}

export function buildUserThemeStyle(user?: ApiUser | null): string {
  return buildThemeStyle(user?.theme, user?.themeOverrides);
}

export function stringifyThemeOverrides(overrides: ThemeOverrides): string {
  const clean = themeKeys.reduce<Record<string, string>>((result, key) => {
    const value = overrides[key];
    if (typeof value === 'string' && value.trim()) {
      result[key] = value.trim();
    }
    return result;
  }, {});
  return JSON.stringify(clean);
}

export type ThemeTemplateContext = Record<string, string | number | null | undefined>;

export function renderThemeTemplate(template: string | null | undefined, context: ThemeTemplateContext): string {
  if (!template?.trim()) return '';
  const expanded = template.replace(/\{\{\s*([a-zA-Z0-9_.-]+)\s*\}\}/g, (_match, key: string) => {
    const value = context[key];
    return value == null ? '' : String(value);
  });
  return sanitizeThemeHtml(expanded);
}

export function sanitizeThemeHtml(html: string): string {
  return html
    .replace(blockedTags, '')
    .replace(eventHandlers, '')
    .replace(dangerousUrls, ' $1="#"')
    .replace(/style\s*=\s*(".*?expression\(.*?".*?|' .*?expression\(.*?'.*?)/gi, '');
}

export function sanitizeThemeCss(css?: string | null): string {
  if (!css) return '';
  return css
    .replace(/@import[^;]+;/gi, '')
    .replace(/expression\s*\([^)]*\)/gi, '')
    .replace(/javascript:/gi, '')
    .replace(/<\/?style[^>]*>/gi, '');
}

export function escapeThemeValue(value: string | number | null | undefined): string {
  return value == null
    ? ''
    : String(value)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

function quoteFont(fontName: string): string {
  return `"${fontName.replace(/"/g, '')}"`;
}
