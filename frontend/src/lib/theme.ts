import type { ApiTheme, ApiUser, ThemeOverrides } from './types';

const themeKeys = ['backgroundColor', 'surfaceColor', 'textColor', 'accentColor', 'headingFont', 'bodyFont'] as const;
const allowedThemeTags = new Set([
  'a', 'article', 'blockquote', 'code', 'div', 'em', 'footer', 'h1', 'h2', 'h3', 'h4', 'header', 'hr',
  'li', 'main', 'nav', 'ol', 'p', 'pre', 'section', 'small', 'span', 'strong', 'time', 'ul'
]);
const allowedThemeAttrs = new Set(['class', 'role', 'aria-label', 'href', 'target', 'rel', 'datetime']);

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
    .replace(/<!--[\s\S]*?-->/g, '')
    .replace(/<[^>]+>/g, (tag) => sanitizeThemeTag(tag));
}

export function sanitizeThemeCss(css?: string | null): string {
  if (!css) return '';
  return css
    .replace(/@import[^;]+;/gi, '')
    .replace(/url\s*\([^)]*\)/gi, '')
    .replace(/expression\s*\([^)]*\)/gi, '')
    .replace(/javascript:/gi, '')
    .replace(/-moz-binding/gi, '')
    .replace(/behavior\s*:/gi, '')
    .replace(/position\s*:\s*fixed/gi, '')
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

function sanitizeThemeTag(tag: string): string {
  const match = tag.match(/^<\s*(\/?)\s*([a-zA-Z0-9-]+)([^>]*)>$/);
  if (!match) return '';

  const [, closingSlash, rawTagName, rawAttrs] = match;
  const tagName = rawTagName.toLowerCase();
  if (!allowedThemeTags.has(tagName)) return '';

  if (closingSlash) {
    return `</${tagName}>`;
  }

  const attrs: string[] = [];
  for (const attrMatch of rawAttrs.matchAll(/([a-zA-Z_:][-a-zA-Z0-9_:.]*)(?:\s*=\s*("([^"]*)"|'([^']*)'|([^\s"'>/=`]+)))?/g)) {
    const attrName = attrMatch[1].toLowerCase();
    const attrValue = attrMatch[3] ?? attrMatch[4] ?? attrMatch[5] ?? '';

    if (!allowedThemeAttrs.has(attrName)) continue;
    if (attrName.startsWith('on') || attrName === 'style' || attrName === 'src') continue;

    if (attrName === 'href') {
      const normalizedHref = attrValue.trim().toLowerCase();
      const safeHref =
        normalizedHref.startsWith('/') ||
        normalizedHref.startsWith('#') ||
        normalizedHref.startsWith('http://') ||
        normalizedHref.startsWith('https://') ||
        normalizedHref.startsWith('mailto:');
      if (!safeHref) continue;
    }

    const escapedValue = escapeThemeValue(attrValue);
    attrs.push(`${attrName}="${escapedValue}"`);
  }

  if (attrs.some((attr) => attr.startsWith('target=') && !attrs.some((candidate) => candidate.startsWith('rel=')))) {
    attrs.push('rel="noopener noreferrer"');
  }

  return attrs.length > 0 ? `<${tagName} ${attrs.join(' ')}>` : `<${tagName}>`;
}
