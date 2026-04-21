function escapeHtml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;');
}

function parseInlineMarkdown(line: string): string {
  let html = escapeHtml(line);
  html = html.replace(/!\[([^\]]*)\]\(([^)]+)\)/g, '<img src="$2" alt="$1">');
  html = html.replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2">$1</a>');
  html = html.replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
  html = html.replace(/\*([^*]+)\*/g, '<em>$1</em>');
  html = html.replace(/`([^`]+)`/g, '<code>$1</code>');
  return html;
}

export function renderMarkdown(markdown: string): string {
  const lines = (markdown || '').split('\n');
  const html: string[] = [];
  let inList = false;
  let inCode = false;
  let codeLines: string[] = [];

  const closeList = () => {
    if (inList) {
      html.push('</ul>');
      inList = false;
    }
  };

  const closeCode = () => {
    if (inCode) {
      html.push(`<pre><code>${escapeHtml(codeLines.join('\n'))}</code></pre>`);
      inCode = false;
      codeLines = [];
    }
  };

  for (const line of lines) {
    if (line.trim().startsWith('```')) {
      if (inCode) {
        closeCode();
      } else {
        closeList();
        inCode = true;
      }
      continue;
    }

    if (inCode) {
      codeLines.push(line);
      continue;
    }

    if (!line.trim()) {
      closeList();
      html.push('');
      continue;
    }

    if (/^###\s+/.test(line)) {
      closeList();
      html.push(`<h3>${parseInlineMarkdown(line.replace(/^###\s+/, ''))}</h3>`);
      continue;
    }

    if (/^##\s+/.test(line)) {
      closeList();
      html.push(`<h2>${parseInlineMarkdown(line.replace(/^##\s+/, ''))}</h2>`);
      continue;
    }

    if (/^#\s+/.test(line)) {
      closeList();
      html.push(`<h1>${parseInlineMarkdown(line.replace(/^#\s+/, ''))}</h1>`);
      continue;
    }

    if (/^>\s+/.test(line)) {
      closeList();
      html.push(`<blockquote>${parseInlineMarkdown(line.replace(/^>\s+/, ''))}</blockquote>`);
      continue;
    }

    if (/^-\s+/.test(line)) {
      if (!inList) {
        html.push('<ul>');
        inList = true;
      }
      html.push(`<li>${parseInlineMarkdown(line.replace(/^-\s+/, ''))}</li>`);
      continue;
    }

    closeList();
    html.push(`<p>${parseInlineMarkdown(line)}</p>`);
  }

  closeList();
  closeCode();
  return html.join('');
}
