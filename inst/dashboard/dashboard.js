/* Shared offline viewer. Figure producers supply images and metadata, not JavaScript. */
(() => {
  'use strict';
  const report = window.STOCKPLOTR_REPORT;
  const $ = id => document.getElementById(id);
  if (!report || report.schema_version !== 1 || !Array.isArray(report.figures)) {
    $('report-title').textContent = 'Report data could not be loaded';
    $('report-subtitle').textContent = 'Extract the entire ZIP and keep report-data.js beside index.html.';
    return;
  }
  const figures = report.figures;
  const ready = figures.filter(f => f.status === 'ready');
  // Caption templates also serve LaTeX reports. Remove escaped punctuation for
  // screen display, while retaining the original strings in the manifest.
  const prose = text => String(text || '').replace(/\\+([%&_#])/g, '$1');
  const categories = [...new Set(figures.map(f => f.category))];
  const state = {category: 'All figures', query: '', sort: 'catalog', view: 'gallery', selected: ready[0]?.id, compare: new Set()};
  const node = (tag, className, text) => {
    const n = document.createElement(tag);
    if (className) n.className = className;
    if (text !== undefined) n.textContent = text;
    return n;
  };
  const button = (text, action, className) => {
    const b = node('button', className, text);
    b.type = 'button'; b.addEventListener('click', action); return b;
  };
  const download = (label, path) => {
    const a = node('a', '', label); a.href = path; a.download = ''; return a;
  };
  function filtered() {
    const q = state.query.toLocaleLowerCase();
    const items = figures.filter(f => (state.category === 'All figures' || f.category === state.category) &&
      [f.title, f.category, f.module, f.caption || '', f.function_name].join(' ').toLocaleLowerCase().includes(q));
    if (state.sort === 'title') items.sort((a, b) => a.title.localeCompare(b.title));
    return items;
  }
  function announce(text) { $('notice').textContent = text; $('notice').hidden = !text; }
  function setView(view) {
    state.view = view;
    if (view !== 'explorer') location.hash = view;
    render();
  }
  function openFigure(id) {
    state.selected = id; state.view = 'explorer';
    location.hash = `figure=${encodeURIComponent(id)}`;
    render();
    $('detail').scrollIntoView({block: 'start'});
  }
  function toggleCompare(f, input) {
    if (state.compare.has(f.id)) state.compare.delete(f.id);
    else if (state.compare.size < 3) state.compare.add(f.id);
    else {
      input.checked = false;
      announce('Compare up to three figures at a time. Uncheck a figure to add another.');
      return;
    }
    announce(''); $('compare-count').textContent = state.compare.size;
  }
  function compareControl(f) {
    const label = node('label', 'compare-label');
    const input = node('input'); input.type = 'checkbox'; input.checked = state.compare.has(f.id);
    input.setAttribute('aria-label', `Compare ${f.title}`);
    input.addEventListener('change', () => toggleCompare(f, input));
    label.append(input, document.createTextNode('Compare')); return label;
  }
  function card(f) {
    const article = node('article', 'card');
    const top = node('div', 'card-top');
    top.append(node('span', 'tag', f.category), node('span', 'number', String(figures.indexOf(f) + 1).padStart(2, '0')));
    article.append(top);
    if (f.status === 'ready') {
      const opener = button('', () => openFigure(f.id), 'plot-open');
      opener.setAttribute('aria-label', `Explore ${f.title}`);
      const img = node('img', 'thumbnail'); img.src = f.png; img.alt = ''; img.loading = 'lazy';
      const body = node('div', 'card-body'); body.append(node('h3', '', f.title), node('div', 'source', f.module));
      opener.append(img, body); article.append(opener);
      const bottom = node('div', 'card-bottom');
      bottom.append(compareControl(f), download('SVG ↗', f.svg)); article.append(bottom);
    } else {
      const content = node('div', 'unavailable-box');
      content.append(node('h3', '', f.title), node('strong', '', 'Figure unavailable'), node('p', '', f.error));
      article.append(content);
    }
    return article;
  }
  function zoom(f) {
    $('zoom-title').textContent = f.title; $('zoom-image').src = f.svg; $('zoom-image').alt = prose(f.alt_text);
    $('zoom-level').value = '100'; $('zoom-value').value = '100%'; $('zoom-image').style.width = '100%';
    $('zoom-dialog').showModal();
  }
  function detail(f, comparison = false) {
    const fragment = document.createDocumentFragment();
    const header = node('div', 'detail-header');
    const title = node('div'); title.append(node('span', 'tag', f.category), node('h2', '', f.title), node('p', 'source', `${f.function_name} · ${f.module}`));
    const actions = node('div', 'detail-actions');
    actions.append(button('⤢ Enlarge', () => zoom(f)), download('SVG ↓', f.svg), download('PNG ↓', f.png), download('Source CSV ↓', f.csv));
    if (comparison) actions.append(button('Remove', () => {state.compare.delete(f.id); render();}));
    header.append(title, actions);
    const figure = node('figure', 'detail-figure');
    const img = node('img', 'large-plot'); img.src = f.svg; img.alt = prose(f.alt_text);
    const caption = node('figcaption', 'caption'); caption.append(node('strong', '', 'Figure caption'), document.createTextNode(prose(f.caption)));
    figure.append(img, caption);
    const meta = node('details', 'metadata');
    meta.append(node('summary', '', 'Alternative text & source details'), node('p', '', prose(f.alt_text)),
      node('p', '', 'Source CSV contains the selected module plus reference quantities. It may include rows not displayed in this figure.'),
      node('pre', '', JSON.stringify({function: f.function_name, module: f.module, arguments: f.arguments}, null, 2)));
    if (f.warnings?.length) meta.append(node('strong', '', 'Plotting warnings'), node('pre', '', f.warnings.join('\n')));
    fragment.append(header, figure, meta); return fragment;
  }
  function render() {
    const items = filtered(); const visibleReady = items.filter(f => f.status === 'ready');
    $('gallery').hidden = state.view !== 'gallery'; $('explorer').hidden = state.view !== 'explorer'; $('comparison').hidden = state.view !== 'compare';
    for (const view of ['gallery', 'explore', 'compare']) $(view + '-view').setAttribute('aria-pressed', String((view === 'explore' ? 'explorer' : view) === state.view));
    $('compare-count').textContent = state.compare.size;
    $('section-title').textContent = state.view === 'compare' ? 'Compare figures' : state.category;
    $('results').textContent = state.view === 'compare' ? `${state.compare.size} selected · selections stay as you browse` :
      `${visibleReady.length} ready · ${items.length - visibleReady.length} unavailable · ${figures.length} figures in this report`;
    $('sort').disabled = state.view === 'compare'; $('search').disabled = state.view === 'compare';
    $('categories').replaceChildren(...['All figures', ...categories].map(category => {
      const b = button(category, () => {state.category = category; announce(''); if (state.view === 'compare') state.view = 'gallery'; render();});
      b.append(node('span', '', figures.filter(f => category === 'All figures' || f.category === category).length));
      b.setAttribute('aria-current', String(state.category === category)); return b;
    }));
    $('gallery').replaceChildren(...(items.length ? items.map(card) : [node('p', 'empty', 'No figures match this search. Try another term or choose All figures.')]));
    $('figure-select').replaceChildren(...visibleReady.map(f => {
      const option = node('option', '', f.title); option.value = f.id; return option;
    }));
    if (!visibleReady.some(f => f.id === state.selected)) state.selected = visibleReady[0]?.id;
    $('figure-select').value = state.selected || '';
    const selected = visibleReady.find(f => f.id === state.selected);
    $('detail').replaceChildren(selected ? detail(selected) : node('p', 'empty', 'No available figures match the current filters.'));
    const index = visibleReady.findIndex(f => f.id === state.selected);
    $('previous').disabled = index <= 0; $('next').disabled = index < 0 || index >= visibleReady.length - 1;
    const comparisons = figures.filter(f => state.compare.has(f.id)).map(f => {const a = node('article', 'detail'); a.append(detail(f, true)); return a;});
    $('comparison-grid').replaceChildren(...(comparisons.length ? comparisons : [node('p', 'empty', 'Build your comparison: open the Gallery and check Compare below two or three figures.')]));
  }
  function step(delta) {
    const items = filtered().filter(f => f.status === 'ready');
    const index = items.findIndex(f => f.id === state.selected);
    if (items[index + delta]) openFigure(items[index + delta].id);
  }
  function readHash() {
    const hash = location.hash.slice(1);
    if (hash.startsWith('figure=')) {
      let id; try { id = decodeURIComponent(hash.slice(7)); } catch (_) { return; }
      if (ready.some(f => f.id === id)) {
        state.selected = id; state.view = 'explorer';
        if (!filtered().some(f => f.id === id)) {state.category = 'All figures'; state.query = ''; $('search').value = '';}
      }
    } else if (hash === 'gallery' || hash === 'compare') state.view = hash;
    render();
  }
  document.title = `${report.title} · stockplotr`;
  $('report-title').textContent = report.title; $('report-subtitle').textContent = report.subtitle;
  $('figure-count').textContent = ready.length; $('category-count').textContent = categories.length;
  $('version').textContent = `BUILT WITH STOCKPLOTR ${report.package_version}`;
  $('generated').textContent = `Generated ${report.generated} · Original stockplotr figures`;
  $('gallery-view').addEventListener('click', () => setView('gallery'));
  $('explore-view').addEventListener('click', () => setView('explorer'));
  $('compare-view').addEventListener('click', () => setView('compare'));
  $('search').addEventListener('input', e => {state.query = e.target.value; render();});
  $('sort').addEventListener('change', e => {state.sort = e.target.value; render();});
  $('figure-select').addEventListener('change', e => openFigure(e.target.value));
  $('previous').addEventListener('click', () => step(-1)); $('next').addEventListener('click', () => step(1));
  $('print').addEventListener('click', () => window.print());
  $('close-zoom').addEventListener('click', () => $('zoom-dialog').close());
  $('zoom-level').addEventListener('input', e => {$('zoom-image').style.width = `${e.target.value}%`; $('zoom-value').value = `${e.target.value}%`;});
  document.addEventListener('keydown', e => {
    if (state.view !== 'explorer' || $('zoom-dialog').open || /INPUT|SELECT|TEXTAREA|BUTTON/.test(e.target.tagName)) return;
    if (e.key === 'ArrowRight') {e.preventDefault(); step(1);}
    if (e.key === 'ArrowLeft') {e.preventDefault(); step(-1);}
  });
  window.addEventListener('hashchange', readHash);
  readHash();
})();
