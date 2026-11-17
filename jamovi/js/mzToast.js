'use strict';

const styleId = 'mz-toast-styles';
if (!document.getElementById(styleId)) {
    const style = document.createElement('style');
    style.id = styleId;
    style.innerHTML = `
        :root { --mz-translate: translate(-50%, -50%); }

        @keyframes mz-toast-pop { 0% { opacity: 0; transform: var(--mz-translate) scale(0.9); } 100% { opacity: 1; transform: var(--mz-translate) scale(1); } }
        @keyframes mz-toast-fade-out { 0% { opacity: 1; transform: var(--mz-translate) scale(1); } 100% { opacity: 0; transform: var(--mz-translate) scale(0.95); } }
        @keyframes mz-progress-shrink { from { width: 100%; } to { width: 0%; } }

        /* Contenitore della notifica */
        .mz-toast-modal {
            position: fixed;
            display: flex;
            flex-direction: row;
            align-items: flex-start;
            justify-content: flex-start;
            padding: 12px 42px 12px 18px;
            border-radius: 8px;
            box-shadow: 0 6px 20px rgba(0, 0, 0, 0.15);
            z-index: 999999;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            text-align: left;
            min-width: 240px;
            max-width: 360px;
            border-left: 8px solid;
            opacity: 0;
            pointer-events: auto;
            overflow: hidden;
            gap: 12px;
            box-sizing: border-box;
        }

        /* --- TEMI VISIVI --- */
        .mz-theme-glass {
            background: rgba(255, 255, 255, 0.88);
            backdrop-filter: blur(12px);
            -webkit-backdrop-filter: blur(12px);
            color: #1E293B;
            border: 1px solid rgba(255, 255, 255, 0.4);
        }

        /* Tema scuro alleggerito (Slate Theme) */
        .mz-theme-dark {
            background: #334155;
            color: #F8FAFC;
            border: 1px solid #475569;
            box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
        }

        .mz-theme-light {
            background: #FFFFFF;
            color: #1E293B;
            border: 1px solid #E2E8F0;
        }

        /* Posizionamento Orizzontale / Verticale */
        .mz-pos-TL { top: 20px; left: 20px; --mz-translate: translate(0, 0); }
        .mz-pos-TC { top: 20px; left: 50%; --mz-translate: translate(-50%, 0); }
        .mz-pos-TR { top: 20px; right: 20px; left: auto; --mz-translate: translate(0, 0); }
        .mz-pos-CL { top: 50%; left: 20px; --mz-translate: translate(0, -50%); }
        .mz-pos-CC { top: 50%; left: 50%; --mz-translate: translate(-50%, -50%); }
        .mz-pos-CR { top: 50%; right: 20px; left: auto; --mz-translate: translate(0, -50%); }
        .mz-pos-BL { bottom: 20px; top: auto; left: 20px; --mz-translate: translate(0, 0); }
        .mz-pos-BC { bottom: 20px; top: auto; left: 50%; --mz-translate: translate(-50%, 0); }
        .mz-pos-BR { bottom: 20px; top: auto; right: 20px; left: auto; --mz-translate: translate(0, 0); }

        .mz-toast-modal.show { animation: mz-toast-pop 0.35s cubic-bezier(0.175, 0.885, 0.32, 1.275) forwards; }
        .mz-toast-modal.hide { animation: mz-toast-fade-out 0.2s ease-in forwards; pointer-events: none; }

        /* Bottone Chiusura Circolare (Default / Light) */
        .mz-toast-close {
            position: absolute;
            top: 9px;
            right: 9px;
            width: 24px;
            height: 24px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 14px;
            font-weight: 700;
            line-height: 1;
            cursor: pointer;
            user-select: none;
            color: #5F6B78;
            background: radial-gradient(circle at 30% 28%, rgba(255,255,255,.95) 0%, rgba(248,250,252,.92) 42%, rgba(226,232,240,.90) 100%);
            border: 1px solid rgba(148,163,184,.45);
            box-shadow: 0 2px 5px rgba(15,23,42,.18), inset 0 1px 0 rgba(255,255,255,.95), inset 0 -1px 1px rgba(148,163,184,.20);
            transition: transform .12s ease, box-shadow .18s ease, color .18s ease, background .18s ease;
        }

        .mz-toast-close:hover {
            color: #0F172A;
            background: radial-gradient(circle at 30% 28%, #FFFFFF 0%, #F8FAFC 40%, #E2E8F0 100%);
            transform: translateY(-1px) scale(1.04);
            box-shadow: 0 4px 10px rgba(15,23,42,.22), inset 0 1px 0 rgba(255,255,255,1), inset 0 -1px 1px rgba(148,163,184,.18);
        }

        .mz-toast-close:active {
            transform: translateY(1px) scale(.96);
            box-shadow: inset 0 2px 4px rgba(15,23,42,.22), inset 0 1px 0 rgba(255,255,255,.45);
        }

        /* Bottone Chiusura Minimal per Tema Scuro */
        .mz-theme-dark .mz-toast-close {
            color: #CBD5E1;
            background: rgba(255, 255, 255, 0.1);
            border: 1px solid rgba(255, 255, 255, 0.15);
            box-shadow: none;
        }

        .mz-theme-dark .mz-toast-close:hover {
            color: #FFFFFF;
            background: rgba(255, 255, 255, 0.2);
            transform: scale(1.08);
            box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        }

        .mz-theme-dark .mz-toast-close:active {
            transform: scale(.94);
            background: rgba(255, 255, 255, 0.15);
        }

        .mz-toast-icon { width: 22px; height: 22px; flex-shrink: 0; margin-top: 1px; fill: var(--mz-toast-accent, currentColor); }

        .mz-toast-content {
            display: flex;
            flex-direction: column;
            gap: 4px;
            flex-grow: 1;
            overflow-wrap: break-word;
        }

        .mz-toast-title { font-size: 13.5px; font-weight: 700; line-height: 1.3; letter-spacing: 0.1px; }

        .mz-toast-message {
            font-size: 12.5px;
            line-height: 1.4;
            font-weight: 400;
            opacity: 0.95;
            text-align: justify;
            text-align-last: left;
        }

        /* Pulsante Azione Opzionale */
        .mz-toast-action-btn {
            margin-top: 6px;
            align-self: flex-start;
            padding: 3px 8px;
            font-size: 11px;
            font-weight: 600;
            border-radius: 4px;
            border: none;
            background: rgba(0, 0, 0, 0.08);
            color: inherit;
            cursor: pointer;
            transition: background 0.2s ease;
        }

        .mz-toast-action-btn:hover { background: rgba(0, 0, 0, 0.16); }

        .mz-theme-dark .mz-toast-action-btn { background: rgba(255, 255, 255, 0.12); }
        .mz-theme-dark .mz-toast-action-btn:hover { background: rgba(255, 255, 255, 0.20); }

        /* Barra di progresso inferiore */
        .mz-toast-progress {
            position: absolute;
            bottom: 0;
            left: 0;
            height: 6px;
        }
    `;

    document.head.appendChild(style);
}

const TOAST_CONFIG = {
    info: {
        color: '#3E6DA9',
        svg: '<svg class="mz-toast-icon" viewBox="0 0 24 24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z"/></svg>'
    },

    success: {
        color: '#2ECC71',
        svg: '<svg class="mz-toast-icon" viewBox="0 0 24 24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"/></svg>'
    },

    warning: {
        color: '#eca038',
        svg: '<svg class="mz-toast-icon" viewBox="0 0 24 24"><path d="M1 21h22L12 2 1 21zm12-3h-2v-2h2v2zm0-4h-2v-4h2v4z"/></svg>'
    },

    error: {
        color: '#e74c3c',
        svg: '<svg class="mz-toast-icon" viewBox="0 0 24 24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-2h2v2zm0-4h-2V7h2v6z"/></svg>'
    }
};

const MZ_TOAST_ALLOWED_TAGS = ['b', 'i', 'strong', 'em', 'br'];

function sanitizeToastHtml(html) {
    if (html === null || html === undefined) return '';
    // Il template è inerte: immagini e gestori di evento non devono attivarsi
    // neppure durante il parsing, prima della rimozione tramite whitelist.
    const container = document.createElement('template');
    container.innerHTML = String(html);

    const walk = (node) => {
        Array.prototype.slice.call(node.childNodes).forEach((child) => {
            if (child.nodeType === Node.ELEMENT_NODE) {
                const tag = child.tagName.toLowerCase();
                if (MZ_TOAST_ALLOWED_TAGS.indexOf(tag) !== -1) {
                    Array.prototype.slice.call(child.attributes).forEach((attr) => {
                        child.removeAttribute(attr.name);
                    });
                    walk(child);
                } else {
                    child.replaceWith(document.createTextNode(child.textContent));
                }
            }
        });
    };
    walk(container.content);

    return container.innerHTML;
}

function escapeToastText(text) {
    if (text === null || text === undefined) return '';
    const div = document.createElement('div');
    div.textContent = String(text);
    return div.innerHTML;
}

/**
 * Mostra un Toast personalizzato
 * @param {string} message - Il testo o HTML del messaggio
 * @param {string} type - 'info' | 'success' | 'warning' | 'error'
 * @param {number|string} duration - Durata in ms. Se impostato a 0, il toast diventa 'Sticky'
 * @param {string} position - 'TL'|'TC'|'TR'|'CL'|'CC'|'CR'|'BL'|'BC'|'BR'
 * @param {string} theme - 'auto' | 'glass' | 'dark' | 'light'
 * @param {string} title - Titolo opzionale mostrato sopra il messaggio
 * @param {Object} options - { actionText: string, onAction: Function }
 */
const mzToastQueue = [];
let mzToastActive = false;

function _processToastQueue() {
    if (mzToastActive || mzToastQueue.length === 0) return;
    
    mzToastActive = true;
    const t = mzToastQueue.shift();
    _renderToast(t.message, t.type, t.duration, t.position, t.theme, t.title, t.options);
}

function showToast(message, type = 'info', duration = 4000, position = 'CC', theme = 'auto', title = '', options = {}) {
    const existingToast = document.getElementById('mz-toast-msg');
    if (existingToast && existingToast.dataset.message === String(message) && existingToast.dataset.type === String(type)) {
        return;
    }
    
    const isQueued = mzToastQueue.some(t => t.message === String(message) && t.type === String(type));
    if (isQueued) {
        return;
    }
    
    mzToastQueue.push({message, type, duration, position, theme, title, options});
    _processToastQueue();
}

function _renderToast(message, type = 'info', duration = 4000, position = 'CC', theme = 'auto', title = '', options = {}) {
    
    // Convertiamo forzatamente duration in numero nel caso in cui R lo passi come stringa ("0")
    duration = Number(duration);

    const config = Object.prototype.hasOwnProperty.call(TOAST_CONFIG, type) ? TOAST_CONFIG[type] : TOAST_CONFIG['info'];

        const isSticky = (duration === 0);

        // Guard difensivo: se duration non è valido (es. null/testo strano/negativo), reset a 4000
        if (!isSticky && (isNaN(duration) || !isFinite(duration) || duration < 0)) {
            duration = 4000;
        }

        const validPos = ['TL', 'TC', 'TR', 'CL', 'CC', 'CR', 'BL', 'BC', 'BR'];
        const safePos = validPos.includes(position) ? position : 'CC';

        let safeTheme = theme;

        if (theme === 'auto') {
            const isDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
            safeTheme = isDark ? 'dark' : 'glass';
        } else if (!['glass', 'dark', 'light'].includes(theme)) {
            safeTheme = 'glass';
        }

        const toast = document.createElement('div');

        toast.id = 'mz-toast-msg';
        toast.dataset.message = String(message);
        toast.dataset.type = String(type);
        toast.className = `mz-toast-modal mz-pos-${safePos} mz-theme-${safeTheme}`;
        toast.style.borderLeftColor = config.color;
        toast.style.setProperty('--mz-toast-accent', config.color);

        toast.setAttribute('role', (type === 'error' || type === 'warning') ? 'alert' : 'status');
        toast.setAttribute('aria-live', type === 'error' ? 'assertive' : 'polite');

        const resolvedTitle = title || options.title;
        const titleHtml = resolvedTitle
            ? `<div class="mz-toast-title">${sanitizeToastHtml(resolvedTitle)}</div>`
            : '';

        const actionLabel = options.actionText || (isSticky ? 'Got it' : null);
        const actionHtml = actionLabel
            ? `<button class="mz-toast-action-btn" id="mz-toast-action-btn">${escapeToastText(actionLabel)}</button>`
            : '';

        const progressHtml = isSticky
            ? ''
            : `<div class="mz-toast-progress" id="mz-toast-progress-bar" style="background-color: ${config.color}; animation: mz-progress-shrink ${duration}ms linear forwards;"></div>`;

        toast.innerHTML = `
            ${config.svg}
            <div class="mz-toast-content">
                ${titleHtml}
                <div class="mz-toast-message">${sanitizeToastHtml(message)}</div>
                ${actionHtml}
            </div>
            <div class="mz-toast-close" id="mz-toast-close-btn" title="Close">&times;</div>
            ${progressHtml}
        `;

        document.body.appendChild(toast);

        let timeoutId = isSticky ? null : setTimeout(() => chiudiToast(), duration);
        let isPaused = false;
        let remainingTime = duration;
        let startTime = Date.now();

        const chiudiToast = () => {
            toast.classList.remove('show');
            toast.classList.add('hide');
            
            document.removeEventListener('keydown', handleEsc);

            toast.addEventListener('animationend', () => {
                if (toast.parentNode) toast.parentNode.removeChild(toast);
                mzToastActive = false;
                _processToastQueue();
            }, { once: true });
        };

        const handleEsc = (e) => {
            if (e.key === 'Escape' || e.key === 'Esc') {
                if (timeoutId) clearTimeout(timeoutId);
                chiudiToast();
            }
        };
        document.addEventListener('keydown', handleEsc);

        toast.querySelector('#mz-toast-close-btn').addEventListener('click', () => {
            if (timeoutId) clearTimeout(timeoutId);
            chiudiToast();
        });

        const actionBtn = toast.querySelector('#mz-toast-action-btn');

        if (actionBtn) {
            actionBtn.addEventListener('click', () => {
                if (timeoutId) clearTimeout(timeoutId);
                if (typeof options.onAction === 'function') {
                    options.onAction();
                }
                chiudiToast();
            });
        }

        if (!isSticky) {
            toast.addEventListener('mouseenter', () => {
                if (!isPaused) {
                    clearTimeout(timeoutId);
                    remainingTime -= (Date.now() - startTime);

                    const bar = document.getElementById('mz-toast-progress-bar');
                    if (bar) bar.style.animationPlayState = 'paused';

                    isPaused = true;
                }
            });

            toast.addEventListener('mouseleave', () => {
                if (isPaused) {
                    startTime = Date.now();
                    timeoutId = setTimeout(() => chiudiToast(), remainingTime);

                    const bar = document.getElementById('mz-toast-progress-bar');
                    if (bar) bar.style.animationPlayState = 'running';

                    isPaused = false;
                }
            });
        }

        requestAnimationFrame(() => toast.classList.add('show'));
}

module.exports = { showToast: showToast };
