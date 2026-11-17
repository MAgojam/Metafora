// mzDebug.js
'use strict';

// -----------------------------------------------------------------------------
// CSS INJECTION — solo classi con prefisso mz-debug- per evitare conflitti
// -----------------------------------------------------------------------------
const css = `
.mz-debug-pill-container {
    position: relative;
    display: inline-block;
    margin-left: 20px;
    min-width: 150px;
    height: 30px;
}

.mz-debug-pill {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 100%;
    height: 100%;
    box-sizing: border-box;
    font-size: 1em;
    font-weight: bold;
    text-transform: uppercase;
    white-space: nowrap;
    color: #3e6da9;
    border: 3px solid #3e6da9;
    border-radius: 9999px;
    box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.3);
    padding: 0 16px;
}

.mz-debug-toast {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    box-sizing: border-box;
    display: flex;
    align-items: center;
    justify-content: center;
    white-space: nowrap;
    font-size: 1em;
    font-weight: bold;
    text-transform: uppercase;
    color: #fff;
    background-color: #3e6da9;
    border: 3px solid #3e6da9;
    border-radius: 9999px;
    box-shadow: none;
    padding: 0 16px;
    opacity: 0;
    transition: opacity 0.5s;
    z-index: 9999;
}

.mz-debug-toast.show { opacity: 1; }

.mz-debug-tooltip {
    display: none;
    position: absolute;
    background-color: white;
    color: black;
    padding: 4px 7px;
    border-radius: 14px;
    z-index: 1000;
    white-space: nowrap;
    box-shadow: 0px 0px 5px rgba(0, 0, 0, 0.2);
    border: 2px solid #3e6da9;
}
`;

const styleNode = document.createElement('style');
styleNode.innerHTML = css;
document.body.appendChild(styleNode);

// -----------------------------------------------------------------------------
// FUNZIONI DI SUPPORTO
// -----------------------------------------------------------------------------

const TooltipManager = {
    createTooltip(element, text, position = 'center') {
        if (!element) return;
        const tooltip = document.createElement('div');
        tooltip.className = 'mz-debug-tooltip';
        tooltip.textContent = text;
        document.body.appendChild(tooltip);

        element.addEventListener('mouseover', (event) => {
            tooltip.style.display = 'block';
            const tooltipWidth = tooltip.offsetWidth;
            const { pageX, pageY } = event;

            switch (position) {
                case 'left':
                    tooltip.style.left = `${pageX - tooltipWidth - 10}px`;
                    break;
                case 'right':
                    tooltip.style.left = `${pageX + 10}px`;
                    break;
                case 'center':
                default:
                    tooltip.style.left = `${pageX - tooltipWidth / 2}px`;
            }
            tooltip.style.top = `${pageY + 10}px`;
        });

        element.addEventListener('mousemove', (event) => {
            const tooltipWidth = tooltip.offsetWidth;
            const { pageX } = event;
            if (position === 'center') {
                tooltip.style.left = `${pageX - tooltipWidth / 2}px`;
            }
        });

        element.addEventListener('mouseout', () => {
            tooltip.style.display = 'none';
        });
    }
};

const KeyboardShortcuts = {
    addShortcut(keys, callback) {
        document.addEventListener('keydown', (event) => {
            const keyCombo = keys.map((key) => key.toLowerCase());
            const isMatch = keyCombo.every((key) => {
                if (key === 'ctrl') return event.ctrlKey || event.metaKey;
                if (key === 'shift') return event.shiftKey;
                if (key === 'alt') return event.altKey;
                return event.key.toLowerCase() === key;
            });

            if (isMatch) {
                event.preventDefault();
                callback(event);
            }
        }, false);
    }
};

const DOMUtils = {
    insertAfter(referenceNode, newNode) {
        if (referenceNode && referenceNode.parentNode) {
            referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
        }
    },
    createDivWithClass(className) {
        const div = document.createElement('div');
        div.className = className;
        return div;
    }
};

const ToastManager = {
    showToast(message, duration = 2000, container) {
        const toast = document.createElement('div');
        toast.className = 'mz-debug-toast';
        toast.textContent = message;

        const parent = container || document.body;
        parent.appendChild(toast);

        requestAnimationFrame(() => {
            toast.classList.add('show');
        });

        setTimeout(() => {
            toast.classList.remove('show');
            toast.addEventListener('transitionend', () => {
                toast.remove();
            }, { once: true });
        }, duration);
    }
};

// -----------------------------------------------------------------------------
// LOGICA PRINCIPALE
// -----------------------------------------------------------------------------

/**
 * Trova il wrapper DOM del deviatore jlogMode generato da mzTabbedControl.
 * L'id assegnato al wrapper è "mz_jlogMode_wrapper", il suo genitore
 * .control-item è il nodo da nascondere/mostrare.
 */
function getDeviatoreContainer() {
    const wrapper = document.getElementById('mz_jlogMode_wrapper');
    if (wrapper) {
        // Il .control-item.control-item-compact che lo contiene
        return wrapper.closest('.control-item') || wrapper;
    }
    return null;
}

/**
 * enableLoggingIndicator — entry point chiamato da main.js nel loaded()
 *
 * Stato iniziale: pillola nascosta, deviatore nascosto, jlogMode = posOff
 * CTRL+SHIFT+L: mostra pillola "DEBUG · SAVE", mostra deviatore (in posOff)
 * CTRL+ALT+L:   nasconde pillola, nasconde deviatore, resetta jlogMode a posOff
 */
const enableLoggingIndicator = (optionsArg) => {
    const ui = (optionsArg && optionsArg.jlogMode) ? optionsArg : (optionsArg ? optionsArg.ui : null);

    if (!ui || !ui.jlogMode) {
        return;
    }

    // Evita duplicazioni
    if (document.querySelector('.mz-debug-pill-container')) {
        return;
    }

    // --- 1. Nascondi il deviatore al load (nessun flash) ---
    setTimeout(() => {
        const devContainer = getDeviatoreContainer();
        if (devContainer) {
            devContainer.style.display = 'none';
        }
    }, 0);

    // --- 2. Crea la pillola (nascosta al load) ---
    const target = document.querySelector('.silky-options-header h1') || document.querySelector('h1');
    let pillContainer = null;
    let pill = null;

    if (target) {
        pillContainer = DOMUtils.createDivWithClass('mz-debug-pill-container');
        pillContainer.style.display = 'none'; // nascosta al load
        DOMUtils.insertAfter(target, pillContainer);

        pill = DOMUtils.createDivWithClass('mz-debug-pill');
        pill.innerHTML = '<span>DEBUG · SAVE</span>';
        pillContainer.appendChild(pill);

        TooltipManager.createTooltip(
            pill,
            'CTRL+ALT+L to disable',
            'center'
        );
    }

    // --- 3. Shortcut CTRL+SHIFT+L — Attiva debug mode ---
    KeyboardShortcuts.addShortcut(['ctrl', 'shift', 'l'], () => {
        try {
            // Mostra il deviatore nel TAB SAVE (lo lascia su posOff)
            const devContainer = getDeviatoreContainer();
            if (devContainer) {
                devContainer.style.display = '';
            }

            // Mostra la pillola
            if (pillContainer) {
                pillContainer.style.display = '';
            }

            ToastManager.showToast('DEBUG ENABLED', 1500, pillContainer);
        } catch (e) { /* silenzioso */ }
    });

    // --- 4. Shortcut CTRL+ALT+L — Disattiva debug mode ---
    KeyboardShortcuts.addShortcut(['ctrl', 'alt', 'l'], () => {
        try {
            // Resetta jlogMode a posOff
            if (ui.jlogMode && typeof ui.jlogMode.setValue === 'function') {
                ui.jlogMode.setValue('posOff');
            }

            // Aggiorna anche il DOM del deviatore per coerenza visiva
            const devRow = document.getElementById('mz_jlogMode');
            if (devRow) {
                devRow.setAttribute('data-state', 'posOff');
            }

            // Nasconde il deviatore
            const devContainer = getDeviatoreContainer();
            if (devContainer) {
                devContainer.style.display = 'none';
            }

            // Mostra il toast PRIMA di nascondere la pillola, altrimenti
            // il toast (figlio di pillContainer) risulta invisibile
            ToastManager.showToast('DEBUG DISABLED', 1500, pillContainer);

            // Nasconde la pillola solo dopo che il toast ha completato
            // il proprio ciclo (durata + transizione di fade-out)
            setTimeout(() => {
                if (pillContainer) {
                    pillContainer.style.display = 'none';
                }
            }, 2000);
        } catch (e) { /* silenzioso */ }
    });
};

module.exports = {
    TooltipManager,
    KeyboardShortcuts,
    DOMUtils,
    ToastManager,
    enableLoggingIndicator
};
