'use strict';

// Valida il contratto del bridge prima di invocare il renderer dei toast.
// La sanificazione del contenuto HTML rimane di competenza di mzToast.
function readToastPayload(event) {
    if (!event || !event.data || event.data.action !== 'triggerToast') return null;
    if (event.origin !== 'null' &&
        !/^https?:\/\/(127\.0\.0\.1|localhost|\[::1\])(:\d+)?$/.test(event.origin)) return null;
    if (!Array.isArray(event.data.toasts)) return null;
    const types = ['info', 'success', 'warning', 'error'];
    const positions = ['TL', 'TC', 'TR', 'CL', 'CC', 'CR', 'BL', 'BC', 'BR'];
    const themes = ['auto', 'glass', 'dark', 'light'];
    return event.data.toasts.filter(toast =>
        toast !== null && typeof toast === 'object' && !Array.isArray(toast) &&
        typeof toast.message === 'string' && typeof toast.title === 'string' &&
        types.includes(toast.type) && positions.includes(toast.position) &&
        themes.includes(toast.theme) && Number.isInteger(toast.duration) &&
        toast.duration >= 0 && toast.duration <= 2147483647
    ).map(toast => ({
        message: toast.message, title: toast.title, type: toast.type,
        position: toast.position, theme: toast.theme, duration: toast.duration
    }));
}

module.exports = { readToastPayload };
