// jamovi/js/css.js - Integrazione TabbedControl
"use strict";

const css = `

/* Simbolo vettoriale decorativo: indipendente dai glifi dei font di sistema. */
:is(.toggle-label, .toggle-label-vertical):has(#mz_showBackTransform) .toggle-text::after {
    content: "";
    display: inline-block;
    width: 14px;
    height: 14px;
    margin-inline-start: 5px;
    vertical-align: -2px;
    background-color: currentColor;
    -webkit-mask: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 20 20'%3E%3Cpath d='M7 3 3 7l4 4M3 7h8a5 5 0 0 1 0 10H8' fill='none' stroke='black' stroke-width='1.5' stroke-linecap='round' stroke-linejoin='round'/%3E%3C/svg%3E") center / contain no-repeat;
    mask: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 20 20'%3E%3Cpath d='M7 3 3 7l4 4M3 7h8a5 5 0 0 1 0 10H8' fill='none' stroke='black' stroke-width='1.5' stroke-linecap='round' stroke-linejoin='round'/%3E%3C/svg%3E") center / contain no-repeat;
}

/* Un solo giro alla selezione; l'icona ruota senza modificare il layout. */
.toggle-text.mz-icon-rotate-once::after {
    transform-origin: center;
    animation: mz-icon-selection-turn 350ms ease-in-out;
}

@keyframes mz-icon-selection-turn {
    from { transform: rotate(0deg); }
    to { transform: rotate(-360deg); }
}

@media (prefers-reduced-motion: reduce) {
    .toggle-text.mz-icon-rotate-once:not(.mz-icon-motion-allowed)::after { animation: none; }
}

/* Pillola informativa delle dipendenze, condivisa da tutti i modelli. */
.mz-dependency-tooltip {
    position: fixed;
    z-index: 10000;
    max-width: min(320px, calc(100vw - 16px));
    box-sizing: border-box;
    padding: 6px 11px;
    border: 1px solid #3e6da9;
    border-radius: 18px;
    background: #fff;
    color: #3e6da9;
    box-shadow: 0 2px 6px rgba(62, 109, 169, .18);
    font: 12px/1.35 Arial, sans-serif;
    pointer-events: none;
}
.mz-dependency-tooltip[hidden] { display: none; }

/* =====================================================
   NAVIGAZIONE SCHEDE - STILE MATERIAL DESIGN - INIZIO
   ===================================================== */

.tab-headers {
    display: flex;                     /* Layout flessibile */
    position: relative;                /* Posizionamento relativo per pseudo-elementi */
    border-bottom: 1px solid rgba(62, 109, 169, 0.50);  /* Linea separatrice inferiore */
    margin-bottom: 0;                  /* Eliminato spazio sotto le schede per avvicinare i contenuti */
    background: transparent;           /* Sfondo trasparente */
}

.tab-header {
    padding: 4px 16px;                 /* Spaziatura interna */
    cursor: pointer;                   /* Cursore a manina per indicare interattività */
    background: transparent;           /* Sfondo trasparente di default */
    border: none;                      /* Nessun bordo */
    color: #666;                       /* Colore testo grigio medio */
    font-weight: 500;                  /* Peso font medio */
    text-transform: uppercase;         /* Testo in maiuscolo per leggibilità */
    font-size: 11px;                   /* Dimensione font piccola e discreta */
    letter-spacing: 0.5px;             /* Spaziatura tra lettere per miglior leggibilità */
    position: relative;                /* Per posizionare l'indicatore attivo */
    white-space: nowrap;               /* Impedisce il wrap del testo */
    flex-shrink: 0;                    /* Impedisce la riduzione della scheda */
    display: flex;                     /* Layout flex per contenuto */
    align-items: center;               /* Centra verticalmente il contenuto */
    min-height: 28px;                  /* Altezza minima per area cliccabile */
    border-radius: 3px;                /* Angoli arrotondati per hover */
    transition: all 0.3s ease, 
                color 0.3s ease, 
                background-color 0.3s ease;  /* Transizione fluida per hover e stati */
}

.tab-header.active {
    color: #3e6da9;                    /* Colore blu jamovi per scheda attiva */
    font-weight: 700;                  /* Peso font più marcato per evidenziare */
    background-color: rgba(62, 109, 169, 0.20); /* Colore azzurro chiaro per lo sfondo del tab attivo */
    border-radius: 3px 3px 0 0;        /* Rende gli angoli superiori arrotondati, evitando spigoli vivi */
}

.tab-header.active::after {
    content: '';                       /* Pseudo-elemento per indicatore */
    position: absolute;                /* Posizionamento assoluto rispetto al parent */
    bottom: -1px;                      /* Sovrappone il bordo del contenitore */
    left: 0;                           /* Allineamento a sinistra */
    right: 0;                          /* Allineamento a destra */
    height: 3px;                       /* Spessore dell'indicatore */
    background: #3e6da9;               /* Colore blu jamovi per coerenza */
    border-radius: 2px 2px 0 0;        /* Arrotondamento solo superiore */
}

.tab-header:hover:not(.active) {
    background: #c8c8c8;               /* Sfondo grigio chiaro al passaggio del mouse */
    border-radius: 3px;                /* Angoli arrotondati per feedback visivo */
}

.tab-header:hover:not(.active)::after {
    content: '';                    /* Pseudo-elemento per l'indicatore */
    position: absolute;
    bottom: -1px;                   /* Sovrappone il bordo del contenitore */
    left: 0;
    right: 0;
    height: 3px;                    /* Spessore dell'indicatore */
    background: #888;               /* Colore grigio scuro per coerenza con l'hover */
    border-radius: 2px 2px 0 0;
    transition: all 0.2s ease;      /* Aggiunge una transizione fluida (opzionale) */
}

/* === NAVIGAZIONE SCHEDE - STILE MATERIAL DESIGN - FINE === */


/* =====================================================
   AREA CONTENUTO SCHEDE - INIZIO
   ===================================================== */

.tab-content-area {
    background: transparent;           /* Sfondo trasparente per integrazione */
    overflow-y: auto;                  /* Scroll verticale quando necessario */
}

.tab-content {
    display: none;                     /* Nascosto di default */
    padding: 0;                        /* Nessun padding di default */
    background: transparent;           /* Sfondo trasparente */
    width: 100%;                       /* Occupa tutta la larghezza */
    max-width: 100%;                   /* Limita la larghezza massima */
    overflow: hidden;                  /* Previene overflow orizzontale */
    box-sizing: border-box;            /* Include padding e bordi nella larghezza */
}

.tab-content.active {
    display: block;                    /* Mostra quando la scheda è attiva */
}

/* === AREA CONTENUTO SCHEDE - FINE === */


/* =====================================================
   SEZIONE TITOLO SCHEDA - TITOLO/SOTTOTITOLO OPZIONALE - INIZIO
   ===================================================== */

.tab-title-section {
    padding: 12px 20px 8px 20px;       /* Spaziatura interna per titolo */
    border-bottom: 1px solid #f0f0f0;  /* Linea separatrice sottile */
    margin-bottom: 0;                  /* Eliminato spazio sotto la sezione titolo */
}

/* Classi per allineamento configurabile del titolo */
.tab-title-section.align-left {
    text-align: left;                  /* Allinea testo a sinistra */
}

.tab-title-section.align-center {
    text-align: center;                /* Centra il testo */
}

.tab-title-section.align-right {
    text-align: right;                 /* Allinea testo a destra */
}

.tab-main-title {
    margin: 0 0 4px 0;                 /* Margine solo inferiore */
    font-size: 14px;                   /* Dimensione font per titolo principale */
    font-weight: 600;                  /* Peso font grassetto */
    color: #333;                       /* Colore testo scuro per contrasto */
    line-height: 1.3;                  /* Interlinea compatta */
}

.tab-main-subtitle {
    margin: 0;                         /* Nessun margine */
    font-size: 12px;                   /* Font più piccolo per sottotitolo */
    font-weight: 400;                  /* Peso font normale */
    color: #666;                       /* Colore più chiaro per gerarchia */
    line-height: 1.4;                  /* Interlinea leggermente maggiore */
}

.tab-body {
    padding: 2px 0 12px 0;             /* Padding: [Top: 2px] [Right: 0] [Bottom: 12px] [Left: 0]. Right/Left a 0 allineano la griglia esternamente. */
    background: transparent;           /* Sfondo trasparente */
}

/* ==== SEZIONE TITOLO SCHEDA - TITOLO/SOTTOTITOLO OPZIONALE - FINE === */


/* =====================================================
   SISTEMA LAYOUT GRIGLIA - INIZIO
   ===================================================== */

.tab-body-grid {
    display: grid;                     /* Attiva layout griglia CSS */
    align-items: start;                /* Allinea elementi all'inizio */
    grid-template-columns: auto 1fr;   /* Titolo colonna sinistra, controlli colonna destra */
    gap: 2px;                         /* Gap minimo tra colonne per massima compattezza visiva */
}

/* Gruppi che devono estendersi su tutta la larghezza */
.group-spanned {
    grid-column: 1 / -1;               /* Occupa entrambe le colonne */
    display: block;                    /* Mostrato a blocco */
}

/* Classe forzata per garantire layout griglia anche con specificità CSS esterne */
.tab-body-grid-forced {
    display: grid !important;          /* Forza display grid */
    width: 100% !important;            /* Forza larghezza piena */
    align-items: stretch !important;   /* RIGHE: Le celle si estendono per occupare tutta l'altezza della riga */
    box-sizing: border-box !important; /* Forza box-sizing */
}

.grid-cell {
    display: flex;                     /* Layout flex per contenuto cella */
    flex-direction: column;            /* Dispone elementi verticalmente */
    gap: 2px;                          /* Spazio minimo tra elementi interni */
    padding: 2px 0;                    /* Rimossa la spaziatura orizzontale (dx e sx) per allineare al limite esterno */
    background: transparent;           /* Sfondo trasparente di default */
    min-height: 0;                     /* Permette il colasso naturale degli accordion */
    width: 100%;                       /* Larghezza piena della cella */
    height: 100%;                      /* Occupa tutta l'altezza della riga (stretch) */
    box-sizing: border-box;            /* Include padding nella larghezza */
}

/* ALLINEAMENTO 3x3 (9 POSIZIONI) */

/* ALLINEAMENTO 3x3 (9 POSIZIONI) */

/* Verticale (justify-content perché flex-direction è column) */
.grid-cell.v-align-top    { justify-content: flex-start !important; }
.grid-cell.v-align-center { justify-content: center !important; }
.grid-cell.v-align-bottom { justify-content: flex-end !important; }

/* Orizzontale (align-items perché flex-direction è column) */
.grid-cell.h-align-left   { align-items: flex-start !important; }
.grid-cell.h-align-center { align-items: center !important; }
.grid-cell.h-align-right  { align-items: flex-end !important; }
.grid-cell.h-align-stretch { align-items: stretch !important; }

/* NUOVO SISTEMA DI SELF-ALIGNMENT (3x3 INDIPENDENTE) */
.mz-control-wrapper {
    display: flex; /* Default: stacking verticale standard jamovi */
    flex-direction: column;
    width: 100%;
    pointer-events: none; /* Il wrapper non deve intercettare click */
}

/* SISTEMA 3X3 SELETTIVO: Attivato solo da .mz-overlap */
.mz-control-wrapper.mz-overlap {
    display: grid !important;
    grid-area: 1/1 !important; /* Forza l'overlap solo per questi elementi */
    width: 100%;               /* Forza larghezza piena per permettere spanning e allineamento */
    height: fit-content;
}

/* Layer specifico per l'overlay dei divisori */
.mz-divider-layer {
    padding: 0 !important;           /* Rimuove padding per permettere alla linea di toccare i bordi */
    align-items: stretch !important; /* Forza il wrapper figlio a occupare tutta la larghezza dello span */
    justify-content: stretch !important;
    background: transparent !important;
    z-index: 100 !important;         /* Garantisce che stia SOPRA A TUTTO */
    pointer-events: none !important; /* Non interferisce con i click sugli oggetti sottostanti */
}

/* I divisori devono poter riempire tutta la larghezza per mostrare la linea */
.mz-control-wrapper.mz-divider-wrapper {
    width: 100% !important;
    justify-self: stretch !important;
    align-self: stretch !important;
}

/* Contenitori dei controlli come genitori relativi per il posizionamento chirurgico */
.grid-single-controls, .grid-span-controls, .span-col-wrapper {
    position: relative !important;
    width: 100%;
}

.mz-control-wrapper > * {
    pointer-events: none !important; /* Le aree vuote/trasparenti del contenitore non devono bloccare i click */
}

/* Riattiva i pointer-events chirurgicamente solo sugli elementi visibili e interattivi */
.mz-control-wrapper label,
.mz-control-wrapper input,
.mz-control-wrapper select,
.mz-control-wrapper button,
.mz-control-wrapper .toggle-switch,
.mz-control-wrapper .toggle-switch-vertical,
.mz-control-wrapper .control-slider,
.mz-control-wrapper .group-title-badges,
.mz-control-wrapper .mz-label-item,
.mz-control-wrapper .control-label,
.mz-control-wrapper .control-label-inline,
.mz-control-wrapper .radio-group-label {
    pointer-events: auto !important;
}


/* Classi di allineamento individuale */
.self-v-top    { align-self: start !important; }
.self-v-center { align-self: center !important; }
.self-v-bottom { align-self: end !important; }
.self-h-left   { justify-self: start !important; }
.self-h-center { justify-self: center !important; }
.self-h-right  { justify-self: end !important; }

/* Inibizione margini per allineamenti precisi 3x3 */
.mz-control-wrapper .control-item,
.mz-control-wrapper .action-button-container {
    margin-bottom: 0 !important;
}

/* CONTENITORI CONTROLLI: Devono essere griglie 1x1 per permettere il 3x3 indipendente dei figli */
.grid-single-controls,
.grid-span-controls,
.span-col-wrapper {
    height: 100% !important;
}

.span-col-wrapper {
    flex: 1; /* Assicura riempimento se usato in contesti flex intermedio */
}

/* Rimuove margin-bottom dall'ultimo elemento per un centramento verticale perfetto */
.grid-cell[class*="v-align-"] .control-item:last-child {
    margin-bottom: 0 !important;
}

/* Rimuove indentazione forzata se la cella ha un allineamento o se richiesto da no-indent */
.grid-cell.h-align-center .control-item-compact,
.grid-cell.h-align-right  .control-item-compact,
.grid-cell .no-indent .control-item-compact {
    margin-left: 0 !important;
}

/* CORREZIONE PER CELLE CON SPAN MULTIPLI (Titolo sopra, Controlli posizionati per proporzione) */

/* 1. Applica l'impilamento verticale (Titolo vs Controlli Wrapper) sulla cella estesa. */
.grid-cell[style*="span"] {
    display: flex !important;           /* Forza l'impilamento verticale: Titolo sopra il contenitore controlli */
    flex-direction: column !important;
    gap: 4px;                           /* Spazio tra il titolo e il contenitore dei controlli */
    width: 100%;
}

/* Supporto per celle espanse (spanned) */
.grid-cell .grid-span-controls {
    display: grid !important;
    gap: 2px;
    width: 100%;
    height: 100% !important; /* Molto importante: deve occupare tutto lo spazio dello span */
}

/* Wrapper interno per le colonne dello span - deve propagare l'altezza per il 3x3 interno */
.span-col-wrapper {
    height: 100% !important;
    display: grid !important;
    grid-template: 1fr / 1fr !important; /* Forza sovrapposizione figli per supportare 3x3 diagonale */
}

/* Affinché i figli si sovrappongano realmente nella griglia 1x1, devono essere forzati nella grid-area 1/1 */
.span-col-wrapper > .mz-control-wrapper {
    grid-area: 1 / 1 !important;
}

/* Campo numerico locale: pulsanti separati dalle cifre, nessuna sovrapposizione. */
.mz-numeric-field { display: inline-flex; align-items: stretch; width: fit-content; max-width: 100%; pointer-events: auto; }
.mz-numeric-field > input.control-input-numeric-force { box-sizing: border-box !important; padding-left: 5px !important; padding-right: 5px !important; }
.mz-numeric-stepper { display: flex; flex-direction: column; width: 16px; flex: 0 0 16px; }
.mz-numeric-stepper button { flex: 1; padding: 0; min-height: 0; font-size: 7px; line-height: 1; color: #333; cursor: pointer; }
.mz-numeric-field:has(input:disabled) .mz-numeric-stepper { opacity: .45; pointer-events: none; }
.mz-numeric-field input[aria-invalid="true"] { outline: 1px solid #c0392b; }

/* Solo le righe esplicitamente in flusso: i figli non occupano tutta la colonna. */
.span-col-wrapper.mz-inline-row > .mz-control-wrapper {
    width: auto !important;
    flex: 0 0 auto;
    padding-right: 0 !important;
}

/* Solo i gruppi contentRows: la label inline non aggiunge una riga di spazio
   rispetto al checkbox nella colonna adiacente. Gli altri gruppi restano invariati. */
.mz-content-rows .mz-inline-row .mz-label-item {
    margin-bottom: 0;
}
.mz-content-rows .mz-inline-row .mz-label-text {
    line-height: 1;
}

/* === SISTEMA LAYOUT GRIGLIA - FINE === */


/* =====================================================
   CLASSI DI ALLINEAMENTO CONFIGURABILI - INIZIO
   ===================================================== */

/* Allineamento a sinistra per celle griglia */
.grid-cell.align-left {
    align-items: flex-start;           /* Allinea elementi a sinistra */
    text-align: left;                  /* Testo allineato a sinistra */
}

/* Allineamento centrale per celle griglia */
.grid-cell.align-center {
    align-items: center;               /* Centra elementi orizzontalmente */
    text-align: center;                /* Centra il testo */
}

/* Specifiche per elementi centrati */
.grid-cell.align-center .group-title {
    align-self: center;                /* Auto-centra il titolo del gruppo */
    text-align: center;                /* Centra il testo del titolo */
    width: 100%;                       /* Occupa tutta la larghezza */
}

.grid-cell.align-center .checkbox-label {
    align-self: center;                /* Centra checkbox */
    justify-content: center;           /* Centra contenuto checkbox */
}

.grid-cell.align-center .control-item-compact {
    align-self: center;                /* Centra controlli compatti */
}

/* Allineamento a destra per celle griglia */
.grid-cell.align-right {
    align-items: flex-end;             /* Allinea elementi a destra */
    text-align: right;                 /* Testo allineato a destra */
}

/* Stile per titoli di gruppo */
.group-title {
    font-size: 11px;                   /* Ridotto per coerenza con i tab principali */
    font-weight: 600;                  /* Peso font semibold */
    color: #3e6da9;                    /* Colore Blu jamovi */
    margin-bottom: 6px;                /* Spazio sotto il titolo ridotto */
    padding-bottom: 4px;               /* Padding per separazione visiva */
    border-bottom: 1px solid rgba(62, 109, 169, 0.50);  /* Linea separatrice */
    align-self: flex-start;            /* Allineamento di default a sinistra */
    width: auto;                       /* Larghezza automatica basata su contenuto */
}

/* === CLASSI DI ALLINEAMENTO CONFIGURABILI - FINE === */


/* =====================================================
   SISTEMA DI ALLINEAMENTO UNIFICATO (3x3) - INIZIO
   ===================================================== */

.mz-control-wrapper {
    display: flex !important;
    width: 100% !important;
    height: auto !important;           /* Permette agli elementi di impilarsi liberamente */
    flex-direction: column !important; /* Mantiene la coerenza del contenuto interno */
    box-sizing: border-box !important;
    padding-right: 4px !important;     /* Respiro laterale per prevenire lo schiacciamento delle label */
}

/* Allineamento Verticale (per flex-direction row di default) */
.mz-align-top-left, .mz-align-top-center, .mz-align-top-right { align-items: flex-start !important; }
.mz-align-center-left, .mz-align-center-center, .mz-align-center-right { align-items: center !important; }
.mz-align-bottom-left, .mz-align-bottom-center, .mz-align-bottom-right { align-items: flex-end !important; }

/* Allineamento Orizzontale */
.mz-align-top-left, .mz-align-center-left, .mz-align-bottom-left { justify-content: flex-start !important; }
.mz-align-top-center, .mz-align-center-center, .mz-align-bottom-center { justify-content: center !important; }
.mz-align-top-right, .mz-align-center-right, .mz-align-bottom-right { justify-content: flex-end !important; }

/* === SISTEMA DI ALLINEAMENTO UNIFICATO (3x3) - FINE === */


/* =====================================================
   ELEMENTI DI CONTROLLO - SPAZIATURA COMPATTA - INIZIO
   ===================================================== */

.control-item {
    margin-bottom: 16px;               /* Spazio tra controlli */
    display: flex;                     /* Layout flex per struttura */
    flex-direction: column;            /* Disposizione verticale */
    gap: 4px;                          /* Spazio tra label e input */
}

/* Spazio per stili consolidati - vedi riga 513 */

/* Indentazione per controlli sotto titoli di gruppo */
.grid-cell .control-item-compact {
    margin-left: 8px;                  /* Indentazione ridotta a 8px per mantenere i controlli compatti e vicini al bordo */
}


/* Regole mz-control-wrapper per allinearsi indipendentemente senza distorcere gli altri figli della stessa griglia flex */

/* In flex-direction: column (grid-cell default), align-self controlla orizzontale, margin controlla verticale se applicato auto */

/* Orizzontale via align-items (essendo il wrapper una colonna flex) */
.mz-control-wrapper.mz-align-top-left, 
.mz-control-wrapper.mz-align-center-left, 
.mz-control-wrapper.mz-align-bottom-left { 
    align-items: flex-start !important; 
}

.mz-control-wrapper.mz-align-top-center, 
.mz-control-wrapper.mz-align-center-center, 
.mz-control-wrapper.mz-align-bottom-center { 
    align-items: center !important; 
}

.mz-control-wrapper.mz-align-top-right, 
.mz-control-wrapper.mz-align-center-right, 
.mz-control-wrapper.mz-align-bottom-right { 
    align-items: flex-end !important; 
}

/* Verticale in una colonna - margin pushing 
   In una flex colonna (la nostra grid-cell e sotto-wrapper), margin-top: auto lo attacca al fondo, margin-bottom: auto in alto.
   Se margin-top/bottom è auto, l'elemento è spinto ai margini disponibili in verticale. 
*/
.mz-control-wrapper.mz-align-top-left, 
.mz-control-wrapper.mz-align-top-center, 
.mz-control-wrapper.mz-align-top-right { 
    margin-bottom: auto !important; 
}

.mz-control-wrapper.mz-align-center-left, 
.mz-control-wrapper.mz-align-center-center, 
.mz-control-wrapper.mz-align-center-right { 
    margin-top: auto !important; 
    margin-bottom: auto !important; 
}

.mz-control-wrapper.mz-align-bottom-left, 
.mz-control-wrapper.mz-align-bottom-center, 
.mz-control-wrapper.mz-align-bottom-right { 
    margin-top: auto !important; 
}


/* Mantiene titolo gruppo allineato senza indentazione */
.grid-cell .group-title {
    margin-left: 0px;                  /* Nessuna indentazione per titolo */
}

/* Etichetta per controlli */
.control-label {
    font-size: 12px;                   /* Font piccolo per etichette */
    font-weight: 500;                  /* Peso medio per leggibilità */
    color: #555;                       /* Colore grigio scuro */
    margin-bottom: 4px;                /* Spazio tra label e controllo */
}

/* Etichetta specifica per checkbox */
.checkbox-label {
    font-size: 12px;                   /* Dimensione font coerente */
    font-weight: 500;                  /* Peso medio */
    color: #555;                       /* Colore grigio scuro */
    display: flex;                     /* Layout flex per allineamento */
    align-items: center;               /* Centra verticalmente checkbox e testo */
    gap: 8px;                          /* Spazio tra checkbox e label */
    cursor: pointer;                   /* Cursore pointer per indicare cliccabilità */
    white-space: nowrap !important;    /* Previene il wrap del testo */
}

/* Configurazione checkbox a sinistra del testo */
.checkbox-label.checkbox-left {
    flex-direction: row;               /* Checkbox poi testo */
}

/* Configurazione checkbox a destra del testo */
.checkbox-label.checkbox-right {
    flex-direction: row;               /* Mantiene direzione riga */
    justify-content: flex-start;       /* Allinea all'inizio */
}

/* Stile checkbox custom vector-based per fedeltà colore */
.control-checkbox {
   appearance: none;
   -webkit-appearance: none;
   width: 15px !important;             /* Dimensione fissa */
   height: 15px !important;            /* Dimensione fissa */
   border: 1.5px solid #ccc;           /* Bordo di base */
   border-radius: 3px;                 /* Bordi arrotondati classici */
   background: #fff;                   /* Fondo bianco */
   margin: 0;                          /* Reset margini */
   cursor: pointer;                    /* Cursore pointer */
   flex-shrink: 0 !important;          /* Previene riduzione in flexbox */
   position: relative;
}

/* Stato Checked Checkbox */
.control-checkbox:checked {
   background-color: var(--mz-checkbox-color, #3e6da9); /* Riempi col custom color */
   border-color: var(--mz-checkbox-color, #3e6da9);
}

/* Disegno della "Spunta" bianca vettoriale */
.control-checkbox:checked::after {
   content: '';
   position: absolute;
   left: 4px;
   top: 1.5px;
   width: 4px;
   height: 8px;
   border: solid #fff;
   border-width: 0 1.5px 1.5px 0;
   transform: rotate(45deg);
}

/* Testo del checkbox base protetto da wrap */
.checkbox-text {
   flex-shrink: 0 !important;          /* Previene riduzione testo */
   white-space: nowrap !important;     /* Impedisce wrap testo */
}

/* Stile input generici */
.control-input {
    padding: 6px 8px;                  /* Padding interno per usabilità */
    border: 1px solid #ccc;            /* Bordo grigio standard */
    border-radius: 3px;                /* Angoli leggermente arrotondati */
    font-size: 12px;                   /* Font coerente con label */
    background: #fff;                  /* Sfondo bianco */
    width: 220px;                      /* Larghezza di default */
    max-width: 100%;                   /* Non eccede il contenitore */
    box-sizing: border-box;            /* Include padding e bordi */
}

/* Stato focus per input */
.control-input:focus {
    outline: none;                     /* Rimuove outline browser default */
    border-color: #3e6da9;             /* Bordo blu jamovi al focus */
    box-shadow: 0 0 0 2px rgba(62, 109, 169, 0.2); /* Alone blu per accessibilità */
}

/* Layout inline per controlli affiancati: label a sinistra, controllo a destra */
.control-inline {
    display: flex !important;          /* Forza layout flex */
    align-items: center !important;    /* Centra verticalmente */
    gap: 8px !important;               /* Spazio tra etichetta e controllo */
    flex-direction: row !important;    /* Disposizione orizzontale */
    flex-wrap: nowrap !important;      /* IMPEDISCE il wrap per mantenere label a sinistra */
    position: relative;                /* Per posizionamento slider se necessario (ora in flow) */
}

.control-label-inline {
    font-size: 12px;
    font-weight: 500;
    color: #555;
    white-space: nowrap !important;
    flex-shrink: 0 !important;
    margin: 0 !important;
    margin-right: 8px !important;
}

/* Label inline con larghezza fissa */
.control-label-inline {
    margin: 0 !important;              /* Reset margini */
    white-space: nowrap !important;    /* Previene wrap testo */
    margin-right: 8px !important;      /* Spazio fisso tra label e controllo per eliminare gap variabile */
    flex-shrink: 0 !important;         /* Mantiene dimensione fissa */
}

/* Input inline con larghezza ridotta */
.control-input-inline {
    width: 120px !important;           /* Larghezza fissa per layout inline */
    max-width: 120px !important;       /* Previene espansione */
    flex-shrink: 0 !important;         /* Mantiene dimensione */
}

/* Gruppo verticale per input+slider+hint nel layout inline */
.control-input-group {
    display: flex !important;
    flex-direction: column !important;
    gap: 4px !important;
    flex-grow: 1;
}

/* Hint inline piccolo */
.control-hint-inline {
    font-size: 10px !important;        /* Font molto piccolo */
    color: #888 !important;            /* Colore grigio chiaro */
    flex-shrink: 0 !important;         /* Mantiene dimensione */
    margin-left: 3px !important;       /* Piccolo margine sinistro */
}

/* Hint range per layout verticale (top) */
.control-item:not(.control-inline) .control-hint-range {
    font-size: 9px;                    /* Font molto piccolo per discrezione */
    color: #888;                       /* Colore grigio chiaro */
    font-style: italic;                /* Corsivo per differenziazione */
    margin-top: 0px;                   /* Nessun margine superiore */
    text-align: left;                  /* Allineamento a sinistra */
    display: block;                    /* Display blocco */
    width: auto;                       /* Larghezza automatica */
    margin-left: 0;                    /* Nessun margine sinistro */
}

/* Hint range per layout inline - in flow */
.control-inline .control-hint-range {
    display: block;
    font-size: 9px;                    /* Font molto piccolo */
    color: #888;                       /* Colore grigio chiaro */
    font-style: italic;                /* Stile corsivo */
    margin-top: 2px;
    margin-left: 8px;
}

/* Hint generico per suggerimenti */
.control-hint {
    font-size: 9px;                    /* Font piccolo per hint */
    color: #888;                       /* Colore grigio chiaro */
    font-style: italic;                /* Corsivo per distinzione */
    margin-top: 2px;                   /* Piccolo margine superiore */
}

/* Hint range generico */
.control-hint-range {
    font-size: 9px;                    /* Font molto piccolo */
    color: #888;                       /* Colore grigio chiaro */
    font-style: italic;                /* Stile corsivo */
    margin-top: 2px;                   /* Margine superiore */
    text-align: center;                /* Centra il testo */
    display: block;                    /* Display blocco */
    width: 100%;                       /* Larghezza piena */
    clear: both;                       /* Clear eventuali float */
    flex-basis: 100%;                  /* Forza nuova riga in flex */
    order: 999;                        /* Posiziona alla fine nel flex */
}

/* === ELEMENTI DI CONTROLLO - SPAZIATURA COMPATTA - FINE === */
 
 
/* =====================================================
   CONTROLLI OTTIMIZZATI PER TIPO - INIZIO
   ===================================================== */

/* Input numerici - dimensioni compatte per numeri */
.control-input[type="number"],
.control-input.control-input-numeric-force {
    width: 80px !important;            /* Larghezza ottimale per numeri */
}

.control-input-inline[type="number"],
.control-input-inline.control-input-numeric-force {
    width: 70px !important;            /* Ancora più compatto per inline */
}

/* Input numerici specifici per plotBlock - extra compatti */
.plot-block .control-input[type="number"],
.plot-block .control-input.control-input-numeric-force {
    width: 60px !important;            /* Larghezza ottimizzata per plotBlock */
    padding: 3px 6px !important;
    font-size: 11px !important;        /* Font compatto */
    line-height: normal !important;
}

/* Gruppo Atomico: Input + Slider (sempre sotto) */
.input-slider-group {
    display: flex !important;
    flex-direction: column !important;
    gap: 6px !important;              /* Spazio aumentato per evitare sovrapposizioni thumb */
    width: 60px !important;           /* Coincide con width dell'input */
    align-items: center;
}

.input-slider-group .slider-container {
    width: 100% !important;           /* Occupa tutta la larghezza dell'input */
    margin: 0 !important;
    padding: 0 !important;
}

.input-slider-group .control-slider {
    width: 100% !important;           /* Slider largo quanto l'input */
    margin: 0 !important;
}

/* Container per dimensioni con label a sinistra */
.plot-dim-item-left {
    display: grid !important;
    grid-template-columns: auto auto !important;
    grid-template-rows: auto auto !important;
    align-items: center !important;     /* Centra perfettamente label e input box */
    gap: 4px 8px !important;            /* 4px tra input e slider, 8px dopo la label */
    margin-bottom: 8px !important;
}

.plot-dim-item-left .control-label-inline {
    grid-column: 1 !important;
    grid-row: 1 !important;
    margin: 0 !important;
}

.plot-dim-item-left .control-input {
    grid-column: 2 !important;
    grid-row: 1 !important;
}

.plot-dim-item-left .plot-dim-extra-area {
    grid-column: 2 !important;
    grid-row: 2 !important;
    display: flex !important;
    flex-direction: column !important;
    align-items: center !important;
    width: 60px !important;            /* Forza la larghezza dell'input box per centrare lo slider */
}

.plot-dim-item-left .plot-dim-extra-area .slider-container {
    width: 100% !important;
    margin: 0 !important;
}

/* Container per dimensioni con label sopra */
.plot-dim-item-top {
    display: flex !important;
    flex-direction: column !important;
    align-items: flex-start !important;
    gap: 2px !important;
}

/* Layout orizzontale per dimensioni plot */
.plot-dimensions-horizontal {
    display: flex !important;          /* Forza layout flex */
    flex-direction: row !important;    /* Disposizione orizzontale */
    gap: 20px !important;              /* Ampio respiro tra W e H */
    flex-wrap: nowrap !important;      /* Impedisce wrap */
    margin-bottom: 12px !important;    /* Spazio verso il basso per evitare contatti con select/slider */
}

/* Layout verticale per dimensioni plot */
.plot-dimensions-vertical {
    display: flex !important;          /* Layout flex */
    flex-direction: column !important; /* Disposizione verticale */
    gap: 12px !important;              /* Spazio per non schiacciare le righe */
    margin-bottom: 12px !important;
}

/* Input di testo - larghezza appropriata per stringhe */
.control-input[type="text"] {
    width: 180px !important;           /* Larghezza standard per testo */
}

.control-input-inline[type="text"] {
    width: 130px !important;           /* Ridotto per layout inline */
}

/* Select (dropdown) - larghezza media */
.control-select {
    padding: 6px 8px;                  /* Padding interno coerente */
    border: 1px solid #ccc;            /* Bordo standard */
    border-radius: 3px;                /* Angoli arrotondati */
    font-size: 12px;                   /* Font coerente */
    background: #fff;                  /* Sfondo bianco */
    width: 160px !important;           /* Larghezza ottimale per dropdown */
    max-width: 100%;                   /* Non eccede contenitore */
    box-sizing: border-box;            /* Include padding e bordi */
    cursor: pointer;                   /* Cursore pointer per interattività */
}

.control-select-inline {
    width: 160px !important;           /* Leggermente ridotto per inline */
}

/* Classi utility per dimensioni personalizzate */
.control-input-small {
    width: 50px !important;            /* Input molto piccolo */
}

.control-input-wide {
    width: 220px !important;           /* Input largo per testi lunghi */
}

/* Stato focus per select */
.control-select:focus {
    outline: none;                     /* Rimuove outline default */
    border-color: #3e6da9;             /* Bordo blu jamovi */
    box-shadow: 0 0 0 2px rgba(62, 109, 169, 0.2); /* Alone blu */
}

/* Stile checkbox personalizzato */
.control-checkbox {
    width: 16px !important;            /* Dimensione fissa checkbox */
    height: 16px !important;           /* Altezza fissa checkbox */
    accent-color: #3e6da9;             /* Colore accent blu jamovi (browser moderni) */
    margin: 0;                         /* Reset margini */
    cursor: pointer;                   /* Cursore pointer */
    flex-shrink: 0;                    /* Mantiene dimensione fissa */
    min-width: 16px;                   /* Larghezza minima garantita */
    min-height: 16px;                  /* Altezza minima garantita */
}

/* === CONTROLLI OTTIMIZZATI PER TIPO - FINE === */


/* =====================================================
   CONTROLLI SLIDER - INIZIO
   ===================================================== */

/* Contenitore per slider */
.slider-container {
    margin-top: 2px;                   /* Piccolo spazio sopra */
    width: auto;                       /* Larghezza automatica */
    display: flex;                     /* Layout flessibile */
    align-items: center;               /* Centra verticalmente */
}

/* Stile base slider */
.control-slider {
    width: 50px !important;            /* Larghezza fissa slider */
    height: 3px !important;            /* Track sottile per eleganza */
    border-radius: 2px !important;     /* Angoli arrotondati track */
    background: #3e6da9 !important;    /* Colore blu jamovi */
    outline: none !important;          /* Rimuove outline */
    -webkit-appearance: none !important; /* Reset stile WebKit */
    appearance: none !important;       /* Reset stile standard */
    cursor: pointer !important;        /* Cursore pointer */
}

/* Posizionamento slider per layout inline (label a sinistra) - in flow */
.control-inline .slider-container {
    margin-left: 8px;                  /* Spazio tra input e slider */
    width: 60px;                       /* Larghezza fissa */
    display: flex;
    align-items: center;
}

.control-inline .slider-container .control-slider {
    width: 100% !important;            /* Occupa il container */
}

/* Posizionamento slider per layout verticale (label sopra) */
.control-item .slider-container {
    width: 60px;                       /* Coerente con plotBlock */
    margin-left: 0;                    /* Allineato a sinistra */
    margin-top: 2px;                   /* Piccolo spazio sopra */
}

/* Track dello slider - WebKit (Chrome, Safari) */
.control-slider::-webkit-slider-track {
    width: 100%;                       /* Occupa tutta la larghezza */
    height: 3px;                       /* Altezza track */
    cursor: pointer;                   /* Cursore pointer */
    background: #3e6da9;               /* Colore blu jamovi */
    border-radius: 2px;                /* Angoli arrotondati */
}

/* Track dello slider - Mozilla Firefox */
.control-slider::-moz-range-track {
    width: 100%;                       /* Occupa tutta la larghezza */
    height: 3px;                       /* Altezza track */
    cursor: pointer;                   /* Cursore pointer */
    background: #3e6da9;               /* Colore blu jamovi */
    border-radius: 2px;                /* Angoli arrotondati */
    border: none;                      /* Rimuove bordo default Firefox */
}

/* Thumb (maniglia) dello slider - WebKit */
.control-slider::-webkit-slider-thumb {
    height: 10px;                      /* Altezza maniglia */
    width: 10px;                       /* Larghezza maniglia */
    border-radius: 50%;                /* Forma circolare */
    background: #fff;                  /* Sfondo bianco */
    cursor: pointer;                   /* Cursore pointer */
    -webkit-appearance: none;          /* Reset stile default */
    border: 2px solid #3e6da9;         /* Bordo blu jamovi */
    box-shadow: 0 1px 2px rgba(0,0,0,0.15); /* Ombra leggera per profondità */
}

/* Thumb (maniglia) dello slider - Firefox */
.control-slider::-moz-range-thumb {
    height: 10px;                      /* Altezza maniglia */
    width: 10px;                       /* Larghezza maniglia */
    border-radius: 50%;                /* Forma circolare */
    background: #fff;                  /* Sfondo bianco */
    cursor: pointer;                   /* Cursore pointer */
    border: 2px solid #3e6da9;         /* Bordo blu jamovi */
    box-shadow: 0 1px 2px rgba(0,0,0,0.15); /* Ombra leggera */
}

/* Stati focus per accessibilità */
.control-slider:focus {
    outline: none;                     /* Rimuove outline default */
}

.control-slider:focus::-webkit-slider-thumb {
    box-shadow: 0 0 0 2px rgba(62, 109, 169, 0.3); /* Alone blu al focus */
}

.control-slider:focus::-moz-range-thumb {
    box-shadow: 0 0 0 2px rgba(62, 109, 169, 0.3); /* Alone blu al focus */
}

/* Nasconde spin buttons per Chrome, Safari, Edge, Opera */
.control-no-spin::-webkit-outer-spin-button,
.control-no-spin::-webkit-inner-spin-button {
    -webkit-appearance: none !important;
    margin: 0 !important;
}

/* Nasconde spin buttons per Firefox */
.control-no-spin[type=number] {
    -moz-appearance: textfield !important;
}

/* Mantiene stili per spin buttons quando richiesti (inputMode: "spin") */
.control-input[type="number"]:not(.control-no-spin):not(.control-slider-enabled)::-webkit-inner-spin-button,
.control-input[type="number"]:not(.control-no-spin):not(.control-slider-enabled)::-webkit-outer-spin-button {
    opacity: 1 !important;
    width: 25px !important;
    height: auto !important;
    cursor: pointer !important;
}

.control-input[type="number"]:not(.control-no-spin):not(.control-slider-enabled) {
    padding-right: 5px !important;
    text-align: left !important;
}

/* === CONTROLLI SLIDER - FINE === */


/* =====================================================
   STILI BLOCCO PLOT - INIZIO
   ===================================================== */

/* Contenitore principale per controlli plot */
.plot-block {
    //border: 1px solid #e0e0e0;       /* Bordo opzionale commentato */
    border-radius: 5px;                /* Angoli arrotondati */
    padding: 0 8px 8px 8px;            /* Rimosso padding-top per evitare disallineamenti con accordion */
    margin-bottom: 8px;                /* Spazio sotto il blocco */
    margin-top: 0;                     /* Forza zero margine superiore */
}

/* Area elementi interni al plot */
.plot-elements-area {
    border: none;                      /* Nessun bordo */
    border-radius: 3px;                /* Angoli leggermente arrotondati */
    padding: 0;                        /* Rimosso padding per allineamento esatto al contenitore superiore */
    display: flex;
    flex-direction: column;
    gap: 6px;                          /* Leggermente ridotto (era 8px) per layout compatto */
}

/* Modalità ALL-HORIZONTAL: Forza tutto su un'unica riga */
.plot-elements-area.all-horizontal {
    flex-direction: row !important;
    align-items: center !important;
    gap: 20px !important;              /* Spazio ampio tra Toggle, W e H */
    flex-wrap: nowrap !important;
}

.plot-elements-area.all-horizontal .control-item-compact,
.plot-elements-area.all-horizontal .plot-dim-item-left {
    margin-bottom: 0 !important;      /* Rimuove margini per allineamento orizzontale perfetto */
}

.plot-elements-area.all-horizontal .plot-dim-item-left {
    gap: 0 8px !important;             /* Gap ridotto per stare in linea */
}

/* Rimosso blocco duplicato a fine sezione (già rimosso o integrato sopra) */

/* Layout orizzontale per dimensioni plot */
.plot-dimensions-horizontal {
    display: flex !important;          /* Forza layout flex */
    flex-direction: row !important;    /* Disposizione orizzontale */
    gap: 4px;                          /* Spazio tra width e height */
    flex-wrap: nowrap !important;      /* Impedisce wrap */
}

.plot-dimensions-horizontal .control-item {
    flex: 1;                           /* Crescita uniforme */
    min-width: 140px;                  /* Larghezza minima */
}

.plot-dimensions-horizontal .control-item:not(.control-inline) {
    min-width: 80px !important;        /* Larghezza minima ridotta */
    flex: 0 0 auto !important;         /* Dimensione fissa */
}

/* 
//Allineamento hint per dimensioni plot 
.plot-dimensions-horizontal .control-hint-range,
.plot-dimensions-vertical .control-hint-range {
    margin-left: 72px;                 /* Allineamento con input */
    width: 70px;                       /* Larghezza fissa */
    text-align: left !important;       /* Allineamento testo */
}
*/

/* Allineamento hint per horizontal - nessun margine */
.plot-dimensions-horizontal .control-hint-range {
    margin-left: 0;                    /* Nessun margine sinistro */
    width: auto;                       /* Larghezza automatica */
    text-align: left !important;       /* Allineamento testo */
}

/* Allineamento hint per vertical - nessun margine */
.plot-dimensions-vertical .control-hint-range {
    margin-left: 0;                    /* Nessun margine sinistro */
    width: auto;                       /* Larghezza automatica */  
    text-align: left !important;       /* Allineamento testo */
}

/* Fix allineamento etichette in horizontal */
.plot-dimensions-horizontal .control-label {
    text-align: left !important;       /* Allinea etichette a sinistra */
}

/* Fix FORTE per Range in vertical - terza riga allineata a sinistra */
.plot-block .plot-dimensions-vertical .control-item .control-hint-range {
    display: block !important;         /* Forza display blocco */
    position: relative !important;     /* Reset posizione */
    margin: 2px 0 0 0 !important;      /* Solo margine top, ZERO a sinistra */
    padding-left: 0 !important;        /* Nessun padding sinistro */
    width: 100% !important;            /* Larghezza piena */
    text-align: left !important;       /* Testo allineato a sinistra */
    left: 0 !important;                /* Posizione da sinistra */
    right: auto !important;            /* Reset right */
}

/* Layout verticale per dimensioni plot */
.plot-dimensions-vertical {
    display: flex;                     /* Layout flex */
    flex-direction: column;            /* Disposizione verticale */
    gap: 4px;                          /* Spazio tra elementi */
}

/* Stili specifici per controlli dentro plot-block */
.plot-block .control-item-compact {
    margin: 0 0 12px 0 !important;     /* Margini personalizzati */
    padding: 0 !important;             /* Rimuove padding */
    text-align: left !important;       /* Forza allineamento a sinistra */
}

.plot-block .checkbox-label {
    margin: 0 !important;              /* Reset margini */
    padding: 0 !important;             /* Reset padding */
    width: 100% !important;            /* Larghezza piena */
    display: flex !important;          /* RIPRISTINO FLEX per allineamento verticale */
    align-items: center !important;    /* Centra il testo rispetto al quadrato della checkbox */
}

.plot-block .control-checkbox {
    margin-left: 0 !important;         /* Nessun margine sinistro */
    margin-right: 0 !important;        /* Nessun margine destro */
}

.plot-block .checkbox-label.checkbox-right {
    margin-right: 0 !important;              /* Reset margine destro */
    justify-content: flex-start !important;  /* Allinea all'inizio */
}

/* === STILI BLOCCO PLOT - FINE === */


/* =====================================================
   STILI RADIO BUTTON - INIZIO
   ===================================================== */

/* Etichetta gruppo radio */
.radio-group-label {
    font-size: 12px;                   /* Font coerente con altri controlli */
    font-weight: 500;                  /* Peso medio */
    color: #555;                       /* Colore grigio scuro */
    margin-bottom: 6px;                /* Spazio sotto l'etichetta */
}

/* Layout inline: label a sinistra + radio orizzontali sulla stessa riga */
/* Attivato da groupLabelPosition: "left" + layout: "horizontal" */
.radio-group-inline-left {
    display: flex;                     /* Flex row: label | radio buttons */
    flex-direction: row;
    align-items: center;               /* Allineamento verticale centrato */
    gap: 8px;                          /* Spazio tra label e gruppo radio */
    flex-wrap: nowrap;
}

/* Label inline (versione senza margin-bottom perché è sulla stessa riga) */
.radio-group-label-inline {
    font-size: 12px;
    font-weight: 500;
    color: #555;
    white-space: nowrap;               /* Impedisce wrap del testo etichetta */
    flex-shrink: 0;                    /* Non si restringe */
}

/* Layout verticale per gruppo radio */
.radio-group-vertical {
    display: flex;                     /* Layout flex */
    flex-direction: column;            /* Disposizione verticale */
    gap: 4px;                          /* Spazio tra opzioni */
}

/* Layout orizzontale per gruppo radio */
.radio-group-horizontal {
    display: flex;                     /* Layout flex */
    flex-direction: row;               /* Disposizione orizzontale */
    gap: 10px;                         /* Spazio maggiore tra opzioni */
    flex-wrap: wrap;                   /* Permette wrap su più righe */
}

/* Etichetta singolo radio button */
.radio-label {
   font-size: 12px;                    /* Font coerente */
   font-weight: 500;                   /* Peso medio */
   color: #555;                        /* Colore grigio scuro */
   display: flex;                      /* Layout flex */
   align-items: center;                /* Centra verticalmente */
   gap: 6px;                           /* Spazio tra radio e testo */
   cursor: pointer;                    /* Cursore pointer per cliccabilità */
   padding: 2px 0;                     /* Padding verticale minimo */
}

/* Radio button a sinistra del testo */
.radio-label.radio-left {
   flex-direction: row-reverse;        /* Inverte ordine: testo poi radio */
   justify-content: flex-end;          /* Allinea alla fine */
}

/* Radio button a destra del testo */
.radio-label.radio-right {
   flex-direction: row;                /* Ordine normale: radio poi testo */
   justify-content: flex-start;        /* Allinea all'inizio */
}

/* Stile radio button custom per evitare problemi di accent-color nativo */
.control-radio {
   appearance: none;
   -webkit-appearance: none;
   width: 15px !important;
   height: 15px !important;
   border: 1.5px solid #ccc;
   border-radius: 50%;
   background: #fff;
   margin: 0;
   cursor: pointer;
   flex-shrink: 0;
   position: relative;
}

/* Stato attivo del radio */
.control-radio:checked {
   border-color: var(--mz-radio-color, #3e6da9);
}

/* Pallino interno del radio */
.control-radio:checked::after {
   content: '';
   position: absolute;
   top: 50%;
   left: 50%;
   transform: translate(-50%, -50%);
   width: 7px;
   height: 7px;
   background: var(--mz-radio-color, #3e6da9);
   border-radius: 50%;
}


/* Testo radio button */
.radio-text {
   flex-shrink: 0;                     /* Previene riduzione testo */
   white-space: nowrap;                /* Impedisce wrap testo */
}

/* Effetto hover commentato per design più pulito */
/* .radio-label:hover {
   background-color: rgba(62, 109, 169, 0.05);
   border-radius: 3px;
   padding: 2px 4px;
} */

/* Focus per accessibilità outline quadratino blu*/
.control-radio:focus {
   /* commento le due righe originali */
   //outline: 2px solid #3e6da9;         /* Outline blu per focus */
   //outline-offset: 2px;                /* Offset per visibilità */
   outline: none;
   box-shadow: none;
}

/* === STILI RADIO BUTTON - FINE === */


/* =====================================================
   STILI TOGGLE SWITCH (Orizzontale) - INIZIO
   ===================================================== */

.toggle-switch {
  position: relative;
  display: inline-block;
  width: 26px;                  /* LARGHEZZA diminuire/aumentare */
  height: 14px;                 /* ALTEZZA diminuire/aumentare */
  margin: 0;
}

/* Nasconde checkbox reale */
.toggle-switch input[type="checkbox"] {
  position: absolute;
  opacity: 0;
  width: 0;
  height: 0;
}

/* Slider */
.toggle-slider {
  position: absolute;
  inset: 0;
  cursor: pointer;
  background-color: #ccc;
  transition: 0.25s;
  border-radius: 14px;
}

/* Pallino */
.toggle-slider:before {
  content: "";
  position: absolute;
  width: 10px;                  /* Larghezza Pallino aumentare/diminuire */
  height: 10px;                 /* Altezza Pallino aumentare/diminuire */
  left: 2px;
  bottom: 2px;
  background-color: #fff;
  border-radius: 50%;
  box-shadow: 0 1px 3px rgba(0,0,0,0.2);
  transition: 0.25s;
}

/* Stato ON */
.toggle-switch input:checked + .toggle-slider {
  background-color: var(--mz-toggle-color, #3e6da9);    /* Colore precedente #2196F3*/
}

.toggle-switch input:checked + .toggle-slider:before {
  transform: translateX(12px);  /* Adattare allo spostamento */
}

/* Focus - focus ring più moderno e accessibile */
.toggle-switch input:focus-visible + .toggle-slider {
  box-shadow: 0 0 0 3px rgba(62, 109, 169, 0.25);
  outline: none;
}

/* Hover effect leggero per feedback utente */
.toggle-switch:hover .toggle-slider {
  filter: brightness(0.95);
}

/* Disabled */
.toggle-switch input:disabled + .toggle-slider {
  opacity: 0.4;
  cursor: not-allowed;
}

/* Label associata */
.toggle-label {
  display: inline-flex;         /* display: flex; */
  align-items: center;          /* centratura verticale switch-testo */
  justify-content: flex-start;  /* allineati a sinistra */
  cursor: pointer;
  user-select: none;
  gap: 6px;                     /* Gap tra switch e testo */
}

.toggle-label.toggle-left {
  flex-direction: row;
}

.toggle-label.toggle-right {
  flex-direction: row;
}

.toggle-label.toggle-top,
.toggle-label.toggle-bottom {
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
}

.toggle-text {
  font-size: 12px;                /* per il Testo */
  color: #333;
  white-space: nowrap !important; /* Previene il wrap del testo */
}

/* === STILI TOGGLE SWITCH (Orizzontale) - FINE === */


/* =====================================================
   STILI TOGGLE SWITCH (Verticale) - INIZIO
   ===================================================== */

/* Contenitore switch */
.toggle-switch-vertical {
  position: relative;
  display: inline-block;
  width: 14px;
  height: 26px;
  margin: 0;
}

/* Nasconde checkbox */
.toggle-switch-vertical input[type="checkbox"] {
  position: absolute;
  opacity: 0;
  width: 0;
  height: 0;
}

/* Slider verticale */
.toggle-slider-vertical {
  position: absolute;
  inset: 0;
  cursor: pointer;
  background-color: #ccc;
  transition: 0.25s;
  border-radius: 14px;
}

/* Pallino interno verticale */
.toggle-slider-vertical:before {
  content: "";
  position: absolute;
  width: 10px;
  height: 10px;
  left: 2px;
  bottom: 2px;
  background-color: #fff;
  border-radius: 50%;
  box-shadow: 0 1px 3px rgba(0,0,0,0.2);
  transition: 0.15s;
}

/* Stato attivo - pallino si sposta in alto */
.toggle-switch-vertical input:checked + .toggle-slider-vertical {
  background-color: var(--mz-toggle-color, #3e6da9);    /* Colore precedente #2196F3*/
}

.toggle-switch-vertical input:checked + .toggle-slider-vertical:before {
  transform: translateY(-12px);
}

/* Focus - focus ring più moderno e accessibile */
.toggle-switch-vertical input:focus-visible + .toggle-slider-vertical {
  box-shadow: 0 0 0 3px rgba(62, 109, 169, 0.25);
  outline: none;
}

/* Hover effect leggero per feedback utente */
.toggle-switch-vertical:hover .toggle-slider-vertical {
  filter: brightness(0.95);
}

/* Stato disabilitato verticale */
.toggle-switch-vertical input:disabled + .toggle-slider-vertical {
  opacity: 0.4;
  cursor: not-allowed;
}


/* Label per toggle verticale */
.toggle-label-vertical {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
  user-select: none;
}

.toggle-label-vertical.toggle-left {
  flex-direction: row;
}

.toggle-label-vertical.toggle-right {
  flex-direction: row;
}

.toggle-label-vertical.toggle-top,
.toggle-label-vertical.toggle-bottom {
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
}

/* Animazione smooth */
.toggle-switch-vertical,
.toggle-label-vertical {
  transition: opacity 0.25s ease;
}

/* === STILI TOGGLE SWITCH (Verticale) - FINE === */


/* =====================================================
   STILE CONTROLLI DISABILITATI - INIZIO
   ===================================================== */

/* Controlli base disabilitati (input, select) */
.control-input:disabled,
.control-select:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

/* Checkbox disabilitati - mantieni visibilità */
.control-checkbox:disabled {
  cursor: not-allowed;
}

/* Toggle switch: lo slider interno rimane visibile */
.toggle-switch input:disabled + .toggle-slider {
  opacity: 1;
  cursor: not-allowed;
}

/* Toggle switch: il container diventa semitrasparente */
.toggle-switch:has(input:disabled) {
  opacity: 0.4;
  cursor: not-allowed;
}

/* Label per checkbox tradizionali disabilitati */
.checkbox-label:has(input:disabled) {
  opacity: 0.5;
  cursor: not-allowed;
}

/* Label per toggle switch disabilitati */
.toggle-label:has(input:disabled) {
  opacity: 0.6;
  cursor: not-allowed;
}

/* Animazione smooth per cambio stato */
.toggle-switch,
.checkbox-label,
.toggle-label {
  transition: opacity 0.2s ease;
}

/* === STILE CONTROLLI DISABILITATI - FINE === */


/* =====================================================
   GEOMETRIA CONDIVISA HEADER GRUPPI (Standard, Accordion, Tab)
   Centralizza le dimensioni verticali: modificando questi valori
   si cambiano le proporzioni di tutti gli header garantendo che 
   rimangano sempre perfettamente allineati in orizzontale.
   ===================================================== */
.group-title,
.group-title-collapsible,
.mz-subtab-headers,
.info-notes-header {
  min-height: 28px;                  /* Altezza fissa sincronizzata per tutti */
  margin-top: 0 !important;          /* Forza il reset di eventuali margini superiori globali di jamovi */
  margin-bottom: 4px;                /* Spazio verso il contenuto sottostante */
  box-sizing: border-box;            /* Calcolo dimensioni invariante */
}

/* =====================================================
   STANDARD GROUPS - TITOLI NORMALI - INIZIO
   ===================================================== */
.group-title {
  font-size: 12px;                   /* Coerenza con accordion e tabs */
  font-weight: 500;                  
  color: #3e6da9;                    /* Blu jamovi */
  padding-bottom: 4px;               /* Padding per separazione */
  border-bottom: 1px solid rgba(62, 109, 169, 0.50); /* Linea uguale ad accordion chiuso */
  align-self: flex-start;            
  width: 100%;                       /* Occupa tutta la larghezza */
  display: flex;                     
  align-items: flex-end;             /* Allinea al fondo (baseline comune) */
}
/* === STANDARD GROUPS - FINE === */


/* =====================================================
   ACCORDION GROUPS E GROUP BADGES - INIZIO
   ===================================================== */

/* Wrapper generale (100% della cella/span) per supportare badge scorporati dal width originale */
.accordion-header-wrapper {
  position: relative;
  width: 100%;
  display: flex;
  align-items: center;               /* Centra verticalmente il badge rispetto al titolo */
}

.group-title-badges {
  pointer-events: auto;              /* Riattiva click sui badge */
  z-index: 10;
  display: flex;
  align-items: center;               /* Centra il badge al proprio interno */
}

.group-title-badges.align-right {
  position: absolute;
  right: 0;
  top: 50%;
  transform: translateY(-50%);
  padding-bottom: 4px;               /* Compensa il padding-bottom del titolo per allineamento alla linea */
}

.group-title-badges.align-center {
  position: absolute;
  left: 50%;
  top: 50%;
  transform: translate(-50%, -50%);
  padding-bottom: 4px;
}

/* Pulizia margini standard per l'HTML del label sciolto iniettato */
.group-title-badges .control-item,
.group-title-badges .mz-label-item {
  margin-bottom: 0 !important;
  padding: 0 !important;
}

/* Forza centraggio testo nel badge quando injettato nel titolo del gruppo */
.group-title-badges .mz-label-text.variant-badge {
  display: inline-flex !important;
  align-items: center !important;
  justify-content: center !important;
  line-height: 1 !important;
  vertical-align: middle !important;
}

/* Header dell'accordion - identico a group-title + freccia */
.group-title-collapsible {
  font-size: 12px;                   /* Ridotto per coerenza con i tab principali */
  font-weight: 500;                  /* Normale/Medium quando collassato */
  color: #3e6da9;                    /* Blu jamovi */
  padding-bottom: 4px;               /* Padding per separazione */
  border-bottom: 1px solid rgba(62, 109, 169, 0.50); /* Linea blu normale quando collassato */
  align-self: flex-start;            /* Allineamento a sinistra */
  width: auto;                       /* Larghezza automatica */
  cursor: pointer;                   /* Indica che è cliccabile */
  user-select: none;                 /* Previene selezione testo */
  display: flex;                     /* Flex per allineare freccia + testo */
  align-items: center;               /* Allineamento verticale centrato */
  gap: 6px;                          /* Spazio tra freccia e testo */
  transition: all 0.3s ease;         /* Animazione smooth */
}

/* Stato espanso - linea più spessa e si allunga + GRASSETTO */
.group-title-collapsible.expanded {
  font-weight: 700;                  /* Grassetto solo quando espanso */
  border-bottom: 2px solid rgba(62, 109, 169, 0.70); /* Linea più spessa (+1px) e più opaca */
  width: 100%;                       /* Si estende per tutta la larghezza disponibile */
}

/* Hover discreto - solo leggero cambio opacità */
.group-title-collapsible:hover {
  color: #1565c0;                    /* Blu leggermente più scuro */
}

/* Icona freccia inline - piccola e discreta */
.accordion-icon {
  width: 10px;                       /* Piccola */
  height: 10px;                      /* Piccola */
  flex-shrink: 0;                    /* Non si ridimensiona */
  transition: transform 0.3s ease;   /* Animazione rotazione smooth */
  color: #3e6da9;                    /* Stesso blu del testo */
}

/* Rotazione freccia quando espanso (► diventa ▼) */
.accordion-icon.expanded {
  transform: rotate(90deg);          /* Ruota 90° */
}

/* Contenitore del contenuto collassabile */
.accordion-content {
  max-height: 0;                        /* Inizialmente collassato (altezza 0) */
  overflow: hidden;                     /* Nasconde contenuto overflow */
  transition: max-height 0.3s ease-out; /* Animazione chiusura smooth */
}

/* Stato espanso del contenuto */
.accordion-content.expanded {
  max-height: 2000px;                   /* Altezza massima generosa per contenuto */
  transition: max-height 0.4s ease-in;  /* Animazione apertura leggermente più lenta */
}

/* Padding interno del contenuto */
.accordion-content-inner {
  padding: 4px 0;                    /* Ridotto per avvicinare il contenuto al titolo */
}

/* Le etichette assolute del deviatore non contribuiscono al min-content.
   Plot Settings deve usare tutta la colonna, evitando il taglio di Interactive. */
#forest-tab .grid-cell[data-row="0"][data-col="2"] > .accordion-content {
  width: 100%;
}

/* === ACCORDION GROUPS - GRUPPI COLLASSABILI - FINE === */

/* =====================================================
   AREA INFO & NOTE COLLASSABILE - INIZIO
   ===================================================== */

/* Contenitore principale info & notes */
.info-notes-container {
    background: transparent;           /* Sfondo trasparente */
    margin-top: 8px;                   /* Spazio sopra */
}

/* Header cliccabile per espandere/collassare */
.info-notes-header {
    font-size: 12px;                   /* Dimensione font come gli altri accordion */
    font-weight: 500;                  /* Medium */
    border: none;                      /* Rimosso bordo */
    background-color: transparent;     /* Sfondo trasparente */
    padding: 4px 0;                    /* Padding equilibrato */
    vertical-align: middle;            /* Allineamento verticale */
    color: #3e6da9;                    /* Colore testo blu jamovi */
    width: 100%;                       /* Larghezza 100% per permettere l'espansione della linea */
    text-align: left;                  /* Testo allineato a sinistra */
    cursor: pointer;                   /* Cursore pointer per indicare cliccabilità */
    display: flex;                     /* Layout flex per contenuto */
    align-items: center;               /* Centra verticalmente elementi */
    transition: all 0.3s ease;         /* Transizione smooth per hover e stato espanso */
    border-radius: 0;                  /* Nessun arrotondamento (stile jamovi) */
}

/* Effetto hover sull'header */
.info-notes-header:hover {
    color: #1565c0;                    /* Blu più scuro al hover */
    background-color: transparent;
}

/* Stato espanso dell'header */
.info-notes-header.expanded {
    font-weight: 700;                  /* Grassetto */
    width: 100%;                       /* Espande a tutta la larghezza contenitore */
}

/* Contenitore per l'icona +/- */
.icon-container {
    width: 18px;                       /* Larghezza fissa */
    height: 18px;                      /* Altezza ridotta per centrature ottimali */
    display: flex;                     /* Layout flex */
    align-items: center;               /* Centra verticalmente */
    justify-content: center;           /* Centra orizzontalmente */
    margin-right: 6px;                 /* Spazio a destra ridotto */
    flex-shrink: 0;                    /* Mantiene dimensione */
    border-right: 1px solid rgba(62, 109, 169, 0.50); /* Separatore verticale stile blu jamovi */
    padding-right: 6px;                /* Padding per separazione */
}

/* Icona SVG plus/minus */
.plus-minus-icon {
    width: 12px;                       /* Dimensione icona ridotta */
    height: 12px;                      /* Dimensione icona ridotta */
    /* Nessun colore fisso qui in modo da ereditare il colore blu jamovi o grigio dal padre */
}

/* Transizioni per animazione icona */
.plus-minus-icon .horizontal {
    transition: transform 0.3s ease, opacity 0.3s ease; /* Transizione smooth */
    transform-origin: center;          /* Punto di rotazione centrale */
}

.plus-minus-icon .vertical {
    transition: transform 0.3s ease, opacity 0.3s ease; /* Transizione smooth */
    transform-origin: center;          /* Punto di rotazione centrale */
}

/* Animazione icona quando espanso (da + a -) */
.info-notes-header.expanded .plus-minus-icon .vertical {
    transform: rotate(90deg);          /* Ruota linea verticale */
    opacity: 0;                        /* Nasconde linea verticale */
}

/* Titolo della sezione con linee orizzontali centrate prima e dopo */
.info-notes-title {
    margin-left: 0px;                  /* Rimosso margine sinistro aggiuntivo */
    display: flex;
    align-items: center;
    flex-grow: 1;                      /* Riempe lo spazio per permettere l'espansione della linea a destra */
}

/* Generazione della linea a metà del testo */
.info-notes-title::before,
.info-notes-title::after {
    content: '';
    height: 1px;
    background-color: rgba(62, 109, 169, 0.50); /* Colore linea sottile */
    transition: all 0.3s ease;
}

/* Linea a sinistra del testo Info & Notes */
.info-notes-title::before {
    flex-basis: 30px;                  /* Lunghezza fissa della riga in apertura */
    margin: 0 8px;                     /* Spaziatura testo / linea */
}

/* Linea a destra del testo Info & Notes */
.info-notes-title::after {
    flex-basis: 30px;                  /* Corta quando collassato */
    margin-left: 8px;                  /* Spaziatura linea / testo */
}

/* Cambiamenti in stato espanso sulle linee */
.info-notes-header.expanded .info-notes-title::before,
.info-notes-header.expanded .info-notes-title::after {
    height: 2px;                       /* Linea più spessa */
    background-color: rgba(62, 109, 169, 0.70); /* Colore intensificato */
}

.info-notes-header.expanded .info-notes-title::after {
    flex-grow: 1;                      /* Si estende per tutta la porzione destra libera */
}

/* Contenuto collassabile */
.info-notes-content {
    max-height: 0;                     /* Altezza 0 quando chiuso */
    overflow: hidden;                  /* Nasconde contenuto overflow */
    transition: max-height 0.3s ease-out; /* Animazione smooth apertura/chiusura */
    background: transparent;           /* Sfondo trasparente */
    border: none;                      /* Nessun bordo */
}

/* Contenuto espanso */
.info-notes-content.expanded {
    max-height: 600px;                 /* Altezza massima quando aperto */
}

/* Corpo del contenuto info & notes */
.info-notes-body {
    padding: 4px 16px 12px 16px;       /* Padding interno */
    background: transparent;           /* Sfondo trasparente */
    width: 100%;                       /* Larghezza piena */
    max-width: 100%;                   /* Larghezza massima */
    box-sizing: border-box;            /* Include padding */
    font-size: 12px;                   /* Font piccolo per info */
    color: #555;                       /* Colore grigio scuro */
    line-height: 1.5;                  /* Interlinea per leggibilità */
    max-height: 280px;                 /* Altezza fissa: mantiene l'ingombro verticale attuale */
    overflow-y: auto;                  /* Scroll verticale se il contenuto eccede */
    overflow-x: hidden;
    scrollbar-width: thin;             /* Firefox */
    scrollbar-color: rgba(62, 109, 169, 0.35) transparent;
}

/* Scrollbar sottile stile jamovi (WebKit) - stessa palette gia' usata
   altrove nel modulo (vedi #forest-wrap, #funnel-wrap, ecc.) */
.info-notes-body::-webkit-scrollbar { width: 5px; }
.info-notes-body::-webkit-scrollbar-track { background: transparent; }
.info-notes-body::-webkit-scrollbar-thumb { background: rgba(62, 109, 169, 0.35); border-radius: 3px; }
.info-notes-body::-webkit-scrollbar-thumb:hover { background: rgba(62, 109, 169, 0.60); }

/* Sezioni strutturate generate da buildInfoNotes() in mzTabConfigs.js */
.info-section { margin-top: 14px; }
.info-section:first-child { margin-top: 4px; }

.info-section-title {
    font-size: 10.5px;
    font-weight: 700;
    letter-spacing: 0.6px;
    text-transform: uppercase;
    color: #3e6da9;
    padding-bottom: 3px;
    margin-bottom: 8px;
    border-bottom: 1px solid rgba(62, 109, 169, 0.25);
}

.info-list {
    display: flex;
    flex-direction: column;
    gap: 8px;
}

.info-item {
    padding-left: 10px;
    border-left: 2px solid rgba(62, 109, 169, 0.18);
}

.info-term {
    font-weight: 600;
    color: #2c4a6e;
    font-size: 12px;
    display: flex;
    align-items: center;
    flex-wrap: wrap;
    gap: 6px;
}

.info-desc {
    color: #666;
    margin-top: 1px;
}

.info-badge {
    display: inline-block;
    font-size: 10px;
    font-weight: 600;
    color: #1565c0;
    background: rgba(62, 109, 169, 0.12);
    border: 1px solid rgba(62, 109, 169, 0.30);
    border-radius: 10px;
    padding: 1px 7px;
    letter-spacing: 0.2px;
}

.info-sublist {
    margin: 5px 0 0 0;
    padding-left: 14px;
    list-style: none;
    display: flex;
    flex-direction: column;
    gap: 3px;
}

.info-sublist li {
    position: relative;
    color: #666;
}

.info-sublist li::before {
    content: '';
    position: absolute;
    left: -12px;
    top: 7px;
    width: 5px;
    height: 1px;
    background: rgba(62, 109, 169, 0.45);
}

.info-sublist li strong {
    color: #444;
    font-weight: 600;
}

/* Variante "dev" - sezioni secondarie di debug/sviluppo, non per l'uso
   quotidiano dell'utente (es. shortcut logging in mzDebug.js). Stile
   visivamente distinto dalle sezioni standard: accento arancio (stesso
   colore della pillola DEBUG e del deviatore jlogMode), separatore
   tratteggiato invece che pieno, font monospazio per gli shortcut. */
.info-section.info-section-dev {
    margin-top: 18px;
    padding-top: 12px;
    border-top: 1px dashed rgba(236, 160, 56, 0.45);
}

.info-section-dev .info-section-title {
    color: #b5711f;
    border-bottom-color: rgba(236, 160, 56, 0.35);
}

.info-section-dev .info-item {
    border-left-color: rgba(236, 160, 56, 0.35);
}

.info-section-dev .info-term {
    color: #8a5416;
}

.info-section-icon {
    display: inline-flex;
    align-items: center;
    margin-right: 5px;
    color: inherit;
}

.info-kbd {
    display: inline-block;
    font-family: Consolas, Monaco, 'Andale Mono', monospace;
    font-size: 10px;
    font-weight: 600;
    color: #6b4a12;
    background: #fff6ea;
    border: 1px solid rgba(236, 160, 56, 0.55);
    border-bottom-width: 2px;
    border-radius: 4px;
    padding: 1px 5px;
    margin: 0 1px;
    line-height: 1.5;
}

.info-kbd-plus {
    color: #b5711f;
    font-size: 10px;
    font-weight: 600;
    margin: 0 1px;
}
/* === AREA INFO & NOTE COLLASSABILE - FINE === */


/* =====================================================
   DESIGN RESPONSIVE - INIZIO
   ===================================================== */

/* Media query per schermi piccoli (mobile) */
@media (max-width: 600px) {
    /* Adatta header schede per mobile */
    .tab-headers .tab-header {
        padding: 4px 12px;             /* Padding ridotto */
        font-size: 10px;               /* Font più piccolo */
        min-height: 24px;              /* Altezza minima ridotta */
    }
    
    /* Padding ridotto per corpo scheda */
    .tab-body {
        padding: 16px;                 /* Padding maggiore per touch */
    }
    
    /* Forza layout a colonna singola su mobile */
    .tab-body-grid {
        grid-template-columns: 1fr !important; /* Colonna singola */
        gap: 10px;                     /* Gap ridotto */
    }
    
    /* Adatta titoli gruppo per mobile */
    .group-title {
        font-size: 12px;               /* Font ridotto */
        margin-bottom: 6px;            /* Margine ridotto */
    }
    
    /* Input e select a larghezza piena su mobile */
    .control-input,
    .control-select {
        width: 100%;                   /* Larghezza piena per touch */
    }
}

/* === DESIGN RESPONSIVE - FINE === */


/* =====================================================
   ACCESSIBILITÀ - INIZIO
   ===================================================== */

/* Focus per navigazione da tastiera - header schede */
.tab-headers .tab-header:focus {
    outline: 2px solid #3e6da9;        /* Outline blu per focus */
    outline-offset: 2px;               /* Offset per visibilità */
}

/* Focus per input e select */
.control-input:focus,
.control-select:focus {
    outline: none;                     /* Rimuove outline default */
    border-color: #3e6da9;             /* Bordo blu */
    box-shadow: 0 0 0 2px rgba(62, 109, 169, 0.2); /* Alone blu */
}

/* Focus per checkbox */
.control-checkbox:focus {
    //outline: 2px solid #3e6da9;        /* Outline blu */
    //outline-offset: 2px;               /* Offset per visibilità */
    outline: none !important;
    box-shadow: none !important;
}

/* Focus visibile (navigazione tastiera) */
.control-checkbox:focus-visible {
    //outline: 2px solid #3e6da9;        /* Outline solo con tastiera */
    //outline-offset: 2px;               /* Offset per visibilità */
    outline: none !important;
    box-shadow: none !important;
}

/* Rimuove outline quando cliccato con mouse */
.control-checkbox:active {
    outline: none;                     /* Nessun outline al click */
}

/* Supporto per utenti che preferiscono animazioni ridotte */
@media (prefers-reduced-motion: reduce) {
    /* Rimuove transizioni per tab headers */
    .tab-headers .tab-header {
        transition: none;              /* Nessuna transizione */
    }
    
    /* Rimuove animazione info notes */
    .info-notes-content {
        transition: none;              /* Nessuna transizione */
    }
}

/* === ACCESSIBILITÀ - FINE === */


/* ============================================================================
   MATERIAL DESIGN BUTTONS - Sistema bottoni con base quadrata + pulsante
   Supporta: shape ('round' o 'square'), size ('small','medium','large','big')
   ============================================================================ */

/* -----------------------------------------------------------------------------
   CONTENITORE ALLINEAMENTO - Gestisce align (left/center/right) nella griglia
   ----------------------------------------------------------------------------- */
.action-button-container {
  display: flex;
  width: auto; /* Rimosso 100% per permettere l'allineamento tramite wrapper genitore */
  margin-bottom: 6px;
}

.action-button-container.align-left {
  justify-content: flex-start;
}

.action-button-container.align-center {
  justify-content: center;
}

.action-button-container.align-right {
  justify-content: flex-end;
}

/* -----------------------------------------------------------------------------
   WRAPPER BOTTONE - Contenitore base + pulsante sovrapposti
   ----------------------------------------------------------------------------- */
.mz-btn-wrapper {
  position: relative;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}

/* Dimensioni wrapper (4 size disponibili) */
.mz-btn-wrapper.size-small { width: 40px; height: 40px; }
.mz-btn-wrapper.size-medium { width: 48px; height: 48px; }
.mz-btn-wrapper.size-large { width: 56px; height: 56px; }
.mz-btn-wrapper.size-big { width: 64px; height: 64px; }

/* -----------------------------------------------------------------------------
   BASE QUADRATA - Supporto fisso sotto il pulsante (sempre quadrato)
   ----------------------------------------------------------------------------- */
.mz-btn-base {
  position: absolute;
  background: linear-gradient(145deg, #e8e8e8, #f5f5f5);
  box-shadow: 
    inset 0 2px 4px rgba(0,0,0,0.1),
    0 1px 2px rgba(0,0,0,0.05);
  border-radius: 8px;
}

/* Dimensioni base per ogni size */
.mz-btn-wrapper.size-small .mz-btn-base { width: 40px; height: 40px; }
.mz-btn-wrapper.size-medium .mz-btn-base { width: 48px; height: 48px; }
.mz-btn-wrapper.size-large .mz-btn-base { width: 56px; height: 56px; }
.mz-btn-wrapper.size-big .mz-btn-base { width: 64px; height: 64px; }

/* -----------------------------------------------------------------------------
   PULSANTE - Elemento cliccabile (round o square)
   ----------------------------------------------------------------------------- */
.mz-btn-new {
  border: none;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.15s cubic-bezier(0.4, 0, 0.2, 1);
  box-shadow: 
    0 2px 4px rgba(0,0,0,0.15),
    0 1px 2px rgba(0,0,0,0.1);
  position: relative;
  z-index: 1;
}

/* --- PULSANTE ROTONDO (type-round) --- */
.mz-btn-new.type-round {
  border-radius: 50%;
}

.mz-btn-wrapper.size-small .mz-btn-new.type-round { 
  width: 28px; 
  height: 28px; 
  font-size: 13px;
}

.mz-btn-wrapper.size-medium .mz-btn-new.type-round { 
  width: 36px; 
  height: 36px; 
  font-size: 15px;
}

.mz-btn-wrapper.size-large .mz-btn-new.type-round { 
  width: 44px; 
  height: 44px; 
  font-size: 18px;
}

.mz-btn-wrapper.size-big .mz-btn-new.type-round { 
  width: 52px; 
  height: 52px; 
  font-size: 22px;
}

/* --- PULSANTE QUADRATO (type-square) --- */
.mz-btn-new.type-square {
  border-radius: 6px;
}

.mz-btn-wrapper.size-small .mz-btn-new.type-square { 
  width: 24px; 
  height: 24px; 
  font-size: 12px;
  border-radius: 5px;
}

.mz-btn-wrapper.size-medium .mz-btn-new.type-square { 
  width: 30px; 
  height: 30px; 
  font-size: 14px;
  border-radius: 6px;
}

.mz-btn-wrapper.size-large .mz-btn-new.type-square { 
  width: 36px; 
  height: 36px; 
  font-size: 17px;
  border-radius: 7px;
}

.mz-btn-wrapper.size-big .mz-btn-new.type-square { 
  width: 42px; 
  height: 42px; 
  font-size: 20px;
  border-radius: 8px;
}

/* -----------------------------------------------------------------------------
   EFFETTI INTERATTIVI - Hover e pressione
   ----------------------------------------------------------------------------- */
.mz-btn-new:hover {
  box-shadow: 
    0 3px 6px rgba(0,0,0,0.2),
    0 2px 4px rgba(0,0,0,0.14);
}

.mz-btn-new:active {
  box-shadow: 
    0 1px 2px rgba(0,0,0,0.12),
    0 0 1px rgba(0,0,0,0.08);
  transform: translateY(2px) scale(0.95);
}

/* -----------------------------------------------------------------------------
   WRAPPER LABEL - Gestisce posizione etichetta rispetto al bottone
   Supporta: labelPosition (left/right/top/bottom)
   ----------------------------------------------------------------------------- */
.action-button-wrapper {
  display: inline-flex;
  align-items: center;
  gap: 8px;
}

/* Label a SINISTRA del bottone (ordine normale HTML) */
.action-button-wrapper.label-left {
  flex-direction: row;
}

/* Label a DESTRA del bottone (row-reverse inverte ordine HTML) */
.action-button-wrapper.label-right {
  flex-direction: row-reverse;
}

/* Label SOPRA il bottone (ordine normale) */
.action-button-wrapper.label-top {
  flex-direction: column;
  gap: 4px;
}

/* Label SOTTO il bottone (column-reverse) */
.action-button-wrapper.label-bottom {
  flex-direction: column-reverse;
  gap: 4px;
}

/* Stile testo etichetta */
.action-button-label {
  font-size: 11px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
  user-select: none;
}

/* === FINE BLOCCO MATERIAL DESIGN BUTTONS === */


/* ============================================================================
   STILI LABEL STANDALONE - Per testi liberi, sottotitoli e istruzioni
   ============================================================================ */
   
.mz-label-item {
  width: 100%;
  margin-bottom: 8px;
  display: flex;
  box-sizing: border-box;
}

.mz-label-item.align-left { justify-content: flex-start; text-align: left; }
.mz-label-item.align-center { justify-content: center; text-align: center; }
.mz-label-item.align-right { justify-content: flex-end; text-align: right; }

.mz-label-text {
  display: block;
  line-height: 1.4;
  user-select: none;
  max-width: 100%;
  word-wrap: break-word;
  text-align: inherit;             /* Eredita allineamento (left, center, right) dal contenitore item */
}

/* Varianti Label */
.mz-label-text.variant-plain {
  font-size: 12px;
  color: #555;
  font-weight: 500;
}

.mz-label-text.variant-title {
  font-size: 14px;
  font-weight: 700;
  color: #3e6da9;
  margin-top: 4px;
  margin-bottom: 4px;
  padding-bottom: 4px;
  border-bottom: 1px solid rgba(62, 109, 169, 0.40);
  width: auto;
}

.mz-label-text.variant-subtitle {
  font-size: 12.5px;
  font-weight: 600;
  color: #444;
  margin-bottom: 2px;
}

.mz-label-text.variant-hint {
  font-size: 10px;
  font-style: italic;
  color: #888;
}

.mz-label-text.variant-badge {
  display: inline-flex;             /* Flex per centratura interna perfetta */
  justify-content: center !important; /* Centra orizzontalmente (prevale su inline) */
  align-items: center !important;   /* Centra verticalmente (prevale su inline) */
  min-height: 20px !important;      /* Altezza minima garantita anche con padding ridotto */
  padding: 3px 12px;                /* Padding di default equilibrato */
  border-radius: 20px;              /* Pillola più morbida */
  font-size: 9px;
  font-weight: 700;
  background: #e0e0e0;
  color: #333;
  line-height: 1 !important;        /* Line-height a 1 evita che il testo sia spinto verso l'alto */
  width: auto;
  text-transform: none;             /* Supporta mixed case */
  align-self: center;               /* Centra il badge nel suo flex container */
  box-sizing: border-box;           /* Include padding nel calcolo min-height */
}

/* --- DIVISORI (VARIANT DIVIDER) --- */
.mz-divider-container {
  display: flex !important;
  align-items: center !important;
  width: 100% !important;
  gap: 0 !important;
  pointer-events: none !important;
}

.mz-divider-line {
  flex-grow: 1 !important;
  border: none !important;
  pointer-events: none !important;
}

.variant-divider {
  padding: 0 12px;
  white-space: nowrap;
  font-weight: 600;
  color: #666;
  flex-shrink: 0;
}

/* Modificatori di allineamento per i margini del testo */
.mz-divider-container.align-left .variant-divider {
  padding-left: 0;
}

.mz-divider-container.align-right .variant-divider {
  padding-right: 0;
}

/* --- SOTTO-TAB (NESTED TABS) - STILE MATERIAL/SCIENTIFIC MINIMALE --- */
/* Il colore accent è configurabile tramite la variabile CSS --mz-subtab-accent. */
/* Default: blu Jamovi (#3e6da9). Impostabile inline per gruppo nel JS.          */
.mz-subtab-headers {
  display: flex;
  background: transparent;
  border-bottom: 1px solid color-mix(in srgb, var(--mz-subtab-accent, #3e6da9) 50%, transparent);
  width: 100%;
  gap: 20px;
  padding: 0 4px;
  align-items: flex-end;             /* Allinea i tab al bordo inferiore (baseline comune) */
}

.mz-subtab-header {
  padding: 4px 2px 6px 2px;
  font-size: 10.5px;
  font-weight: 500;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: #999;
  cursor: pointer;
  background: transparent;
  border: none;
  border-bottom: 3px solid transparent;
  transition: all 0.2s ease;
  user-select: none;
  margin-bottom: -1px;
}

.mz-subtab-header:hover:not(.active) {
  color: #666;
  border-bottom-color: color-mix(in srgb, var(--mz-subtab-accent, #3e6da9) 20%, transparent);
}

.mz-subtab-header.active {
  color: var(--mz-subtab-accent, #3e6da9);
  font-weight: 700;
  border-bottom: 3.5px solid var(--mz-subtab-accent, #3e6da9);
}

.mz-subtab-content {
  display: none;
  padding: 4px 0;                    /* Sincronizzato con .accordion-content-inner (era 8px 4px) */
  animation: mzFadeIn 0.3s ease;
}

.mz-subtab-content.active {
  display: block;
}

@keyframes mzFadeIn {
  from { opacity: 0; transform: translateY(3px); }
  to { opacity: 1; transform: translateY(0); }
}

/* =====================================================
   TOGGLE ROCKER — interruttore a bilancere 3 stati
   horizontal: left | off | right
   vertical:   top  | off | bottom
   ===================================================== */

.toggle-rocker-wrapper {
  display: inline-flex;
  flex-direction: column;
  gap: 4px;
  pointer-events: auto !important;
}

/* allineamento etichetta principale */
.toggle-rocker-wrapper.main-label-left   { align-items: flex-start; }
.toggle-rocker-wrapper.main-label-center { align-items: center; }
.toggle-rocker-wrapper.main-label-right  { align-items: flex-end; }

.toggle-rocker-main-label {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

/* --- layout orizzontale --- */
.toggle-rocker-row {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  user-select: none;
  pointer-events: auto !important;
}

/* --- layout verticale --- */
.toggle-rocker-row.vertical {
  flex-direction: column;
  align-items: center;
  gap: 4px;
}

/* etichette laterali */
.toggle-rocker-label-l,
.toggle-rocker-label-r,
.toggle-rocker-label-t,
.toggle-rocker-label-b {
  font-size: 11px;
  font-weight: 500;
  color: #bbb;
  white-space: nowrap;
  transition: color 0.2s ease, font-weight 0.2s ease, font-size 0.2s ease;
  cursor: pointer;
  pointer-events: auto !important;
}

/* etichetta attiva — ingrandisce, grassetto, colore */
.toggle-rocker-row[data-state="posLorT"] .toggle-rocker-label-l,
.toggle-rocker-row[data-state="posLorT"] .toggle-rocker-label-t {
  color: var(--mz-rocker-color-a, #eca038);
  font-weight: 700;
  font-size: 12px;
}
.toggle-rocker-row[data-state="posRorB"] .toggle-rocker-label-r,
.toggle-rocker-row[data-state="posRorB"] .toggle-rocker-label-b {
  color: var(--mz-rocker-color-b, #3e6da9);
  font-weight: 700;
  font-size: 12px;
}

/* --- track --- */
.toggle-rocker-track {
  position: relative;
  flex-shrink: 0;
  background-color: #ccc;
  box-shadow: inset 0 1px 3px rgba(0,0,0,0.18);
  transition: background 0.25s ease;
  cursor: pointer;
  pointer-events: auto !important;
}

/* track orizzontale */
.toggle-rocker-row:not(.vertical) .toggle-rocker-track {
  width: 48px;
  height: 16px;
  border-radius: 8px;
}

/* track verticale */
.toggle-rocker-row.vertical .toggle-rocker-track {
  width: 16px;
  height: 48px;
  border-radius: 8px;
}

/* colore track — horizontal */
.toggle-rocker-row[data-state="posLorT"]:not(.vertical) .toggle-rocker-track {
  background: linear-gradient(to right,
    var(--mz-rocker-color-a, #eca038) 50%, #ddd 50%);
  box-shadow: inset 0 1px 3px rgba(0,0,0,0.15);
}
.toggle-rocker-row[data-state="posRorB"]:not(.vertical) .toggle-rocker-track {
  background: linear-gradient(to right,
    #ddd 50%, var(--mz-rocker-color-b, #3e6da9) 50%);
  box-shadow: inset 0 1px 3px rgba(0,0,0,0.15);
}
.toggle-rocker-row[data-state="posOff"]:not(.vertical) .toggle-rocker-track {
  background-color: #ccc;
  box-shadow: inset 0 1px 3px rgba(0,0,0,0.18);
}

/* colore track — vertical */
.toggle-rocker-row.vertical[data-state="posLorT"] .toggle-rocker-track {
  background: linear-gradient(to bottom,
    var(--mz-rocker-color-a, #eca038) 50%, #ddd 50%);
  box-shadow: inset 1px 0 3px rgba(0,0,0,0.15);
}
.toggle-rocker-row.vertical[data-state="posRorB"] .toggle-rocker-track {
  background: linear-gradient(to bottom,
    #ddd 50%, var(--mz-rocker-color-b, #3e6da9) 50%);
  box-shadow: inset 1px 0 3px rgba(0,0,0,0.15);
}
.toggle-rocker-row.vertical[data-state="posOff"] .toggle-rocker-track {
  background-color: #ccc;
  box-shadow: inset 1px 0 3px rgba(0,0,0,0.18);
}

/* --- pallino --- */
.toggle-rocker-thumb {
  position: absolute;
  background-color: #fff;
  border: 1px solid rgba(0,0,0,0.12);
  box-shadow: 0 2px 4px rgba(0,0,0,0.22), 0 1px 2px rgba(0,0,0,0.14);
  border-radius: 3px;
  transition:
    left          0.25s cubic-bezier(0.4, 0, 0.2, 1),
    top           0.25s cubic-bezier(0.4, 0, 0.2, 1),
    border-radius 0.2s ease;
  pointer-events: none;
}

/* dimensioni pallino horizontal */
.toggle-rocker-row:not(.vertical) .toggle-rocker-thumb {
  width: 12px;
  height: 12px;
  top: 2px;
}

/* dimensioni pallino vertical */
.toggle-rocker-row.vertical .toggle-rocker-thumb {
  width: 12px;
  height: 12px;
  left: 2px;
}

/* posizioni pallino — horizontal */
.toggle-rocker-row:not(.vertical)[data-state="posOff"]  .toggle-rocker-thumb { left: 18px; border-radius: 3px; }
.toggle-rocker-row:not(.vertical)[data-state="posLorT"] .toggle-rocker-thumb { left: 2px;  border-radius: 6px 2px 2px 6px; }
.toggle-rocker-row:not(.vertical)[data-state="posRorB"] .toggle-rocker-thumb { left: 34px; border-radius: 2px 6px 6px 2px; }

/* posizioni pallino — vertical */
.toggle-rocker-row.vertical[data-state="posOff"]  .toggle-rocker-thumb { top: 18px; border-radius: 3px; }
.toggle-rocker-row.vertical[data-state="posLorT"] .toggle-rocker-thumb { top: 2px;  border-radius: 6px 6px 2px 2px; }
.toggle-rocker-row.vertical[data-state="posRorB"] .toggle-rocker-thumb { top: 34px; border-radius: 2px 2px 6px 6px; }

/* hover track */
.toggle-rocker-track:hover {
  filter: brightness(0.92);
}

/* disabled */
.toggle-rocker-wrapper.disabled .toggle-rocker-row {
  opacity: 0.4;
  cursor: not-allowed;
  pointer-events: none !important;
}

/* === TOGGLE ROCKER - FINE === */


/* =====================================================
   DEVIATORE — interruttore fisico 3 stati
   horizontal: posLorT | posOff | posRorB
   vertical:   posLorT | posOff | posRorB
   ===================================================== */

.deviatore-wrapper {
  display: inline-flex;
  flex-direction: column;
  gap: 4px;
  pointer-events: auto !important;
}

/* allineamento orizzontale dell'etichetta principale */
.deviatore-wrapper.main-label-left {
  align-items: flex-start;
}

.deviatore-wrapper.main-label-center {
  align-items: center;
}

.deviatore-wrapper.main-label-right {
  align-items: flex-end;
}

/* etichetta principale */
.deviatore-main-label {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

/* --- riga controllo --- */
.deviatore-row {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  user-select: none;
  pointer-events: auto !important;
}

.deviatore-row.vertical {
  flex-direction: column;
  align-items: center;
  gap: 4px;
}

/* etichette laterali */
.deviatore-label-l,
.deviatore-label-r,
.deviatore-label-t,
.deviatore-label-b {
  font-size: 11px;
  font-weight: 500;
  color: #bbb;
  white-space: nowrap;
  transition: color 0.15s ease, font-weight 0.15s ease, font-size 0.15s ease;
  pointer-events: none !important;
  cursor: default;
}

/* =====================================================
   STATE-LABEL ORIZZONTALI
   labelState:
     top    → sopra il deviatore
     center → esterne a sinistra/destra del deviatore
     bottom → sotto il deviatore
   ===================================================== */

.deviatore-horizontal-stage {
  position: relative;
  flex-shrink: 0;
}

.deviatore-state-labels {
  position: absolute;
  pointer-events: none;
  z-index: 4;
}

.deviatore-state-labels .deviatore-label-l,
.deviatore-state-labels .deviatore-label-r {
  position: absolute;
  white-space: nowrap;
}

/* CENTER */
.deviatore-horizontal-stage[data-label-state="center"] {
  width: 54px;
  height: 14px;
}

.deviatore-horizontal-stage[data-label-state="center"] .deviatore-state-labels {
  left: 0;
  top: 0;
  width: 54px;
  height: 14px;
}

.deviatore-horizontal-stage[data-label-state="center"] .deviatore-label-l {
  right: calc(100% + 10px);
  top: 50%;
  transform: translateY(-50%);
  text-align: right;
}

.deviatore-horizontal-stage[data-label-state="center"] .deviatore-label-r {
  left: calc(100% + 10px);
  top: 50%;
  transform: translateY(-50%);
  text-align: left;
}

/* TOP */
.deviatore-horizontal-stage[data-label-state="top"] {
  width: 54px;
  height: 14px;
  margin-top: 18px;
}

.deviatore-horizontal-stage[data-label-state="top"] .deviatore-state-labels {
  left: 0;
  top: -18px;
  width: 54px;
  height: 14px;
}

.deviatore-horizontal-stage[data-label-state="top"] .deviatore-label-l {
  right: calc(50% + 10px);
  bottom: 0;
  text-align: right;
}

.deviatore-horizontal-stage[data-label-state="top"] .deviatore-label-r {
  left: calc(50% + 10px);
  bottom: 0;
  text-align: left;
}

/* BOTTOM */
.deviatore-horizontal-stage[data-label-state="bottom"] {
  width: 54px;
  height: 14px;
  margin-bottom: 18px;
}

.deviatore-horizontal-stage[data-label-state="bottom"] .deviatore-state-labels {
  left: 0;
  top: 18px;
  width: 54px;
  height: 14px;
}

.deviatore-horizontal-stage[data-label-state="bottom"] .deviatore-label-l {
  right: calc(50% + 10px);
  top: 0;
  text-align: right;
}

.deviatore-horizontal-stage[data-label-state="bottom"] .deviatore-label-r {
  left: calc(50% + 10px);
  top: 0;
  text-align: left;
}

/* =====================================================
   ETICHETTE ATTIVE
   ===================================================== */

/* posLorT */
.deviatore-row[data-state="posLorT"] .deviatore-label-l,
.deviatore-row[data-state="posLorT"] .deviatore-label-t,
.deviatore-state-labels[data-state="posLorT"] .deviatore-label-l {
  color: var(--mz-dev-color-a, #eca038);
  font-weight: 700;
  font-size: 12px;
}

/* posRorB */
.deviatore-row[data-state="posRorB"] .deviatore-label-r,
.deviatore-row[data-state="posRorB"] .deviatore-label-b,
.deviatore-state-labels[data-state="posRorB"] .deviatore-label-r {
  color: var(--mz-dev-color-b, #3e6da9);
  font-weight: 700;
  font-size: 12px;
}

/* --- alloggiamento del deviatore standard --- */
.deviatore-track {
  position: relative;
  flex-shrink: 0;
  background: #e0e0e0;
  border: 1.5px solid var(--mz-dev-shape-color, #3e6da9);
  border-radius: 7px;
  box-shadow:
    inset 0 1px 2px rgba(0,0,0,0.14),
    0 1px 1px rgba(0,0,0,0.10);
  cursor: pointer;
  pointer-events: auto !important;
  overflow: visible;
}

/* track orizzontale */
.deviatore-row:not(.vertical) .deviatore-track {
  width: 54px;
  height: 14px;
}

/* track verticale */
.deviatore-row.vertical .deviatore-track {
  width: 14px;
  height: 54px;
}

/* tacche divisorie */
.deviatore-track::before,
.deviatore-track::after {
  content: '';
  position: absolute;
  background: var(--mz-dev-shape-color, #3e6da9);
  opacity: 0.65;
  pointer-events: none;
  z-index: 3;
}

/* tacche orizzontali */
.deviatore-row:not(.vertical) .deviatore-track::before {
  width: 1px;
  height: 70%;
  top: 15%;
  left: calc(33.333% - 0.5px);
}

.deviatore-row:not(.vertical) .deviatore-track::after {
  width: 1px;
  height: 70%;
  top: 15%;
  left: calc(66.666% - 0.5px);
}

/* tacche verticali */
.deviatore-row.vertical .deviatore-track::before {
  height: 1px;
  width: 70%;
  left: 15%;
  top: calc(33.333% - 0.5px);
}

.deviatore-row.vertical .deviatore-track::after {
  height: 1px;
  width: 70%;
  left: 15%;
  top: calc(66.666% - 0.5px);
}

/* =====================================================
   STATO ORIZZONTALE
   ===================================================== */

/* Classic / posLorT:
   solo il primo terzo usa colorA */
.deviatore-row[data-state="posLorT"]:not(.vertical) .deviatore-track {
  background: linear-gradient(
    to right,
    var(--mz-dev-color-a, #3e6da9) 0%,
    var(--mz-dev-color-a, #3e6da9) 33.333%,
    #e0e0e0 33.333%,
    #e0e0e0 100%
  );
}

/* Interac / posRorB:
   solo l'ultimo terzo usa colorB */
.deviatore-row[data-state="posRorB"]:not(.vertical) .deviatore-track {
  background: linear-gradient(
    to right,
    #e0e0e0 0%,
    #e0e0e0 66.666%,
    var(--mz-dev-color-b, #eca038) 66.666%,
    var(--mz-dev-color-b, #eca038) 100%
  );
}

/* OFF:
   nessuna zona colorata */
.deviatore-row[data-state="posOff"]:not(.vertical) .deviatore-track {
  background: #e0e0e0;
}

/* =====================================================
   STATO VERTICALE
   ===================================================== */

/* posLorT:
   solo il primo terzo usa colorA */
.deviatore-row.vertical[data-state="posLorT"] .deviatore-track {
  background: linear-gradient(
    to bottom,
    var(--mz-dev-color-a, #3e6da9) 0%,
    var(--mz-dev-color-a, #3e6da9) 33.333%,
    #e0e0e0 33.333%,
    #e0e0e0 100%
  );
}

/* posRorB:
   solo l'ultimo terzo usa colorB */
.deviatore-row.vertical[data-state="posRorB"] .deviatore-track {
  background: linear-gradient(
    to bottom,
    #e0e0e0 0%,
    #e0e0e0 66.666%,
    var(--mz-dev-color-b, #eca038) 66.666%,
    var(--mz-dev-color-b, #eca038) 100%
  );
}

/* OFF:
   nessuna zona colorata */
.deviatore-row.vertical[data-state="posOff"] .deviatore-track {
  background: #e0e0e0;
}

/* --- leva — dimensioni via CSS custom properties --- */
.deviatore-thumb {
  position: absolute;
  background: var(--mz-dev-thumb-color, #ffffff);
  border: 1.5px solid rgba(0,0,0,0.20);
  border-radius: var(--mz-dev-thumb-radius, 2px);
  box-shadow: none;
  transition:
    left 0.08s cubic-bezier(0.4, 0, 0.6, 1),
    top 0.08s cubic-bezier(0.4, 0, 0.6, 1);
  pointer-events: none;
  z-index: 2;
}

/* dimensioni leva horizontal — centrata sul padding box, indipendente da box-sizing */
.deviatore-row:not(.vertical) .deviatore-thumb {
  width: var(--mz-dev-thumb-w, 8px);
  height: var(--mz-dev-thumb-h, 20px);
  top: 50%;
  transform: translate(-50%, -50%);
}

/* dimensioni leva vertical */
.deviatore-row.vertical .deviatore-thumb {
  width: var(--mz-dev-thumb-h, 20px);
  height: var(--mz-dev-thumb-w, 8px);
  left: 50%;
  transform: translate(-50%, -50%);
}

/* posizioni leva horizontal — centri delle tre zone (1/6, 3/6, 5/6) */
.deviatore-row:not(.vertical)[data-state="posOff"] .deviatore-thumb {
  left: 50%;
}

.deviatore-row:not(.vertical)[data-state="posLorT"] .deviatore-thumb {
  left: 16.6667%;
}

.deviatore-row:not(.vertical)[data-state="posRorB"] .deviatore-thumb {
  left: 83.3333%;
}

/* posizioni leva vertical */
.deviatore-row.vertical[data-state="posOff"] .deviatore-thumb {
  top: 50%;
}

.deviatore-row.vertical[data-state="posLorT"] .deviatore-thumb {
  top: 16.6667%;
}

.deviatore-row.vertical[data-state="posRorB"] .deviatore-thumb {
  top: 83.3333%;
}

/* hover */
.deviatore-track:hover {
  filter: brightness(0.95);
}

/* disabled */
.deviatore-wrapper.disabled .deviatore-row {
  opacity: 0.4;
  cursor: not-allowed;
  pointer-events: none !important;
}

/* === DEVIATORE - FINE === */


/* =====================================================
   DEVIATORE — SHAPE LINE
   Solo orientamento orizzontale.

   Tre posizioni:
   posLorT | posOff | posRorB

   Tre marker di riferimento:
   sinistra → colorA
   centro   → bianco
   destra   → colorB

   Il selettore grande:
   - è trasparente
   - ha bordo shapeColor
   - segue la forma dei marker di stato

   Shape disponibili:
   circle | square | diamond
   ===================================================== */

.deviatore-line-stage {
  position: relative;
  width: 57px;
  height: 18px;
  flex-shrink: 0;
}

.deviatore-line-state-labels {
  position: absolute;
  pointer-events: none;
  z-index: 4;
}

.deviatore-line-state-labels .deviatore-label-l,
.deviatore-line-state-labels .deviatore-label-r {
  position: absolute;
  white-space: nowrap;
}

/* =====================================================
   STATE LABEL — CENTER
   ===================================================== */

.deviatore-line-stage[data-label-state="center"] {
  width: 57px;
  height: 18px;
}

.deviatore-line-stage[data-label-state="center"]
.deviatore-line-state-labels {
  left: 0;
  top: 0;
  width: 57px;
  height: 18px;
}

.deviatore-line-stage[data-label-state="center"]
.deviatore-label-l {
  right: calc(100% + 10px);
  top: 50%;
  transform: translateY(-50%);
  text-align: right;
}

.deviatore-line-stage[data-label-state="center"]
.deviatore-label-r {
  left: calc(100% + 10px);
  top: 50%;
  transform: translateY(-50%);
  text-align: left;
}

/* =====================================================
   STATE LABEL — TOP
   ===================================================== */

.deviatore-line-stage[data-label-state="top"] {
  margin-top: 18px;
}

.deviatore-line-stage[data-label-state="top"]
.deviatore-line-state-labels {
  left: 0;
  top: -18px;
  width: 57px;
  height: 14px;
}

.deviatore-line-stage[data-label-state="top"]
.deviatore-label-l {
  right: calc(50% + 10px);
  bottom: 0;
  text-align: right;
}

.deviatore-line-stage[data-label-state="top"]
.deviatore-label-r {
  left: calc(50% + 10px);
  bottom: 0;
  text-align: left;
}

/* =====================================================
   STATE LABEL — BOTTOM
   ===================================================== */

.deviatore-line-stage[data-label-state="bottom"] {
  margin-bottom: 18px;
}

.deviatore-line-stage[data-label-state="bottom"]
.deviatore-line-state-labels {
  left: 0;
  top: 18px;
  width: 57px;
  height: 14px;
}

.deviatore-line-stage[data-label-state="bottom"]
.deviatore-label-l {
  right: calc(50% + 10px);
  top: 0;
  text-align: right;
}

.deviatore-line-stage[data-label-state="bottom"]
.deviatore-label-r {
  left: calc(50% + 10px);
  top: 0;
  text-align: left;
}

/* =====================================================
   RIGA
   ===================================================== */

.deviatore-line-row {
  width: 57px;
  height: 18px;
  gap: 0;
}

/* =====================================================
   LINEETTA
   ===================================================== */

.deviatore-line-track {
  position: relative;
  width: 57px;
  height: 18px;
  background: transparent;
  cursor: pointer;
  pointer-events: auto !important;
}

.deviatore-line-track::before {
  content: '';
  position: absolute;
  left: 0;
  top: 50%;
  width: 57px;
  height: 3px;
  transform: translateY(-50%);
  background: var(--mz-dev-shape-color, #3e6da9);
  border-radius: 1.5px;
  pointer-events: none;
}

/* Area di intercettazione estesa: il click resta sul track, quindi il calcolo
   della zona in mzUIX.js non cambia. Fuori dal track i valori ratio sono <0 e >1. */
.deviatore-line-track::after {
  content: '';
  position: absolute;
  inset: -6px -10px;
  background: transparent;
  pointer-events: auto !important;
  z-index: 0;
}

/* =====================================================
   TRE MARKER DI STATO
   ===================================================== */

.deviatore-line-dot {
  position: absolute;
  width: 8px;
  height: 8px;
  top: 50%;
  transform: translate(-50%, -50%);
  box-sizing: border-box;
  pointer-events: none;
  z-index: 1;
}

/* -----------------------------------------------------
   FORMA CIRCLE
   ----------------------------------------------------- */

.deviatore-line-dot.marker-circle {
  border-radius: 50%;
}

/* -----------------------------------------------------
   FORMA SQUARE
   ----------------------------------------------------- */

.deviatore-line-dot.marker-square {
  border-radius: 1px;
}

/* -----------------------------------------------------
   FORMA DIAMOND
   ----------------------------------------------------- */

.deviatore-line-dot.marker-diamond {
  width: 6px;
  height: 6px;
  border-radius: 0;
  transform: translate(-50%, -50%) rotate(45deg);
}

/* =====================================================
   POSIZIONE E COLORE DEI TRE MARKER
   ===================================================== */

/* sinistra — colorA */
.deviatore-line-dot.dot-a {
  left: 0;
  background: var(--mz-dev-color-a, #eca038);
}

/* centro — OFF */
.deviatore-line-dot.dot-off {
  left: 50%;
  background: var(--mz-dev-shape-color, #3e6da9);
}

/* destra — colorB */
.deviatore-line-dot.dot-b {
  left: 100%;
  background: var(--mz-dev-color-b, #3e6da9);
}

/* =====================================================
   SELETTORE GRANDE
   ===================================================== */

.deviatore-line-marker {
  position: absolute;
  width: 16px;
  height: 16px;
  top: 50%;
  left: 50%;
  background: var(--mz-dev-marker-fill, transparent);
  border: 2px solid var(--mz-dev-shape-color, #3e6da9);
  box-sizing: border-box;
  box-shadow: 0 1px 2px rgba(0,0,0,0.12);
  transition:
    left 0.10s cubic-bezier(0.4, 0, 0.6, 1);
  pointer-events: none;
  z-index: 2;
  transform: translate(-50%, -50%);
}

/* -----------------------------------------------------
   SELETTORE CIRCLE
   ----------------------------------------------------- */

.deviatore-line-marker.marker-circle {
  border-radius: 50%;
}

/* -----------------------------------------------------
   SELETTORE SQUARE
   ----------------------------------------------------- */

.deviatore-line-marker.marker-square {
  border-radius: 2px;
}

/* -----------------------------------------------------
   SELETTORE DIAMOND
   ----------------------------------------------------- */

.deviatore-line-marker.marker-diamond {
  width: 12px;
  height: 12px;
  border-radius: 0;
  transform: translate(-50%, -50%) rotate(45deg);
}

/* =====================================================
   POSIZIONI DEL SELETTORE
   ===================================================== */

.deviatore-line-row[data-state="posLorT"]
.deviatore-line-marker {
  left: 0;
}

.deviatore-line-row[data-state="posOff"]
.deviatore-line-marker {
  left: 50%;
}

.deviatore-line-row[data-state="posRorB"]
.deviatore-line-marker {
  left: 100%;
}

/* =====================================================
   STATE LABEL ATTIVE
   ===================================================== */

.deviatore-line-state-labels[data-state="posLorT"]
.deviatore-label-l {
  color: var(--mz-dev-color-a, #eca038);
  font-weight: 700;
  font-size: 12px;
}

.deviatore-line-state-labels[data-state="posRorB"]
.deviatore-label-r {
  color: var(--mz-dev-color-b, #3e6da9);
  font-weight: 700;
  font-size: 12px;
}

/* =====================================================
   DEVIATORE — SHAPE BEAM (bilancere)
   Riusa il markup line: tre punti e barra dello stesso colore,
   etichette neutre, selettore bianco in Off e colorato negli stati attivi.
   ===================================================== */

.deviatore-beam .deviatore-line-dot.dot-a,
.deviatore-beam .deviatore-line-dot.dot-off,
.deviatore-beam .deviatore-line-dot.dot-b {
  background: var(--mz-dev-shape-color, #3e6da9);
}

.deviatore-beam .deviatore-line-marker {
  background: #ffffff;
  transition:
    left 0.10s cubic-bezier(0.4, 0, 0.6, 1),
    background-color 0.15s ease;
}

.deviatore-beam .deviatore-line-row[data-state="posLorT"] .deviatore-line-marker,
.deviatore-beam .deviatore-line-row[data-state="posRorB"] .deviatore-line-marker {
  background: var(--mz-dev-beam-active, #2ecc71);
}

/* Etichette sempre uguali: nessun colore, peso o dimensione per lo stato attivo */
.deviatore-beam .deviatore-line-state-labels[data-state] .deviatore-label-l,
.deviatore-beam .deviatore-line-state-labels[data-state] .deviatore-label-r {
  color: var(--mz-dev-label-color, #555555);
  font-weight: var(--mz-dev-label-weight, 400);
  font-style: var(--mz-dev-label-style, normal);
  font-size: var(--mz-dev-label-size, 11px);
}

/* === DEVIATORE SHAPE LINE - FINE === */


/* =====================================================
   VERTICAL LIST SLIDER — controllo lista con slider verticale
   ===================================================== */

.vls-wrapper {
  display: inline-flex;
  flex-direction: column;
  gap: 4px;
  pointer-events: auto !important;
}

.vls-wrapper.main-label-left   { align-items: flex-start; }
.vls-wrapper.main-label-center { align-items: center; }
.vls-wrapper.main-label-right  { align-items: flex-end; }

.vls-main-label {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

/* --- header accordion --- */
.vls-header {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  cursor: pointer;
  pointer-events: auto !important;
  user-select: none;
  padding: 2px 0;
}

.vls-header-text {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

.vls-header-value {
  font-size: 12px;
  font-weight: 700;
  color: #3e6da9;
  white-space: nowrap;
}

.vls-header-arrow {
  width: 10px;
  height: 10px;
  flex-shrink: 0;
  color: #3e6da9;
  transition: transform 0.25s ease;
}

.vls-header-arrow.expanded {
  transform: rotate(180deg);
}

/* --- body collassabile --- */
.vls-body {
  max-height: 0;
  overflow: hidden;
  transition: max-height 0.3s ease-out;
}

.vls-body.expanded {
  max-height: 600px;
  transition: max-height 0.4s ease-in;
}

/* --- riga track + labels --- */
.vls-row {
  display: inline-flex;
  align-items: flex-start;
  gap: 0;
  pointer-events: auto !important;
  padding: 4px 0;
}

/* --- track container --- */
.vls-track-col {
  position: relative;
  width: 20px;
  flex-shrink: 0;
  display: flex;
  justify-content: center;
}

.vls-track-bg {
  position: absolute;
  left: 50%;
  transform: translateX(-50%);
  width: 3px;
  border-radius: 2px;
  background: #ddd;
  top: 0;
  bottom: 0;
}

.vls-track-fill {
  position: absolute;
  left: 50%;
  transform: translateX(-50%);
  width: 3px;
  border-radius: 2px;
  bottom: 0;
  transition: height 0.2s ease;
}

.vls-thumb {
  position: absolute;
  left: 50%;
  transform: translateX(-50%) translateY(-50%);
  transition: top 0.2s cubic-bezier(0.4, 0, 0.2, 1);
  pointer-events: none;
  z-index: 2;
}

/* --- notch tick sul track --- */
.vls-notch-tick {
  position: absolute;
  left: 50%;
  transform: translateX(-50%);
  height: 1px;
  background: rgba(0,0,0,0.18);
  z-index: 1;
}

/* --- colonna labels --- */
.vls-labels-col {
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  padding-left: 8px;
}

.vls-label-item {
  font-size: 12px;
  color: #888;
  white-space: nowrap;
  line-height: 1;
  cursor: pointer;
  padding: 1px 0;
  pointer-events: auto !important;
  transition: color 0.15s, font-weight 0.15s;
}

.vls-label-item.active {
  font-weight: 700;
}

/* disabled */
.vls-wrapper.disabled .vls-row {
  opacity: 0.4;
  cursor: not-allowed;
  pointer-events: none !important;
}

/* === VERTICAL LIST SLIDER - FINE === */


/* =====================================================
   SEGMENTED CONTROL
   ===================================================== */

.mz-seg-wrapper {
  display: inline-flex;
  flex-direction: column;
  gap: 4px;
  pointer-events: auto !important;
}

.mz-seg-wrapper.main-label-left   { align-items: flex-start; }
.mz-seg-wrapper.main-label-center { align-items: center; }
.mz-seg-wrapper.main-label-right  { align-items: flex-end; }

.mz-seg-main-label {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

/* --- TRACK ORIZZONTALE (default) --- */
.mz-seg-track {
  display: inline-flex;
  position: relative;
  border-bottom: 1px solid rgba(62, 109, 169, 0.50);
  padding: 0;
  gap: 0;
  pointer-events: auto !important;
  background: var(--mz-seg-bg, transparent);
}

/* --- TRACK VERTICALE base --- */
.mz-seg-track.vertical {
  flex-direction: column;
  border-bottom: none;
}

/* lineetta guida sinistra (labelPosition: left) */
.mz-seg-track.vertical.guide-left {
  border-left: 1px solid rgba(62, 109, 169, 0.50);
}

/* lineetta guida destra (labelPosition: right) */
.mz-seg-track.vertical.guide-right {
  border-right: 1px solid rgba(62, 109, 169, 0.50);
}

/* center: nessun bordo guida */
.mz-seg-track.vertical.guide-center {
  border: none;
}

/* --- BOTTONE BASE --- */
.mz-seg-btn {
  font-family: inherit;
  padding: 4px 16px;
  font-size: 11px;
  font-weight: 500;
  color: #666;
  cursor: pointer;
  border: none;
  background: transparent;
  white-space: nowrap;
  user-select: none;
  pointer-events: auto !important;
  position: relative;
  display: flex;
  align-items: center;
  min-height: 28px;
  border-radius: 3px 3px 0 0;
  letter-spacing: 0.3px;
  transition: all 0.2s ease, color 0.2s ease, background-color 0.2s ease;
}
.mz-seg-btn:focus-visible {
  outline: 2px solid var(--mz-seg-active-color, #3e6da9);
  outline-offset: 2px;
}

.mz-seg-track.vertical .mz-seg-btn {
  border-radius: 3px;
  min-height: 24px;
  padding: 4px 14px;
}

/* --- HOVER orizzontale --- */
.mz-seg-btn:hover:not(.active) {
  background: #c8c8c8;
  color: #333;
  border-radius: 3px 3px 0 0;
}

.mz-seg-btn:hover:not(.active)::after {
  content: '';
  position: absolute;
  bottom: -1px;
  left: 0;
  right: 0;
  height: 3px;
  background: #888;
  border-radius: 2px 2px 0 0;
}

/* --- HOVER verticale (tutte le varianti) --- */
.mz-seg-track.vertical .mz-seg-btn:hover:not(.active) {
  background: #c8c8c8;
  color: #333;
  border-radius: 3px;
}

/* hover left: barretta grigia a sinistra */
.mz-seg-track.vertical.guide-left .mz-seg-btn:hover:not(.active)::after {
  content: '';
  position: absolute;
  left: -1px;
  top: 0;
  bottom: 0;
  right: auto;
  width: 3px;
  height: 100%;
  background: #888;
  border-radius: 0 2px 2px 0;
}

/* hover right: barretta grigia a destra */
.mz-seg-track.vertical.guide-right .mz-seg-btn:hover:not(.active)::after {
  content: '';
  position: absolute;
  right: -1px;
  top: 0;
  bottom: 0;
  left: auto;
  width: 3px;
  height: 100%;
  background: #888;
  border-radius: 2px 0 0 2px;
}

/* hover center: barretta grigia sotto */
.mz-seg-track.vertical.guide-center .mz-seg-btn:hover:not(.active)::after {
  content: '';
  position: absolute;
  bottom: -1px;
  left: 0;
  right: 0;
  height: 3px;
  width: 100%;
  background: #888;
  border-radius: 2px 2px 0 0;
}

/* --- ACTIVE orizzontale --- */
.mz-seg-btn.active {
  font-weight: 700;
  background-color: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.20);
  color: var(--mz-seg-active-color, #3e6da9);
  border-radius: 3px 3px 0 0;
}

.mz-seg-btn.active::after {
  content: '';
  position: absolute;
  bottom: -1px;
  left: 0;
  right: 0;
  height: 3px;
  background: var(--mz-seg-active-color, #3e6da9);
  border-radius: 2px 2px 0 0;
}

/* --- ACTIVE verticale left: barretta accent a sinistra --- */
.mz-seg-track.vertical.guide-left .mz-seg-btn.active {
  border-radius: 0 3px 3px 0;
  background-color: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.20);
  color: var(--mz-seg-active-color, #3e6da9);
  font-weight: 700;
}

.mz-seg-track.vertical.guide-left .mz-seg-btn.active::after {
  content: '';
  position: absolute;
  left: -1px;
  top: 0;
  bottom: 0;
  right: auto;
  width: 3px;
  height: 100%;
  background: var(--mz-seg-active-color, #3e6da9);
  border-radius: 0 2px 2px 0;
}

/* --- ACTIVE verticale right: barretta accent a destra --- */
.mz-seg-track.vertical.guide-right .mz-seg-btn.active {
  border-radius: 3px 0 0 3px;
  background-color: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.20);
  color: var(--mz-seg-active-color, #3e6da9);
  font-weight: 700;
}

.mz-seg-track.vertical.guide-right .mz-seg-btn.active::after {
  content: '';
  position: absolute;
  right: -1px;
  top: 0;
  bottom: 0;
  left: auto;
  width: 3px;
  height: 100%;
  background: var(--mz-seg-active-color, #3e6da9);
  border-radius: 2px 0 0 2px;
}

/* --- ACTIVE verticale center: barretta accent sotto --- */
.mz-seg-track.vertical.guide-center .mz-seg-btn.active {
  border-radius: 3px 3px 0 0;
  background-color: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.20);
  color: var(--mz-seg-active-color, #3e6da9);
  font-weight: 700;
}

.mz-seg-track.vertical.guide-center .mz-seg-btn.active::after {
  content: '';
  position: absolute;
  bottom: -1px;
  left: 0;
  right: 0;
  top: auto;
  width: 100%;
  height: 3px;
  background: var(--mz-seg-active-color, #3e6da9);
  border-radius: 2px 2px 0 0;
}

.mz-seg-wrapper.disabled .mz-seg-track {
  opacity: 0.4;
  pointer-events: none !important;
}

/* === SEGMENTED CONTROL - FINE === */


/* =====================================================
   PRIORITY PICKER
   ===================================================== */

.mz-pp-wrapper {
  display: inline-flex;
  flex-direction: column;
  gap: 4px;
  pointer-events: auto !important;
}

.mz-pp-wrapper.main-label-left   { align-items: flex-start; }
.mz-pp-wrapper.main-label-center { align-items: center; }
.mz-pp-wrapper.main-label-right  { align-items: flex-end; }

.mz-pp-main-label {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

.mz-pp-row {
  display: inline-flex;
  align-items: flex-end;
  gap: 3px;
  pointer-events: auto !important;
}

.mz-pp-seg {
  border-radius: 2px;
  cursor: pointer;
  transition: opacity 0.12s, transform 0.1s;
  pointer-events: auto !important;
  flex-shrink: 0;
}

.mz-pp-seg:hover { transform: scaleY(1.12); }

.mz-pp-value-label {
  font-size: 11px;
  color: #888;
  margin-left: 8px;
  white-space: nowrap;
  align-self: flex-end;
  padding-bottom: 1px;
}

.mz-pp-wrapper.disabled .mz-pp-row {
  opacity: 0.4;
  pointer-events: none !important;
}

/* === PRIORITY PICKER - FINE === */


/* =====================================================
   CONDITIONAL REVEAL
   ===================================================== */

.mz-cr-wrapper {
  display: flex;
  flex-direction: column;
  gap: 0;
  pointer-events: auto !important;
  width: 100%;
}

.mz-cr-trigger {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
  user-select: none;
  padding: 3px 0;
  pointer-events: auto !important;
}

.mz-cr-toggle {
  width: 28px;
  height: 16px;
  border-radius: 8px;
  background: #ccc;
  position: relative;
  transition: background 0.2s;
  flex-shrink: 0;
  pointer-events: auto !important;
}

.mz-cr-toggle.on {
  background: var(--mz-cr-color, #3e6da9);
}

.mz-cr-knob {
  position: absolute;
  width: 12px;
  height: 12px;
  background: #fff;
  border-radius: 50%;
  top: 2px;
  left: 2px;
  transition: left 0.2s;
  box-shadow: 0 1px 2px rgba(0,0,0,0.2);
  pointer-events: none;
}

.mz-cr-toggle.on .mz-cr-knob { left: 14px; }

.mz-cr-trigger-label {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

.mz-cr-reveal {
  overflow: hidden;
  max-height: 0;
  opacity: 0;
  transition: max-height 0.28s ease, opacity 0.2s ease;
  padding-left: 36px;
}

.mz-cr-reveal.open {
  max-height: 600px;
  opacity: 1;
}

.mz-cr-inner {
  display: flex;
  flex-direction: column;
  padding: 6px 0 4px 10px;
  border-left: 2px solid rgba(62, 109, 169, 0.35);
  margin-top: 4px;
}

.mz-cr-wrapper.disabled .mz-cr-trigger {
  opacity: 0.4;
  pointer-events: none !important;
}

/* === CONDITIONAL REVEAL - FINE === */


/* =====================================================
   LINKED SLIDER
   ===================================================== */

.mz-ls-wrapper {
  display: flex;
  flex-direction: column;
  gap: 4px;
  pointer-events: auto !important;
  width: 100%;
}

.mz-ls-wrapper.main-label-left   { align-items: flex-start; }
.mz-ls-wrapper.main-label-center { align-items: center; }
.mz-ls-wrapper.main-label-right  { align-items: flex-end; }

.mz-ls-main-label {
  font-size: 12px;
  font-weight: 500;
  color: #555;
  white-space: nowrap;
}

.mz-ls-body {
  display: flex;
  flex-direction: column;
  gap: 4px;
  width: 100%;
}

.mz-ls-row {
  display: flex;
  align-items: center;
  gap: 8px;
}

.mz-ls-row-label {
  font-size: 11px;
  color: #777;
  width: 44px;
  text-align: right;
  flex-shrink: 0;
  white-space: nowrap;
}

.mz-ls-track {
  flex: 1;
  height: 4px;
  background: #ddd;
  border-radius: 2px;
  position: relative;
  cursor: pointer;
  pointer-events: auto !important;
}

.mz-ls-fill-lo {
  position: absolute;
  height: 100%;
  left: 0;
  background: #ddd;
  border-radius: 2px;
}

.mz-ls-fill-hi {
  position: absolute;
  height: 100%;
  right: 0;
  background: #ddd;
  border-radius: 2px;
}

.mz-ls-fill-active {
  position: absolute;
  height: 100%;
  border-radius: 2px;
  transition: background 0.15s;
}

.mz-ls-thumb {
  position: absolute;
  width: 14px;
  height: 14px;
  background: #fff;
  border: 2px solid #3e6da9;
  border-radius: 50%;
  top: 50%;
  transform: translate(-50%, -50%);
  cursor: grab;
  transition: box-shadow 0.12s, border-color 0.15s;
  pointer-events: auto !important;
  z-index: 2;
}

.mz-ls-thumb:active { cursor: grabbing; }
.mz-ls-thumb:hover  { box-shadow: 0 0 0 4px rgba(62,109,169,0.15); }
.mz-ls-thumb.error  { border-color: #c0392b; }

.mz-ls-value {
  font-size: 12px;
  font-weight: 500;
  color: #3e6da9;
  width: 48px;
  text-align: left;
  flex-shrink: 0;
  font-family: monospace;
  transition: color 0.15s;
}

.mz-ls-value.error { color: #c0392b; }

.mz-ls-link {
  display: flex;
  justify-content: flex-start;
  align-items: center;
  padding-left: 52px;
  position: relative;
  height: 14px;
}

.mz-ls-link-line {
  position: absolute;
  left: 52px;
  top: 0;
  bottom: 0;
  width: 1px;
  background: rgba(62,109,169,0.3);
  transform: translateX(calc(var(--mz-ls-link-x, 50%) - 1px));
}

.mz-ls-link-icon {
  position: relative;
  z-index: 1;
  width: 16px;
  height: 16px;
  border-radius: 50%;
  background: #eef4fb;
  border: 0.5px solid #b0c8e8;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 9px;
  color: #3e6da9;
  flex-shrink: 0;
  margin-left: calc(var(--mz-ls-link-x, 50%) - 8px);
  transition: margin-left 0.12s;
}

.mz-ls-error-msg {
  font-size: 10px;
  color: #c0392b;
  padding-left: 52px;
  display: none;
}

.mz-ls-error-msg.show { display: block; }

/* orientamento verticale */
.mz-ls-wrapper.vertical .mz-ls-body {
  flex-direction: row;
  align-items: flex-end;
  gap: 0;
}

.mz-ls-wrapper.vertical .mz-ls-row {
  flex-direction: column;
  align-items: center;
  gap: 6px;
  width: auto;
}

.mz-ls-wrapper.vertical .mz-ls-row-label {
  width: auto;
  text-align: center;
  writing-mode: horizontal-tb;
}

.mz-ls-wrapper.vertical .mz-ls-track {
  flex: none;
  width: 4px;
  height: 120px;
  cursor: pointer;
}

.mz-ls-wrapper.vertical .mz-ls-fill-active {
  left: 0 !important;
  right: auto !important;
  width: 100% !important;
  bottom: 0;
  top: auto !important;
  transition: height 0.15s;
}

.mz-ls-wrapper.vertical .mz-ls-thumb {
  left: 50% !important;
  transform: translate(-50%, 50%) !important;
  bottom: 0;
  top: auto !important;
}

.mz-ls-wrapper.vertical .mz-ls-value {
  text-align: center;
  width: auto;
}

.mz-ls-wrapper.vertical .mz-ls-link {
  flex-direction: column;
  padding-left: 0;
  padding-bottom: 0;
  width: 24px;
  height: 120px;
  justify-content: center;
  align-self: flex-end;
  margin-bottom: 0;
}

.mz-ls-wrapper.vertical .mz-ls-link-line {
  position: absolute;
  left: 50%;
  top: 0;
  bottom: 0;
  width: 1px;
  height: auto;
  transform: none;
}

.mz-ls-wrapper.vertical .mz-ls-link-icon {
  margin-left: 0 !important;
  margin-top: calc(var(--mz-ls-link-y, 50%) - 8px);
}

.mz-ls-wrapper.vertical .mz-ls-error-msg {
  padding-left: 0;
  text-align: center;
  margin-top: 4px;
}

.mz-ls-wrapper.disabled .mz-ls-body {
  opacity: 0.4;
  pointer-events: none !important;
}

/* === LINKED SLIDER - FINE === */


/* =============================================================
   VARIABLE SUPPLIER — stile dei campi obbligatori e facoltativi
   Target box nativi jamovi (VariableSupplier/VariablesListBox),
   NON gestiti da mzTabbedControl.js. Applicato via classi da
   main.js::styleVariableSupplier() sui sei modelli di Metafora.
   Palette derivata dal blu/arancio jamovi gia' in uso nel resto
   del file (deviatori, badge, pillola debug).
   ============================================================== */

.mz-vs-source {
  border-radius: 8px !important;
  border: 2px solid #3e6da9 !important;
  /* border: 2px solid rgba(62, 109, 169, 0.55) !important; */
  background-color: #ffffff !important;
  box-shadow: none !important;
}

.mz-vs-transfer-btn {
  border-radius: 6px !important;
  border: 1px solid rgba(62, 109, 169, 0.45) !important;
  background: #f5f8fc !important;
  box-shadow: 0 1px 3px rgba(62, 109, 169, 0.15) !important;
  transition: all 0.2s ease-in-out !important;
}

.mz-vs-transfer-btn:hover {
  background: #eef4fb !important;
  border-color: #3e6da9 !important;
}

.mz-vs-target {
  border-radius: 8px !important;
  /* I campi singoli non devono generare barre per pochi pixel di eccedenza. */
  overflow: hidden !important;
  transition: background-color 0.25s ease, border-color 0.25s ease, box-shadow 0.25s ease !important;
}

/* Le classi native distinguono la capacità effettiva, anche con lista vuota. */
.mz-vs-target.multi-item {
  overflow-x: hidden !important;
  overflow-y: auto !important;
}

/* Pista interna agli angoli arrotondati, senza alterare la griglia nativa.
   Il margine riguarda la barra, non il padding delle righe del supplier. */
.mz-vs-target.multi-item::-webkit-scrollbar,
.mz-vs-source::-webkit-scrollbar {
  width: 10px;
}

.mz-vs-target.multi-item::-webkit-scrollbar-track:vertical,
.mz-vs-source::-webkit-scrollbar-track:vertical {
  margin-top: 8px;
  margin-bottom: 8px;
  background: transparent;
}

.mz-vs-target.multi-item::-webkit-scrollbar-thumb,
.mz-vs-source::-webkit-scrollbar-thumb {
  background: rgba(62, 109, 169, 0.45);
  background-clip: padding-box;
  border: 2px solid transparent;
  border-radius: 8px;
}

.mz-vs-target.multi-item::-webkit-scrollbar-thumb:hover,
.mz-vs-source::-webkit-scrollbar-thumb:hover {
  background-color: rgba(62, 109, 169, 0.7);
}

.mz-vs-target.multi-item::-webkit-scrollbar-button,
.mz-vs-source::-webkit-scrollbar-button {
  display: none;
}

/* Required — vuoto: bordo pieno blu tenue. Compilato: bordo pieno blu
   saturo + sfondo/ombra piu' marcati (stesso blu di brand del progetto). */
.mz-vs-required.mz-vs-empty {
  border: 2px solid rgba(62, 109, 169, 0.55) !important;
  background: rgba(62, 109, 169, 0.06) !important;
}

.mz-vs-required.mz-vs-filled {
  border: 2px solid #3e6da9 !important;
  background: rgba(62, 109, 169, 0.14) !important;
  box-shadow: 0 2px 6px rgba(62, 109, 169, 0.20) !important;
}

/* Optional — vuoto: bordo tratteggiato grigio (segnala non obbligatorio).
   Compilato: bordo arancio (stesso accento jamovi usato per i deviatori
   e i badge del progetto), per distinguerlo a colpo d'occhio dal required. */
.mz-vs-optional.mz-vs-empty {
  border: 2px dashed rgba(148, 163, 184, 0.55) !important;
  background: rgba(148, 163, 184, 0.06) !important;
}

.mz-vs-optional.mz-vs-filled {
  border: 2px solid #eca038 !important;
  background: rgba(236, 160, 56, 0.12) !important;
  box-shadow: 0 2px 6px rgba(236, 160, 56, 0.18) !important;
}

/* Ghost-label (placeholder nativo jamovi) - Required */
/* Il filler sticky nativo usa z-index 2000; l'anteprima deve precederlo.
   La preview resta pointer-events:none come previsto dal drag nativo. */
body:has(.mz-vs-target) > .silky-item-dragging {
  z-index: 2100;
}

/* La classe è applicata e rimossa dal gestore nativo di trascinamento. */
.mz-vs-target .item-hovering .column-ghost-label {
  visibility: hidden;
}

.mz-vs-required.mz-vs-empty .column-ghost-label {
  color: rgba(62, 109, 169, 0.7) !important; /* Blu desaturato e trasparente */
  font-weight: 400 !important; /* Peso normale */
  font-style: italic !important; /* Corsivo per distinguere dai dati reali */
}

/* Ghost-label (placeholder nativo jamovi) - Optional */
.mz-vs-moderator .column-ghost-label,
.mz-vs-rob .column-ghost-label {
  white-space: pre-line;
  line-height: 1.35;
}
.mz-vs-optional.mz-vs-empty .column-ghost-label {
  color: rgba(148, 163, 184, 0.8) !important; /* Grigio-blu slate */
  font-weight: 400 !important; 
  font-style: italic !important;
}

/* =======================================================
   Sostituisco freccia sinistra e destra con freccia tipo
   Doppio Chevron e aggiungo interattività (hover, active)
   ======================================================= */

/* Impostazioni base icona e transizioni */
.jmv-toolbar .jmv-toolbar-button.jmv-variable-transfer > .jmv-toolbar-button-icon {
  width: 22px !important;
  height: 22px !important;
  background-position: center !important;
  background-repeat: no-repeat !important;
  opacity: 0.65;
  transition: opacity 0.2s ease, transform 0.1s ease !important;
}

/* Piena visibilità al passaggio del mouse sul bottone genitore */
.mz-vs-transfer-btn:hover > .jmv-toolbar-button-icon {
  opacity: 1;
}

/* Feedback tattile al momento del click */
.mz-vs-transfer-btn:active > .jmv-toolbar-button-icon {
  transform: scale(0.85);
}

/* Freccia Destra (Blu e Arancio #eca038) */
.jmv-toolbar.arrow-right .jmv-toolbar-button.jmv-variable-transfer > .jmv-toolbar-button-icon {
  background-image: url("data:image/svg+xml,%3Csvg%20xmlns='http://www.w3.org/2000/svg'%20viewBox='0%200%2024%2024'%20fill='none'%3E%3Cpath%20d='M11%206.3%2016.7%2012%2011%2017.7'%20stroke='%233E6DA9'%20stroke-width='3.75'%20stroke-linecap='round'%20stroke-linejoin='round'/%3E%3Cpath%20d='M5.3%206.3%2011%2012%205.3%2017.7'%20stroke='%23ECA038'%20stroke-width='3.75'%20stroke-linecap='round'%20stroke-linejoin='round'/%3E%3C/svg%3E") !important;
}

/* Freccia Sinistra (Arancio #eca038 e Blu) */
.jmv-toolbar.arrow-left .jmv-toolbar-button.jmv-variable-transfer > .jmv-toolbar-button-icon {
  background-image: url("data:image/svg+xml,%3Csvg%20xmlns='http://www.w3.org/2000/svg'%20viewBox='0%200%2024%2024'%20fill='none'%3E%3Cpath%20d='M11%206.3%205.3%2012%2011%2017.7'%20stroke='%23ECA038'%20stroke-width='3.75'%20stroke-linecap='round'%20stroke-linejoin='round'/%3E%3Cpath%20d='M18.7%206.3%2013%2012%2018.7%2017.7'%20stroke='%233E6DA9'%20stroke-width='3.75'%20stroke-linecap='round'%20stroke-linejoin='round'/%3E%3C/svg%3E") !important;
}

/* === VARIABLE SUPPLIER - FINE === */

/* Variante discreta per selettori che sostituiscono un'etichetta del supplier. */
.mz-seg-wrapper.mz-seg-compact {
  gap: 0;
}
.mz-seg-wrapper.mz-seg-compact .mz-seg-track {
  background: transparent;
  border: none;
  gap: 12px;
}
.mz-seg-wrapper.mz-seg-compact .mz-seg-track .mz-seg-btn {
  min-height: 0;
  padding: 3px 0 5px;
  font-size: 12px;
  line-height: 1.35;
  letter-spacing: normal;
  font-weight: 400;
  border-radius: 0;
  background: transparent;
  transition: color 0.15s ease;
}
.mz-seg-wrapper.mz-seg-compact .mz-seg-track .mz-seg-btn.active {
  font-weight: 500;
  background: transparent;
}
.mz-seg-wrapper.mz-seg-compact .mz-seg-track .mz-seg-btn:hover:not(.active) {
  color: var(--mz-seg-active-color, #3e6da9);
  background: transparent;
}
.mz-seg-wrapper.mz-seg-compact .mz-seg-track:not(.vertical) .mz-seg-btn::after {
  bottom: 0;
  height: 2px;
  border-radius: 1px;
}
.mz-seg-wrapper.mz-seg-compact .mz-seg-track .mz-seg-btn:hover:not(.active)::after {
  background: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.3);
}

/* Pillole leggere: il bordo trasparente conserva le dimensioni al cambio di scelta. */
.mz-seg-wrapper.mz-seg-pill {
  gap: 0;
}
.mz-seg-wrapper.mz-seg-pill .mz-seg-track {
  background: transparent;
  border: none;
  gap: 6px;
}
.mz-seg-wrapper.mz-seg-pill .mz-seg-track .mz-seg-btn {
  min-height: 0;
  padding: 3px 9px;
  font-size: 12px;
  line-height: 1.35;
  letter-spacing: normal;
  font-weight: 400;
  border: 1px solid transparent;
  border-radius: 999px;
  background: transparent;
  transition: color 0.15s ease, background-color 0.15s ease, border-color 0.15s ease;
}
.mz-seg-wrapper.mz-seg-pill .mz-seg-track .mz-seg-btn.active {
  font-weight: 400;
  border-color: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.6);
  background: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.07);
}
.mz-seg-wrapper.mz-seg-pill .mz-seg-track .mz-seg-btn:hover:not(.active) {
  color: var(--mz-seg-active-color, #3e6da9);
  background: rgba(var(--mz-seg-active-rgb, 62, 109, 169), 0.035);
}
.mz-seg-wrapper.mz-seg-pill .mz-seg-track .mz-seg-btn::after {
  content: none;
}

/* Linguetta aperta in basso, con fondo trasparente come le etichette native. */
.mz-seg-wrapper.mz-seg-tab {
  gap: 0;
}
.mz-seg-wrapper.mz-seg-tab .mz-seg-track {
  background: transparent;
  border: none;
  gap: 2px;
}
/* Il centro si riferisce alla larghezza del supplier, senza il padding del tab custom. */
.mz-control-wrapper.mz-native-label-control {
  width: var(--mz-native-target-width, 100%) !important;
  padding-right: 0 !important;
}
.mz-native-label-control > .control-item {
  width: 100%;
  align-self: stretch;
}
.mz-native-label-control .mz-seg-wrapper.mz-seg-tab {
  width: 100%;
  align-items: center;
}
.mz-seg-wrapper.mz-seg-tab .mz-seg-track .mz-seg-btn {
  min-height: 0;
  padding: 3px 9px 2px;
  font-size: 12px;
  line-height: 1.35;
  letter-spacing: normal;
  font-weight: 400;
  border: 1px solid transparent;
  border-bottom: none;
  border-radius: 9px 9px 0 0;
  background: transparent;
  transition: color 0.15s ease, border-color 0.15s ease;
}
.mz-seg-wrapper.mz-seg-tab .mz-seg-track .mz-seg-btn.active {
  font-weight: 400;
  border-color: var(--mz-seg-active-color, #3e6da9);
  background: transparent;
}
.mz-seg-wrapper.mz-seg-tab .mz-seg-track .mz-seg-btn:hover:not(.active) {
  color: var(--mz-seg-active-color, #3e6da9);
  background: transparent;
}
.mz-seg-wrapper.mz-seg-tab .mz-seg-track .mz-seg-btn::after {
  content: none;
}

/* Parentesi laterali: lo spazio resta riservato anche nella scelta inattiva. */
.mz-seg-wrapper.mz-seg-bracket {
  gap: 0;
}
.mz-native-label-control .mz-seg-wrapper.mz-seg-bracket {
  width: 100%;
  align-items: center;
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track {
  background: transparent;
  border: none;
  gap: 2px;
  max-width: 100%;
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn {
  display: grid;
  min-height: 0;
  padding: 3px 10px;
  font-size: 12.8px;
  line-height: 1.35;
  letter-spacing: normal;
  font-weight: 400;
  border: none;
  border-radius: 0;
  background: transparent;
  transition: color 0.15s ease;
}
.mz-seg-bracket .mz-seg-text,
.mz-seg-bracket .mz-seg-text-size {
  grid-area: 1 / 1;
}
.mz-seg-bracket .mz-seg-text-size {
  font-size: 13.5px;
  visibility: hidden;
  pointer-events: none;
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn.active {
  font-size: 13.5px;
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn.active,
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn:hover:not(.active) {
  background: transparent;
  font-weight: 400;
  color: var(--mz-seg-active-color, #3e6da9);
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn::before,
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn::after {
  content: '';
  position: absolute;
  top: 4px;
  bottom: 4px;
  width: 3px;
  height: auto;
  background: transparent;
  border: 1px solid transparent;
  border-radius: 0;
  pointer-events: none;
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn::before {
  left: 1px;
  border-right: none;
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn::after {
  left: auto;
  right: 1px;
  border-left: none;
}
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn.active::before,
.mz-seg-wrapper.mz-seg-bracket .mz-seg-track .mz-seg-btn.active::after {
  border-color: var(--mz-seg-active-color, #3e6da9);
}

/* Generic ES: riga di ingresso stabile, senza cambiare le altre varianti UIX. */
.mz-control-wrapper.mz-native-label-control.mz-effectsize-input-header {
  display: flex !important;
  flex-direction: row !important;
  align-items: center;
  justify-content: flex-start;
  gap: 8px;
}
.mz-effectsize-input-header > .control-item {
  width: auto !important;
  flex: 0 0 auto;
  margin-left: 0 !important;
}
.mz-effectsize-input-header .mz-seg-wrapper.mz-seg-bracket {
  width: auto;
  align-items: flex-start;
}
.mz-effectsize-input-header .mz-seg-track {
  justify-content: flex-start;
}
.mz-effectsize-input-header .mz-seg-bracket .mz-seg-track .mz-seg-btn {
  padding-left: 6px;
  padding-right: 6px;
}
.mz-effectsize-input-header .mz-seg-bracket .mz-seg-track .mz-seg-btn.active,
.mz-effectsize-input-header .mz-seg-bracket .mz-seg-text-size {
  font-weight: 700;
}
.mz-effectsize-ci-level {
  display: inline-flex;
  align-items: center;
  flex: 0 0 auto;
  gap: 4px;
  font-size: 12.8px;
  font-weight: 400;
  white-space: nowrap;
}
.mz-effectsize-ci-level[hidden] { display: none !important; }
.mz-effectsize-ci-level label,
.mz-effectsize-ci-level input,
.mz-effectsize-ci-level .silky-option-suffix { margin: 0 !important; }
.mz-effectsize-ci-level input {
  width: 40px !important;
  min-width: 0;
  box-sizing: border-box;
}

/* Solo i select compatti dichiarati: etichetta e input costituiscono un blocco. */
.mz-control-wrapper.mz-compact-inline-select {
  align-items: center !important;
  padding-right: 0 !important;
}
.mz-compact-inline-select > .control-inline {
  width: auto !important;
  flex: 0 0 auto !important;
}
.mz-compact-inline-select .control-label-inline {
  margin-right: 0 !important;
}
.mz-compact-inline-select .control-input-group {
  flex: 0 0 auto !important;
}
`;

// Crea elemento style e inserisce CSS nel DOM
let node = document.createElement("style");
node.innerHTML = css;
document.body.appendChild(node);

// Esporta undefined per compatibilità modulo
module.exports = undefined;
