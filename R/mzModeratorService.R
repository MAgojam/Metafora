# Conserva il livello di misura: codici numerici non rendono continua una categoria.
# Nei dati jamovi i fattori numerici possono avere l'attributo values; questo
# descrive la codifica, non autorizza la conversione in covariata.
.mzPrepareModerator <- function(column) {
    measure <- attr(column, "jmv-measure", exact=TRUE)
    if (identical(measure, "continuous"))
        return(jmvcore::toNumeric(column))
    if (any(measure %in% c("nominal", "ordinal")) || is.factor(column))
        return(as.factor(column))
    # Per le chiamate R senza metadati conserva la natura del vettore originale.
    if (is.numeric(column)) return(column)
    as.factor(column)
}
