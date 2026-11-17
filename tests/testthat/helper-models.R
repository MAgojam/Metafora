# Fixture sintetiche deterministiche: nessun file utente o percorso locale necessario.
model_specs <- list(
    mzDichotomous=list(measure='RR',vars=c(ai='ai',bi='bi',ci='ci',di='di')),
    mzCorrelations=list(measure='ZCOR',vars=c(ri='ri',ni='ni')),
    mzAlphas=list(measure='ARAW',vars=c(ai='alpha',mi='mi',ni='ni')),
    mzMeanDifferences=list(measure='SMD',vars=c(m1i='m1i',sd1i='sd1i',n1i='n1i',m2i='m2i',sd2i='sd2i',n2i='n2i')),
    mzProportions=list(measure='PR',vars=c(xi='xi',ni='ni')),
    mzEffectSizes=list(measure='GEN',vars=c(yi='yi',vi='vi'))
)

model_data <- function() {
    i <- seq_len(13L)
    data.frame(slab=paste0('Study ',i),ai=30+round(5*sin(i)),bi=200+10*i,
               ci=30+round(3*cos(i)),di=250+12*i,ri=.05+sin(i)/4,
               ni=100+10*i,alpha=.7+.08*sin(i),mi=5+i%%3,
               m1i=5+sin(i),m2i=5+cos(i)/3,sd1i=2+.1*i,sd2i=2.5+cos(i)/4,
               n1i=50+10*i,n2i=60+10*i,xi=20+round(6*sin(i)),
               yi=sin(i)/2+.1*i/13,vi=.1+.01*(i%%4),
               mod=i,group=factor(rep(c('Z','A'),length.out=13L)))
}

make_analysis <- function(name,data=model_data(),extra=list(),previous=NULL,
                          changes=character(),variables=character(),run=TRUE) {
    ns <- asNamespace('Metafora')
    spec <- model_specs[[name]]
    args <- c(list(slab='slab',modelmea=spec$measure),as.list(spec$vars))
    args <- utils::modifyList(args,extra)
    options <- do.call(get(paste0(name,'Options'),ns)$new,args)
    analysis <- get(paste0(name,'Class'),ns)$new(options=options,data=data)
    analysis$init()
    if (!is.null(previous)) analysis$results$fromProtoBuf(previous,changes,variables)
    analysis$postInit()
    if (run) analysis$run()
    analysis
}

native_args <- function(name,data=model_data(),extra=list()) {
    spec <- model_specs[[name]]
    utils::modifyList(c(list(measure=spec$measure,method='REML'),
                       lapply(spec$vars,function(column)data[[column]])),extra)
}

# La serializzazione è l'API nativa jmvcore, non una copia della sua logica.
RProtoBuf::readProtoFiles(system.file('jamovi.proto',package='jmvcore'))
snapshot <- function(analysis) analysis$results$asProtoBuf(status=3L)

# Options$new non popola il protobuf della richiesta: le prove del ciclo .load
# devono riprodurre anche options$fromProtoBuf, come jmvcore::create nel motore.
options_with_protobuf <- function(options) {
    encode <- function(value) {
        pb <- RProtoBuf::new(jamovi.coms.AnalysisOption)
        if (is.null(value)) pb$o <- 2L
        else if (is.logical(value) && length(value)==1L) pb$o <- as.integer(value)
        else if (is.numeric(value) && length(value)==1L) pb$d <- as.numeric(value)
        else if (is.character(value) && length(value)==1L) pb$s <- value
        else {
            children <- RProtoBuf::new(jamovi.coms.AnalysisOptions)
            if (!is.null(names(value))) {children$hasNames <- TRUE;children$names <- names(value)}
            for (child in value) children$add('options',encode(child))
            pb$c <- children
        }
        pb
    }
    pb <- RProtoBuf::new(jamovi.coms.AnalysisOptions)
    pb$hasNames <- TRUE;pb$names <- options$names
    for (name in options$names) {
        option <- options$option(name)
        value <- if (inherits(option,'OptionOutput')) list(value=option$value,synced=FALSE) else option$value
        pb$add('options',encode(value))
    }
    options$fromProtoBuf(pb)
    options
}
