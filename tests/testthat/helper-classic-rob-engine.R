# Il motore jamovi rilascia i dati dopo .run(): il RoB deve restare nello stato Image.
classic_rob_engine <- function(name,data,extra,previous=NULL,changes=character(),run=TRUE) {
 spec <- model_specs[[name]];ns <- asNamespace('Metafora')
 args <- utils::modifyList(c(list(slab='slab',modelmea=spec$measure),as.list(spec$vars)),extra)
 options <- do.call(get(paste0(name,'Options'),ns)$new,args)
 a <- get(paste0(name,'Class'),ns)$new(options=options,data=NULL)
 reads <- 0L
 a$.setReadDatasetHeaderSource(function(vars)data[0,intersect(unlist(vars),names(data)),drop=FALSE])
 a$.setReadDatasetSource(function(vars) {
  reads <<- reads+1L
  data[,intersect(unlist(vars),names(data)),drop=FALSE]
 })
 a$init()
 if(!is.null(previous))a$results$fromProtoBuf(previous,changes,character())
 a$postInit()
 if(run)a$run()
 list(analysis=a,reads=function()reads)
}
