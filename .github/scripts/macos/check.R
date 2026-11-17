args <- commandArgs(trailingOnly=TRUE)
root<-normalizePath(args[1]);out<-normalizePath(args[2])
app<-'/Applications/jamovi.app/Contents'
libs<-list.dirs(file.path(root,'build'),recursive=FALSE,full.names=TRUE)
libs<-libs[file.exists(file.path(libs,'Metafora/DESCRIPTION'))];stopifnot(length(libs)==1L)
.libPaths(c(libs,file.path(app,'Resources/modules/base/R'),file.path(app,'Resources/modules/jmv/R'),.libPaths()))
library(Metafora);library(testthat)
stopifnot(as.character(packageVersion('Metafora'))=='2.0.0',as.character(packageVersion('metafor'))=='5.3.4')
setwd(root)
res<-testthat::test_dir('tests/testthat',reporter='summary',stop_on_failure=FALSE,
    filter='six-model-native-statistics|model-inference|cache-results|hettest|predictive-probability|native-forest-lifecycle')
tab<-as.data.frame(res)
write.csv(tab[,!vapply(tab,is.list,logical(1)),drop=FALSE],file.path(out,'tests_compiled.csv'),row.names=FALSE)
cat('BLOCKS',nrow(tab),'EXPECTATIONS',sum(tab$nb),'FAILED',sum(tab$failed),
    'ERRORS',sum(tab$error),'WARNINGS',sum(tab$warning),'SKIPPED',sum(tab$skipped),'\n')
if(any(tab$failed>0|tab$error|tab$warning>0|tab$skipped))stop('Compiled backend checks failed')
writeLines('PASSED',file.path(out,'BACKEND_PASSED.txt'))
