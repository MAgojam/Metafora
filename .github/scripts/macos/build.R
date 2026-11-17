args <- commandArgs(trailingOnly=TRUE)
root <- normalizePath(args[1]); out <- normalizePath(args[2])
app <- '/Applications/jamovi.app/Contents'
tools <- file.path(Sys.getenv('RUNNER_TEMP'),'metafora-build-tools')
dir.create(tools,recursive=TRUE,showWarnings=FALSE)
.libPaths(c(tools,file.path(app,'Resources/modules/base/R'),
            file.path(app,'Resources/modules/jmv/R'),.libPaths()))
options(repos=c(jamovi='https://repo.jamovi.org',CRAN='https://cloud.r-project.org'),timeout=900)
cat('R:',R.version.string,'\nPlatform:',R.version$platform,'\nR home:',R.home(),'\n')
if(!requireNamespace('node',quietly=TRUE))
    install.packages('node',lib=tools,type='source',INSTALL_opts='--no-help')
install.packages('https://repo.jamovi.org/src/contrib/jmvtools_28.17.tar.gz',
                 lib=tools,repos=NULL,dependencies=FALSE,type='source',INSTALL_opts='--no-help')
stopifnot(as.character(packageVersion('jmvtools'))=='28.17')
compiler <- getFromNamespace('jmcPath','jmvtools')()
node <- Sys.which('node'); stopifnot(nzchar(node))
home <- file.path(app,'MacOS');setwd(root)
stopifnot(system2(node,c(compiler,'--check','--home',shQuote(home)))==0L)
# The first pass installs the pinned dependencies. Relocate external native
# libraries in the disposable build cache before the compiler's final packaging.
stopifnot(system2(node,c(compiler,'--build',shQuote(root),'--home',shQuote(home)))==0L)
libs <- list.dirs(file.path(root,'build'),recursive=FALSE,full.names=TRUE)
libs <- libs[file.exists(file.path(libs,'Metafora/DESCRIPTION'))]
stopifnot(length(libs)==1L)
stopifnot(system2('python3',c(shQuote(file.path(root,'.github/scripts/macos/relocate.py')),
                            shQuote(libs),shQuote(app),shQuote(out)))==0L)
version <- read.dcf(file.path(root,'DESCRIPTION'),fields='Version')[1,1]
dest <- file.path(out,paste0('Metafora_',version,'_macos_',Sys.getenv('EXPECTED_ARCH'),'.jmo'))
stopifnot(system2(node,c(compiler,'--build',shQuote(root),'--home',shQuote(home),
                       '--skip-deps','--skip-remotes','--jmo',shQuote(dest)))==0L)
writeLines(c(paste('R:',R.version.string),paste('Platform:',R.version$platform),
             paste('R.home:',R.home()),paste('jmvtools:',packageVersion('jmvtools'))),file.path(out,'runtime.txt'))
