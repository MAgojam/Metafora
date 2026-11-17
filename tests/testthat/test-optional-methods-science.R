# Riferimenti numerici separati dal renderer e dalla parametrizzazione chi-quadro.
test_that('p-curve agrees with conditional normal-tail probabilities', {
    compute <- get('.mzPcurveStats',asNamespace('Metafora'))
    p <- c(.002,.008,.015,.023,.031,.044,.2)
    z <- qnorm(p/2,lower.tail=FALSE)
    result <- compute(z,rep(1,length(z)))
    critical <- qnorm(.975)
    tail <- function(threshold,shift)
        pnorm(-threshold-shift)+pnorm(threshold-shift,lower.tail=FALSE)
    shift <- uniroot(function(d)tail(critical,d)-1/3,c(0,10),tol=1e-12)$root
    cdf <- function(probability)tail(qnorm(1-probability/2),shift)/(1/3)
    expected <- diff(c(0,vapply(seq(.01,.05,.01),cdf,numeric(1))))*100
    expect_equal(result$green,expected,tolerance=2e-5)
    expect_equal(sum(result$green),100,tolerance=1e-10)
    expect_true(all(result$green>=0))
    half <- p<.025
    expected_pp <- 1-vapply(z[half],tail,numeric(1),shift=shift)/tail(qnorm(.9875),shift)
    expect_equal(result$pp33_half[half],expected_pp,tolerance=2e-5)
    expect_equal(result$p_Zpp33_half,pnorm(sum(qnorm(expected_pp))/sqrt(sum(half))),tolerance=2e-5)
    expect_equal(result$blue,c(2,1,1,1,1)/6*100)
})

test_that('p-curve handles an empty half-curve and insufficient significant studies', {
    compute <- get('.mzPcurveStats',asNamespace('Metafora'))
    p <- c(.028,.032,.041,.045,.3)
    result <- compute(qnorm(p/2,lower.tail=FALSE),rep(1,length(p)))
    expect_equal(result$khalf,0)
    expect_true(is.na(result$p_Zppr_half))
    expect_true(is.na(result$p_Zpp33_half))
    expect_true(is.finite(result$hat))
    expect_null(compute(c(0,.2,.3),rep(1,3)))
    expect_error(compute(1,0),'strictly positive')
})

for (model_name in names(model_specs)) local({
    name <- model_name
    test_that(paste(name,'passes canonical effects, variances and selection arguments to p-uniform'), {
        seen <- list()
        local_mocked_bindings(puniform=function(yi=NULL,vi=NULL,ri=NULL,ni=NULL,side,method,alpha) {
            seen <<- list(yi=yi,vi=vi,ri=ri,ni=ni,side=side,method=method,alpha=alpha)
            list(est=.4,ci.lb=.2,ci.ub=.6,ksig=6,L.0=2,pval.0=.02,L.pb=1,pval.pb=.1)
        },.package='puniform')
        a <- make_analysis(name,extra=list(showPuniform=TRUE,methodPunif='LNP',
                                          puniformSide='right',alphaPunif=.05,level=90))
        reference <- if(name=='mzEffectSizes')model_data()[,c('yi','vi')] else
            do.call(metafor::escalc,native_args(name)[setdiff(names(native_args(name)),'method')])
        if(name=='mzCorrelations') {
            # La libreria riceve correlazioni grezze e numerosità, preservando il proprio test.
            expect_equal(seen$ri,model_data()$ri)
            expect_equal(seen$ni,model_data()$ni)
            expect_null(seen$yi)
        } else {
            expect_equal(unname(seen$yi),unname(reference$yi),tolerance=1e-10)
            expect_equal(unname(seen$vi),unname(reference$vi),tolerance=1e-10)
        }
        expect_identical(seen$side,'right')
        expect_identical(seen$method,'LNP')
        expect_equal(seen$alpha,.05)
        html <- a$results$outHtmlPuniform$content
        expect_match(html,'95% CI',fixed=TRUE)
        expect_false(grepl('90% CI',html,fixed=TRUE))
    })
})

test_that('p-uniform HTML preserves library results and escapes error text', {
    html <- get('mzHtmlPuniform',asNamespace('Metafora'))
    fit <- puniform::puniform(yi=c(.4,.5,.6,.45,.7,.55,.1),vi=rep(.01,7),side='right',method='P',alpha=.05)
    options <- list(puniformSide='right',methodPunif='P',alphaPunif=.05,level=99,modelest='REML',modelmea='GEN')
    rendered <- html(fit,options)
    expect_match(rendered,'95% CI',fixed=TRUE)
    expect_match(rendered,formatC(fit$ci.lb,3,format='f'),fixed=TRUE)
    expect_match(rendered,formatC(fit$ci.ub,3,format='f'),fixed=TRUE)
    error <- html(list(error=TRUE,message='<script>controlled</script> & invalid'),options)
    expect_false(grepl('<script>controlled',error,fixed=TRUE))
    expect_match(error,'&lt;script&gt;',fixed=TRUE)
})

test_that('PFT always stays transformed even when a saved flag requests an inverse', {
    get_transform <- get('.mzGetBackTransform',asNamespace('Metafora'))
    n <- c(20,80,500,2000)
    expected <- get_transform('proportion','PFT',FALSE)
    for (samples in list(NULL,n)) {
        specification <- get_transform('proportion','PFT',TRUE,ni=samples)
        expect_identical(specification,expected)
        expect_null(specification$atransf)
        expect_null(specification$targs)
        expect_identical(specification$transf_type,'identity')
        expect_identical(specification$xlab,'Double Arcsine Proportion')
    }
})

test_that('selection HTML does not count coefficient p-values as study p-values', {
    renderer <- get('mzHtmlSelmodel',asNamespace('Metafora'))
    # Oggetto minimo senza ptable: due coefficienti non sono due studi.
    result <- list(b=.3,se=.1,zval=3,pval=.003,ci.lb=.1,ci.ub=.5,
                   delta=c(1,.8,.5,.2),se.delta=rep(.1,4),zval.delta=1:4,
                   pval.delta=rep(.1,4),ci.lb.delta=rep(.1,4),ci.ub.delta=rep(1,4),
                   steps=c(.01,.025,.05,1),k=13)
    rendered <- renderer(result,list(selModelType='stepfun',level=95))
    expect_match(rendered,'0.05 &lt; p &lt;= 1|0.05 < p <= 1')
    expect_false(grepl('>k</th>',rendered,fixed=TRUE))
})

test_that('report measure labels are explicit and AS remains an arcsine difference', {
    label <- get('.mz_measure_label',asNamespace('Metafora'))
    expect_identical(label(list(measure='OR')),'log odds ratio')
    expect_identical(label(list(measure='AS')),'arcsine difference')
    expect_identical(label(list(measure='GEN')),'outcome')
    expect_identical(label(list(measure='ZCOR')),'Fisher r-to-z transformed correlation coefficient')
    expect_identical(label(list(measure='UNKNOWN')),'unknown')
})

for (selection_type in c('halfnorm','beta','negexp','logistic','power','stepfun')) local({
    type <- selection_type
    test_that(paste('selection',type,'presents the native adjusted estimate and uncertainty'), {
        d <- metadat::dat.hackshaw1998
        fit <- metafor::rma(yi,vi,data=d,method='REML',level=90)
        selected <- suppressWarnings(if(type=='stepfun')
            metafor::selmodel(fit,type=type,steps=c(.01,.025,.05,1)) else
            metafor::selmodel(fit,type=type))
        rendered <- get('mzHtmlSelmodel',asNamespace('Metafora'))(
            selected,list(selModelType=type,level=90))
        expect_match(rendered,formatC(selected$b[1],3,format='f'),fixed=TRUE)
        expect_match(rendered,'90% CI',fixed=TRUE)
        expect_identical(selected$method,'ML')
        if(is.finite(selected$se[1])) {
            expect_equal(selected$ci.lb[1],as.numeric(selected$b[1])-qnorm(.95)*selected$se[1],tolerance=1e-7)
            expect_match(rendered,formatC(selected$ci.lb[1],3,format='f'),fixed=TRUE)
        } else {
            expect_match(rendered,'Uncertainty estimates are unavailable',fixed=TRUE)
        }
        if(type=='stepfun') {
            expect_equal(sum(selected$ptable$k),nrow(d))
            observed <- pnorm(d$yi/sqrt(d$vi),lower.tail=FALSE)
            expect_equal(as.integer(selected$ptable$k),as.integer(table(cut(observed,c(0,.01,.025,.05,1)))))
        }
    })
})
