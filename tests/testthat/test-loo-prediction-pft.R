test_that('PFT prediction stays on its transformed scale with back-transform requested',{
 opts<-list(modelmea='PFT',leave1outPrediction=TRUE,leave1outTableShow=TRUE,showBackTransform=TRUE)
 a<-make_analysis('mzProportions',extra=opts)
 r<-a$leave1outResults
 built<-get('mzBuildLeaveOneOutTable',asNamespace('Metafora'))(a)
 expect_equal(built$data$PredictionLower[seq_len(nrow(r))],r$pi.lb)
 expect_equal(built$data$PredictionUpper[seq_len(nrow(r))],r$pi.ub)
 expect_true(all(is.finite(r$pi.lb)))
 expect_match(built$html,'95% PI',fixed=TRUE)
})
