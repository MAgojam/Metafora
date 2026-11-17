# Il vero report Html resta disponibile; solo l'avviso SVG legacy viene eliminato.
for(model in names(model_specs))local({name <- model
 test_that(paste(name,'hides reporting with moderators without legacy SVG and restores genuine report'), {
  a <- make_analysis(name,extra=list(showTextRepo=TRUE))
  expect_true(a$results$OutputHtml$visible)
  expect_match(a$results$OutputHtml$content,'data-mz-html-key=',fixed=TRUE)
  expect_false(grepl('Does not currently work with moderators.',a$results$OutputHtml$content,fixed=TRUE))
  # Simula anche un vecchio salvataggio con il placeholder legacy gia compilato.
  a$results$OutputHtml$setContent('<svg><animate/>Does not currently work with moderators.</svg>')
  b <- with_mocked_bindings(
    make_analysis(name,extra=list(showTextRepo=TRUE,moderator='mod'),
      previous=snapshot(a),changes='moderator'),
    mz_create_report_html=function(...)stop('Must not create report with moderators'),.package='Metafora')
  expect_false(b$results$OutputHtml$visible)
  expect_identical(b$results$OutputHtml$content,'')
  expect_true(b$options$showTextRepo)
  c <- make_analysis(name,extra=list(showTextRepo=TRUE),previous=snapshot(b),changes='moderator')
  expect_true(c$results$OutputHtml$visible)
  expect_match(c$results$OutputHtml$content,'data-mz-html-key=',fixed=TRUE)
  expect_false(grepl('Does not currently work with moderators.',c$results$OutputHtml$content,fixed=TRUE))
  off <- make_analysis(name,extra=list(showTextRepo=FALSE),previous=snapshot(c),changes='showTextRepo')
  expect_false(off$results$OutputHtml$visible)
 })
})
