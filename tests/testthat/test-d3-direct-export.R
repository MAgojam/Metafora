test_that('D3 snapshot validation binds PNG dimensions and source identity',{
    read<-get('.mzReadD3Snapshot',asNamespace('Metafora'))
    bytes<-png::writePNG(array(1,dim=c(64,96,4)),target=raw())
    data<-list(token='expected',width=96,height=64,png=jsonlite::base64_enc(bytes))
    encode<-function(x)jsonlite::toJSON(x,auto_unbox=TRUE)
    expect_equal(read(encode(data),'expected')$bytes,bytes)
    expect_null(read(encode(data),'obsolete'))
    changed<-data;changed$width<-95
    expect_null(read(encode(changed),'expected'))
    changed<-data;changed$png<-'invalid'
    expect_null(read(encode(changed),'expected'))
    expect_null(read('', 'expected'))
})

for(model in names(model_specs))local({
    name<-model
    test_that(paste(name,'direct D3 exports preserve fit and reject stale images'),{
        base<-list(forestPlotMode='posRorB',funnelPlotMode='posRorB',
                   leave1outPlotShow=TRUE,leave1outPlotMode='posRorB',
                   cumulativePlotMode='posRorB',subgroup='group')
        a<-make_analysis(name,extra=base)
        ns<-asNamespace('Metafora');allKinds<-get('.mzD3ExportKinds',ns)
        # Le sole viste Html usano il trasporto PNG; Svg gestisce il proprio snapshot.
        native<-vapply(allKinds,function(key)inherits(a$results[[key]],'Svg'),logical(1))
        kinds<-allKinds[!native]
        for(key in allKinds[native]) {
            expect_match(a$results[[key]]$content,'MetaforaD3.mountNative(',fixed=TRUE)
            expect_false(a$results[[paste0(key,'Export')]]$visible)
        }
        option<-get('.mzD3ExportOption',ns)
        bytes<-png::writePNG(array(1,dim=c(64,96,4)),target=raw())
        extra<-base
        for(kind in names(kinds)){
            token<-a$results$d3ExportMeta$state[[kind]]
            expect_type(token,'character');expect_length(token,1)
            extra[[option(kind)]]<-jsonlite::toJSON(list(token=token,width=96,height=64,
                png=jsonlite::base64_enc(bytes)),auto_unbox=TRUE)
            content<-a$results[[kinds[[kind]]]]$content
            expect_match(content,'data-mz-export-token=',fixed=TRUE)
            expect_false(grepl('</iframe><script>',content,fixed=TRUE))
            expect_false(a$results[[paste0(kinds[[kind]],'Export')]]$visible)
        }
        b<-testthat::with_mocked_bindings(
            make_analysis(name,extra=extra,previous=snapshot(a),
                changes=vapply(names(kinds),option,character(1))),
            rma=function(...)stop('Unexpected model refit for export pixels'),
            rma.uni=function(...)stop('Unexpected model refit for export pixels'),
            .package='metafor')
        expect_equal(b$modelResults$beta,a$modelResults$beta)
        for(kind in names(kinds)){
            image<-b$results[[paste0(kinds[[kind]],'Export')]]
            expect_true(image$visible)
            pb<-image$asProtoBuf(status=3L)
            expect_equal(pb$image$width,96L);expect_equal(pb$image$height,64L)
            expect_lt(length(serialize(image$state,NULL)),1000)
            expect_null(image$state$png)
        }
        stale<-make_analysis(name,extra=utils::modifyList(extra,list(level=90)),
             previous=snapshot(b),changes='level')
        for(kind in names(kinds))expect_false(stale$results[[paste0(kinds[[kind]],'Export')]]$visible)
    })
})
