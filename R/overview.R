
listMetrics = function(mzqc_obj, where=c("runQualities", "setQualities")) {

  fromQualities = function(qualities) {
    ret = do.call(rbind, lapply(qualities, function(runQuality) {
       r = do.call(rbind, lapply(runQuality$qualityMetrics, function(qMetric) data.frame(accession = qMetric$accession,
                                                                                              name = qMetric$name)))
       r
      } ))
    ## make unique
    vals = unique(ret)
    ## add counts
    counts = table(ret$accession)
    vals$counts = counts[match(vals$accession, names(counts))]
    vals
  }

  rall = do.call(rbind, lapply(where, function(name) {
    r_int = fromQualities(mzqc_obj[[name]])
    r_int$from = name
    r_int
  }))
  rall
}


extractMetricData = function(mzqc_obj, accession, where = "runQualities") {
    ret = do.call(rbind, lapply(mzqc_obj[[where]], function(runQuality) {
      r = do.call(rbind, lapply(runQuality$qualityMetrics, function(qMetric) {
        if (qMetric$accession != accession) return(NULL)
        (list(value = qMetric$value, meta_label = runQuality$metadata$label))
      }))
      r
    }), )
    ret
}





