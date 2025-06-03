# readMZQC and writeMZQC - check for extra data in mzQC file and warn

    Code
      readMZQCFromJSON(json_raw)
    Condition
      Warning:
      Unexpected field(s) found in data for class 'MzQCmzQC' in MzQCmzQC: newData
      Warning:
      Unexpected field(s) found in data for class 'MzQCmetadata': stuff
      Warning:
      Unexpected field(s) found in data for class 'MzQCmetadata' in MzQCmzQC$runQualities[1]$metadata: stuff
    Output
      Reference class object of class "MzQCmzQC"
      Field "version":
      [1] "1.0.0"
      Field "creationDate":
      Reference class object of class "MzQCDateTime"
      Field "datetime":
      [1] "2022-10-17T00:00:00Z"
      Field "contactName":
      [1] "bielow"
      Field "contactAddress":
      [1] "test@user.info"
      Field "description":
      [1] "A minimal mzQC test document with bogus data"
      Field "runQualities":
      [[1]]
      Reference class object of class "MzQCrunQuality"
      Field "metadata":
      Reference class object of class "MzQCmetadata"
      Field "label":
      [1] "file:///c:/data/special.raw"
      Field "inputFiles":
      [[1]]
      Reference class object of class "MzQCinputFile"
      Field "name":
      [1] "special.raw"
      Field "location":
      [1] "file:///c:/data/special.raw"
      Field "fileFormat":
      Reference class object of class "MzQCcvParameter"
      Field "accession":
      [1] "MS:1000563"
      Field "name":
      [1] "Thermo RAW format"
      Field "value":
      [1] NA
      Field "description":
      [1] NA
      Field "fileProperties":
      list()
      
      Field "analysisSoftware":
      [[1]]
      Reference class object of class "MzQCanalysisSoftware"
      Field "accession":
      [1] "MS:1003162"
      Field "name":
      [1] "PTX-QC"
      Field "version":
      [1] "1.0.13"
      Field "uri":
      [1] "https://github.com/cbielow/PTXQC/"
      Field "description":
      [1] "\"Proteomics (PTX) - QualityControl (QC) software for QC report generation and visualization.\" [DOI:10.1021/acs.jproteome.5b00780, PMID:26653327, https://github.com/cbielow/PTXQC/]"
      Field "value":
      [1] NA
      
      Field "cvParameters":
      list()
      Field "qualityMetrics":
      [[1]]
      Reference class object of class "MzQCqualityMetric"
      Field "accession":
      [1] "MS:4000059"
      Field "name":
      [1] "number of MS1 spectra"
      Field "description":
      [1] "\"The number of MS1 events in the run.\" [PSI:MS]"
      Field "value":
      [1] 13405
      Field "unit":
      list()
      
      
      Field "setQualities":
      list()
      Field "controlledVocabularies":
      [[1]]
      Reference class object of class "MzQCcontrolledVocabulary"
      Field "name":
      [1] "Proteomics Standards Initiative Mass Spectrometry Ontology"
      Field "uri":
      [1] "https://github.com/HUPO-PSI/psi-ms-CV/releases/download/v4.1.95/psi-ms.obo"
      Field "version":
      [1] "4.1.95"
      

