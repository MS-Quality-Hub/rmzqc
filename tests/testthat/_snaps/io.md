# readMZQC and writeMZQC - check for extra data in mzQC file and warn

    Code
      readMZQCFromJSON(json_raw)
    Warning <simpleWarning>
      Unexpected field(s) found in data for class 'MzQCmzQC' in MzQCmzQC: newData
      Unexpected field(s) found in data for class 'MzQCmetadata': stuff
      Unexpected field(s) found in data for class 'MzQCmetadata' in MzQCmzQC$runQualities[1]$metadata: stuff
    Output
      <MzQCmzQC>
        Public:
          contactAddress: test@user.info
          contactName: bielow
          controlledVocabularies: list
          creationDate: MzQCDateTime, R6
          description: A minimal mzQC test document with bogus data
          fromData: function (data, context = "MzQCmzQC") 
          initialize: function (version = NA_character_, creationDate = MzQCDateTime$new(), 
          isValid: function (context = "MzQCmzQC") 
          runQualities: list
          self: MzQCmzQC, R6
          setQualities: list
          toJSON: function (...) 
          version: 1.0.0

