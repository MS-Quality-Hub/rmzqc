# Test improved validation error reporting

test_that("isValidMzQC provides detailed error messages", {
  # Create an invalid MzQCcvParameter with missing name
  invalid_param <- MzQCcvParameter$new(accession = "MS:4000059")

  # Capture the warning message
  expect_warning(
    result <- isValidMzQC(invalid_param),
    "MzQCcvParameter$self$name is NA/NULL",
    fixed = TRUE
  )

  # Check that validation fails
  expect_false(result)

  # Create a nested structure with an invalid element
  valid_param <- MzQCcvParameter$new("MS:4000059", "Number of MS1 spectra")
  invalid_param2 <- MzQCcvParameter$new(accession = "MS:4000060")
  param_list <- list(valid_param, invalid_param2)

  warnings <- testthat::capture_warnings({
    result <- isValidMzQC(param_list)
  })
  expect_false(result)
  expect_length(warnings, 1)
  expect_match(warnings[1],
               "MzQCcvParameter$self$name is NA/NULL",
               fixed = TRUE)


  # Test with a more complex nested structure
  file_format <- MzQCcvParameter$new("MS:1000584", "mzML format")
  input_file <- MzQCinputFile$new(
    name = "test_file",
    location = "file:///path/to/file.mzML",
    fileFormat = file_format
  )

  # Create an invalid metadata object with missing label
  metadata <- MzQCmetadata$new(
    inputFiles = list(input_file),
    analysisSoftware = list(MzQCanalysisSoftware$new(
      accession = "MS:1000799",
      name = "custom software tool",
      version = "1.0"
    ))
  )

  # Capture the warning message
  expect_warning(
    result <- isValidMzQC(metadata),
    "MzQCmetadata$self$label is NA/NULL",
    fixed = TRUE
  )

  # Check that validation fails
  expect_false(result)

  # Test with an invalid nested component
  metadata2 <- MzQCmetadata$new(
    label = "test_run",
    inputFiles = list(input_file),
    analysisSoftware = list(MzQCanalysisSoftware$new(
      accession = "MS:1000799",
      name = "custom software tool"
      # Missing version
    ))
  )

  # Capture the warning message
  expect_warning(
    result <- isValidMzQC(metadata2),
    "MzQCmetadata$analysisSoftware[1]$MzQCanalysisSoftware$self$version is NA/NULL",
    fixed = TRUE
  )

  # Check that validation fails
  expect_false(result)
})

test_that("Complex nested validation provides clear path to invalid field", {
  # Create a valid run quality
  file_format <- MzQCcvParameter$new("MS:1000584", "mzML format")
  input_file <- MzQCinputFile$new(
    name = "test_file",
    location = "file:///path/to/file.mzML",
    fileFormat = file_format
  )

  software <- MzQCanalysisSoftware$new(
    accession = "MS:1000799",
    name = "custom software tool",
    version = "1.0"
  )

  metadata <- MzQCmetadata$new(
    label = "test_run",
    inputFiles = list(input_file),
    analysisSoftware = list(software)
  )

  # Create a quality metric with missing name
  invalid_metric <- MzQCqualityMetric$new(
    accession = "MS:4000059"
    # Missing name
  )

  run_quality <- MzQCrunQuality$new(
    metadata = metadata,
    qualityMetrics = list(invalid_metric)
  )

  # Create an mzQC object with the invalid run quality
  mzqc <- MzQCmzQC$new(
    version = "1.0.0",
    runQualities = list(run_quality),
    controlledVocabularies = list(
      MzQCcontrolledVocabulary$new(
        "Proteomics Standards Initiative Quality Control Ontology",
        "https://github.com/HUPO-PSI/psi-ms-CV/releases/download/v4.1.129/psi-ms.obo",
        "4.1.129"
      )
    )
  )

  # Capture the warning message
  expect_warning(
    result <- isValidMzQC(mzqc),
    "MzQCmzQC$runQualities[1]$MzQCrunQuality$qualityMetrics[1]$MzQCqualityMetric$self$name is NA/NULL",
    fixed = TRUE
  )

  # Check that validation fails
  expect_false(result)
})
