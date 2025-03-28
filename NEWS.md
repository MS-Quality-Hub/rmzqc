This NEWS file mainly serves as a changelog file,
i.e. a list of new features / bugfixes by version number.

Versions uploaded to CRAN are marked with [CRAN].


# [CRAN] version 0.5.5 (2025/03/22)

* improved api documentation
* more robust handling of units

# [CRAN] version 0.5.4 (2024/04/15)

* more robust handling of faulty internet connection when retrieving the latest PSI-MS CV

# [CRAN] version 0.5.3 (2023/08/24)

* some convenience functions to create an mzQC document, e.g. 'localFileToURI()'

# [CRAN] version 0.5.2 (2023/08/05)

* Loads the PSI-MS CV only when required (not when the package is loaded, but only when running certain functions which depend on the CV).
* Some documentation fixes (mostly CV related)

# [CRAN] version 0.5.1 (2023/07/10)

* fixes a bug in 0.5.0 when internet connection is missing

# [CRAN] version 0.5.0 (2023/07/04)

* support the latest PSI-MS controlled vocabulary (via automatic download)

# [CRAN] version 0.4.2 (2023/04/23)

* bugfix release (CV_ member not found when rmzqc is not attached)

# [CRAN] version 0.4.1 (2023/04/11)

* Feature: support validation of mzQC files (via validateFrom...() functions, see Vignettes)

# version 0.3.0 (2022/10/11)

* Feature: support reading mzQC files (via readMZQC(), see Vignettes)

# version 0.2.2 (2022/08/21)

* Fix: make the CV singleton object available outside the package (via 'getCVSingleton()')

# version 0.2.1 (2022/09/28)

* Update documentation

# version 0.2.0 (2022/09/26)

* Add jupyter notebook with links to Google Colab
* Add a pkgdown website
* Update documentation


# [CRAN] version 0.1.0 (2022/08/18)

* initial release (core mzQC data structures; writing mzQC JSON; tests)
* Versions uploaded to CRAN are marked with [CRAN].
