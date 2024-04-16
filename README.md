# `rmzqc`

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
[![CRAN/METACRAN](https://img.shields.io/cran/v/rmzqc)](https://cran.r-project.org/package=rmzqc)

An R package for reading, validating, and writing `mzQC` files.

## Installation

You can grab `rmzqc` from either CRAN or GitHub. GitHub installation will give you the latest package; the CRAN version might be a little older, but is faster to install. 


    ## CRAN
    install.packages("rmzqc")
or

    ## GitHub
    if (!require(devtools, quietly = TRUE)) install.packages("devtools")
    library("devtools")
    
    install_github("MS-Quality-hub/rmzqc", build_vignettes = TRUE, dependencies = TRUE)

To get started, see the help and/or vignettes:

    help(package="rmzqc")
    browseVignettes(package = 'rmzqc')

Please feel free to report bugs (see below), or issue pull requests!    

## Latest changes / ChangeLog

Check the version history in 
[NEWS.md](https://ms-quality-hub.github.io/rmzqc/news/index.html).

## Usage

The package vignettes provide examples on how to use `rmzqc`. *After* the package is installed (see below),
you can browse the vignettes using either of these commands within R:

    help(package="rmzqc")
    browseVignettes(package = 'rmzqc')
  
If you do not want to wait that long, you can look at the 
[latest online vignette at CRAN](https://cran.r-project.org/package=rmzqc)

Currently, there are vignettes on:
 - Creating and storing a basic mzQC document using `rmzqc`
 (more to come)

## Bug reporting / Feature requests

If you encounter a bug, please use the [GitHub issue tracker][issuetracker] and file a report.

You should include
  - **stage** you encounter the bug, e.g. during installation, importing, exporting, etc
  - **version of `rmzqc`**, e.g. call `help(package="rmzqc")` within R
  - **error message** (very important!). Either copy it or provide a screen shot.

Please be as precise as possible when providing the bug report: 
just imagine what kind of information you would like to have in order
to track down the issue.
In certain situations, the input/output file you are trying to read/write is helpful.


## Contributing - Get Involved!

We welcome input from our user base!
`rmzqc` has a very permissive **MIT License** (see [DESCRIPTION](DESCRIPTION) file), so feel free to fork, patch and contribute!

There are many ways to get involved, _you do not need to be a developer_!
  - suggest a new metric (and why you think it's useful) by opening [a new ticket][issuetracker] here on GitHub.
  - suggest changes to functions (improvements or bugfixes), see above.
  - suggest improvements to our documentation
  - write code (in R) and submit a [Pull Request (PR)][PullRequest].


## Links

See https://github.com/HUPO-PSI/mzQC/ for the specification details of the mzQC format.

  [issuetracker]: https://github.com/MS-Quality-hub/rmzqc/issues
  [PullRequest]: https://github.com/MS-Quality-hub/rmzqc/pulls
