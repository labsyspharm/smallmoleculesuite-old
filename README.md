# <img src="query_gene_app/www/dcic.png" height = "50" width= "85" alt="LINCS DCIC"> HMS LINCS Small Molecule Library apps <img src="query_gene_app/www/logo_harvard_150.png" height = "50" width = "42" alt = "HMS LINCS Center">

This repository contains three web applications that are currently in development as part of the [NIH LINCS Consortium](http://www.lincsproject.org/) in a collaboration between Harvard Medical School's [Sorger Lab](http://sorger.med.harvard.edu/) and [Laboratory of Systems Pharmacology (LSP)](http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/about/) and the University of Cincinnati's [Laboratory for Statistical Genomics and Systems Biology](http://eh3.uc.edu/). The former (HMS) is one of the LINCS [Data and Signature Generation Centers (DSGCs)](http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers) and the latter (Cincinnati) is part of the [BD2K-LINCS Data Coordination and Integration Center (DCIC)](http://lincs-dcic.org/). All of the web applications are implemented in [R](https://www.r-project.org/), using the [Shiny](https://shiny.rstudio.com/) framework for interactive applications.

The applications are in active development (as of Sept. 16th 2017) and are likely to change in name, features, and hosting location in the near future. The apps may be visited on the web at the links below (these are temporary/development links and subject to change) or run offline in a local R installation.

[link to publication]

### Gene app

This app lets you see all compounds in the [HMS Laboratory of Systems Pharmacology (LSP)](http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/about/) collection that are shown to bind your target of interest. To find compounds, first select your target of interest and binding criteria. Subsequently, you select a region in the main plot with compounds of your interest. You can then select three compounds and view their known binding affinities in detail

Visit on the web:

https://shiny.ilincs.org/query_gene_app/

Or run locally in R:

```r
shiny::runGitHub("sorgerlab/drug_browser", subdir = "query_gene_app")
```

### Drug app

This app is designed to let you explore compounds that are similar to your compound of interest. Similarity is regarded in threefold: structural similarity, target affinity spectrum similarity (TAS) and phenotypic fingerprint similarity (PFP). To view compounds that are similar, you first select a reference compound of your choice, you then set some thresholds for the distance metrics and finally select up to three similar compounds that you like to explore further. We show the known targets and affinities for the selected compounds, for your information.

Visit on the web:

https://shiny.ilincs.org/query_drug_app/

Or run locally in R:

```r
shiny::runGitHub("sorgerlab/drug_browser", subdir = "query_drug_app")
```

### Library app

[description here]

Visit on the web:

https://shiny.ilincs.org/custom_library_app/

Or run locally in R:

```r
shiny::runGitHub("sorgerlab/drug_browser", subdir = "custom_library_app")
```

## Development

Design/idea by [Nienke Moret](https://github.com/nmoret/) and [Marc Hafner](https://scholar.harvard.edu/hafner) (HMS)

R code by [Nienke Moret](https://github.com/nmoret/) (HMS)

Shiny/R web application development by [Nicholas Clark](https://github.com/NicholasClark) (U of Cincinnati)

Supervision by [Peter Sorger](https://sorger.med.harvard.edu/people/peter-sorger-phd/) (HMS)

### Links

NIH LINCS Consortium http://www.lincsproject.org/

HMS LINCS Center http://lincs.hms.harvard.edu/

HMS Laboratory of Systems Pharmacology (LSP) http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/

Sorger Lab http://sorger.med.harvard.edu/

University of Cincinnati Laboratory for Statistical Genomics and Systems Biology http://eh3.uc.edu/

LINCS Data and Signature Generation Centers (DSGCs) http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers

BD2K-LINCS Data Coordination and Integration Center (DCIC) http://lincs-dcic.org/