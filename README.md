# <img src="query_gene_app/www/dcic.png" height = "50" width= "85" alt="LINCS DCIC"> HMS LINCS Small Molecule Library apps <img src="query_gene_app/www/logo_harvard_150.png" height = "50" width = "42" alt = "HMS LINCS Center">

This repository contains three related web applications that are currently in development as part of the [NIH LINCS Consortium](http://www.lincsproject.org/) in a collaboration between Harvard Medical School's [Sorger Lab](http://sorger.med.harvard.edu/) and [Laboratory of Systems Pharmacology (LSP)](http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/about/) and the University of Cincinnati's [Laboratory for Statistical Genomics and Systems Biology](http://eh3.uc.edu/). The former (HMS) is one of the [LINCS Data and Signature Generation Centers (DSGCs)](http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers) and the latter (Cincinnati) is part of the [BD2K-LINCS Data Coordination and Integration Center (DCIC)](http://lincs-dcic.org/). 

All of the web applications are implemented in [R](https://www.r-project.org/), using the [Shiny](https://shiny.rstudio.com/) framework for interactive applications.

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

This app composes a custom chemical genetics library for a set of genes. The compounds of genes are selected based on affinity, selectivity and clinical development phase. Additionally we source several expert opinion "best-in-class" list. For this tool to work, please submit a list of genes symbols* of your interest in the text box (or load our example gene list) and click 'Submit'. 
After submitting your gene list, a downloadable table of drugs targeting those genes will be generated. You may further adjust the library by selectivity level and clinical development phase. You will also have the opportunity to add compounds from the expert opinion "best-in-class" compounds.

*Only gene symbols from HUGO Gene Nomenclature Committee (HGNC) are accepted. Non-HGNC gene symbols and genes for which we lack drug information will be ignored.

Visit on the web:

https://shiny.ilincs.org/custom_library_app/

Or run locally in R:

```r
shiny::runGitHub("sorgerlab/drug_browser", subdir = "custom_library_app")
```
<br>

# Development and links

Design/idea by [Nienke Moret](https://github.com/nmoret/) and [Marc Hafner](https://scholar.harvard.edu/hafner) (HMS)

R code by [Nienke Moret](https://github.com/nmoret/) (HMS)

Shiny/R web application development by [Nicholas Clark](https://github.com/NicholasClark) (U of Cincinnati)

Supervision by [Peter Sorger](https://sorger.med.harvard.edu/people/peter-sorger-phd/) (HMS)

### BD2K-LINCS Project <img src="query_gene_app/www/dcic.png" height = "50" width= "85" alt="BD2K-LINCS">

**NIH LINCS Consortium**<br>http://www.lincsproject.org/

**HMS LINCS Center**<br>http://lincs.hms.harvard.edu/

**LINCS Data and Signature Generation Centers (DSGCs)**<br>http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers

**BD2K-LINCS Data Coordination and Integration Center (DCIC)**<br>http://lincs-dcic.org/

### Harvard Medical School <img src="query_gene_app/www/logo_harvard_150.png" height = "50" width = "42" alt = "Harvard Medical School">
**Laboratory of Systems Pharmacology (LSP)**<br>http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/

**Sorger Lab**<br>http://sorger.med.harvard.edu/

### University of Cincinnati <img src="shared/uc_logo_crop.png" height = "50" width ="64"  alt = "University of Cincinnati">

**Laboratory for Statistical Genomics and Systems Biology**<br>http://eh3.uc.edu/