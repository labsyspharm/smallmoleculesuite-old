# <img src="SelectivitySelectR/www/dcic.png" height = "50" width= "85" alt="LINCS DCIC"> HMS-LINCS Small Molecule Suite Applications <img src="SelectivitySelectR/www/logo_harvard_150.png" height = "50" width = "42" alt = "HMS LINCS Center">

This repository contains three related web applications that were developed as part of the [NIH LINCS Consortium](http://www.lincsproject.org/) in a collaboration between Harvard Medical School's [Sorger Lab](http://sorger.med.harvard.edu/) and [Laboratory of Systems Pharmacology (LSP)](http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/about/) and the University of Cincinnati's [Laboratory for Statistical Genomics and Systems Biology](http://eh3.uc.edu/). The former (HMS) is one of the [LINCS Data and Signature Generation Centers (DSGCs)](http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers) and the latter (Cincinnati) is part of the [BD2K-LINCS Data Coordination and Integration Center (DCIC)](http://lincs-dcic.org/). 

All of the web applications are implemented in [R](https://www.r-project.org/), using the [Shiny](https://shiny.rstudio.com/) framework for interactive applications.

The apps may be visited on the web at [www.smallmoleculesuite.org](http://www.smallmoleculesuite.org/) or run offline in a local R installation (see details below) or via [Docker](https://www.docker.com/). A docker image containing the application, as well as version-controlled instances of all of its dependencies, is [available on DockerHub](https://hub.docker.com/r/ucbd2k/smallmoleculesuite/). Details on how to install and run Docker on Linux, macOS, and Windows can be found [here](https://docs.docker.com/get-started/#docker-concepts).

Publication:

Nienke Moret, Marc Hafner, Nicholas A. Clark, Yuan Wang, Eugen Lounkine, Mario Medvedovic, Nathanael Gray, Jeremy Jenkins, Peter K. Sorger. *Cheminformatics tools for analyzing and designing optimized small molecule libraries*. Manuscript submitted for publication.

### Necessary R packages for offline installation:

```r
### Install R packages using Microsoft MRAN repository to ensure correct version
install.packages('readr', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('shiny', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('shiny.semantic', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('shinyjs', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('DT', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('dplyr', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('crosstalk', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('plotly', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('markdown', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('clipr', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
install.packages('rclipboard', repos = 'https://mran.microsoft.com/snapshot/2017-11-28')
```

### SelectivitySelectR

<p><b><a href = "http://www.smallmoleculesuite.org/apps/SelectivitySelectR/">SelectivitySelectR</a></b> shows the affinity and selectivity of compounds in the <a href = "http://lincs.hms.harvard.edu/db/sm/">HMS-LINCS collection</a> for a gene of interest. To use SelectivitySelectR, first select your target of interest and binding criteria. Subsequently, select a region in the main plot with compounds of your interest. You can then select three compounds in the bottom table and view their known binding affinities in detail.</p>

Visit on the web:

http://www.smallmoleculesuite.org/apps/SelectivitySelectR/

Or run locally in R:

```r
shiny::runGitHub("sorgerlab/smallmoleculesuite", subdir = "SelectivitySelectR")
```

### SimilaritySelectR

<p><b><a href = "http://www.smallmoleculesuite.org/apps/SimilaritySelectR/">SimilaritySelectR</a></b> shows the similarity of compounds in the <a href = "http://lincs.hms.harvard.edu/db/sm/">HMS-LINCS collection</a> to a reference compound. Similarity is regarded in threefold: structural similarity (Tanimoto similarity of Morgan2 fingerprints), target affinity spectrum similarity (TAS) and phenotypic fingerprint similarity (PFP). To use SimilaritySelectR, select a reference compound and adjust filters as desired. From the main plots, select a region with compounds of interest. Then, select up to three compounds in the bottom table and view their known binding affinities in detail.</p>

Visit on the web:

http://www.smallmoleculesuite.org/apps/SimilaritySelectR/

Or run locally in R:

```r
shiny::runGitHub("sorgerlab/smallmoleculesuite", subdir = "SimilaritySelectR")
```

### LibraryR

<p><b><a href = "http://www.smallmoleculesuite.org/apps/LibraryR/">LibraryR</a></b> composes custom chemical genetics libraries for gene-sets of interest. Compounds in the library are selected based on affinity, selectivity, structural similarity and clinical development phase. Additionally we source several expert opinion "best-in-class" lists. To use LibraryR, simply submit a list of genes for which the library should be designed, or load one of the example gene-sets, click 'Submit' and adjust the inclusion criteria to fit your research purpose. The library will be adjusted based on the input genes and inclusion criteria.
</p>

Visit on the web:

http://www.smallmoleculesuite.org/apps/LibraryR/

Or run locally in R:

```r
shiny::runGitHub("sorgerlab/smallmoleculesuite", subdir = "LibraryR")
```
<br>

# Development and links

Design/idea by [Nienke Moret](https://scholar.harvard.edu/nienkemoret) and [Marc Hafner](https://scholar.harvard.edu/hafner) (HMS - LINCS data and signature generation center)<br>
R code by [Nienke Moret](https://scholar.harvard.edu/nienkemoret) (HMS - LINCS data and signature generation center)<br>
Shiny/R web application development by [Nicholas Clark](https://github.com/NicholasClark) (U of Cincinnati - LINCS data coordination and integration center)<br>
Supervision by [Peter Sorger](https://sorger.med.harvard.edu/people/peter-sorger-phd/) (HMS - LINCS data and signature generation center)<br>
Icon design and development by [Vasileios Stathias](http://ccs.miami.edu/team_member/vasileios-vas-stathias/) (U of Miami - LINCS data coordination and integration center)

This work was supported by NIH grants **U54-HL127365**, **U24-DK116204** and **U54-HL127624**.

### Related web-tools

**LINCS data portal** - A unified resource for accessing all LINCS dataset packages and entities.
<br>http://lincsportal.ccs.miami.edu/dcic-portal/

**iLINCS** - A data analysis platform aimed at developing statistical methods and computational tools for integrative analysis of the data produced by the LINCS program.
<br>http://www.ilincs.org/


### BD2K-LINCS Project <img src="SelectivitySelectR/www/dcic.png" height = "50" width= "85" alt="BD2K-LINCS">

**HMS-LINCS Small Molecule Library**<br>http://lincs.hms.harvard.edu/db/sm/

**NIH LINCS Consortium**<br>http://www.lincsproject.org/

**HMS LINCS Center**<br>http://lincs.hms.harvard.edu/

**LINCS Data and Signature Generation Centers (DSGCs)**<br>http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers

**BD2K-LINCS Data Coordination and Integration Center (DCIC)**<br>http://lincs-dcic.org/

### Harvard Medical School <img src="SelectivitySelectR/www/logo_harvard_150.png" height = "50" width = "42" alt = "Harvard Medical School">
**Laboratory of Systems Pharmacology (LSP)**<br>http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/

**Sorger Lab**<br>http://sorger.med.harvard.edu/

### University of Cincinnati <img src="shared/uc_logo_crop.png" height = "50" width ="64"  alt = "University of Cincinnati">

**Laboratory for Statistical Genomics and Systems Biology**<br>http://eh3.uc.edu/