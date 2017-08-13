# Shiny PCA Maker

This [Shiny](http://shiny.rstudio.com/) application takes a CSV file of count/expression data and a CSV of metadata, allows you to select a few parameters to compute a Principal Components Analysis, returning several diagnostic plots and tables. The plots include a scree plot and a biplot of Principal Components.

You can chose which columns to include in the PCA, and which column to use as a grouping variable for coloring. You can choose to center, scale, or normalize (via rlog or vst). You can choose which PCs to include on the biplot.

The biplot of PCs is interactive, so you can click on points or select points and inspect the details of those points in a table. 

## How to run or install

There are several ways to run/install this app.

### Running from Github

If you have R/RStudio and all of the prerequisites installed (see below for dependency installation), you can run it directly from Github like so:

```
library(shiny)
runGitHub("shiny-pca-maker", "LJI-Bioinformatics", launch.browser = TRUE)
```

### Running locally

There are multiple ways to run locally, including with Docker or directly in R/RStudio.  Both require you to clone the git repository to your machine:

```
git clone https://github.com/LJI-Bioinformatics/Shiny-PCA-Maker.git LOCAL_DIR
```

Replace LOCAL_DIR with the directory into which you would like to clone.  For the rest of this README, we will assume it is in your home directory, at:

```
~/Shiny-PCA-Maker
```

#### Running locally with Docker

If you have [Docker](https://www.docker.com/) installed, you can start a container to run the server:

```
cd ~/Shiny-PCA-Maker
sh docker_start.sh
```

This will start a server at port 3838, which you can reach from your browser at:

http://localhost:3838


To stop the server:

```
cd ~/Shiny-PCA-Maker
sh docker_stop.sh
```

#### Running locally with R/RStudio

To run this app directly from your R/RStudio installation, first ensure that dependencies are installed (as described below).  Open up R/RStudio and type:

```
setwd("~/Shiny-PCA-Maker")
runApp(launch.browser = TRUE) 
```

### Dependency installation

If running from Github or locally with R/RStudio, you can install the dependencies with
the following commands:

```
install.packages(c('shiny', 'ggplot2', 'DT', 'GGally', 'psych', 'Hmisc', 'MASS', 'tabplot'))
source("https://bioconductor.org/biocLite.R")
biocLite('DESeq2')
```

## How to use

Start on the first (left-most) tab to upload/validate your count and metadata files, then click on each tab, in order from left to right, to see the results.

### Input data

This tool requires two input files:

#### Count Matrix

This is a matrix of counts/expression values where the rows are genes/features and the columns are samples.  The names of the columns are the sample names and they must correspond to the row names of the metadata (described below).  The format is similar to what would be accepted for the 'expr' slot of a [Bioconductor ExpressionSet](http://www.bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf).  Here is an [example count matrix](www/GSE81741.counts.tsv).

#### Metadata

This file should contain one row per sample and have columns that indicate properties of interest.  These will be the variables by which the PCA plots can be colored.  The format is similar to what would be accepted for the 'phenoData' slot of a [Bioconductor ExpressionSet](http://www.bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf).  Here is an [example metadata file](www/GSE81741.metadata.tsv).

## Feedback, contributing, etc.

Please [open an issue](https://github.com/LJI-Bioinformatics/Shiny-PCA-Maker/issues/new) if you find something that doesn't work as expected.

## License

This code is licensed under the [GPL v3.0](https://www.gnu.org/licenses/gpl.html).  Aternative, commercial-friendly licenses available upon request.