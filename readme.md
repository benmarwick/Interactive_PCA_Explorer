# Interactive PCA Explorer

This [Shiny](http://shiny.rstudio.com/) application takes a CSV file of clean data, allows you to inspect the data and compute a Principal Components Analysis, and will return several diagnostic plots and tables. The plots include a tableplot, a correlation matrix, a scree plot, and a biplot of Principal Components.

You can chose which columns to include in the PCA, and which column to use as a grouping variable. You can choose the center and/or scale the data, or not. You can choose which PCs to include on the biplot.

The biplot of PCs is interactive, so you can click on points or select points and inspect the details of those points in a table. 

## How to run or install

There are three ways to run/install this app.

First, you can access the app online at <https://benmarwick.shinyapps.io/interactive_pca_explorer/>. You can upload your CSV there and use all the functions. 

Second, you can run it on your computer like so:

```
library(shiny)
runGitHub("interactive_pca_explorer", "benmarwick")

```

Third, you can clone this repo to have the code on your computer, and run the app from there, like so:

```
# First clone the repository with git. If you have cloned it into
# ~/interactive_pca_explorer, first change your working directory to ~/interactive_pca_explorer, then use runApp() to start the app.
setwd("~/interactive_pca_explorer") # change to match where you downloaded this repo to
runApp() # runs the app 
```

## How to use

Start on the first (left-most) tab to upload your CSV file, then click on each tab, in order from left to right, to see the results.

## Screenshots

Here's what it looks like. Here we have input a CSV file that contain the [iris data](https://en.wikipedia.org/wiki/Iris_flower_data_set) (included with this app).

![](figures/001_input.png)   

Then we can see some simple descriptions of the data, and the raw data at the bottom of the page.   


![](figures/002_inspect.png)    

Below we see how we can choose the variables to explore in a correlation matrix. We also have a table that summarizes the correlations and gives p-values.  

![](figures/003_corr.png)  

Below we have a few popular diagnostic tests that many people like to do before doing a PCA. They're not very informative and can be skipped, but people coming from SPSS might feel more comfortable if they can see them here also. 

![](figures/004_diag.png)   

Below are the options for computing the PCA. We can choose which columns to include, and a few details about the PCA function. We are using the [prcomp](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html) function to compute the PCA. 

![](figures/005_compute.png)    

Here are the classic PCA plots. First is the scree plot summarizing how important the first few PCs are. Second is the interactive PC biplot. You can see that I've used my mouse to draw a rectangle around a few of the points in the biplot (this is called 'brushing') and in the table below we can see the details of those points in the selected area. We can choose which column to use for grouping (this only affects the colouring of the plot, it doesn't change the PCA results), and we can choose which PCs to show on the plot. 

![](figures/006_pca_plots.png)

Finally we have some of the raw output from the PCA.

![](figures/007_pca_output.png)


## Feedback, contributing, etc.

Please [open an issue](https://github.com/benmarwick/wordcountaddin/issues/new) if you find something that doesn't work as expected. Note that this project is released with a [Guide to Contributing](CONTRIBUTING.md) and a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.