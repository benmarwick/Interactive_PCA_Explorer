# Shiny PCA Maker

This [Shiny](http://shiny.rstudio.com/) application takes a CSV file of clean data and a CSV of metadata, allows you to select a few parameters to compute a Principal Components Analysis, and will return several diagnostic plots and tables. The plots include a scree plot, and a biplot of Principal Components.

You can chose which columns to include in the PCA, and which column to use as a grouping variable. You can choose to center, scale, or normalize (via rlog or vst). You can choose which PCs to include on the biplot.

The biplot of PCs is interactive, so you can click on points or select points and inspect the details of those points in a table. 

## How to run or install

There are two ways to run/install this app.

First, you can run it on your computer like so:

```
library(shiny)
runGitHub("shiny-pca-maker", "LJI-Bioinformatics")

```

Second, you can clone this repo to have the code on your computer, and run the app from there, like so:

```
# First clone the repository with git. If you have cloned it into
# ~/shiny-pca-maker, first change your working directory to ~/shiny-pca-maker, then use runApp() to start the app.
setwd("~/shiny-pca-maker") # change to match where you downloaded this repo to
runApp() # runs the app 
```

This app depends on several R packages (ggplot2, DT, GGally, psych, Hmisc, MASS, tabplot). The app will check to see if you have them installed, and if you don't, it will try to download and install them for you.

## How to use

Start on the first (left-most) tab to upload your CSV files, then click on each tab, in order from left to right, to see the results.

## Screenshots

Here's what it looks like. Here we have input a CSV file that contain the [iris data](https://en.wikipedia.org/wiki/Iris_flower_data_set) (included with this app).

![](figures/001_input.png)   

Then we can see some simple descriptions of the data, and the raw data at the bottom of the page.   



Below we have a few popular diagnostic tests that many people like to do before doing a PCA. They're not very informative and can be skipped, but people coming from SPSS might feel more comfortable if they can see them here also. 

![](figures/004_diag.png)   

Below are the options for computing the PCA. We can choose which columns to include, and a few details about the PCA function. We are using the [prcomp](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html) function to compute the PCA. 

![](figures/005_compute.png)    

Here are the classic PCA plots. First is the scree plot summarizing how important the first few PCs are. Second is the interactive PC biplot. You can see that I've used my mouse to draw a rectangle around a few of the points in the biplot (this is called 'brushing') and in the table below we can see the details of those points in the selected area. We can choose which column to use for grouping (this only affects the colouring of the plot, it doesn't change the PCA results), and we can choose which PCs to show on the plot. 

![](figures/006_pca_plots.png)

Finally we have some of the raw output from the PCA.

![](figures/007_pca_output.png)


## Feedback, contributing, etc.

Please [open an issue](https://github.com/LJI-Bioinformatics/Shiny-PCA-Maker/issues/new) if you find something that doesn't work as expected.

## License

This code is licensed under the [GPL v3.0](https://www.gnu.org/licenses/gpl.html).  Aternative, commercial-friendly licenses available upon request.