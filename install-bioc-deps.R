if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install('DESeq2', update=FALSE, ask=FALSE)
BiocManager::install('genefilter', update=FALSE, ask=FALSE)