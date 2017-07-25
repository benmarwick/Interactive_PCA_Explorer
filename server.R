# global items 

# check if pkgs are installed already, if not, install automatically:
# (http://stackoverflow.com/a/4090208/1036500)
list.of.packages <- c("ggplot2", 
                      "DT", 
                      "GGally",
                      "psych",
                      "Hmisc",
                      "MASS",
                      "tabplot")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load all these
lapply(list.of.packages, require, character.only = TRUE)

# updating the max upload size to 1GB
options(shiny.maxRequestSize=1024*1024^2)

server <- function(input, output) {
  
  # read in the CSV
  # this is reactive and should only change if the CSV file is changed
  the_data_fn <- reactive({
    inFile <- input$count_file
    if (is.null(inFile)) return(NULL)
    the_data <-   read.csv(inFile$datapath, header = TRUE,
                           sep = input$count_sep, quote = input$count_quote, 
                           row.names = 1, stringsAsFactors=TRUE, check.names = FALSE)
    
    # transform the data so that the rows are samples and columns are genes
    the_data <- as.data.frame(t(the_data))
    
    # sort the countData by row names for good measure
    the_data <- the_data[order(row.names(the_data)),]
    # TODO: implement the ability to subselect samples/genes
    return(the_data)
  })
  
  the_metadata_fn <- reactive({
    inFile <- input$metadata_file
    if (is.null(inFile)) return(NULL)
    the_metadata <-   read.csv(inFile$datapath, header = TRUE,
                               sep = input$metadata_sep, quote = input$metadata_quote, 
                               row.names=1, stringsAsFactors=TRUE, check.names=FALSE)
    # sort the colData by row names for good measure
    the_metadata <- the_metadata[order(row.names(the_metadata)),]
    # make each column a factor
    the_metadata[1:length(the_metadata)] <- as.data.frame(lapply(the_metadata,factor))
    # TODO: implement the ability to subselect metadata
    return(the_metadata)
  })
  
  # combine the data & metadata for PCA visualization
  combined_data_fn <- function() {
    the_data <- the_data_fn()
    the_metadata <- the_metadata_fn()
    
    # now combine them according to the row / column names
    all_data <- merge(the_data, the_metadata, by="row.names")
    # assign the row names and remove the row.names column
    rownames(all_data) <- all_data$Row.names
    all_data <- all_data[-1]
    # sort by row names
    all_data <- all_data[order(row.names(all_data)),]
    
    return(all_data)
    
  }
  
  # removed this functionality for minimal version - restore as time permits
  # tableplot
  # output$tableplot <- renderPlot({
  #   if(is.null(the_data_fn())) return()
  #   the_data <- the_data_fn()
  #   tabplot::tableplot(the_data)
  #   
  # })
  
  # display a table of the CSV contents
  output$contents <-  DT::renderDataTable({
    #
    the_data_fn()
  })
  
  # display a summary of the CSV contents
  output$summary <-  renderTable({
    the_data <- the_data_fn()
    psych::describe(the_data)
    cat(file=stderr(),"past describe")
  })
  
  # removed this functionality for minimal version - restore as time permits
  # # Check boxes to choose columns
  # output$choose_columns_biplot <- renderUI({
  #   
  #   the_data <- the_data_fn()
  #   
  #   colnames <- names(the_data)
  #   
  #   # Create the checkboxes and select them all by default
  #   checkboxGroupInput("columns_biplot", "Choose up to five columns to display on the scatterplot matrix",
  #                      # TODO: fix this
  #                      # use the columns that are factors as choices
  #                      #choices = get_factors(),
  #                      choices  = colnames,
  #                      selected = colnames[1:5])
  # })
  # 
  # # corr plot
  # output$corr_plot <- renderPlot({
  #   the_data <- the_data_fn()
  #   # Keep the selected columns
  #   columns_biplot <-    input$columns_biplot
  #   the_data_subset_biplot <- the_data[, columns_biplot, drop = FALSE]
  #   ggpairs(the_data_subset_biplot)
  #    })
  # 
  # # corr tables
  # output$corr_tables <- renderTable({
  #   the_data <- the_data_fn()
  #   # we only want to show numeric cols
  #   the_data_num <- the_data[,sapply(the_data,is.numeric)]
  #   # exclude cols with zero variance
  #   the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
  #   
  #   
  #     res <- Hmisc::rcorr(as.matrix(the_data_num))
  #     cormat <- res$r
  #     pmat <- res$P
  #     ut <- upper.tri(cormat)
  #    df <- data.frame(
  #       row = rownames(cormat)[row(cormat)[ut]],
  #       column = rownames(cormat)[col(cormat)[ut]],
  #       cor  = (cormat)[ut],
  #       p = pmat[ut]
  #     )
  #    with(df, df[order(-cor), ])
  #   
  # })
  
  output$bartlett <- renderPrint({
    the_data <- the_data_fn()
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    cortest.bartlett(cor(the_data_num), n = nrow(the_data_num))
  })  
  
  output$kmo <- renderPrint({
    the_data <- the_data_fn()
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    # R <- cor(the_data_num)
    # KMO(R)
    
    # http://www.opensubscriber.com/message/r-help@stat.math.ethz.ch/7315408.html
    # KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy 
    kmo = function( data ){ 
      
      library(MASS) 
      X <- cor(as.matrix(data)) 
      iX <- ginv(X) 
      S2 <- diag(diag((iX^-1))) 
      AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix 
      IS <- X+AIS-2*S2                         # image covariance matrix 
      Dai <- sqrt(diag(diag(AIS))) 
      IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix 
      AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix 
      a <- apply((AIR - diag(diag(AIR)))^2, 2, sum) 
      AA <- sum(a) 
      b <- apply((X - diag(nrow(X)))^2, 2, sum) 
      BB <- sum(b) 
      MSA <- b/(b+a)                        # indiv. measures of sampling adequacy 
      
      AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the 
      # correlation matrix. That is the 
      # negative of the partial correlations, 
      # partialling out all other variables. 
      
      kmo <- BB/(AA+BB)                     # overall KMO statistic 
      
      # Reporting the conclusion 
      if (kmo >= 0.00 && kmo < 0.50){ 
        test <- 'The KMO test yields a degree of common variance 
      unacceptable for FA.' 
      } else if (kmo >= 0.50 && kmo < 0.60){ 
        test <- 'The KMO test yields a degree of common variance miserable.' 
      } else if (kmo >= 0.60 && kmo < 0.70){ 
        test <- 'The KMO test yields a degree of common variance mediocre.' 
      } else if (kmo >= 0.70 && kmo < 0.80){ 
        test <- 'The KMO test yields a degree of common variance middling.' 
      } else if (kmo >= 0.80 && kmo < 0.90){ 
        test <- 'The KMO test yields a degree of common variance meritorious.' 
      } else { 
        test <- 'The KMO test yields a degree of common variance marvelous.' 
      } 
      
      ans <- list(  overall = kmo, 
                    report = test, 
                    individual = MSA, 
                    AIS = AIS, 
                    AIR = AIR ) 
      return(ans) 
      
    }    # end of kmo() 
    kmo(na.omit(the_data_num))
    
  }) 
  
  
  #commented this sectio nout as we will use all columns (Genes)
  #add a new section on picking the rows (samples to exclude)
  # Check boxes to choose columns
  output$choose_samples_pca <- renderUI({
    
    the_data <- the_data_fn()
    
    samplenames <- rownames(the_data)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("samples", "Choose samples",
                       choices  = samplenames,
                       selected = samplenames)
  })
  
  # choose a grouping variable
  output$the_grouping_variable <- renderUI({
    the_metadata <- the_metadata_fn()
    
    the_data_group_cols <- names(the_metadata)
    
    # drop down selection
    selectInput(inputId = "the_grouping_variable", 
                label = "Grouping variable:",
                choices=c("None", the_data_group_cols))
    
  })
  
  
  pca_objects <- reactive({
    withProgress(message = 'PCA calculation in progress',
                 detail = 'This may take a while...', value = 0, {

    the_data <- na.omit(the_data_fn())
    incProgress(0.1)
    the_metadata <- the_metadata_fn()
    incProgress(0.1)
    all_the_data <- combined_data_fn()
    incProgress(0.1)
    
    # Keep the selected samples
    samples <-    input$samples
    # if the samples have not been selected, use all
    if (is.null(samples)) {
      samples <- rownames(the_data)
    }
    
    #the_data_subset <- the_data
    # TODO: move this into 'the_data_fn' or somehow allow for normalization to not have to be recalculated each time any of the PCA
    #       parameters is changed...although maybe it should?
    the_data_subset <- the_data[which(rownames(the_data) %in% samples), ]
    incProgress(0.1)
    
    # remove columns with 0 variance:
    the_data_subset <- the_data_subset[,!apply(the_data_subset, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    incProgress(0.1)
    
    # normalize, if we were requested to do so
    normalization = input$normalization
    if (normalization == 'rlog') {
      print("proceeding with rlog transformation")
      library(DESeq2)
      # for rlog & vst we first need to transform the data
      transformed_subset <- as.matrix(t(round(the_data_subset)))
      transformed_rlog_subset <- rlog(transformed_subset)
      the_data_subset <- t(transformed_rlog_subset)
    } else if (normalization == 'vst') {
      print("proceeding with vst transformation")
      library(DESeq2)
      # for rlog & vst we first need to transform the data
      transformed_subset <- as.matrix(t(round(the_data_subset)))
      transformed_vst_subset <- vst(transformed_subset)
      the_data_subset <- t(transformed_vst_subset)
    } else if (is.null(normalization) | normalization == 'NONE') {
      print("no normalization requested")
    } else {
      print(paste("Unrecognized normalization type: ", normalization, sep=""))
    }
    incProgress(0.2)

    the_metadata_subset <- the_metadata[which(rownames(the_metadata) %in% rownames(the_data_subset)), ]
    all_the_data_subset <- all_the_data[which(rownames(all_the_data) %in% rownames(the_data_subset)), ]
    incProgress(0.1)
    
    # from http://rpubs.com/sinhrks/plot_pca
    pca_output <- prcomp(na.omit(the_data_subset), 
                         center = input$center, 
                         scale = input$scale_data)
    incProgress(0.1)
    
    # data.frame of PCs
    pcs_df <- cbind(all_the_data_subset, pca_output$x)
    incProgress(0.1)
                 }) # end of withProgress
    return(list(the_data = the_data, 
                the_data_subset = the_data_subset,
                pca_output = pca_output, 
                pcs_df = pcs_df,
                all_the_data = all_the_data_subset,
                the_metadata = the_metadata_subset))
    
  })
  
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_x", 
                label = "X axis:",
                choices= colnames(pca_output), 
                selected = 'PC1')
  })
  
  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_y", 
                label = "Y axis:",
                choices= colnames(pca_output), 
                selected = 'PC2')
  })
  
  
  
  output$plot2 <- renderPlot({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- paste(round(cumsum(variance),1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "white", colour = "black") +
      geom_text(label = cumvar, size = 4,
                vjust=-0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0,(max(eig_df$eig) * 1.1))
  })
  
  
  # PC plot
  pca_biplot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    pca_output <-  pca_objects()$pca_output
    
    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    labels <- rownames(pca_output$x)
    grouping <- input$the_grouping_variable
    if (is.null(grouping)) {
      grouping = 'None'
    }
    
    #TODO: consolidate these two versions of the plot
    #TODO: separate the plot + legend since the legends can vary in size considerably
    
    pc_plot <- ggplot(pcs_df)
    
    pc_plot <- pc_plot +
      geom_point() +
      theme_bw(base_size = 14) +
      scale_colour_discrete(guide = FALSE) +
      guides(fill = guide_legend(title = "groups")) +
      theme(legend.position="top") +
      coord_equal() +
      xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
      ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
    
    
    if (grouping == 'None') {
      pc_plot <- pc_plot + aes_string(input$the_pcs_to_plot_x, 
                           input$the_pcs_to_plot_y)
    } else {
      fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
      pc_plot <- pc_plot +  aes_string(input$the_pcs_to_plot_x, 
                                                   input$the_pcs_to_plot_y, 
                                                   fill =  'fill_', 
                                                   colour = 'fill_')
      + stat_ellipse(geom = "polygon", alpha = 0.1)
    }
    
    pc_plot
    
    # if(grouping == 'None'){
    #   # plot without grouping variable
    #   pc_plot_no_groups  <- ggplot(pcs_df, 
    #                                aes_string(input$the_pcs_to_plot_x, 
    #                                           input$the_pcs_to_plot_y
    #                                )) +
    #     
    #     geom_point() +
    #     #geom_text(aes(label = labels),  size = 5) +
    #     theme_bw(base_size = 14) +
    #     coord_equal() +
    #     xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
    #     ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
    #   # the plot
    #   pc_plot_no_groups
    #   
    #   
    # } else {
    #   # plot with grouping variable
    #   
    #   pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
    #   pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x, 
    #                                                input$the_pcs_to_plot_y, 
    #                                                fill = 'fill_', 
    #                                                colour = 'fill_'
    #   )) +
    #     stat_ellipse(geom = "polygon", alpha = 0.1) +
    #     geom_point() +
    #     theme_bw(base_size = 14) +
    #     scale_colour_discrete(guide = FALSE) +
    #     guides(fill = guide_legend(title = "groups")) +
    #     theme(legend.position="top") +
    #     coord_equal() +
    #     xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
    #     ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
    #   # the plot
    #   pc_plot_groups
    # }
    
    
  })
  
  output$brush_info <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush)
  })
  
  
  # for zooming
  output$z_plot1 <- renderPlot({
    
    pca_biplot() 
    
  })
  
  # commented out and replaced by calling brushes directly
  # # zoom ranges
  # zooming <- reactiveValues(x = NULL, y = NULL)
  # 
  # observe({
  #   brush <- input$z_plot1Brush
  #   if (!is.null(brush)) {
  #     zooming$x <- c(brush$xmin, brush$xmax)
  #     zooming$y <- c(brush$ymin, brush$ymax)
  #   }
  #   else {
  #     zooming$x <- NULL
  #     zooming$y <- NULL
  #   }
  # })
  
  
  # for zooming
  output$z_plot2 <- renderPlot({
    
    brush <- input$z_plot1Brush
    
    if (is.null(brush)) {
      pca_biplot()
    } else {
      pca_biplot() + coord_cartesian(xlim = c(brush$xmin, brush$xmax), ylim = c(brush$ymin, brush$ymax)) 
    }
    
  })
  
  #TODO: make this only output a list of points
  output$brush_info_after_zoom <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  })
  
  output$pca_details <- renderPrint({
    #
    print(pca_objects()$pca_output$x)
    summary(pca_objects()$pca_output)
    
  })
  
  output$Colophon <- renderPrint({
    
    
  })
  
  
}