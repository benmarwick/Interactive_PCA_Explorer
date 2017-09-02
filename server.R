# global items

#TODO: allow to change shape of points - see here: https://www.bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html

list.of.packages <- c("ggplot2",
                      "DT",
                      "psych",
                      "Hmisc",
                      "MASS",
                      "DESeq2",
                      "genefilter",
                      "data.table")

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) {
   #throw an error
  plist <- paste(new.packages, sep=",")
   stop(paste("The following packages are not installed: ", plist, "\n", sep=""))
}
# load all these
lapply(list.of.packages, library, character.only = TRUE)

# updating the max upload size to 1GB
options(shiny.maxRequestSize = 1024 * 1024 ^ 2)
options(shiny.reactlog = TRUE)

server <- function(input, output, session) {
  data_validated <- 0
  
  # initialize the output validated flag to 0
  output$validated <- reactive({
    data_validated
  })
  
  # read in the CSV
  # this is reactive and should only change if the CSV file is changed
  the_data_fn <- reactive({
    data_validated <- 0
    inFile <- input$count_file
    inFileURL <- input$count_file_url
    
    # prioritize reading from the file box over the URL
    #TODO: if a file is selected, the URL should disappear
    
    # initialize the data to null
    the_data <- NULL
    
    if (!is.null(inFile)) {
      the_data <- read.csv(
        inFile$datapath,
        header = TRUE,
        sep = input$count_sep,
        quote = input$count_quote,
        row.names = 1,
        stringsAsFactors = TRUE,
        check.names = FALSE
      )
    } else if (!is.null(inFileURL)) {
      #TODO: set separator parameters
      # read the data using fread from data.table library
      temp_data <- fread(inFileURL,
                         header = TRUE,
                         data.table=FALSE,
                         sep = input$count_sep,
                         stringsAsFactors = TRUE,
                         check.names = FALSE)
      # since we can't tell it about row names the first
      # column will contain the row names; we create a new
      # data frame from all but the first column, then assign
      # the row names
      the_data <- temp_data[,2:length(temp_data)]
      rownames(the_data) <- temp_data[,1]
    } else {
      return(NULL)
    }
    
    # transform the data so that the rows are samples and columns are genes
    the_data <- as.data.frame(t(the_data))
    
    # sort the countData by row names for good measure
    the_data <- the_data[order(row.names(the_data)), ]
    return(the_data)
  })
  
  the_metadata_fn <- reactive({
    data_validated <- 0
    inFile <- input$metadata_file
    inFileURL <- input$metadata_file_url
    the_metadata <- NULL
    if (!is.null(inFile)) {
      the_metadata <-   read.csv(
        inFile$datapath,
        header = TRUE,
        sep = input$metadata_sep,
        quote = input$metadata_quote,
        row.names = 1,
        stringsAsFactors = TRUE,
        check.names = FALSE
      )
    } else if (!is.null(inFileURL)) {
      temp_data <- fread(inFileURL,
                         header = TRUE,
                         data.table=FALSE,
                         sep = input$metadata_sep,
                         stringsAsFactors = TRUE,
                         check.names = FALSE)
      the_metadata <- temp_data[,2:length(temp_data)]
      rownames(the_metadata) <- temp_data[,1]      
    } else {
      return(NULL)
    }
    # sort the colData by row names for good measure
    the_metadata <- the_metadata[order(row.names(the_metadata)), ]
    # make each column a factor
    the_metadata[1:length(the_metadata)] <-
      as.data.frame(lapply(the_metadata, factor))
    return(the_metadata)
  })
  
  # combine the data & metadata for PCA visualization
  # and validate that data are good
  combined_data_fn <- function() {
    
    the_data <- the_data_fn()
    
    num_rows <- length(the_data[, 1])
    num_cols <- length(the_data)
    
    # check if it is small
    if (num_cols < 3) {
      validationModal(
        msg = paste(
          "Count dataset seems small with ",
          num_rows,
          " rows and ",
          num_cols,
          " columns. Check that it is formatted correctly and the proper delimiter was selected and try again.  See the README for more information on the format."
        )
      )
      return(-1)
    }
    
    # now load the metadata
    the_metadata <- the_metadata_fn()
    
    num_rows <- length(the_metadata[, 1])
    num_cols <- length(the_metadata)
    
    # check if it is small
    if (num_cols < 2) {
      validationModal(
        msg = paste(
          "Metadata dataset seems small with ",
          num_rows,
          " rows and ",
          num_cols,
          " columns. Check that it is formatted correctly and the proper delimiter was selected and try again.  See the README for more information on the format."
        )
      )
      return(-1)
    }
    
    # check that all samples from the count data are present in the metadata and vice versa
    metadata_names <- rownames(the_metadata)
    countdata_names <- rownames(the_data)
    
    countdata_missing_from_metadata <-
      countdata_names[!(countdata_names %in% metadata_names)]
    metadata_missing_from_countdata <-
      metadata_names[!(metadata_names %in% countdata_names)]
    
    if (length(countdata_missing_from_metadata) > 0) {
      missing_names_string = paste(countdata_missing_from_metadata, sep = ",")
      validationModal(
        msg = paste(
          "Some sample names from the count data are missing from the metadata:\n",
          missing_names_string,
          sep = ""
        )
      )
      return(-1)
    }
    
    if (length(metadata_missing_from_countdata) > 0) {
      missing_names_string = paste(metadata_missing_from_countdata, sep = ",")
      validationModal(
        msg = paste(
          "Some sample names from the metadata are missing from the countdata:\n",
          missing_names_string,
          sep = ""
        )
      )
      return(-1)
    }
    
    # now combine them according to the row / column names
    all_data <- merge(the_data, the_metadata, by = "row.names")
    # assign the row names and remove the row.names column
    rownames(all_data) <- all_data$Row.names
    all_data <- all_data[-1]
    # sort by row names
    all_data <- all_data[order(row.names(all_data)), ]
    
    return(all_data)
    
  }
  
  # display a table of the CSV contents
  output$contents <-  DT::renderDataTable({
    #
    the_data_fn()
  })
  
  # display a summary of the CSV contents
  output$summary <-  renderTable({
    the_data <- the_data_fn()
    psych::describe(the_data)
    cat(file = stderr(), "past describe")
  })
  
  output$selectNumGenes <- renderUI({
    
    the_data <- the_data_fn()
    
    max_genes = length(the_data[1,])
    
    sliderInput('num_top_genes', 'Number of genes to use for calculating PCA (sorted by variance)',
                min=100,
                max=max_genes,
                step=100,
                value=min(max_genes))
  })
  
  
  # Check boxes to choose samples to display on the plot
  output$choose_samples_display <- renderUI({
    the_metadata <- pca_objects()$the_metadata
    
    samplenames <- rownames(the_metadata)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("display_samples",
                       "Choose samples to display on the plot:",
                       choices  = samplenames,
                       selected = samplenames)
  })
  
  # Check boxes to choose columns
  output$choose_samples_pca <- renderUI({
    all_the_data <- combined_data_fn()
    
    samplenames <- rownames(all_the_data)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("samples",
                       "Choose samples to include in the PCA calculation:",
                       choices  = samplenames,
                       selected = samplenames)
  })
  
  # choose a grouping variable
  output$the_grouping_variable <- renderUI({
    the_metadata <- the_metadata_fn()
    
    the_data_group_cols <- names(the_metadata)
    
    # drop down selection
    selectInput(
      inputId = "the_grouping_variable",
      label = "Color by:",
      choices = c("None", the_data_group_cols)
    )
    
  })
  
  # run the PCA an create the necessary data frames
  pca_objects <- reactive({
    withProgress(message = 'PCA calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,
                 {
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
                   
                   # TODO: move this into 'the_data_fn' or somehow allow for normalization to not have to be recalculated each time any of the PCA
                   #       parameters is changed...although maybe it should?
                   
                   # subselect the samples
                   the_data_subset <-
                     the_data[which(rownames(the_data) %in% samples),]
                   incProgress(0.1)
                   
                   # remove columns with 0 variance:
                   the_data_subset <-
                     the_data_subset[, !apply(the_data_subset, MARGIN = 2, function(x)
                       max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
                   incProgress(0.1)
                   
                   # subselect the genes (either given as a list or as the topX most variable)
                   rv <- rowVars(t(the_data_subset))
                   ntop = input$num_top_genes
                   if (ntop > length(rv)) {
                     ntop = length(rv)
                   }
                   select_genes <- order(rv, decreasing = TRUE)[seq_len(ntop)]
                   the_data_subset <- the_data_subset[,select_genes]
                   
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
                     print(paste(
                       "Unrecognized normalization type: ",
                       normalization,
                       sep = ""
                     ))
                   }
                   incProgress(0.2)
                   
                   the_metadata_subset <-
                     the_metadata[which(rownames(the_metadata) %in% rownames(the_data_subset)),]
                   all_the_data_subset <-
                     all_the_data[which(rownames(all_the_data) %in% rownames(the_data_subset)),]
                   incProgress(0.1)
                   
                   # from http://rpubs.com/sinhrks/plot_pca
                   pca_output <- prcomp(
                     na.omit(the_data_subset),
                     center = input$center,
                     scale = input$scale_data
                   )
                   incProgress(0.1)
                   
                   # data.frame of PCs
                   pcs_df <- cbind(all_the_data_subset, pca_output$x)
                   incProgress(0.1)
                 }) # end of withProgress
    return(list(
      pca_output = pca_output,
      pcs_df = pcs_df,
      the_metadata = the_metadata_subset
    ))
    
  })
  
  # output a numeric control with the range of the PCs
  # for selecting in the scree plot
  output$pc_range <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    numericInput("pc_range",
                 "Number of PCs to plot",
                 value=10,
                 min = 1,
                 max = length(pca_output[,1]),
                 width= '100px')  
    
  })
  
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(
      inputId = "the_pcs_to_plot_x",
      label = "X axis:",
      choices = colnames(pca_output),
      selected = 'PC1'
    )
  })
  
  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(
      inputId = "the_pcs_to_plot_y",
      label = "Y axis:",
      choices = colnames(pca_output),
      selected = 'PC2'
    )
  })
  
  output$SCREE_PLOT <- renderPlot({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev) ^ 2
    variance <- eig * 100 / sum(eig)
    cumvar <- paste(round(cumsum(variance), 1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    
    num_PCS_to_plot = input$pc_range
    
    # limit to 10 PCs
    eig_df <- eig_df[1:num_PCS_to_plot,]
    eig <- eig[1:num_PCS_to_plot]
    cumvar <- cumvar[1:num_PCS_to_plot]
    
    ggplot(eig_df, aes(reorder(PCs,-eig), eig)) +
      geom_bar(stat = "identity",
               fill = "white",
               colour = "black") +
      geom_text(label = cumvar,
                size = 4,
                vjust = -0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0, (max(eig_df$eig) * 1.1))
  })
  
  # PC plot
  pca_biplot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    pca_output <-  pca_objects()$pca_output
    
    # filter the pca objects for values that should not be plotted
    display_samples <- input$display_samples
    if (!is.null(display_samples)) {
      pcs_df <- pcs_df[which(rownames(pcs_df) %in% display_samples),]
      pca_output$x <- pca_output$x[which(rownames(pca_output$x) %in% display_samples),]
      #pca_output$sdev <- pca_output$sdev[which(rownames(pca_output$sdev) %in% display_samples),]
    }
    
    var_expl_x <-
      round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))] ^
              2 / sum(pca_output$sdev ^ 2), 1)
    var_expl_y <-
      round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))] ^
              2 / sum(pca_output$sdev ^ 2), 1)
    labels <- rownames(pca_output$x)
    grouping <- input$the_grouping_variable
    if (is.null(grouping)) {
      grouping = 'None'
    }
    
    #TODO: separate the plot + legend since the legends can vary in size considerably
    
    if (grouping == 'None') {
      pc_plot <<- ggplot(pcs_df,
                         aes_string(input$the_pcs_to_plot_x,
                                    input$the_pcs_to_plot_y))
    } else {
      pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
      pc_plot <<- ggplot(
        pcs_df,
        aes_string(
          input$the_pcs_to_plot_x,
          input$the_pcs_to_plot_y,
          colour = 'fill_'
        )
      )
    }
    
    if (input$label_points) {
      pc_plot = pc_plot + geom_text(aes(label = labels),  size = 5)
    } else {
      pc_plot = pc_plot + geom_point()
    }
    
    pc_plot <- pc_plot +
      theme_gray(base_size = 14)
    
    if (grouping != 'None') {
      pc_plot <- pc_plot +
        scale_colour_discrete(name = "groups") +
        theme(legend.position = "top")
    }
    
    if (input$draw_ellipse) {
      pc_plot = pc_plot + stat_ellipse(
        aes(fill = 'fill_'),
        geom = "polygon",
        alpha = 0.1,
        show.legend = FALSE
      )
    }
    
    pc_plot <- pc_plot +
      coord_equal() +
      xlab(paste0(
        input$the_pcs_to_plot_x,
        " (",
        var_expl_x,
        "% explained variance)"
      )) +
      ylab(paste0(
        input$the_pcs_to_plot_y,
        " (",
        var_expl_y,
        "% explained variance)"
      ))
    
    
    pc_plot
  })
  
  # This is the main PCA biplot
  # TODO: determine how to to make the zoom/reset work in this plot instead of dividing the functionality
  #       between two plots
  output$PCA_PLOT <- renderPlot({

    pca_biplot()
    
  })
  
  # This is the zoomed in plot
  # for zooming
  output$ZOOMED_PLOT <- renderPlot({
    brush <- input$PCA_PLOTBrush
    
    if (is.null(brush)) {
      pca_biplot()
    } else {
      pca_biplot() + coord_cartesian(
        xlim = c(brush$xmin, brush$xmax),
        ylim = c(brush$ymin, brush$ymax)
      )
    }
    
  })
  
  output$brush_info_after_zoom <- renderTable({
    # get the pca metadata
    the_metadata_subset <- pca_objects()$the_metadata
    metadata_cols <- names(the_metadata_subset)
    
    brush <- input$PCA_PLOTBrush
    the_pca_data <- pca_objects()$pcs_df
    if (!is.null(brush)) {
      the_pca_data <- brushedPoints(the_pca_data, brush)
    }
    
    # now return only the columns from the pca data tha match the metadata colnames
    data.frame(sample=rownames(the_pca_data),the_pca_data[, metadata_cols])
    
  })
  
  output$pca_details <- renderPrint({
    #
    print(pca_objects()$pca_output$x)
    summary(pca_objects()$pca_output)
    
  })
  
  # Validate the input and set the 'input validated variable'
  observeEvent(input$validateButton, {
    the_data <- the_data_fn()
    
    num_rows <- length(the_data[, 1])
    num_cols <- length(the_data)
    
    # check if it is small
    if (num_cols < 3) {
      validationModal(
        msg = paste(
          "Count dataset seems small with ",
          num_rows,
          " rows and ",
          num_cols,
          " columns. Check that it is formatted correctly and the proper delimiter was selected and try again.  See the README for more information on the format."
        )
      )
      return(-1)
    }
    
    # now load the metadata
    the_metadata <- the_metadata_fn()
    
    num_rows <- length(the_metadata[, 1])
    num_cols <- length(the_metadata)
    
    # check if it is small
    if (num_cols < 2) {
      validationModal(
        msg = paste(
          "Metadata dataset seems small with ",
          num_rows,
          " rows and ",
          num_cols,
          " columns. Check that it is formatted correctly and the proper delimiter was selected and try again.  See the README for more information on the format."
        )
      )
      return(-1)
    }
    
    # check that all samples from the count data are present in the metadata and vice versa
    metadata_names <- rownames(the_metadata)
    countdata_names <- rownames(the_data)
    
    countdata_missing_from_metadata <-
      countdata_names[!(countdata_names %in% metadata_names)]
    metadata_missing_from_countdata <-
      metadata_names[!(metadata_names %in% countdata_names)]
    
    if (length(countdata_missing_from_metadata) > 0) {
      missing_names_string = paste(countdata_missing_from_metadata, sep = ",")
      validationModal(
        msg = paste(
          "Some sample names from the count data are missing from the metadata:\n",
          missing_names_string,
          sep = ""
        )
      )
      return(-1)
    }
    
    if (length(metadata_missing_from_countdata) > 0) {
      missing_names_string = paste(metadata_missing_from_countdata, sep = ",")
      validationModal(
        msg = paste(
          "Some sample names from the metadata are missing from the countdata:\n",
          missing_names_string,
          sep = ""
        )
      )
      return(-1)
    }
    
    #TODO:  if we get here, set the data_validated variable to 1
    data_validated <- 1
    
    validationModal(msg = "Input looks good!", title = "Validation passed!")
    
  })
  
  validationModal <- function(msg = "", title = "Validation failed") {
    showModal(modalDialog(p(msg),
                          title = title,
                          footer = tagList(
                            modalButton("Dismiss"),
                            actionButton("returnToInput", "Return To Input Tab")
                          )))
    
  }
  
  # download PCA data
  output$downloadPCAOutput <- downloadHandler(
    filename = 'PCA_data.tsv',
    content = function(file) {
      data = pca_objects()$pca_output$x
      write.table(data, file, sep="\t")
    }
  )
  
  # code to return to the input tab when validation fails
  observeEvent(
    input$returnToInput,
    {
      updateTabsetPanel(session, "mainTabPanel", selected="Input")
      removeModal(session)
    }
    
  )
  
  # get the URL of the count file supplied by the user
  output$countFileURL <- renderUI({
         query <- parseQueryString(session$clientData$url_search)
         
         cfu <- query$countFileURL
         
         if(!is.null(cfu)) {
          updateRadioButtons(session, 'count_file_method', selected = 'download')
         }
         textInput('count_file_url','Count file URL:', value=cfu, width=600)
         
    })
  
  output$metadataFileURL <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    
    mfu <- query$metadataFileURL
    if(!is.null(mfu)) {
      updateRadioButtons(session, 'metadata_file_method', selected = 'download')
    }
    textInput('metadata_file_url','Metadata file URL:', value=mfu, width=600)
  })
  
  
  
  
}