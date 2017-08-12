ui <- fluidPage(
  mainPanel(
    titlePanel("Shiny PCA Maker"),
        
        tabsetPanel(
          
          tabPanel("Input",
                   fluidRow(column(4,
                   p("Count matrix and metadata must be uploaded separately.  Count matrices will have rows as genes (or some other feature),
                      and columns as samples. The values of the cells are the 'counts' or some other measure of expression, which can be provided as raw or normalized.
                      Metadata files are provided with columns as conditions/phenotypes/other and rows as samples.  The first column
                      must include the sample names that match the sample names from the count matrix file. Examples below are taken from ",
                     a("GSE81741.", href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE81741")
                     ),
                   p("Before uploading your data, check that it is clean, especially ensure that
                     the the numeric variables contain only the digits 0-9 or NA (to indicate missing data)."),
                   p("Rows that contain one or more NAs will be excluded from the PCA."),
                   p("Columns that contain a mixture of numbers and text will not be included in the computation of the PCA results.")
                   ),
                   column(4,
                   h3('Count Matrix'),
                   a("(Count file example)", href= "./GSE81741.counts.tsv"),
                   fileInput('count_file', 'Choose a file:',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),
                   radioButtons('count_sep', 'Separator',
                                c(Tab='\t',
                                  Comma=',',
                                  Semicolon=';'
                                  ),
                                '\t'),
          
                   radioButtons('count_quote', 'Quotes around strings',
                                c(
                                  'Both'="\"'",
                                  'Double Quote'='"',
                                  'Single Quote'="'",
                                  None=''
                                ),
                                "\"'")
                   ),column(4,
                   h3('Metadata'),
                   a("(Metadata file example)", href= "./GSE81741.metadata.tsv"),
                   fileInput('metadata_file', 'Choose a file:',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),
                   radioButtons('metadata_sep', 'Separator',
                                c(Tab='\t',
                                  Comma=',',
                                  Semicolon=';'),
                                '\t'),
                   
                   radioButtons('metadata_quote', 'Quotes around strings',
                                c(
                                  'Both'="\"'",
                                  'Double Quote'='"',
                                  'Single Quote'="'",
                                  None=''
                                  ),
                                "\"'")
                  )),
                  fluidRow(column(2,
                          actionButton('validateButton',
                                       'Validate Input'),
                          offset = 5
                  ))
                 ), # end file  tab
          
          tabPanel("Parameters",
                   # commenting out the conditional portion; fix this
                    #verbatimTextOutput("validated"),
                    #conditionalPanel(
                    #  condition = "output.validated == '0'",
                    #  p("The input datasets have not yet been validated.  Please return to the 'Input' tab and try again")
                    #),
                    #conditionalPanel(
                    #  condition = "output.validated == '1'",
                   fluidRow(column(4,
                   p("Select options for the PCA computation (we are using the ", a("prcomp", href = "http://stat.ethz.ch/R-manual/R-patched/library/stats/html/prcomp.html"), "function):"),
                   checkboxInput(inputId = 'center',
                                 label = 'Shift variables to be zero-centered',
                                 value = TRUE),
                   checkboxInput(inputId = 'scale_data',
                                 label = 'Scale variables to have unit variance',
                                 value = TRUE),
                   radioButtons('normalization', 'Normalization',
                                choices = c('None'='NONE',
                                            'Variance Stabilizing Transform (vst)'='vst', 
                                            'Regularized logarithm (rlog) - WARNING: this can take considerable time'='rlog'), 
                                selected = 'NONE')
                   ),column(4,
               p("Choose the samples to include in the PCA."),
               p("The PCA is automatically re-computed each time you change your selection."),
               uiOutput("choose_samples_pca")
                   ))
                   #)
          ), # end  tab

          tabPanel("Plots",
                   h2("Scree plot"),
                   p("The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %) "),
                   plotOutput("plot2", height = "300px"),
                   tags$hr(),
                   h2("PC plot: zoom and select points"),
                   #p("Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful)."),
                   p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                   p("Then select points on zoomed plot below to get more information about the points."),
                   p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
                   fluidRow(column(8,
                   plotOutput ("z_plot1", height = 400,
                                brush = brushOpts(
                                  id = "z_plot1Brush",
                                  resetOnNew = TRUE))
                   ), column(4,
                             p("Select the grouping variable."),
                             uiOutput("the_grouping_variable"),
                             p("Select the PCs to plot"),
                             uiOutput("the_pcs_to_plot_x"),
                             uiOutput("the_pcs_to_plot_y"),
                             checkboxInput(inputId = 'draw_ellipse',
                                           label = 'Draw ellipse around groups',
                                           value = TRUE),
                             checkboxInput(inputId = 'label_points',
                                           label = 'Use sample labels for data points',
                                           value = FALSE)
                             # actionButton('resetZoomButton',
                             #              'Reset Zoom')
                   )),
                   #tags$hr(),
                   fluidRow(column(8,
                   #p("Click and drag on the plot below to select points, and inspect the table of selected points below"),
                  plotOutput("z_plot2", height = 400,
                            brush = brushOpts(
                               id = "z_plot2Brush",
                               resetOnNew = TRUE))
                   )),
                   p("Details of the selected points"),
                  tableOutput("brush_info_after_zoom")
          ), # end  tab 
          
          tabPanel("Output",
                   p("Output of the PCA function"),
                   verbatimTextOutput("pca_details"),
                   tags$hr()
                   # TODO: determine which of the following outputs are problematic
                   # p("Among SPSS users, these tests are considered to provide some guidelines on the suitability of the data for a principal components analysis. However, they may be safely ignored in favour of common sense. Variables with zero variance are excluded."),
                   # tags$hr(),
                   # p("Here is the output of Bartlett's sphericity test. Bartlett's test of sphericity tests whether the data comes from multivariate normal distribution with zero covariances. If p > 0.05 then PCA may not be very informative"),
                   # verbatimTextOutput("bartlett"),
                   # tags$hr(),
                   # p("Here is the output of the Kaiser-Meyer-Olkin (KMO) index test. The overall measure varies between 0 and 1, and values closer to 1 are better. A value of 0.6 is a suggested minimum. "),
                   # verbatimTextOutput("kmo")
                   
          ), # end  tab          
          
          tabPanel("License",
                   p("The code for this Shiny app is online at ", a("https://github.com/LJI-Bioinformatics/Shiny-PCA-Maker"), ". Please post any feedback, question, etc. as an ", a("issue on github", href = "https://github.com/LJI-Bioinformatics/Shiny-PCA-Maker/issues/new"), "."),
                   p("The code for this Shiny app was forked from ", a("https://github.com/benmarwick/Interactive_PCA_Explorer")), 
                   p("The text is licensed ", a("CC-BY", href = "http://creativecommons.org/licenses/by/4.0/"), " and the code ", a(href = "https://opensource.org/licenses/GPL-3.0", "GPL-3.0"), ".")
                   
                   
          ) # end  tab 
          
          
          ))) 