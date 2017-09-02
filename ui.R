ui <- fluidPage(
    titlePanel("","Shiny PCA Maker"),
        
        tabsetPanel(
          
          tabPanel("Input",
                   checkboxInput("input_show_help", label = 'Show help text', value = FALSE),
                   conditionalPanel(condition="input.input_show_help==true",
                   p("Count matrix and metadata must be uploaded separately and can be supplied by browsing your filesystem or via a URL.  Count matrices will have rows as genes (or some other feature),
                      and columns as samples. The values of the cells are the 'counts' or some other measure of expression, which can be provided as raw or normalized.
                      Metadata files are provided with columns as conditions/phenotypes/other and rows as samples.  The first column
                      must include the sample names that match the sample names from the count matrix file. Examples below are taken from ",
                     a("GSE81741.", href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE81741")
                     ),
                   p("Before uploading your data, check that it is clean, especially ensure that
                     the the numeric variables contain only the digits 0-9 or NA (to indicate missing data)."),
                   p("Rows that contain one or more NAs will be excluded from the PCA."),
                   p("Columns that contain a mixture of numbers and text will not be included in the computation of the PCA results.")
                   ),# end conditionalPanel
                   checkboxInput("input_count_addl_options", label = 'Show additional options', value = FALSE),
                   fluidRow(
                   column(4,wellPanel(
                   h4('Count Matrix'),
                   a("(Count file example)", href= "./GSE81741.counts.tsv"),
                   radioButtons('count_file_method',
                                "Input method",
                                selected = 'upload',
                                inline = FALSE,
                                choiceNames = c('Upload from computer','Download from web'),
                                choiceValues = c('upload','download')),
                   conditionalPanel(condition="input.count_file_method=='upload'",
                   fileInput('count_file', '',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             ),
                             placeholder = "")
                   ),
                   conditionalPanel(condition="input.count_file_method=='download'",
                    uiOutput('countFileURL')
                   ),
                   conditionalPanel(condition="input.input_count_addl_options==true",
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
                                "\"'"))
                   )), # end column 1
                   column(4,wellPanel(
                          h4('Metadata'),
                          a("(Metadata file example)", href= "./GSE81741.metadata.tsv"),
                          fileInput('metadata_file', '',
                                    accept = c(
                                      'text/csv',
                                      'text/comma-separated-values',
                                      'text/tab-separated-values',
                                      'text/plain',
                                      '.csv',
                                      '.tsv'
                                    ),
                                    placeholder = ""),
                          uiOutput(('metadataFileURL')),
                          conditionalPanel(condition="input.input_count_addl_options==true",
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
                                       "\"'"))
                   ) # end well panel
                   ) # end column 2
                   ) # end fluidRow
                  ), # end tab panel
          tabPanel("Parameters",
                  fluidRow(column(4,
                   p("Select options for the PCA (we are using the ", a("prcomp", href = "http://stat.ethz.ch/R-manual/R-patched/library/stats/html/prcomp.html"), "function):"),
                   wellPanel(
                     # NOTE: this is placed on this tab, otherwise each time the slider is moved
                     # just a little bit, it will cause the plots to recalculate. We can get around
                     # this if we change to a different type of control
                    uiOutput('selectNumGenes'),
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
                   ) # end wellPanel
                   ),column(6,
               wellPanel(
                uiOutput("choose_samples_pca")
               )
                   ))
          ), # end  tab

          tabPanel("Plots",
                   h3("Scree plot"),
                   p("The scree plot shows the variances of each PC, and the cumulative variance explained by each successive PC (in %) "),
                   fluidRow(column(8,
                     plotOutput("SCREE_PLOT", height = "300px")
                   ),
                   column(4,
                    wellPanel(uiOutput("pc_range"))
                   )
                   ),
                   h3("PC plot: zoom and select points"),
                   p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                   p("Then select points on zoomed plot below to get more information about the points."),
                   p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
                   fluidRow(column(8,
                   plotOutput ("PCA_PLOT", height = 400,
                                brush = brushOpts(
                                  id = "PCA_PLOTBrush",
                                  resetOnNew = TRUE))
                   ),column(4,
                   wellPanel(
                     uiOutput("the_grouping_variable"),
                     uiOutput("the_pcs_to_plot_x"),
                     uiOutput("the_pcs_to_plot_y"),
                     checkboxInput(inputId = 'draw_ellipse',
                                   label = 'Draw ellipse around groups',
                                   value = TRUE),
                     checkboxInput(inputId = 'label_points',
                                   label = 'Use sample labels for data points',
                                   value = FALSE),
                     checkboxInput(inputId = 'select_display',
                                   label = 'Show list of samples to display on plot',
                                   value = FALSE),
                     conditionalPanel(condition="input.select_display==true",
                      p("Deselecting samples from the list below will remove them from the plot without recalculating the PCA.
                        To remove samples from the PCA calculation, deselect them from the 'Parameters' tab"),
                      uiOutput("choose_samples_display")
                     )
                   ))
                   ), # end row
                   h3("Zoomed biplot"),
                   p("The selected points in the plots above are zoomed in on this plot and their details are available in the table below."),
                   fluidRow(column(8,
                  plotOutput("ZOOMED_PLOT", height = 400)
                   )),
                   h3("Zoomed points table"),
                   p("Details of the points displayed in the zoomed plot above:"),
                  tableOutput("brush_info_after_zoom")
          ), # end  tab 
          
          tabPanel("Output",
                   p("Output of the PCA function"),
                   downloadLink("downloadPCAOutput", "Download PCA output"),
                   verbatimTextOutput("pca_details")
          ), # end  tab          
          
          tabPanel("License",
                   p("The code for this Shiny app is online at ", a("https://github.com/LJI-Bioinformatics/Shiny-PCA-Maker"), ". Please post any feedback, question, etc. as an ", a("issue on github", href = "https://github.com/LJI-Bioinformatics/Shiny-PCA-Maker/issues/new"), "."),
                   p("The code for this Shiny app was forked from ", a("https://github.com/benmarwick/Interactive_PCA_Explorer")), 
                   p("The text is licensed ", a("CC-BY", href = "http://creativecommons.org/licenses/by/4.0/"), " and the code ", a(href = "https://opensource.org/licenses/GPL-3.0", "GPL-3.0"), ".")
                   
                   
          ), # end  tab 
          id="mainTabPanel",
          selected="Input"
          )# end tabsetPanel
    ) 