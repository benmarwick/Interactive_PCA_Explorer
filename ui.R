ui <- bootstrapPage(
  mainPanel(
    titlePanel("Interactive PCA Explorer"),
        
        tabsetPanel(
          
          tabPanel("Data input", 
                   p("Before uploading your data, check that it is clean, especially ensure that the the numeric variables contain only the digits 0-9 or NA (to indicate missing data). Rows that contain one or more NAs will be excluded from the PCA. Columns that contain a mixture of numbers and text will not be included in the PCA. Have a look at the iris.csv file included with this app to see what a clean CSV file looks like."),
                   
                   fileInput('file1', 'Choose a CSV file to upload:',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),
                   

                   radioButtons(inputId = 'header',  
                                label = 'Header',
                                choices = c('Columns have headers'='Yes',
                                            'Columns do not have headers'='No'), 
                                selected = 'Yes'),
          
                   radioButtons('sep', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                ','),
          
                   radioButtons('quote', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                '"')
                 ), # end file  tab
          
          tabPanel("Inspect the data",
                   
                   p("The tableplot below may be useful to explore the relationships between the variables, to discover strange data patterns, and to check the occurrence and selectivity of missing values."),
                   plotOutput("tableplot"),
                   tags$hr(),
                   p("Here is a summary of the data"),
                   tableOutput('summary'),
                   tags$hr(),
                   p("Here is the raw data from the CSV file"),
                   DT::dataTableOutput('contents')
          ), # end  tab
          
          
          tabPanel("Correlation Plots",
                   uiOutput("choose_columns_biplot"),
                   tags$hr(),
                   p("This plot may take a few moments to appear when analysing large datasets. You may want to exclude highly correlated variables from the PCA."),
                   
                   plotOutput("corr_plot"),
                   tags$hr(),
                   p("Summary of correlations"),
                   tableOutput("corr_tables")
          ), # end  tab
          
          tabPanel("Diagnostics",
                   
                   p("Among SPSS users, these tests are considered to provide some guidelines on the suitability of the data for a principal components analysis. However, they may be safely ignored in favour of common sense."),
                   tags$hr(),
                   p("Here is the output of Bartlett’s sphericity test. Bartlett's test of sphericity tests whether the data comes from multivariate normal distribution with zero covariances. If p > 0.05 then PCA may not be very informative"),
                   verbatimTextOutput("bartlett"),
                   tags$hr(),
                   p("Here is the output of the Kaiser-Meyer-Olkin (KMO) index test. The overall measure varies between 0 and 1, and values closer to 1 are better. A value of 0.6 is a suggested minimum. "),
                   verbatimTextOutput("kmo")
                   
                   
                   
          ), # end  tab
          
          tabPanel("Compute PCA",
                   
                   p("Choose the columns of your data to include in the PCA. Only columns containing numeric data are shown here because PCA doesn't work with non-numeric data. The PCA is automatically re-computed each time you change your selection. Observations (ie. rows) are removed if they contain any missing values."),
                   uiOutput("choose_columns_pca"),
                   tags$hr(),
                   p("Select options for the PCA computation (we are using the prcomp function here)"),
                   radioButtons(inputId = 'center',  
                                label = 'Center',
                                choices = c('Shift variables to be zero centered'='Yes',
                                            'Do not shift variables'='No'), 
                                selected = 'Yes'),
                   
                   radioButtons('scale.', 'Scale',
                                choices = c('Scale variables to have unit variance'='Yes',
                                            'Do not scale variables'='No'), 
                                selected = 'Yes')
                   
          ), # end  tab
          

      
          tabPanel("PC Plots",
                   p("The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %) "),
                   plotOutput("plot2"),
                   tags$hr(),
                   p("Select the grouping variable (only columns of character and integer type are shown here)"),
                   uiOutput("the_grouping_variable"),
                   tags$hr(),
                   p("Select the PCs to plot"),
                   uiOutput("the_pcs_to_plot_x"),
                   uiOutput("the_pcs_to_plot_y"),
                   tags$hr(),
                   p("Click and drag on the plot to select points, and inspect the table of selected points below"),
                   plotOutput("plot1",  brush = "plot_brush", click = "plot_click", height = "400px"),
                   tags$hr(),
                   p("Details of the brushed points"),
                   verbatimTextOutput("brush_info"),
                   p("Details of the clicked points"),
                   verbatimTextOutput("click_info")
          ), # end  tab 
          
          tabPanel("PCA output",
                   verbatimTextOutput("pca_details")
                   
          ) # end  tab 
          
          
          ))) 