#create interface UI
ui <- fluidPage(
  
  #Set the interface theme
  theme = shinytheme("lumen"),
  
  
  # tags$head(tags$style(HTML("
  #   .progress-striped .bar {
  #     background-color: #149bdf;
  #     background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
  #     background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
  #     background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
  #     background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
  #     background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
  #     -webkit-background-size: 40px 40px;
  #        -moz-background-size: 40px 40px;
  #          -o-background-size: 40px 40px;
  #             background-size: 40px 40px;
  #   }
  # "))),
  
  
  
  
  #set the check box style
  # tags$head(tags$style(HTML("
  #                           .multicol .shiny-options-group{
  #                           -webkit-column-count: 3; /* Chrome, Safari, Opera */
  #                           -moz-column-count: 3;    /* Firefox */
  #                           column-count: 3;
  #                           -moz-column-fill: balanced;
  #                           -column-fill: balanced;
  #                           }
  #                           .checkbox{
  #                           margin-top: 0px !important;
  #                           -webkit-margin-after: 0px !important; 
  #                           }
  #                           "))),
  
  
  #set the title
  titlePanel("Advanced Syllabus Analyzer"),
  
  sidebarLayout(
    #Side Panel: upload the syllabus file
    sidebarPanel(
      p("Upload the syllabus .txt file using the input button below"),
      fileInput('syllabus','Upload syllabus file (.txt)', multiple = FALSE),
      actionButton('go',label = 'Analyze data', icon("bolt"),style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    #Main Panel: show the results
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel('Matched_Occupation',DT::dataTableOutput("syllabus_occupation_result"), uiOutput("select_occupation")
                  ),
                  tabPanel('Jobs_from_Indeed', downloadButton("downloadData","Download"), uiOutput("displaySearch"),DT::dataTableOutput("jobs_result")
                  ),
                  tabPanel('Text_Frequency_Analysis',tableOutput("text_frequency_syllabus"), tableOutput("text_frequency_job"))
      )
    )
  )
  
)