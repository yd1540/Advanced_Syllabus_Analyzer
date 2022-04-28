

#Server-------------------------------------------------------------------------

#create the server   
server<-function(input, output, session){
  
 
  
  #Text similarity : syllabus vs occupation database
  restab <- eventReactive(input$go, {
    req(input$syllabus)
    scoreResult(input$syllabus$datapath)
  })
  
  #Scrape jobs from Indeed
  job_indeed <- eventReactive(input$go_jobs,{
    #req(input$checka |)
    selected <- c(input$checka, input$checkb, input$checkc)
    jobs <- parse_job(selected)
    jobs
  })
  
 
  
  
  
  #Text frequency analysis
  text_freq <- eventReactive(input$go_jobs,{
      req(input$syllabus)
      extractKeyword(input$syllabus$datapath)
    })
  
  text_freq_jobs <- eventReactive(input$go_jobs,{
    req(input$syllabus)
    selected <- c(input$checka, input$checkb, input$checkc)
    vec <- parse_job(selected)$Description
    extractKeyword(vec)
  })
  
  #Output: text similarity comparison result
  output$syllabus_occupation_result <- DT::renderDataTable({
    restab()
  },server = TRUE)
  
  
  #Checkbox Input: user select up to 5 occupations
  output$select_occupation <- renderUI({
     dropdownButton(
        label = "Select Occupations",
        status = "default", width = 600,
        icon = icon("bars"),
        tooltip = "Click the button to select occupations",
        circle = FALSE,
        tags$h3("Select up to 5 occupations:"),
        br(),
        fluidRow(
          column(
            width = 4,
            checkboxGroupInput(
              inputId = "checka", 
              label = NULL,
              choices = restab()$Title[1:7]
            )
          ),
          column(
            width = 4,
            checkboxGroupInput(
              inputId = "checkb", 
              label = NULL,
              choices = restab()$Title[8:14]
            )
          ),
          column(
            width = 4,
            checkboxGroupInput(
              inputId = "checkc", 
              label = NULL,
              choices = restab()$Title[15:20]
            )
          )
        ),
      
 #     column(
#        width = 4,
 #       checkboxGroupInput("select_occu","Select Up to 5 Occupations", choices = restab()$Title[1:20], inline = FALSE)
  #    ),
     actionButton('go_jobs','Submit', icon("bolt"),style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  })
  
  observeEvent(input$go_jobs, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Jobs_from_Indeed")
  })
  
  # popup <- observeEvent(input$go_jobs,{
  #   session$sendCustomMessage(type = 'testmessage', message = 'Please select up to 5 occupations')
  # })
  
  # popup <- eventReactive(input$go_jobs,{
  #   req(input$syllabus)
  #   session$sendCustomMessage(type = 'testmessage', message = 'Please select up to 5 occupations')
  # })
  
  #Clear the checkgroupbox inputs if the input number > 5
  observe({
  if(length(c(input$checka, input$checkb, input$checkc)) > 5){
    showModal(modalDialog(
      tags$h3('Please Select up to 5 Occupations!')
      )
    )
    updateCheckboxGroupInput(session, "checka", selected = character(0))
    updateCheckboxGroupInput(session, "checkb", selected = character(0))
    updateCheckboxGroupInput(session, "checkc", selected = character(0))
  }
  })

  #Output: jobs from Indeed
  output$displaySearch <- renderUI({
    # tags$h4(
    #   paste("You are searching for occupations:",toString(c(input$checka, input$checkb, input$checkc)),". Please use the search box to filter the data. Sometimes there are no jobs found for certain occupations")
    # )
    tags$div(
      tags$h3(style = "color: SteelBlue",
              paste("You are searching for occupations:",toString(c(input$checka, input$checkb, input$checkc)))),
      tags$br(),
      tags$h4(style = "color: SteelBlue","Please use the search box to filter the data. Sometimes there are no jobs found for certain occupations"),
      tags$br()
    )
      # if (length(c(input$checka, input$checkb, input$checkc)) != length(unique(job_indeed()$job_occupation))){
      #   paste("Please note no results found for occupation(s): ", 
      #         setdiff(c(input$checka, input$checkb, input$checkc)),unique(job_indeed()$job_occupation))
      # }
  })
  
  output$jobs_result <- DT::renderDataTable({
    withProgress(message = "Analyzing",
                 detail ="This may take a while..." ,
                 value = 10,
                 job_indeed())
    },
    options = list(autoWidth = TRUE,
                   scrollX = TRUE,
                   columnDefs = list(list(
                     render = JS(
                       "function(data, type, row, meta) {",
                       "return type === 'display' && data.length > 50 ?",
                       "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                       "}"),
                     targets = (6))
                     ))
  ,server = TRUE)
  
  
  #Output: download button to enable user download the parse jobs
  output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(job_indeed(), con)
      }
    )
  
  #Add caption for table outputs
  syllabus_caption_str <- as.character(shiny::tags$h3(style = "color: SteelBlue", "Text frequency of Syllabus (freq >= 5)"))
  jd_caption_str <- as.character(shiny::tags$h3(style = "color: SteelBlue", "Text frequency of Job Descriptions (freq >= 5)"))
  
  
  #Output: text frequency analysis
  fluidRow(
    div(
      width = 8,
      output$text_frequency_syllabus <- renderTable(
        {withProgress(message = "Analyzing",
                      detail ="This may take a while..." ,
                      value = 10, 
                      {text_freq()})},
        caption = syllabus_caption_str,
        caption.placement = getOption("xtable.caption.placement","top"),
        caption.width = getOption("xtable.caption.width",NULL),
        server = TRUE)
    ),
    div(
      width = 8,
      output$text_frequency_job <- renderTable(
        {withProgress(message = "Analyzing",
                      detail ="This may take a while..." ,
                      value = 10, {text_freq_jobs()})},
        caption = jd_caption_str,
        caption.placement = getOption("xtable.caption.placement","top"),
        caption.width = getOption("xtable.caption.width",NULL),
        server = TRUE
      )
    ))
}  
  # output$text_frequency_syllabus <- renderTable(
  #   {withProgress(message = "Analyzing",
  #                 detail ="This may take a while..." ,
  #                 value = 10, 
  #                 {text_freq()})},
  #   caption = syllabus_caption_str,
  #   caption.placement = getOption("xtable.caption.placement","top"),
  #   caption.width = getOption("xtable.caption.width",NULL),
  #   server = TRUE)
  
  

#Run the app-------------------------------------------------------------------------
  
#shinyApp(ui = ui, server = server)

