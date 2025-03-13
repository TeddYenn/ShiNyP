# Page_8_AI_Report
##### Page 8: AI Report #####
#' @title Page_8_AI_Report_UI
#' @export
Page_8_AI_Report_UI = function() {
  tabPanel("AI Report",
           div(class = "AIReport-tab",
               fluidPage(
                 uiOutput("guide_AI"),
                 tags$hr(),
                 fluidRow(
                   column(4,
                          tags$h4("1. Preliminary Results", class = "custom-h4"),
                          textInput("AI_species", "Please specify the species for your SNP dataset:", value = "", placeholder = "Ex: Wild rice (Oryza rufipogon)"),
                          actionButton("autogenerate", "Auto-generate", class = "AI1-action-button"),
                          tags$hr(class = "dashed-hr"),
                          actionButton("Input_autogenerate", "Or click here to upload...", class = "S-action-button"),
                          actionButton("Input_autogenerate_Reset", "Reset", class = "AI2-action-button")
                   ),
                   column(8,
                          textOutput("AItitle1"),
                          tags$style("#AItitle1 { font-size: 22px; font-weight: bold; color: #00a595;}"),
                          verbatimTextOutput("AI_response1"),
                          uiOutput("download_AI_autogenerate")
                   )
                 ),
                 tags$hr(),
                 tags$br(),
                 fluidRow(
                   column(4,
                          tags$h4("2. AI-Driven Report", class = "custom-h4"),
                          selectInput("AI_model", "Choose AI model:",
                                      choices = names(AI_model_choice), selected = "o3-mini"),
                          selectInput("AI_prompt", "Specify AI task:",
                                      choices = c("Summary Request", "Data Interpretation", "Report Structuring", "Idea Expansion"), selected = "Data Interpretation"),
                          fileInput("AI_api_key", "API key file:", multiple = F, accept = c(".txt")),
                          actionButton("runAIreport", "Get Report", class = "AI1-action-button"),
                          actionButton("AIreport_Reset", "Reset", class = "AI2-action-button"),
                          div(id = "AIStatus", style = "color: #7A1CAC; font-weight: bold;", "Generating...")
                   ),
                   column(8,
                          textOutput("AItitle2"),
                          tags$style("#AItitle2 { font-size: 22px; font-weight: bold; color: #00a595;}"),
                          verbatimTextOutput("AI_response2"),
                          uiOutput("download_AI_report")
                   )
                 )
               )
           )
  )
}
#' @title Page_8_AI_Report_Server
#' @export
Page_8_AI_Report_Server = function(input, output, session) {
  ##### Page 8: AI Report #####
  ##### STEP 1
  observeEvent(input$Input_autogenerate, {
    showModal(modalDialog(
      title = "Upload preliminary results",
      fileInput("Upload_preliminary_results", "Choose a file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      footer = tagList(
        actionButton("ok", "Close")
      )
    ))
  })
  
  observeEvent(input$ok, {
    removeModal()
  })
  
  observeEvent(input$Upload_preliminary_results, {
    req(input$Upload_preliminary_results)
    data = readLines(input$Upload_preliminary_results$datapath, warn = FALSE)
    data = paste(data, collapse = "\n")
    preliminary_results(data)
    AItitle1("Your Preliminary Results (as the prompts of AI-driven report)")
  })
  
  observeEvent(input$Input_autogenerate_Reset, {
    preliminary_results(NULL)
    AItitle1("")
    showNotification("Data have been reset.")
  })
  
  observeEvent(input$autogenerate, {
    req(pre_results())
    preliminary_results = pre_results()
    preliminary_results[[3]] = paste0("Input raw data: ", input$AI_species, " SNP dataset")
    preliminary_results[[16]] = paste("##", input$AI_species, "SNP dataset for downstream analysis")
    preliminary_results = grep("NULL", preliminary_results, invert = TRUE, value = TRUE)
    preliminary_results = paste(preliminary_results, collapse = "\n")
    preliminary_results(preliminary_results)
    AItitle1("Your Preliminary Results (as the prompts of AI-driven report)")
  })
  
  output$AI_response1 = renderText({
    req(preliminary_results())
    if (AItitle1() == "Your Preliminary Results (as the prompts of AI-driven report)"){
      preliminary_results()
    }
  })
  
  output$download_AI_autogenerate = renderUI({
    if (AItitle1() == "Your Preliminary Results (as the prompts of AI-driven report)") {
      downloadButton("DAI_autogenerate", "Download",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
  })
  
  output$DAI_autogenerate = downloadHandler(
    filename = "Preliminary_Results.txt",
    content = function(file) {
      write.table(preliminary_results(), file, row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  )
  
  ##### STEP 2
  observeEvent(input$runAIreport, {
    req(preliminary_results(), input$AI_api_key$datapath)
    shinyjs::show("AIStatus")
    
    if (input$AI_prompt == "Summary Request"){
      Role = "You can act as my research assistant, helping me interpret data, summarize findings, and generate new research ideas based on my SNP analysis."
      Start = Summary_Request_Prompt
    } else if (input$AI_prompt == "Data Interpretation"){
      Role = "You are a professional researcher, proficient in interpreting biological informatics and statistical results to deliver biologically meaningful insights."
      Start = Data_Interpretation_Prompt
    } else if (input$AI_prompt == "Report Structuring"){
      Role = "You can assist in structuring and refining your report, ensuring it is clear, concise, and well-organized."
      Start = Report_Structuring_Prompt
    } else if (input$AI_prompt == "Idea Expansion"){
      Role = "You can engage in brainstorming sessions with me, generating ideas and exploring implications of my findings."
      Start = Idea_Expansion_Prompt
    }
    
    result = tryCatch({
      key = readLines(input$AI_api_key$datapath, warn = FALSE)
      # Sys.setenv(OPENAI_API_KEY = key)
      model = AI_model_choice[input$AI_model]
      
      if (model %in% c("o1-mini", "o3-mini")){
        chat = chat_openai(
          system_prompt = NULL,
          turns = NULL,
          base_url = "https://api.openai.com/v1",
          api_key = key,
          model = model,
          seed = NULL,
          echo = "text"
        )
      } else if (model %in% c("deepseek-chat")){
        chat = chat_deepseek(
          system_prompt = Start,
          turns = NULL,
          base_url = "https://api.deepseek.com",
          api_key = key,
          model = model,
          seed = NULL,
          api_args = list(timeout = 1200, max_tokens = 1000, stream = TRUE),
          echo = "text"
        )
      } else{
        chat = chat_openai(
          system_prompt = Start,
          turns = NULL,
          base_url = "https://api.openai.com/v1",
          api_key = key,
          model = model,
          seed = NULL,
          echo = "text"
        )
      }
      message = paste(Role, "\n", preliminary_results())
      content = chat$chat(message)
      
      report = paste0("---------- ShiNyP ----------", "\n", "\n",
                         "----- Successful Request -----", "\n",
                         "AI Model: ", input$AI_model, "\n",
                         "AI Task: ", input$AI_prompt, "\n",
                         "Total Tokens Used: ", sum(token_usage()[,2:3]), "\n",
                         "Prompt Tokens: ", token_usage()[,2], "\n",
                         "Completion Tokens: ", token_usage()[,3], "\n", "\n",
                         "----- AI-driven report -----", "\n",
                         content, "\n", "\n",
                         "## WARNING: ", "\n",
                         "### This report was generated with the assistance of AI model and is for informational purposes only.", "\n",
                         "### It should not be considered as professional advice or a basis for decision-making.", "\n",
                         "### Please review and validate the content thoroughly before use.")
      AI_report(NULL)
      AI_report(report)
      AItitle2("Here's Your AI Report!")
      shinyjs::hide("AIStatus")
      NULL
    }, error = function(e) {
      shinyjs::hide("AIStatus")
      shinyjs::alert(paste("An error occurred:", e$message))
      NULL
    })
  })
  
  output$DAI_report = downloadHandler(
      filename = paste0("AI_Report-", input$AI_model, "-", input$AI_prompt,".txt"),
      content = function(file) {
        write.table(AI_report(), file, row.names = FALSE, col.names = FALSE, quote = FALSE)
        }
      )
  
  output$AI_response2 = renderText({
    req(AI_report())
    if (AItitle2() == "Here's Your AI Report!"){
      AI_report()
    }
  })
  
  observeEvent(input$AIreport_Reset, {
    AI_report(NULL)
    AItitle2("")
    showNotification("Data have been reset.")
  })
  
  output$download_AI_report = renderUI({
    if (AItitle2() == "Here's Your AI Report!") {
      downloadButton("DAI_report", "Download",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
  })
  
  output$guide_AI = renderUI({
    div(
      style = "white-space: pre-wrap;
             font-size: 16px;
             color: #f5f5f5;
             background: linear-gradient(145deg, #34495e, #2c3e50);
             padding: 15px;
             border: 1px solid #1a242f;
             border-radius: 10px;
             box-shadow: 5px 5px 15px rgba(0, 0, 0, 0.3);",
      guide_AI()
    )
  })
  
  output$AItitle1 = renderText({ AItitle1() })
  output$AItitle2 = renderText({ AItitle2() })
}