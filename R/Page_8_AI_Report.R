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
                   column(3,
                          tags$h4("1. Preliminary Results", class = "custom-h4"),
                          textInput("AI_species", "Specify the species for your SNP data:", value = "", placeholder = "Ex: Wild rice (Oryza rufipogon)"),
                          actionButton("autogenerate", "Auto-generate", class = "AI1-action-button"),
                          tags$hr(class = "dashed-hr"),
                          actionButton("Input_autogenerate", "Or click here to upload...", class = "S-action-button"),
                          actionButton("Input_autogenerate_Reset", "Reset", class = "AI2-action-button")
                   ),
                   column(9,
                          textOutput("AItitle1"),
                          tags$style("#AItitle1 { font-size: 22px; font-weight: bold; color: #00a595;}"),
                          verbatimTextOutput("AI_response1"),
                          uiOutput("download_AI_autogenerate")
                   )
                 ),
                 tags$hr(),
                 tags$br(),
                 fluidRow(
                   column(3,
                          tags$h4("2. AI-Driven Report", class = "custom-h4"),
                          selectInput("AI_model", "AI model:",
                                      choices = names(AI_model_choice), selected = "Gemini 2.0 Flash"),
                          selectInput("AI_prompt", "AI task:",
                                      choices = c("Summary Request", "Data Interpretation", "Report Structuring", "Idea Expansion"), selected = "Data Interpretation"),
                          selectInput("AI_turn", "Conversation:",
                                      choices = c("Single-Turn", "Multi-Turn"), selected = "Single-Turn"),
                          selectInput("AI_lang", "Language:",
                                      choices = c("English", "繁體中文", "简体中文", "Español", "日本語", "Français", "Deutsch", "Português", "Русский язык", "Meow 🐱"), selected = "English"),
                          fileInput("AI_api_key", "API key file:", multiple = F, accept = c(".txt")),
                          actionButton("runAIreport", "Get Report", class = "AI1-action-button"),
                          actionButton("AIreport_Reset", "Reset", class = "AI2-action-button"),
                          div(id = "AIStatus", style = "color: #7A1CAC; font-weight: bold;", "Generating...")
                   ),
                   column(9,
                          textOutput("AItitle2"),
                          tags$style("#AItitle2 { font-size: 22px; font-weight: bold; color: #00a595;}"),
                          verbatimTextOutput("AI_response2"),
                          uiOutput("download_AI_report_word"),
                          uiOutput("download_AI_report_txt")
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
    AItitle1("Your Preliminary Results (as the prompts of AI report)")
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
    AItitle1("Your Preliminary Results (as the prompts of AI report)")
  })
  
  output$AI_response1 = renderText({
    req(preliminary_results())
    if (AItitle1() == "Your Preliminary Results (as the prompts of AI report)"){
      preliminary_results()
    }
  })
  
  output$download_AI_autogenerate = renderUI({
    if (AItitle1() == "Your Preliminary Results (as the prompts of AI report)") {
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
      Role = "You are a professional researcher assisting me in interpreting data, summarizing findings, and generating novel research ideas based on my SNP analysis."
      Start = Summary_Request_Prompt
    } else if (input$AI_prompt == "Data Interpretation"){
      Role = "You are a professional researcher with expertise in bioinformatics and statistical analysis, skilled in interpreting complex data to extract biologically meaningful insights."
      Start = Data_Interpretation_Prompt
    } else if (input$AI_prompt == "Report Structuring"){
      Role = "You can assist in structuring and refining my report, ensuring that it is clear, concise, and well-organized."
      Start = Report_Structuring_Prompt
    } else if (input$AI_prompt == "Idea Expansion"){
      Role = "You can assist with brainstorming, generating ideas, and exploring the implications of my findings."
      Start = Idea_Expansion_Prompt
    }
    
    if (input$AI_lang != "Meow 🐱"){
      Role = paste(Role, 
                   "Please prepare a Markdown-formatted report written in formal academic", input$AI_lang,
                   "Do not include a main title, but provide a clear section heading as a level-two heading (##) for each section. Use level-three headings (###) for any subheadings within the section. Do not apply numerical ordering to headings. Refrain from introducing any fictional or fabricated content.",
                   "Do not explain, respond, or greet—just directly write the report."
      )
    } else{
      Role = paste(Role, 
                   "Please prepare a Markdown-formatted report composed exclusively of the word 'Meow' and cat-related words (as many times as possible) along with numerous cat-related symbols.",
                   "Do not include a main title, but ensure that each section contains a heading (text only, without numerical ordering).",
                   "Do not explain, respond, or greet—just directly write the report."
      )
    }
    
    result = tryCatch({
      key = readLines(input$AI_api_key$datapath, warn = FALSE)
      model = AI_model_choice[input$AI_model]
      
      if (model %in% c("o1-mini", "o3-mini", "o4-mini")){
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
      } else if (model %in% c("gemini-2.0-flash", "gemini-2.0-flash-lite")){
        chat = chat_gemini(
          system_prompt = Start,
          turns = NULL,
          base_url = "https://generativelanguage.googleapis.com/v1beta/",
          api_key = key,
          model = model,
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
      
      if (input$AI_turn == "Single-Turn"){
        message = paste(Role, "\n", preliminary_results())
        content = chat$chat(message)
        
      } else {
        sections = parse_sections(preliminary_results())
        
        sections = list(
          paste(sections[["Data Input"]], sections[["Data QC"]], sections[["SNP dataset for downstream analysis"]], collapse = "\n"),
          paste(sections[["Population Structure"]], collapse = "\n"),
          paste(sections[["Genetic Diversity"]], collapse = "\n"),
          paste(sections[["Selection Sweep"]], collapse = "\n"),
          paste(sections[["Core Collection"]], collapse = "\n")
        )
        
        content = c()
        topic = list(
          "Data Input, Data QC, amd SNP dataset for downstream analysis",
          "Population Structure", "Genetic Diversity", "Selection Sweep", "Core Collection"
        )
        
        for (i in 1:5) {
          if (length(sections[[i]]) != 0){
            Role = paste("This document contains my preliminary results, organized into five sections. The current section focuses exclusively on the topic of '", topic[i], "'.",
                         "Please exclusively expand on the topic '", topic[i], "'", "with detailed, relevant content, while maintaining the formal academic writing style established in the previous sections.",
                         Role)
            
            message = paste(Role, "\n", sections[[i]])
            content[i] = c(chat$chat(message), "\n\n\n")
            
          }
        }
      }
      content = paste(content, collapse = "\n\n\n")
      
      report = paste0("───── ✅  Successful Request  ─────","\n", "\n",
                      "- AI Model: ", input$AI_model, "\n", "\n",
                      "- AI Task: ", input$AI_prompt, "\n", "\n",
                      "- AI Conversation Type: ", input$AI_turn, "\n", "\n",
                      "- AI Report Language: ", input$AI_lang, "\n", "\n",
                      content, "\n", "\n",
                      "## WARNING: ", "\n",
                      "*This report was generated with the assistance of AI model and is for informational purposes only.*", "\n", "\n",
                      "*It should not be considered as professional advice or a basis for decision-making.*", "\n", "\n",
                      "*Please review and validate the content thoroughly before use.*")
      
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
  
  output$download_AI_report_txt = renderUI({
    if (AItitle2() == "Here's Your AI Report!") {
      downloadButton("DAI_report1", "Download as a .txt file",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
  })
  
  output$DAI_report1 = downloadHandler(
    filename = paste0("AI_Report-", input$AI_model, "-", input$AI_prompt,".txt"),
    content = function(file) {
      write.table(AI_report(), file, row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  )
  
  output$download_AI_report_word = renderUI({
    if (AItitle2() == "Here's Your AI Report!") {
      downloadButton("DAI_report2", "Download as a .docx file",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
  })
  
  output$DAI_report2 = downloadHandler(
    filename = function() {
      paste0("AI_Report-", input$AI_model, "-", input$AI_prompt, "-", input$AI_turn, "-", input$AI_lang, ".docx")
    },
    content = function(file) {
      temp_rmd = tempfile(fileext = ".Rmd")
      path = system.file("AI", "template.docx", package = "ShiNyP")
      writeLines(c(
        "---",
        "title: \"AI Report from ShiNyP\"",
        "output:",
        "  word_document:",
        "    number_sections: true",
        paste0("    reference_docx: \"", path, "\""),
        "---",
        "",
        "```{r, echo=FALSE, results='asis'}",
        paste0("cat(AI_report())"),
        "```"
      ), temp_rmd)
      
      output_file = rmarkdown::render(temp_rmd, output_format = "word_document", quiet = TRUE)
      file.copy(output_file, file)
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