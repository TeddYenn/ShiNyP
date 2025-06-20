# Page_8_AI_Report
##### Page 8: AI Report #####
#' @title Page_8_AI_Report_UI
#' @export
Page_8_AI_Report_UI = function() {
  tabPanel("AI Report",
           div(class = "AIReport-tab",
               fluidPage(
                 theme = bs_theme(version = 5), # For tooltip
                 uiOutput("guide_AI"),
                 tags$hr(),
                 fluidRow(
                   column(3,
                          tags$h4("1. Preliminary Results", class = "custom-h4"),
                          textInput("AI_species", "Specify the species for your SNP data:", value = "", placeholder = "Ex: Wild rice (Oryza rufipogon)"),
                          actionButton("autogenerate", "Auto-generate", class = "AI1-action-button"),
                          tags$hr(class = "dashed-hr"),
                          bslib::tooltip(
                            actionButton("Input_autogenerate", "Or click here to upload", class = "S-action-button"),
                            "Upload: Compiled preliminary results"
                          ),
                          actionButton("Input_autogenerate_Reset", "Reset", class = "AI2-action-button")
                   ),
                   column(9,
                          textOutput("AItitle1"),
                          tags$style("#AItitle1 { font-size: 22px; font-weight: bold; color: #4f5b62;}"),
                          div(class = "ai-response-container",
                              verbatimTextOutput("AI_response1")
                          ),
                          uiOutput("download_AI_autogenerate")
                   )
                 ),
                 tags$hr(),
                 tags$br(),
                 fluidRow(
                   column(3,
                          tags$h4("2. AI-Driven Report", class = "custom-h4"),
                          bslib::tooltip(
                            selectInput("AI_model", "AI model:", 
                                        choices = names(AI_model_choice), 
                                        selected = "Gemini 2.0 Flash (API Free)"),
                            "Choose an LLM-based model"
                          ),
                          bslib::tooltip(
                            selectInput("AI_prompt", "AI task:",
                                        choices = c("Summary Request", "Data Interpretation", "Report Structuring", "Idea Expansion", "Custom Template"),
                                        selected = "Data Interpretation"),
                            "Select the type of task you want the AI to perform"
                          ),
                          
                          bslib::tooltip(
                            uiOutput("AI_template"),
                            "Upload a custom template (.txt)", placement = "top"
                          ),
                          bslib::tooltip(
                            uiOutput("AI_turn_type"),
                            "Single-Turn: one-shot; Multi-Turn: five-shot with longer responses"
                          ),
                          bslib::tooltip(
                            selectInput("AI_lang", "Language:",
                                        choices = c("English", "繁體中文", "简体中文", "Español", "日本語", "Français", "Deutsch", "Português", "Русский язык", "Meow 🐱"),
                                        selected = "English"),
                            "Select the report language"
                          ),
                          bslib::tooltip(
                            uiOutput("AI_api_key"),
                            "Upload a .txt file containing your API key", placement = "top"
                          ),
                          actionButton("runAIreport", "Get Report", class = "AI1-action-button"),
                          actionButton("AIreport_Reset", "Reset", class = "AI2-action-button"),
                          div(
                            id = "AIStatus",
                            style = "color: #7A1CAC; font-weight: bold;",
                            HTML("Generating  <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")
                          )
                   ),
                   column(9,
                          textOutput("AItitle2"),
                          tags$style("#AItitle2 { font-size: 22px; font-weight: bold; color: #4f5b62;}"),
                          div(class = "ai-response-container",
                              verbatimTextOutput("AI_response2")
                          ),
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
  
  #### STEP 1 #### 
  
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
      downloadButton("DAI_autogenerate", "Download", class = "AI1-action-button")
    }
  })
  
  output$DAI_autogenerate = downloadHandler(
    filename = "Preliminary_Results.txt",
    content = function(file) {
      write.table(preliminary_results(), file, row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  )
  
  #### API #### 
  
  observeEvent(input$AI_model, {
    if (input$AI_model == "Gemini 2.0 Flash (API Free)") {
      showModal(modalDialog(
        title = "API Not Required",
        tagList(
          p("The selected model ", tags$b("does not require"), " an API key."),
          p("You can proceed without uploading one.")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      showModal(modalDialog(
        title = "API Key Required",
        tagList(
          p("The selected model (", input$AI_model, ") ", tags$b("requires "), "a valid API key."),
          p("Please upload your API key"),
          p(tags$a(href = "https://teddyenn.github.io/ShiNyP-guide/sec-ai-report.html#how-to-get-the-api-key", 
                   "How to get the API Key - ShiNyP User Guide", target = "_blank"))
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  }, ignoreInit = TRUE)
  
  #### STEP 2 ####
  
  observeEvent(input$AI_prompt, {
    if (input$AI_prompt == "Custom Template") {
      output$AI_template = renderUI({
        fileInput("AI_template_file", "Template file:", multiple = FALSE, accept = c(".txt"))
      })
      output$AI_turn_type = renderUI({
        selectInput("AI_turn", "Conversation:", choices = "Single-Turn")
      })
    } else {
      output$AI_template = renderUI({ NULL })
      output$AI_turn_type = renderUI({
        selectInput("AI_turn", "Conversation:", choices = c("Single-Turn", "Multi-Turn"), selected = "Single-Turn")
      })
    }
  })
  
  
  observeEvent(input$AI_template_file, {
    req(input$AI_template_file)
    ext = tools::file_ext(input$AI_template_file$name)
    if (tolower(ext) %in% c("txt", "md", "rmd")) {
      text = readLines(input$AI_template_file$datapath, warn = FALSE)
      AI_template_text(paste(text, collapse = "\n"))
      
      showNotification("Uploaded successfully", type = "message")
    } else {
      showNotification("Unsupported template file type.", type = "error")
    }
  })
  
  output$AI_api_key = renderUI({
    fileInput("AI_api_key", "API key file:", multiple = FALSE, accept = c(".txt"))
  })
  
  observeEvent(input$AI_api_key, {
    req(input$AI_api_key)
    showNotification("Uploaded successfully", type = "message")
  })
  
  observeEvent(input$runAIreport, {
    req(preliminary_results())
    shinyjs::show("AIStatus")
    
    tryCatch({
      
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
      } else if (input$AI_prompt == "Custom Template"){
        if (is.null(AI_template_text())) stop("Template is not loaded.")
        Start = "Please fill in the blanks “______” with the corresponding analysis results."
        Role = paste("The relevant information from the following SNP data analysis summary:", AI_template_text())
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
      
      if (input$AI_model == "Gemini 2.0 Flash (API Free)") {
        key = KEY
      } else{
        req(input$AI_api_key$datapath)
        key = readLines(input$AI_api_key$datapath, warn = FALSE)
      }
      
      model = AI_model_choice[input$AI_model]
      
      if (model %in% c("o3-mini", "o4-mini")){
        chat = chat_openai(
          system_prompt = Start,
          base_url = "https://api.openai.com/v1",
          api_key = key,
          model = model,
          echo = "text"
        )
      } else if (model %in% c("deepseek-chat")){
        chat = chat_deepseek(
          system_prompt = Start,
          base_url = "https://api.deepseek.com",
          api_key = key,
          model = model,
          api_args = list(timeout = 1200, max_tokens = 1000, stream = TRUE),
          echo = "text"
        )
      } else if (model %in% c("gemini-2.0-flash", "gemini-2.5-flash", "gemini-2.5-flash-lite-preview-06-17")){
        chat = chat_google_gemini(
          system_prompt = Start,
          base_url = "https://generativelanguage.googleapis.com/v1beta/",
          api_key = key,
          model = model,
          echo = "text"
        )
      } else{
        chat = chat_openai(
          system_prompt = Start,
          base_url = "https://api.openai.com/v1",
          api_key = key,
          model = model,
          echo = "text"
        )
      }
      
      showNotification("Requesting...", type = "message")
      
      if (input$AI_turn == "Single-Turn"){
        message = paste(Role, "\n", "Here is the analysis summary: ", preliminary_results())
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
          "Data Input, Data QC, and SNP dataset for downstream analysis",
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
      
      report = paste0("---", "\n", "\n",
                      "───── ✅  Successful Request  ─────","\n", "\n",
                      "- AI Model: ", input$AI_model, "\n",
                      "- Task: ", input$AI_prompt, "\n",
                      "- Conversation Mode: ", input$AI_turn, "\n",
                      "- Report Language: ", input$AI_lang, "\n",
                      "---", "\n", "\n",
                      content, "\n", "\n",
                      "## WARNING: ", "\n",
                      "*This report was generated with the assistance of AI model and is for informational purposes only.*", "\n", "\n",
                      "*It should not be considered as professional advice or a basis for decision-making.*", "\n", "\n",
                      "*Please review and validate the content thoroughly before use.*")
      showNotification("Request successfully", type = "message")
      AI_report(NULL)
      AI_report(report)
      AItitle2("Here's Your AI Report!")
      shinyjs::hide("AIStatus")
      NULL
    }, error = function(e) {
      shinyjs::hide("AIStatus")
      showNotification(paste("Fail: ", e$message), type = "error", duration = 10)
      NULL
    })
  })
  
  output$download_AI_report_txt = renderUI({
    if (AItitle2() == "Here's Your AI Report!") {
      downloadButton("DAI_report1", "Download .txt file", class = "AI1-action-button")
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
      downloadButton("DAI_report2", "Download .docx file", class = "AI1-action-button")
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
    output$AI_api_key = renderUI({
      fileInput("AI_api_key", "API key file:", multiple = FALSE, accept = c(".txt"))
    })
    output$AI_template = renderUI({
      fileInput("AI_template_file", "Template file:", multiple = FALSE, accept = c(".txt"))
    })
    AI_template_text(NULL)
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