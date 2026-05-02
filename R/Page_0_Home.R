# Page_0_Home

##### Home Page #####
#' @title Page_0_Home_UI
#' @export
Page_0_Home_UI = function() {
  tabPanel("Home",
           useShinyjs(),
           fluidPage(
             tags$h3(HTML("<em>ShiNyP</em>: SNP Analysis and Visualization Platform"),
                     style = "color: #34495e; font-weight: bold;"),
             p("Yen-Hsiang Huang, Chung-Feng Kao", br(),
               "National Chung-Hsing University (NCHU), Taiwan"), 
             # checkpoint
             tags$h6(HTML("<em>ShiNyP</em>  v1.2.0 under AGPL-3.0"),
                     style = "color: #34495e; font-weight: bold;"),
             br(),
               #"For any inquiries, please email us at: ",
               #tags$a(href = "mailto:teddyhuangyh@gmail.com", "teddyhuangyh@gmail.com"),
               #style = "color: #34495e; font-size: 16px;"),
             
             h4("🔶 Key Features", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p("Real-time Processing, Analysis, and Visualization of SNP Datasets:",
               style = "font-size: 16px; margin-bottom: 5px;"),
             p("▸ Input: Genome-wide biallelic SNP in Variant Call Format (VCF) file.", br(),
               "▸ Analysis: Data QC, population genetics analysis, core collection, and more.", br(),
               "▸ Output: Publication-ready figures, tables, data objects, and AI-driven report."),
             
             h4("🔶 Quickstart", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p("To get started, go to the ",
               tags$strong("'Data Input'"), 
               " page and upload your SNP dataset in VCF format to begin downstream analysis.", 
               br(), 
               "For detailed instructions, please refer to the ",
               "📖 User Guide: ", 
               tags$a(href = "https://teddyenn.github.io/ShiNyP-guide", "https://teddyenn.github.io/ShiNyP-guide", target = "_blank"),
               style = "font-size: 16px; line-height: 1.6;"),
             
             h4("🔶 AI Activation", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p("Configure your AI model and API key once to activate both ", tags$strong("AI bot"), " and ", tags$strong("AI Report"), ".", style = "font-size: 16px;"),
             bslib::tooltip(
               actionButton("activate_ai_modal_btn", "Activate Now!", class = "AI1-action-button"),
               "Upload API key and test connection"
             ),
             br(),
             textOutput("shared_ai_api_status"),
            
             h4("🔶 Publication", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p(HTML("Huang, Y.-H., Chen, L.-Y., Septiningsih, E. M., Kao, P.-H., & Kao, C.-F. (2025). <em>ShiNyP</em>: Unlocking SNP-based population genetics—AI-assisted platform for rapid and interactive visual exploration. <em>Molecular Biology and Evolution, 42</em>(6), msaf117."),
                     tags$a(href = "https://doi.org/10.1093/molbev/msaf117", 
                            "https://doi.org/10.1093/molbev/msaf117", target = "_blank"),
                     style = "font-size: 16px;"),
             h4("🔶 Support", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p(
               "If you encounter any issues or have suggestions for new features, please submit a request on the ",
               tags$a(href = "https://github.com/TeddYenn/ShiNyP/issues", 
                      "GitHub Issues", target = "_blank"),
               " page or email us at: ",
               tags$a(href = "mailto:teddyhuangyh@gmail.com", "teddyhuangyh@gmail.com"),
               style = "font-size: 16px;"
             )
           )
  )
}
#' @title Page_0_Home_Server
#' @export
Page_0_Home_Server = function(input, output, session) {
  options(warn = -1)
  options(shiny.maxRequestSize = 10^5*1024^3) # Maximum size: 10^5 GB
  shinyjs::hide("inputStatus")
  shinyjs::hide("samplemissingStatus")
  shinyjs::hide("samplehStatus")
  shinyjs::hide("missingStatus")
  shinyjs::hide("mafStatus")
  shinyjs::hide("hStatus")
  shinyjs::hide("hweStatus")
  shinyjs::hide("SNPdensityStatus")
  shinyjs::hide("glStatus")
  shinyjs::hide("gl2Status")
  shinyjs::hide("input2Status")
  shinyjs::hide("PCAStatus")
  shinyjs::hide("DAPCStatus")
  shinyjs::hide("UPGMAStatus")
  shinyjs::hide("NJStatus")
  shinyjs::hide("KinshipStatus")
  shinyjs::hide("ScatterStatus")
  shinyjs::hide("TreeStatus")
  shinyjs::hide("GDStatus")
  shinyjs::hide("CircosStatus")
  shinyjs::hide("GTStatus")
  shinyjs::hide("AMOVAStatus")
  shinyjs::hide("pcadaptStatus")
  shinyjs::hide("OutFLANKStatus")
  shinyjs::hide("IBSStatus")
  shinyjs::hide("ManhattanStatus")
  shinyjs::hide("CoreSampleStatus")
  shinyjs::hide("CoreSNPStatus")
  shinyjs::hide("AIStatus")

  observeEvent(input$activate_ai_modal_btn, {
    showModal(modalDialog(
      title = "Activate AI Features",
      fileInput("modal_ai_api_key", "1. Upload API key file (.txt):", multiple = FALSE, accept = c(".txt")),
      selectInput("modal_ai_provider", "2. Choose AI Provider:", 
                  choices = c("Google Gemini", "OpenAI", "DeepSeek"),
                  selected = "Google Gemini"),
      uiOutput("modal_ai_model_ui"),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$span("Hints: Your API key is safe and not saved permanently.", style = "font-size: 13px; color: #6c757d;"),
        tags$br(),
        tags$a(href = "https://teddyenn.github.io/ShiNyP-guide/sec-ai-report.html#how-to-get-the-api-key", 
               "How to get the API Key - ShiNyP User Guide", target = "_blank", style = "font-size: 13px;")
      ),
      actionButton("test_ai_connection", "Test Connection & Save", class = "run-action-button"),
      div(id = "connection_status", style = "margin-top: 15px; font-weight: bold;"),
      footer = tagList(
        modalButton("Close")
      )
    ))
  })

  output$modal_ai_model_ui = renderUI({
    req(input$modal_ai_provider)
    if (input$modal_ai_provider == "Google Gemini") {
      choices = c("Gemini 3 Flash" = "Gemini 3 Flash",
                  "Gemini 2.5 Flash" = "Gemini 2.5 Flash",
                  "Gemini 2.5 Flash-Lite" = "Gemini 2.5 Flash-Lite")
    } else if (input$modal_ai_provider == "OpenAI") {
      choices = c("GPT-5.5" = "GPT-5.5",
                  "GPT-5" = "GPT-5",
                  "GPT-5 mini" = "GPT-5 mini",
                  "GPT-4.1" = "GPT-4.1")
    } else if (input$modal_ai_provider == "DeepSeek") {
      choices = c("DeepSeek-V3" = "DeepSeek-V3")
    }
    
    selectInput("modal_ai_model", "3. Select AI Model:", choices = choices, selected = ai_user_model())
  })

  observeEvent(input$test_ai_connection, {
    shinyjs::html("connection_status", "Testing connection <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")
    shinyjs::runjs('document.getElementById("connection_status").style.color = "#7A1CAC";')
    
    model_name = input$modal_ai_model
    model_code = AI_model_choice[model_name]
    
    key = NULL
    if (!is.null(input$modal_ai_api_key)) {
      key = paste(readLines(input$modal_ai_api_key$datapath, warn = FALSE), collapse = "")
      key = trimws(key)
    } else if (!is.null(ai_user_api_key())) {
      key = ai_user_api_key()
    }
    
    if (is.null(key)) {
      shinyjs::html("connection_status", "Error: Please upload an API key.")
      shinyjs::runjs('document.getElementById("connection_status").style.color = "red";')
      return()
    }
    
    tryCatch({
      chat = NULL
      if (model_code %in% c("gpt-5.5", "gpt-5", "gpt-5-mini", "gpt-4.1")){
        chat = chat_openai(
          system_prompt = "You are a helpful assistant.",
          base_url = "https://api.openai.com/v1",
          api_key = key,
          model = model_code,
          echo = "none"
        )
      } else if (model_code %in% c("deepseek-chat")){
        chat = chat_deepseek(
          system_prompt = "You are a helpful assistant.",
          base_url = "https://api.deepseek.com",
          api_key = key,
          model = model_code,
          echo = "none"
        )
      } else if (model_code %in% c("gemini-3-flash-preview", "gemini-2.5-flash", "gemini-2.5-flash-lite")){
        chat = chat_google_gemini(
          system_prompt = "You are a helpful assistant.",
          base_url = "https://generativelanguage.googleapis.com/v1beta/",
          api_key = key,
          model = model_code,
          echo = "none"
        )
      }
      
      if (!is.null(chat)) {
        res = chat$chat("Hello! Please reply with exactly one word: 'OK'.")
        if (grepl("OK|ok|Ok", res, ignore.case = TRUE)) {
          ai_user_model(model_name)
          if (!is.null(key)) ai_user_api_key(key)
          
          shinyjs::html("connection_status", "Connection successful! Settings saved.")
          shinyjs::runjs('document.getElementById("connection_status").style.color = "green";')
          showNotification(paste("Activated", model_name), type = "message")
        } else {
          shinyjs::html("connection_status", "Connection failed: Unexpected response.")
          shinyjs::runjs('document.getElementById("connection_status").style.color = "red";')
        }
      } else {
        shinyjs::html("connection_status", "Connection failed: Chat instance could not be created.")
        shinyjs::runjs('document.getElementById("connection_status").style.color = "red";')
      }
    }, error = function(e) {
      shinyjs::html("connection_status", paste("Connection failed:", e$message))
      shinyjs::runjs('document.getElementById("connection_status").style.color = "red";')
    })
  })
  
  output$shared_ai_api_status = renderText({
    if (is.null(ai_user_api_key())) {
      paste("Status: Not activated. Selected Model:", ai_user_model())
    } else {
      paste("Status: Activated. Selected Model:", ai_user_model())
    }
  })

  output$progressUI = renderUI({
    progress = progressVal()
    if (progress > 0 && progress < 1) {
      progressPercent = progress * 100
      tags$div(class = "progress",
               tags$div(class = "progress-bar progress-bar-striped progress-bar-animated",
                        role = "progressbar",
                        style = sprintf("width: %s%%;", progressPercent),
                        sprintf("%.0f%%", progressPercent)))
    }
  })
}

