#' Launch the ShiNyP Shiny app
#'
#' @export
# This file is part of ShiNyP
#
# ShiNyP is free R/Shiny package/web-app: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation.

run_ShiNyP = function() {
  ShiNyP_ui = tagList(
    tags$head(
      tags$title("ShiNyP: Unlocking SNP-Based Population Genetics")
    ),
    CSS_UI(),
    CSS_UI_Bot(),
    HTML1_UI_Bot(),
    HTML2_UI_Bot(),
    HTML3_UI_Bot(),
    HTML4_UI_Bot(),
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/marked/marked.min.js")
    ),
    actionButton(
      inputId = "ai-fab",
      label = NULL,
      icon = icon("comment-dots"),
      style = "position: fixed; right:30px; bottom:30px; z-index:1050; background:#34495e;color:white;width:50px;height:50px;border-radius:30%;border:none;font-size:28px;box-shadow:0 4px 10px rgba(0,0,0,0.2);display:flex;align-items:center;justify-content:center;"
    ),
    tags$div(
      id = "ai-chat-panel",
      tags$div(class = "ai-resizer top"),
      tags$div(class = "ai-resizer right"),
      tags$div(class = "ai-resizer bottom"),
      tags$div(class = "ai-resizer left"),
      tags$div(
        id = "ai-chat-header", 
        style = "display: flex; justify-content: space-between; align-items: center; background: #34495e; color: white; height: 48px; padding: 0 16px; border-top-left-radius: 20px; border-top-right-radius: 20px;",
        tags$em("🐣 𝖲𝗁𝗂𝖭𝗒𝖯 𝖠𝖨"),
        tags$div(
          id = "ai-header-actions",
          style = "display: flex; align-items: center; gap: 10px;",
          tags$button(id = "ai-chat-help", icon("circle-question"), 
            `data-toggle` = "tooltip",
            title = "The AI assistant will respond to your inquiry. For optimal results, we recommend keeping your questions brief, specific, and focused on relevant keywords. This chat is NOT monitored.",
            style = "border:none; background:transparent; color:white; width:28px; height:28px; border-radius:50%; display:flex; align-items:center; justify-content:center; font-size:18px;"),
          tags$button(id = "ai-chat-close", icon("circle-xmark"), style = "border:none; background:transparent; color:white; width:28px; height:28px; border-radius:50%; display:flex; align-items:center; justify-content:center; font-size:18px;")
        )
      ),
      tags$div(
        id = "ai-chat-messages", 
        tags$span(
          style = "color: #888;", 
          "Try asking: Draft an academic-style figure legend for DAPC scatter plot."
        )
      ),
      tags$div(
        id = "ai_chat_input-row",
        tags$textarea(id = "ai_chat_input", placeholder = "Type your message...", style = "width:100%; border-radius:8px; min-height:40px; resize:vertical;"),
        actionButton("ai_chat_send", "Send", style = "margin-left:8px;")
      )
    ),
    navbarPage(
      title = HTML('<img src="https://raw.githubusercontent.com/TeddYenn/ShiNyP/main/inst/app/www/ShiNyP-icon_20250510.ico" width="65">'),
      theme = bslib::bs_theme(bootswatch = "zephyr", bg = "#f3f1e5", fg = "#34495e"),
      Page_0_Home_UI(),
      Page_1_Data_Input_UI(),
      Page_2_Data_QC_UI(),
      Page_3_Data_Transform_UI(),
      Page_4_Population_Structure_UI(),
      Page_5_Genetic_Diversity_UI(),
      Page_6_Selection_Sweep_UI(),
      Page_7_Core_Collection_UI(),
      Page_8_AI_Report_UI()
    )
  )
  ShiNyP_server = function(input, output, session) {
    AI_Bot_Server(input, output, session)
    Page_0_Home_Server(input, output, session)
    Page_1_Data_Input_Server(input, output, session)
    Page_2_Data_QC_Server(input, output, session)
    Page_3_Data_Transform_Server(input, output, session)
    Page_4_Population_Structure_Server(input, output, session)
    Page_5_Genetic_Diversity_Server(input, output, session)
    Page_6_Selection_Sweep_Server(input, output, session)
    Page_7_Core_Collection_Server(input, output, session)
    Page_8_AI_Report_Server(input, output, session)
  }
  shiny::shinyApp(ui = ShiNyP_ui, server = ShiNyP_server)
}
