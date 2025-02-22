#' Launch the ShiNyP Shiny app
#'
#' @export
run_ShiNyP = function() {
  ShiNyP_ui = tagList(
    CSS_UI(),
    navbarPage(
      title = HTML("<strong><em>ShiNyP</em></strong>"),
      theme = bslib::bs_theme(bootswatch = "zephyr", bg = "#f3f1e5", fg = "#0C1844"),
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
