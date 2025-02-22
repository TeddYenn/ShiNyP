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
             p("Yen-Hsiang Huang", br(),
               "National Chung-Hsing University (NCHU), Taiwan", br(),
               "For any inquiries, please email us at: ",
               tags$a(href = "mailto:teddyhuangyh@gmail.com", "teddyhuangyh@gmail.com"),
               style = "color: #34495e; font-size: 16px;"),
             
             h4("🔶 Key Features", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p("Real-time Processing, Analysis, and Visualization of SNP Datasets:",
               style = "font-size: 16px; margin-bottom: 5px;"),
             p("▶️ Input: Genome-wide biallelic SNP in Variant Call Format (VCF) file format.", br(),
               "▶️ Analysis: Data QC, population genetics analysis, core collection, and more.", br(),
               "▶️ Output: Publication-ready figures, tables, data objects, and AI-driven report."),
             h4("🔶 Quickstart", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p("To begin, navigate to the 'Data Input' page, where you can upload SNP dataset in VCF and start the downstream analysis.", br(),
               "Please visit the 📖 ", tags$a(href = "https://teddyenn.github.io/ShiNyP-guide", "User Guide"),
               " for detailed instructions on using each feature.",
               tags$a(href = "https://teddyenn.github.io/ShiNyP-guide", "https://teddyenn.github.io/ShiNyP-guide"),
               style = "font-size: 16px; margin-bottom: 0px;"),
             # actionButton("guide_button", "Go to User Guide", icon = icon("book"), class = "web-button"),
             
             #h4("Case Studies", style = "color: #34495e; margin-top: 5px;"),
             #p("Explore how ShiNyP has been applied in whole genome sequencing (WGS) SNP datasets: wild rice (",
             #  tags$i("Oryza rufipogon"), ") & chicken (",
             #  tags$i("Gallus gallus"), "). Check it out!",
             #  style = "font-size: 16px; margin-bottom: 0px;"),
             #actionButton("case_button", " View Case Studies", icon = icon("chart-bar"), class = "web-button"),
             
             h4("🔶 Publication", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p("Huang et al. (upcoming 2025) ShiNyP: An Interactive Shiny-Based Platform for Genome-Wide SNP Analysis and Visualization",
               style = "font-size: 16px;"),
             # , tags$a(href = "https://www.example.com", "https://www.example.com", target = "_blank")
             h4("🔶 Support", style = "color: #34495e; margin-top: 5px; font-weight: bold;"),
             p("If you encounter any issues or have suggestions for new features, please submit a report through our Feedback Form:",
               style = "font-size: 16px;", tags$a(href = "https://forms.gle/GPCggSo5czyNLfoB7", "https://forms.gle/GPCggSo5czyNLfoB7  (Google Form)", target = "_blank"), 
               "or email us at: teddyhuangyh@gmail.com")
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