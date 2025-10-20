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
             tags$h6(HTML("<em>ShiNyP</em>  v1.1.1 under AGPL-3.0"),
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