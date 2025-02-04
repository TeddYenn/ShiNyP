##### UI #####
ui = navbarPage(
  title = HTML("<strong><em>ShiNyP</em></strong>"),
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "zephyr", bg = "#f3f1e5", fg = "#0C1844"),
  tags$style(HTML("
      body {
        font-family: 'Sans Serif Collection', sans-serif;
      }
    ")),
  ##### Home Page #####
  tabPanel("Home",
           fluidPage(
             h3(HTML("<em>ShiNyP</em>: An Interactive Shiny-Based Platform for Genome-Wide SNP Analysis and Visualization"),
                style = "color: #34495e; font-weight: bold;"),
             p("Yen-Hsiang Huang", br(),
               "National Chung-Hsing University (NCHU), Taiwan", br(),
               "For any inquiries, please email us at: ",
               tags$a(href = "mailto:teddyhuangyh@gmail.com", "teddyhuangyh@gmail.com"),
               style = "color: #34495e; font-size: 16px; font-weight: bold;"),

             h4("Key Features", style = "color: #34495e; margin-top: 5px;"),
             p("Real-time Processing, Analysis, and Visualization of SNP Datasets:",
               style = "font-size: 16px; margin-bottom: 5px;"),
             tags$ul(
               tags$li("Comprehensive statistical and computational exploration", style = "font-size: 16px;"),
               tags$li("Customizable visualization options", style = "font-size: 16px;"),
               tags$li("Publication-ready figures and tables", style = "font-size: 16px;"),
               tags$li("Reproducible analyzed data objects", style = "font-size: 16px;"),
               tags$li("AI-driven report generation", style = "font-size: 16px;")
             ),

             h4("Quickstart", style = "color: #34495e; margin-top: 5px;"),
             p("To begin, navigate to the 'Data Input' page, where you can upload SNP dataset in VCF and start the downstream analysis.", br(),
               "Visit the User Guide for detailed instructions on using each feature: ",
               tags$a(href = "https://teddyenn.github.io/ShiNyP-guide", "https://teddyenn.github.io/ShiNyP-guide"),
               style = "font-size: 16px; margin-bottom: 0px;"),
             # actionButton("guide_button", "Go to User Guide", icon = icon("book"), class = "web-button"),

             #h4("Case Studies", style = "color: #34495e; margin-top: 5px;"),
             #p("Explore how ShiNyP has been applied in whole genome sequencing (WGS) SNP datasets: wild rice (",
             #  tags$i("Oryza rufipogon"), ") & chicken (",
             #  tags$i("Gallus gallus"), "). Check it out!",
             #  style = "font-size: 16px; margin-bottom: 0px;"),
             #actionButton("case_button", " View Case Studies", icon = icon("chart-bar"), class = "web-button"),

             h4("Publication", style = "color: #34495e; margin-top: 5px;"),
             p("Huang et al. (upcoming 2024) ShiNyP: An Interactive Shiny-Based Platform for Genome-Wide SNP Analysis and Visualization",
               style = "font-size: 16px;"),
             # , tags$a(href = "https://www.example.com", "https://www.example.com", target = "_blank")
             h4("Support", style = "color: #34495e; margin-top: 5px;"),
             p("If you encounter any issues or have suggestions for new features, please submit a report through our feedback form:",
               style = "font-size: 16px;", tags$a(href = "https://forms.gle/GPCggSo5czyNLfoB7", "https://forms.gle/GPCggSo5czyNLfoB7  (Google Form)", target = "_blank"))
           )
  ),
  ##### Page 1: Data Input #####
  tabPanel("Data Input",
           tabsetPanel(
             tags$br(),
             tabPanel("VCF",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h5("1. Input VCF File"),
                          uiOutput("Uploaddata"),
                          checkboxInput("FVCFtools", "VCF File from VCFtools", value = FALSE),
                          actionButton("Inputdata", "Input VCF File", class = "run-action-button"),
                          actionButton("resetInput", "Reset"),
                          tags$br(),
                          actionButton("demo_data", "Use Demo Data", class = "S-action-button"),
                          tags$hr(),
                          tags$h5("2. Transform to data.frame"),
                          actionButton("vcf2df", "Transform to data.frame", class = "run-action-button"),
                          actionButton("resetvcf2df", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_input"),
                          div(id = "inputStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("input1")),
                          verbatimTextOutput("fileInfo"),
                          tags$style("#fileInfo { font-size: 14px;}"),
                          uiOutput("download_input"),
                          tags$br(),
                          div(class = "title-text-style", textOutput("input2")),
                          verbatimTextOutput("fileInfo2"),
                          tags$style("#fileInfo2 { font-size: 14px;}"),
                          uiOutput("download_df"),
                          uiOutput("download_snpInfo"),
                          tags$br(),
                          div(class = "title-text-style", textOutput("input3")),
                          uiOutput("presample"),
                          DT::dataTableOutput("contents"),
                          width = 9)
                      )),
             tabPanel("data.frame/genind/genlight",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h5("Input data.frame File"),
                          uiOutput("uploaddf"),
                          actionButton("inputdf", "Input", class = "run-action-button"),
                          actionButton("resetdf", "Reset"),
                          tags$hr(),
                          tags$h5("Input genind File"),
                          uiOutput("uploadgi"),
                          actionButton("inputgi", "Input", class = "run-action-button"),
                          actionButton("resetgi", "Reset"),
                          tags$hr(),
                          tags$h5("Input genlight File"),
                          uiOutput("uploadgl"),
                          actionButton("inputgl", "Input", class = "run-action-button"),
                          actionButton("resetgl", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_input2"),
                          div(id = "input2Status", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("dfstatus")),
                          verbatimTextOutput("dfinfo"),
                          tags$br(),
                          div(class = "title-text-style", textOutput("gistatus")),
                          verbatimTextOutput("giinfo"),
                          tags$br(),
                          div(class = "title-text-style", textOutput("glstatus")),
                          verbatimTextOutput("glinfo"),
                          width = 9)
                      ))
           )),
  ##### Page 2: Data QC #####
  tabPanel("Data QC",
           tabsetPanel(
             tags$br(),
             tabPanel("Sample QC",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("fileSelection1"),
                          verbatimTextOutput("SampleQCfileInfo"),
                          tags$hr(),
                          tags$h5("1. Summary"),
                          tags$h6("Sample missing rate"),
                          actionButton("sampleQCmissing", "Summary", class = "run-action-button"),
                          tags$br(),
                          tags$h6("Sample heterozygosity rate"),
                          actionButton("sampleQCH", "Summary", class = "run-action-button"),
                          tags$hr(),
                          tags$h5("2. Sample QC"),
                          sliderInput("sampleThrMR", "Threshold of missing rate (remove > [threshold])", min = 0, max = 0.5, value = 0.05, step = 0.001),
                          sliderInput("sampleThrH", "Threshold of heterozygosity rate (remove > [threshold])", min = 0, max = 1, value = 0.10),
                          actionButton("sampleQC", "Sample QC by Thresholds", class = "run-action-button"),
                          actionButton("resetsampleQC", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_sampleQC"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("sampleQCstatus")),
                          verbatimTextOutput("sampleQCresult"),
                          tags$style("#sampleQCresult { font-size: 14px;}"),
                          uiOutput("download_sampleQC"),
                          uiOutput("download_sampleQC_Site_info"),
                          tags$hr(),
                          uiOutput("progressUI"),
                          div(class = "title-text-style", textOutput("samplemissing1")),
                          div(id = "samplemissingStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("samplemissing2"),
                          plotOutput("samplemissing3", width = "800px", height = "350px"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("sampleh1")),
                          div(id = "samplehStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("sampleh2"),
                          plotOutput("sampleh3", width = "800px", height = "350px"),
                          tags$hr(),
                          width = 9)
                      )),
             tabPanel("SNP QC",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("fileSelection2"),
                          verbatimTextOutput("SNPQCfileInfo"),
                          tags$hr(),
                          tags$h5("1. Summary"),
                          tags$h6("SNP missing rate"),
                          actionButton("QCmissing", "Summary", class = "run-action-button"),
                          tags$br(),
                          tags$h6("SNP minor allele frequency (MAF)"),
                          actionButton("QCMAF", "Summary", class = "run-action-button"),
                          tags$br(),
                          tags$h6("SNP heterozygosity rate"),
                          actionButton("QCH", "Summary", class = "run-action-button"),
                          tags$br(),
                          tags$h6("SNP p-value for Hardy-Weinberg equilibrium (HWE)"),
                          actionButton("QCHWE", "Summary", class = "run-action-button"),
                          tags$hr(),
                          tags$h5("2. SNP QC"),
                          sliderInput("ThrMR", "Threshold of missing rate (remove > [threshold])", min = 0, max = 1, value = 0.05),
                          sliderInput("ThrMAF", "Threshold of MAF (remove < [threshold])", min = 0, max = 0.5, value = 0.05),
                          sliderInput("ThrH0", "Threshold of heterozygosity rate (remove < [threshold])", min = 0, max = 1, value = 0.0),
                          sliderInput("ThrH", "Threshold of heterozygosity rate (remove > [threshold])", min = 0, max = 1, value = 0.10),
                          uiOutput("doThrHWE"),
                          checkboxInput("doHWE", "Do SNP QC by HWE", value = FALSE),
                          actionButton("QC", "SNP QC by Thresholds", class = "run-action-button"),
                          actionButton("resetSNPQC", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_QC"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("SNPQCstatus")),
                          verbatimTextOutput("QCresult"),
                          tags$style("#QCresult { font-size: 14px;}"),
                          uiOutput("download_snpQC"),
                          uiOutput("download_SNPQC_Site_info"),
                          tags$hr(),
                          uiOutput("progressUI"),
                          div(class = "title-text-style", textOutput("missing1")),
                          div(id = "missingStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("missing2"),
                          plotOutput("missing3", width = "800px", height = "350px"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("maf1")),
                          div(id = "mafStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("maf2"),
                          plotOutput("maf3", width = "800px", height = "350px"),
                          tags$hr(),
                          textOutput("h1"),
                          tags$style("#h1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          div(id = "hStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("h2"),
                          plotOutput("h3", width = "800px", height = "350px"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("HWE1")),
                          div(id = "hweStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("HWE2"),
                          plotOutput("HWE3", width = "800px", height = "380px"),
                          tags$hr(),
                          width = 9)
                      )),
             tabPanel("SNP Density",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("Site_Info0"),
                          uiOutput("Chr_Info0"),
                          sliderInput("WindowSize0", "Window size (kb)", min = 0, max = 1000, value = 500, step = 10),
                          actionButton("SNPdensity", "Summary", class = "run-action-button"),
                          actionButton("resetSNPdensity", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_SNPdensity"),
                          tags$hr(),
                          uiOutput("progressUI"),
                          div(id = "SNPdensityStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          verbatimTextOutput("SNPdensity_result1"),
                          div(class = "title-text-style", textOutput("SNPdensity1")),
                          plotOutput("SNPdensityplot", width = "950px", height = "350px"),
                          uiOutput("download_SNPdensity_plot"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("SNPdensity2")),
                          DT::dataTableOutput("SNPdensity_result2"),
                          uiOutput("download_SNPdensity_result2"),
                          width = 9)
                      ))
           )),
  ##### Page 3: Data Transform #####
  tabPanel("Data Transform",
           sidebarLayout(
             sidebarPanel(
               uiOutput("fileSelection3"),
               verbatimTextOutput("CfileInfo"),
               tags$hr(),
               tags$h5("1. Transform data.frame to  genind"),
               uiOutput("groupfile1"),
               actionButton("Cdf2gi", "Transform to genind", class = "run-action-button"),
               tags$hr(),
               tags$h5("2. Transform genind to  genlight"),
               actionButton("Cgi2gl", "Transform to genlight", class = "run-action-button"),
               width = 3),
             mainPanel(
               uiOutput("guide_C"),
               tags$hr(),
               uiOutput("progressUI"),
               div(class = "title-text-style", textOutput("Cstatus2")),
               div(id = "giStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
               verbatimTextOutput("CTable2"),
               uiOutput("download_gi"),
               tags$br(),
               div(class = "title-text-style", textOutput("Cstatus3")),
               div(id = "glStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
               verbatimTextOutput("CTable3"),
               uiOutput("download_gl"),
               width = 9)
           )),
  ##### Page 4: Population Structure #####
  tabPanel("Population Structure",
           tabsetPanel(
             tags$br(),
             tabPanel("PCA", # Principal Component Analysis
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Principal Component Analysis (PCA)"),
                          uiOutput("fileSelection_PCA"),
                          verbatimTextOutput("PCAfileInfo"),
                          tags$style("#PCAfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          actionButton("runPCA", "Run PCA", class = "run-action-button"),
                          actionButton("resetPCA", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_PCA"),
                          div(id = "PCAStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6,
                                   div(class = "title-text-style", textOutput("PCAtitle1")),
                                   plotOutput("PCAplot", width = "500px", height = "500px"),
                                   uiOutput("pc1"),
                                   uiOutput("pc2"),
                                   uiOutput("groupfile4"),
                                   uiOutput("download_PCA_plot")
                            ),
                            column(6,
                                   div(class = "title-text-style", textOutput("PCAtitle2")),
                                   plotOutput("PCAexpplot", width = "500px", height = "500px"),
                                   uiOutput("PC"),
                                   uiOutput("download_Expplot")
                            )
                          ),
                          tags$hr(),
                          uiOutput("download_var"),
                          uiOutput("download_PCA_transformed"),
                          uiOutput("download_PCA_result"),
                          width = 9)
                      )),
             tabPanel("DAPC", # Discriminant analysis of principal components
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Discriminant Analysis of Principal Components (DAPC)"),
                          tags$br(),
                          uiOutput("fileSelection_DAPC"),
                          verbatimTextOutput("DAPCfileInfo"),
                          tags$style("#DAPCfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          tags$h5("STEP I: Cluster Identification"),
                          sliderInput("npca", "The number of PC axes retained", min = 1, max = 1000, value = 10, step = 1),
                          sliderInput("Maxgrp", "Maximum number of clusters ", min = 3, max = 35, value = 15, step = 1),
                          actionButton("runDAPC1", "Run DAPC I", class = "run-action-button"),
                          actionButton("resetDAPC1", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: DAPC Analysis"),
                          sliderInput("grp", "Number of cluster (K)", min = 3, max = 35, value = 5, step = 1),
                          actionButton("runDAPC2", "Run DAPC II", class = "run-action-button"),
                          actionButton("resetDAPC2", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_DAPC"),
                          div(id = "DAPCStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(5,
                                   div(class = "title-text-style", textOutput("DAPCtitle1")),
                                   plotOutput("BICplot", width = "400px", height = "270px"),
                                   uiOutput("download_BIC_plot"),
                                   div(class = "title-text-style", textOutput("DAPCtitle2")),
                                   plotOutput("DF1plot", width = "400px", height = "270px"),
                                   uiOutput("download_DF1_plot"),
                                   div(class = "title-text-style", textOutput("DAPCtitle3")),
                                   plotOutput("DF2plot", width = "400px", height = "270px"),
                                   uiOutput("download_DF2_plot")
                            ),
                            column(7,
                                   uiOutput("download_DAPC_pop"),
                                   uiOutput("download_DAPC_transformed"),
                                   uiOutput("download_DAPC_result"),
                                   tags$hr(),
                                   div(class = "title-text-style", textOutput("DAPCtitle4")),
                                   plotOutput("DAPCplot", width = "600px", height = "600px"),
                                   uiOutput("download_DAPC_plot"),
                                   div(class = "title-text-style", textOutput("DAPCtitle5")),
                                   plotOutput("probplot", width = "600px", height = "300px"),
                                   uiOutput("download_prob_plot")
                            )
                          )
                          , width = 9)
                      )),
             tabPanel("UPGMA Tree", # Unweighted Pair Group Method with Arithmetic mean
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Unweighted Pair Group Method with Arithmetic mean (UPGMA) Tree"),
                          tags$br(),
                          uiOutput("fileSelection_UPGMA"),
                          verbatimTextOutput("UPGMAfileInfo"),
                          tags$style("#UPGMAfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          sliderInput("sample", "Number of bootstrap replicates", min = 10, max = 1000, value = 10, step = 10),
                          actionButton("runUPGMA", "Run UPGMA", class = "run-action-button"),
                          actionButton("resetUPGMA", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_UPGMA"),
                          div(id = "UPGMAStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("UPGMAtitle1")),
                          uiOutput("Layout"),
                          plotOutput("UPGMA", width = "800px", height = "800px"),
                          uiOutput("download_UPGMA_plot"),
                          uiOutput("download_UPGMA_result"),
                          width = 9)
                      )),
             tabPanel("NJ Tree", # Neighbor-Joining Tree
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Neighbor-Joining (NJ) Tree"),
                          tags$br(),
                          uiOutput("fileSelection_NJ"),
                          verbatimTextOutput("NJfileInfo"),
                          tags$style("#NJfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          actionButton("runNJ", "Run NJ", class = "run-action-button"),
                          actionButton("resetNJ", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_NJ"),
                          div(id = "NJStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("NJtitle1")),
                          uiOutput("NJLayout"),
                          plotOutput("NJ", width = "800px", height = "800px"),
                          uiOutput("download_NJ_plot"),
                          uiOutput("download_NJ_result"),
                          width = 9)
                      )),
             tabPanel("Kinship", # Kinship analysis
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Kinship Analysis"),
                          tags$br(),
                          uiOutput("fileSelection_Kinship"),
                          verbatimTextOutput("KinshipfileInfo"),
                          tags$style("#KinshipfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("groupfile2"),
                          selectInput("Kinship_method", "Method", choices = c("astle", "IBS", "vanRaden", "identity"),
                                      selected = "vanRaden"),
                          actionButton("runKinship", "Run Kinship", class = "run-action-button"),
                          actionButton("resetKinship", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_Kinship"),
                          div(id = "KinshipStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("Kinshiptitle1")),
                          plotOutput("Kinship", width = "800px", height = "800px"),
                          uiOutput("download_Kinship_plot"),
                          uiOutput("download_Kinship_result"),
                          width = 9)
                      )),
             tabPanel(HTML("Scatter Plot <sup>Plus</sup>"),
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4(HTML("Scatter Plot <sup>Plus</sup>")),
                          tags$hr(),
                          tags$h5("1. Upload PCA or DAPC Object (in RDS)"),
                          uiOutput("scatter_Upload"),
                          verbatimTextOutput("scatter_fileInfo"),
                          tags$hr(),
                          tags$h5("2. Upload Group and Other Info. (in CSV)"),
                          uiOutput("scatter_Upload2"),
                          verbatimTextOutput("scatter_fileInfo2"),
                          actionButton("runScatter", "Run Scatter Plot", class = "run-action-button"),
                          actionButton("resetScatter", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_scatter"),
                          div(id = "ScatterStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   selectInput("Scatter_xvar", "X-axis variable:", choices = "X-axis"),
                                   selectInput("Scatter_yvar", "Y-axis variable:", choices = "Y-axis"),
                                   selectInput("Scatter_zvar", "Z-axis variable:", choices = "Z-axis"),
                                   selectInput("Scatter_colvar", "Color variable:", choices = NULL)
                            ),
                            column(4,
                                   sliderInput("Scatter_size", "Point size:", min = 1, max = 20, value = 10, step = 1),
                                   sliderInput("Scatter_opacity", "Opacity:", min = 0, max = 1, value = 0.8, step = 0.1),
                                   selectInput("Scatter_color", "Colors:", choices = c("Default", "Black - single color", "Grey - single color", "Bright", "Cool Tone", "Warm Tone", "Earthy", "Vibrant", "Neon")),
                                   selectInput("Scatter_show.legend", "Legend:", choices = c("Show", "NULL"))
                            ),
                            column(4,
                                   selectInput("Scatter_axis.title", "Axis titles:", choices = c("Show", "NULL")),
                                   selectInput("Scatter_axis.line", "Axis lines:", choices = c("Show", "NULL")),
                                   selectInput("Scatter_axis.tick.labels", "Axis tick labels:", choices = c("Show", "NULL")),
                                   selectInput("Scatter_zero.line", "Zero titles:", choices = c("Show", "NULL"))
                            )
                          ),
                          tags$hr(),
                          fluidRow(
                            column(6,
                                   div(class = "title-text-style", textOutput("scatter2D")),
                                   plotlyOutput("scatter2DPlot"),
                                   uiOutput("download_scatter2DPlot_HTML")
                            ),
                            column(6,
                                   div(class = "title-text-style", textOutput("scatter3D")),
                                   plotlyOutput("scatter3DPlot"),
                                   uiOutput("download_scatter3DPlot_HTML")
                            )
                          ),
                          tags$hr(),
                          tags$br(),
                          tags$br(),
                          width = 9)
                      )),
             tabPanel(HTML("Tree Plot <sup>Plus</sup>"),
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4(HTML("Tree Plot <sup>Plus</sup>")),
                          tags$hr(),
                          tags$h5("1. Upload UPGMA or NJ Object (in RDS)"),
                          uiOutput("Tree_Upload"),
                          verbatimTextOutput("Tree_fileInfo"),
                          tags$hr(),
                          tags$h5("2. Upload Group and Other Info. (in CSV)"),
                          uiOutput("Tree_Upload2"),
                          verbatimTextOutput("Tree_fileInfo2"),
                          actionButton("runTree", "Run Tree Plot", class = "run-action-button"),
                          actionButton("resetTree", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_Tree"),
                          div(id = "TreeStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(3,
                                   selectInput("Tree_str_layout", "Layout:", choices = names(Tree_layout_choice), selected = "Circular"),
                                   selectInput("Tree_str_color_var", "Line color variable:", choices = NULL),
                                   selectInput("Tree_str_color", "Line colors:", choices = c("Default", "Black - single color", "Grey - single color", "Bright", "Cool Tone", "Warm Tone", "Earthy", "Vibrant", "Neon")),
                                   sliderInput("Tree_str_size", "Line size:", min = 0, max = 5, value = 0.5, step = 0.1)
                            ),
                            column(3,
                                   selectInput("Tree_taxa", "Taxa label:", choices = c("Show", "NULL")),
                                   selectInput("Tree_taxa_color_var", "Text color variable:", choices = NULL),
                                   selectInput("Tree_taxa_color", "Text colors:", choices = c("Default", "Black - single color", "Grey - single color", "Bright", "Cool Tone", "Warm Tone", "Earthy", "Vibrant", "Neon")),
                                   sliderInput("Tree_taxa_size", "Text size:", min = 0, max = 5, value = 3, step = 0.1),
                                   selectInput("Tree_taxa_align", "Align label:", choices = c("TRUE", "FALSE"))
                            ),
                            column(3,
                                   selectInput("Tree_sym", "Symbol:", choices = c("Show", "NULL")),
                                   selectInput("Tree_sym_color_var", "Symbol color variable:", choices = NULL),
                                   selectInput("Tree_sym_color", "Symbol colors:", choices = c("Default", "Black - single color", "Grey - single color", "Bright", "Cool Tone", "Warm Tone", "Earthy", "Vibrant", "Neon")),
                                   selectInput("Tree_sym_shape_var", "Symbol shape variable:", choices = NULL),
                                   sliderInput("Tree_sym_size", "Symbol size:", min = 0, max = 5, value = 3, step = 0.1)
                            ),
                            column(3,
                                   selectInput("Tree_treescale", "Treescale:", choices = c("Show", "NULL"), selected = "NULL"),
                                   selectInput("Tree_Bt", "Bootstrap values:", choices = c("Show", "NULL"), selected = "NULL"),
                                   selectInput("Tree_legend", "Legend:", choices = names(Legend_choice), selected = "bottom")
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("TreePlot1")),
                          plotOutput("TreePlot", width = "800px", height = "800px"),
                          uiOutput("download_TreePlot"),
                          tags$hr(),
                          tags$br(),
                          tags$br(),
                          width = 9)
                      ))
           )),
  ##### Page 5: Genetic Diversity #####
  tabPanel("Genetic Diversity",
           tabsetPanel(
             tags$br(),
             tabPanel("Diversity Parameter",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Diversity Parameter"),
                          tags$br(),
                          uiOutput("fileSelection_GD"),
                          verbatimTextOutput("GDfileInfo"),
                          tags$style("#GDfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info1"),
                          uiOutput("groupfile3"),
                          actionButton("runGD", "Run Diversity Analysis", class = "run-action-button"),
                          actionButton("resetGD", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_GD"),
                          div(id = "GDStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("GDtitle1")),
                          plotOutput("GDplot", width = "950px", height = "350px"),
                          uiOutput("Type"),
                          uiOutput("Parameter"),
                          uiOutput("download_GD_plot"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("GDtitle2")),
                          DT::dataTableOutput("GDresults"),
                          uiOutput("download_GD_site"),
                          uiOutput("download_GD"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("GDtitle3")),
                          DT::dataTableOutput("GDgroupresults"),
                          uiOutput("download_GD_group"),
                          uiOutput("download_Fst"),
                          width = 9),
                      )),
             tabPanel("Circos Plot",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Circos Plot"),
                          tags$br(),
                          verbatimTextOutput("GDInfo"),
                          tags$style("#GDInfo { font-size: 14px;}"),
                          tags$hr(),
                          tags$h5("STEP I: Sliding Window"),
                          selectInput("SelePara", "Select parameters:", choices = NULL, multiple = TRUE),
                          sliderInput("WindowSize", "Window size (kb)", min = 0, max = 1000, value = 500, step = 10),
                          sliderInput("StepSize", "Step size (kb)", min = 0, max = 500, value = 100, step = 5),
                          actionButton("runSW", "Run Sliding Window", class = "run-action-button"),
                          actionButton("resetSW", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: Circos Plot"),
                          uiOutput("Chr_Info"), # Track 1
                          selectInput("Track1", "Track 1 & 2: Chromosome Info.", choices = NULL), # Track 1
                          uiOutput("Track3"), # Track 3-
                          actionButton("addTrack", "Add Track", class = "S-action-button"),
                          actionButton("runCircos", "Run Circos Plot", class = "run-action-button"),
                          actionButton("resetCircos", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_Circos"),
                          div(id = "CircosStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          uiOutput("progressUI"),
                          div(class = "title-text-style", textOutput("Circostitle1")),
                          DT::dataTableOutput("SWresults"),
                          uiOutput("download_SW"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("Circostitle2")),
                          verbatimTextOutput("Circosplotinfo"),
                          uiOutput("downloadCircosplot"),
                          width = 9),
                      )),
             tabPanel("Genetic Distance", # Genetic Distance
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Genetic Distance"),
                          tags$br(),
                          uiOutput("fileSelection_GT"),
                          verbatimTextOutput("GTfileInfo"),
                          tags$style("#GTfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          selectInput("GT_method", "Method",
                                      choices = names(GT_method_choice), selected = "Cavalli-Sforza's chord distance"),
                          actionButton("runGT", "Run Genetic Distance", class = "run-action-button"),
                          actionButton("resetGT", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_GT"),
                          div(id = "GTStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6,
                                   div(class = "title-text-style", textOutput("GTtitle1")),
                                   plotOutput("GTplot", width = "400px", height = "400px"),
                                   uiOutput("download_GT_plot")
                            ),
                            column(6,
                                   div(class = "title-text-style", textOutput("GTtitle2")),
                                   tableOutput("GTresults"),
                                   uiOutput("download_GT_result")
                            )
                          ),width = 9)
                      )),
             tabPanel("AMOVA",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Analysis of Molecular Variance (AMOVA)"),
                          tags$br(),
                          uiOutput("fileSelection_AMOVA"),
                          verbatimTextOutput("AMOVAfileInfo"),
                          tags$style("#AMOVAfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          tags$h5("STEP I: AMOVA"),
                          actionButton("runAMOVA", "Run AMOVA", class = "run-action-button"),
                          actionButton("resetAMOVA", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: Permutation Test"),
                          sliderInput("nperm", "Number of permutations", min = 10, max = 1000, value = 99, step = 1),
                          actionButton("runTest", "Run Permutation Test", class = "run-action-button"),
                          actionButton("resetTest", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_AMOVA"),
                          div(id = "AMOVAStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6,
                                   div(class = "title-text-style", textOutput("AMOVAtitle1")),
                                   plotOutput("AMOVAvarplot", width = "450px", height = "600px"),
                                   uiOutput("download_AMOVA_plot")
                            ),
                            column(6,
                                   div(class = "title-text-style", textOutput("AMOVAtitle2")),
                                   plotOutput("AMOVAtestplot", width = "450px", height = "600px"),
                                   uiOutput("download_AMOVA_test_plot")
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("AMOVAtitle3")),
                          tableOutput("AMOVAresults"),
                          uiOutput("download_AMOVA_results"),
                          width = 9)
                      ))
           )),
  ##### Page 6: Selection Sweep #####
  tabPanel("Selection Sweep",
           tabsetPanel(
             tags$br(),
             tabPanel("pcadapt",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("PCA-based genome scan for selection (pcadapt)"),
                          tags$br(),
                          uiOutput("fileSelection_pcadapt"),
                          verbatimTextOutput("pcadaptfileInfo"),
                          tags$style("#pcadaptfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info2"),
                          sliderInput("pcadapt_PC", "The number of PC axes retained", min = 1, max = 35, value = 5, step = 1),
                          actionButton("SNPthin", "SNP Thinning", class = "S-action-button"),
                          uiOutput("SNPthin_size"),
                          uiOutput("SNPthin_thr"),
                          actionButton("runpcadapt", "Run pcadapt", class = "run-action-button"),
                          actionButton("resetpcadapt", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_pcadapt"),
                          div(id = "pcadaptStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   uiOutput("pcadapt_adj"),
                                   uiOutput("pcadapt_alpha"),
                                   uiOutput("download_pcadapt")
                            ),
                            column(8,
                                   verbatimTextOutput("pcadapt_result")
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("pcadapttitle1")),
                          plotOutput("pcadaptplot1", width = "950px", height = "350px"),
                          uiOutput("download_pcadapt_plot1"),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   div(class = "title-text-style", textOutput("pcadapttitle2")),
                                   plotOutput("pcadaptplot2", width = "350px", height = "350px"),
                                   uiOutput("download_pcadapt_plot2")
                            ),
                            column(4,
                                   div(class = "title-text-style", textOutput("pcadapttitle3")),
                                   plotOutput("pcadaptplot3", width = "350px", height = "350px"),
                                   uiOutput("download_pcadapt_plot3")
                            ),
                            column(4,
                                   div(class = "title-text-style", textOutput("pcadapttitle4")),
                                   plotOutput("pcadaptplot4", width = "350px", height = "350px"),
                                   uiOutput("download_pcadapt_plot4")
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("pcadapttitle5")),
                          DT::dataTableOutput("pcadapt_Sign_SNP"),
                          uiOutput("download_pcadapt_results"),
                          width = 9)
                      )),
             tabPanel("OutFLANK",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Fst-based genome scan for selection (OutFLANK)"),
                          tags$br(),
                          uiOutput("fileSelection_OutFLANK"),
                          verbatimTextOutput("OutFLANKfileInfo"),
                          tags$style("#OutFLANKfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info3"),
                          actionButton("runOutFLANK", "Run OutFLANK", class = "run-action-button"),
                          actionButton("resetOutFLANK", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_OutFLANK"),
                          div(id = "OutFLANKStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   uiOutput("OutFLANK_adj"),
                                   uiOutput("OutFLANK_alpha"),
                                   uiOutput("download_OutFLANK")
                            ),
                            column(8,
                                   verbatimTextOutput("OutFLANK_result")
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("OutFLANKtitle1")),
                          plotOutput("OutFLANKplot1", width = "950px", height = "350px"), # p-value
                          uiOutput("download_OutFLANK_plot1"),
                          tags$br(),
                          plotOutput("OutFLANKplot2", width = "950px", height = "350px"), # Fst
                          uiOutput("download_OutFLANK_plot2"),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   div(class = "title-text-style", textOutput("OutFLANKtitle2")),
                                   plotOutput("OutFLANKplot3", width = "350px", height = "350px"),
                                   uiOutput("download_OutFLANK_plot3")
                            ),
                            column(4,
                                   div(class = "title-text-style", textOutput("OutFLANKtitle3")),
                                   plotOutput("OutFLANKplot4", width = "350px", height = "350px"),
                                   uiOutput("download_OutFLANK_plot4")
                            ),
                            column(4,
                                   div(class = "title-text-style", textOutput("OutFLANKtitle4")),
                                   plotOutput("OutFLANKplot5", width = "350px", height = "350px"),
                                   uiOutput("download_OutFLANK_plot5")
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("OutFLANKtitle5")),
                          DT::dataTableOutput("OutFLANK_Sign_SNP"),
                          uiOutput("download_OutFLANK_Sign_SNP"),
                          width = 9)
                      )),
             tabPanel("IBS",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Identity By State (IBS)"),
                          tags$br(),
                          uiOutput("fileSelection_IBS"),
                          verbatimTextOutput("IBSfileInfo"),
                          tags$style("#IBSfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info4"),
                          uiOutput("Chr_Info2"),
                          selectInput("REF", "Reference", choices = NULL),
                          selectInput("COMPAR", "Comparison", choices = NULL),
                          sliderInput("WindowSize2", "Window size (kb)", min = 0, max = 1000, value = 500, step = 10),
                          sliderInput("StepSize2", "Step size (kb)", min = 0, max = 500, value = 100, step = 5),
                          checkboxInput("rmH", "Remove heterozygous SNPs", value = TRUE),
                          actionButton("runIBS", "Run IBS", class = "run-action-button"),
                          actionButton("resetIBS", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_IBS"),
                          div(id = "IBSStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          verbatimTextOutput("IBSres"),
                          div(class = "title-text-style", textOutput("IBStitle1")),
                          plotOutput("IBSplot", width = "950px", height = "350px"),
                          uiOutput("download_IBS_plot"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("IBStitle2")),
                          DT::dataTableOutput("IBS_SW"),
                          uiOutput("download_IBS_SW"),
                          width = 9)
                      )),
             tabPanel(HTML("Manhattan Plot <sup>Plus</sup>"),
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4(HTML("Manhattan Plot <sup>Plus</sup>")),
                          tags$hr(),
                          tags$h5("1. Upload genetic_diversity/pcadapt_pvalue/OutFLANK_pvalue per site (in RDS)"),
                          uiOutput("Manhattan_Upload"),
                          verbatimTextOutput("Manhattan_fileInfo"),
                          tags$hr(),
                          tags$h5("2. Upload Chromosome Info. (in CSV)"),
                          uiOutput("Manhattan_Upload2"),
                          verbatimTextOutput("Manhattan_fileInfo2"),
                          actionButton("runManhattan", "Run Manhattan Plot", class = "run-action-button"),
                          actionButton("resetManhattan", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_Manhattan"),
                          div(id = "ManhattanStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(3,
                                   selectInput("Manhattan_y_axis", "Y axis variable:", choices = NULL),
                                   selectInput("Manhattan_y_axis_trans", "Y axis transformation:", choices = c("NULL", "-log10", "Standardization", "Mean-Centering"), selected = "-log10"),
                                   selectInput("Manhattan_hover_text", "Hover text variable:", choices = NULL, multiple = TRUE)
                            ),
                            column(3,
                                   selectInput("Manhattan_color", "Point colors:", choices = c("Default", "Black - single color", "Grey - single color", "Bright", "Cool Tone", "Warm Tone", "Earthy", "Vibrant", "Neon")),
                                   sliderInput("Manhattan_size", "Point size:", min = 0, max = 2, value = 0.8, step = 0.1),
                                   sliderInput("Manhattan_opacity", "Point opacity:", min = 0, max = 1, value = 0.8, step = 0.1)
                            ),
                            column(2,
                                   selectInput("Manhattan_y_threshold", "Threshold line:", choices = c("Show", "NULL"), selected = "NULL"),
                                   selectInput("Manhattan_highlight_color", "Highlight point color:", choices = c("NULL", "Red", "Dark red", "Black", "Grey"), selected = "NULL"),
                                   sliderInput("Manhattan_highlight_size", "Highlight point size:", min = 0, max = 2, value = 1, step = 0.1)
                            ),
                            column(2,
                                   selectInput("Manhattan_x_axis_title", "X-axis title:", choices = c("Show", "NULL")),
                                   sliderInput("Manhattan_x_axis_title_size", "X-axis title size:", min = 6, max = 22, value = 14, step = 0.5),
                                   sliderInput("Manhattan_x_axis_text_size", "X-axis text size:", min = 6, max = 18, value = 12, step = 0.5)
                            ),
                            column(2,
                                   selectInput("Manhattan_y_axis_title", "Y-axis title:", choices = c("Show", "NULL")),
                                   sliderInput("Manhattan_y_axis_title_size", "Y-axis title size:", min = 6, max = 22, value = 14, step = 0.5),
                                   sliderInput("Manhattan_y_axis_text_size", "Y-axis text size:", min = 6, max = 18, value = 12, step = 0.5)
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("ManhattanPlot1")),
                          plotlyOutput("ManhattanPlot"),
                          uiOutput("download_ManhattanPlot"),
                          tags$hr(),
                          tags$br(),
                          tags$br(),
                          width = 9)
                      ))
           )),
  ##### Page 7: Core Collection #####
  tabPanel("Core Collection",
           tabsetPanel(
             tags$br(),
             tabPanel("Core Sample Set",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Core Sample Set"), # CoreSample
                          tags$br(),
                          uiOutput("fileSelection_CoreSample"),
                          verbatimTextOutput("CoreSamplefileInfo"),
                          tags$style("#CoreSamplefileInfo { font-size: 14px;}"),
                          tags$hr(),
                          sliderInput("coverage", "Coverage (%)", min = 90, max = 100, value = 95, step = 0.1),
                          selectInput("diff", "Coverage differences between iterations", choices = c(1, 0.1, 0.01, 0.001),
                                      selected = 0.001),
                          actionButton("runCoreSample", "Run Core Sample", class = "run-action-button"),
                          actionButton("resetCoreSample", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_CoreSample"),
                          div(id = "CoreSampleStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   div(class = "title-text-style", textOutput("CoreSampletitle1")),
                                   verbatimTextOutput("CoreSampleres"),
                                   uiOutput("download_core_sample_dataset"),
                                   uiOutput("download_core_sample_info")
                            ),
                            column(8,
                                   div(class = "title-text-style", textOutput("CoreSampletitle2")),
                                   plotOutput("CoreSampleplot", width = "690px", height = "500px"),
                                   uiOutput("download_CoreSample_plot"),
                                   uiOutput("download_core_sample_coverage")
                            )
                          ),
                          width = 9)
                      )),
             tabPanel("Core SNP Set",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Core SNP Set"), # CoreSNP
                          tags$br(),
                          uiOutput("fileSelection_CoreSNP"),
                          verbatimTextOutput("CoreSNPfileInfo"),
                          tags$style("#CoreSNPfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info5"),
                          uiOutput("Chr_Info3"),
                          uiOutput("dapc_Upload"),
                          sliderInput("CoreSNPratio", "Core SNPs ratio (%)", min = 1, max = 100, value = 10, step = 1),
                          actionButton("runCoreSNP", "Run Core SNP", class = "run-action-button"),
                          actionButton("resetCoreSNP", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_CoreSNP"),
                          div(id = "CoreSNPStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("CoreSNPtitle1")),
                          verbatimTextOutput("CoreSNPres"),
                          uiOutput("download_core_SNP_dataset"),
                          uiOutput("download_core_SNP_info"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("CoreSNPtitle2")),
                          plotOutput("CoreSNPplot", width = "950px", height = "350px"),
                          uiOutput("download_CoreSNP_plot"),
                          uiOutput("download_CoreSNP_site_info"),
                          width = 9)
                      ))
           )),
  ##### Page 8: AI Report #####
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
                                      choices = names(AI_model_choice), selected = "GPT-4o mini"),
                          selectInput("AI_prompt", "Specify AI task:",
                                      choices = c("Summary Request", "Data Interpretation", "Report Structuring", "Idea Expansion"), selected = "Data Interpretation"),
                          fileInput("AI_api_key", "OpenAI API key file:", multiple = F, accept = c(".txt")),
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
  ),
  tags$head(
    tags$style(HTML("
      body.modal-open {
        overflow: auto !important;
      }

      .title-text-style {
        font-size: 20px;
        font-weight: bold;
        color: #853717;
      }

      .run-action-button {
        color: #fff !important;
        background-color: #007ACC !important;
      }

      .run-action-button:hover {
        color: #fff !important;
        background-color: #025e9c !important;
      }

      .S-action-button {
        background-color: #b9d8ed !important;
      }

      .S-action-button:hover {
        color: #fff !important;
        background-color: #007ACC !important;
      }

      .AI1-action-button {
        color: #fff !important;
        background-color: #00a595 !important;
      }

      .AI1-action-button:hover {
        color: #fff !important;
        background-color: #006a60 !important;
      }

      .AI2-action-button {
        background-color: #f7f7ff !important;
      }

      .AI2-action-button:hover {
        background-color: #eaeaed !important;
      }

      .web-button {
        background-color: #99866a !important;
        color: #efefef !important;
        margin-top: 10px;
        margin-bottom: 10px;
        font-size: 15px;
        border-radius: 5px;
        padding: 8px 12px;
      }

      .web-button:hover {
        background-color: #c9bfb0 !important;
        color: #544939 !important;
      }

      .guide-text-block {
        white-space: pre-wrap;
        font-size: 16px;
        color: #333333;
        background: linear-gradient(145deg, #f0f4f8, #e0e6ed);
        padding: 15px;
        border: 1px solid #d0d9e3;
        border-radius: 8px;
        box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
      }

      .progress-bar {
        background-color: #b68d4d;
      }

      .dashed-hr {
        border: none;
        border-top: 1px dashed #000;
        margin: 20px 0;
      }

      .AIReport-tab {
        background-color: #e6e6f0;
        color: #081142;
        border: 1px solid #b0b0cc;
        box-shadow: 0 0 10px rgba(0, 122, 204, 0.4);
        padding: 20px;
        border-radius: 10px;
      }

      .AIReport-tab .form-control {
        background-color: #f7f7ff;
        color: #081142;
        border: 1px solid #b0b0cc;
        border-radius: 6px;
      }

      .AIReport-tab .form-control:focus {
        border-color: #007ACC;
        box-shadow: 0 0 8px rgba(0, 122, 204, 0.6);
      }

      .AIReport-tab .custom-h4 {
        color: #112288;
        font-weight: bold;
        font-size: 24px;
      }

      .AIReport-tab #AI_response1 {
        background-color: #ffffff;
        border: 1px solid #cccccc;
        padding: 20px;
        margin-top: 20px;
        border-radius: 5px;
        box-shadow: 0 0 5px #cccccc;
        white-space: pre-wrap;
      }

      .AIReport-tab #AI_response2 {
        background-color: #ffffff;
        border: 1px solid #cccccc;
        padding: 20px;
        margin-top: 20px;
        border-radius: 5px;
        box-shadow: 0 0 5px #cccccc;
        white-space: pre-wrap;
      }
    "))
  )
)
