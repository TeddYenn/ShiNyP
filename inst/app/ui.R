##### UI ##### 
ui = navbarPage(
  title = "ShiNyP",
  theme = bslib::bs_theme(bootswatch = "zephyr", bg = "#f3f1e5", fg = "#0C1844"), 
  useShinyjs(),
  ##### Home Page #####
  tabPanel("Home",
           fluidPage(
             h3("ShiNyP: A Shiny-Based Interactive Platform for Genome-Wide SNP Analysis and Visualization",
                style = "color: #34495e; font-weight: bold;"),
             p("Yen-Hsiang Huang", br(),
               "National Chung-Hsing University (NCHU)", br(),
               "For any inquiries, please email us at: ", 
               tags$a(href = "mailto:teddyhuangyh@gmail.com", "teddyhuangyh@gmail.com"),
               style = "color: #34495e; font-size: 16px; font-weight: bold;"),
             
             h4("Key Features", style = "color: #34495e; margin-top: 5px;"),
             p("Handling and Analyzing Large Genome-Wide SNP Datasets:",
               style = "font-size: 16px; margin-bottom: 5px;"),
             tags$ul(
               tags$li("Statistical and computational exploration", style = "font-size: 16px;"),
               tags$li("Customizable visualization options", style = "font-size: 16px;"),
               tags$li("Download publication-ready figures and tables", style = "font-size: 16px;"),
               tags$li("Download analyzed data objects", style = "font-size: 16px;"),
               tags$li("Auto-generate customized preliminary results", style = "font-size: 16px;"),
               tags$li("AI-driven report - powered by OpenAI", style = "font-size: 16px;")
             ),
             
             h4("Quickstart", style = "color: #34495e; margin-top: 5px;"),
             p("Demo SNP datasets: ", tags$a(href = "https://drive.google.com/drive/folders/18enzKdE1cb-JXxNFkKCVLWDRTCJ39C00?usp=sharing", "https://reurl.cc/0dpDyk (Google Drive)", target = "_blank"), br(),
               "To begin, navigate to the 'Data Input' tab, where you can upload SNP dataset in VCF format and start the downstream analysis.", br(),
               "Visit the User Guide for detailed instructions on using each feature.",
               style = "font-size: 16px; margin-bottom: 0px;"),
             actionButton("guide_button", "Go to User Guide", icon = icon("book"), style = "background-color: #99866a; color: #efefef; margin-top: 10px; margin-bottom: 10px;"),
             
             h4("Case Studies", style = "color: #34495e; margin-top: 5px;"),
             p("Explore how ShiNyP has been applied in WGS SNP datasets: wild rice & chicken. Check it out!",
               style = "font-size: 16px; margin-bottom: 0px;"),
             actionButton("case_button", "View Case Studies", icon = icon("chart-bar"), style = "background-color: #99866a; color: #efefef; margin-top: 10px; margin-bottom: 10px;"),
             
             h4("Publication", style = "color: #34495e; margin-top: 5px;"),
             p("Huang et al. (upcoming 2024) ShiNyP: A Shiny-Based Interactive Platform for Genome-Wide SNP Analysis and Visualization",
               style = "font-size: 16px;", tags$a(href = "https://www.example.com", "https://www.example.com", target = "_blank")),
             
             h4("Support", style = "color: #34495e; margin-top: 5px;"),
             p("If you encounter any issues or have suggestions for new features, please submit a report through our feedback form:",
               style = "font-size: 16px;", tags$a(href = "https://forms.gle/GPCggSo5czyNLfoB7", "https://forms.gle/GPCggSo5czyNLfoB7  (Google Form)", target = "_blank"))
           )
  ),
  ##### Page 1: Data Input #####
  tabPanel("Data Input",
           sidebarLayout(
             sidebarPanel(
               tags$h5("1. Upload a VCF File"),
               uiOutput("Uploaddata"),
               tags$hr(),
               tags$h5("2. Input VCF Data"),
               tags$br(),
               actionButton("Inputdata", "Input",
                            style = "color: #fff; background-color: #007ACC; border-color: #2e6da4"),
               actionButton("resetInput", "Reset"),
               tags$br(),
               tags$br(),
               sliderInput("presample", "Preview number of samples", min = 1, max = 100, value = 5, step = 1),
               width = 3),
             mainPanel(
               uiOutput("guide_input"),
               div(id = "inputStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
               tags$br(),
               verbatimTextOutput("fileInfo"),
               tags$style("#fileInfo { font-size: 14px;}"),
               tags$hr(),
               textOutput("uploadStatus"),
               tags$style("#uploadStatus { font-size: 20px; font-weight: bold; color: #853717;}"),
               DT::dataTableOutput("contents"),
               width = 9)
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
                          actionButton("sampleQCmissing", "Summary",
                                       style = "color: #fff; background-color: #007ACC; margin-bottom: 10px"),
                          tags$br(),
                          tags$h6("Sample heterozygosity rate"),
                          actionButton("sampleQCH", "Summary",
                                       style = "color: #fff; background-color: #007ACC"),
                          tags$hr(),
                          tags$h5("2. Sample QC"),
                          sliderInput("sampleThrMR", "Threshold of missing rate (remove > [threshold])", min = 0, max = 0.5, value = 0.05, step = 0.001),
                          sliderInput("sampleThrH", "Threshold of heterozygosity rate (remove > [threshold])", min = 0, max = 1, value = 0.10),
                          actionButton("sampleQC", "Sample QC by Thresholds", 
                                       style = "color: #fff; background-color: #007ACC; border-color: #2e6da4"),
                          actionButton("resetsampleQC", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_sampleQC"),
                          tags$br(),
                          textOutput("sampleQCstatus"),
                          tags$style("#sampleQCstatus { font-size: 20px; font-weight: bold; color: #853717;}"),
                          verbatimTextOutput("sampleQCresult"),
                          tags$style("#sampleQCresult { font-size: 14px;}"),
                          uiOutput("download_sampleQC"),
                          tags$hr(),
                          uiOutput("progressUI"),
                          textOutput("samplemissing1"),
                          tags$style("#samplemissing1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          div(id = "samplemissingStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("samplemissing2"),
                          plotOutput("samplemissing3", width = "800px", height = "350px"),
                          tags$hr(),
                          textOutput("sampleh1"),
                          tags$style("#sampleh1 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          actionButton("QCmissing", "Summary",
                                       style = "color: #fff; background-color: #007ACC; margin-bottom: 10px"),
                          tags$br(),
                          tags$h6("SNP minor allele frequency (MAF)"),
                          actionButton("QCMAF", "Summary",
                                       style = "color: #fff; background-color: #007ACC; margin-bottom: 10px"),
                          tags$br(),
                          tags$h6("SNP heterozygosity rate"),
                          actionButton("QCH", "Summary",
                                       style = "color: #fff; background-color: #007ACC"),
                          tags$hr(),
                          tags$h5("2. SNP QC"),
                          sliderInput("ThrMR", "Threshold of missing rate (remove > [threshold])", min = 0, max = 1, value = 0.05),
                          sliderInput("ThrMAF", "Threshold of MAF (remove < [threshold])", min = 0, max = 0.5, value = 0.05),
                          sliderInput("ThrH0", "Threshold of heterozygosity rate (remove < [threshold])", min = 0, max = 1, value = 0.0),
                          sliderInput("ThrH", "Threshold of heterozygosity rate (remove > [threshold])", min = 0, max = 1, value = 0.10),
                          actionButton("QC", "SNP QC by Thresholds", 
                                       style = "color: #fff; background-color: #007ACC; border-color: #2e6da4"),
                          actionButton("resetSNPQC", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_QC"),
                          tags$br(),
                          textOutput("SNPQCstatus"),
                          tags$style("#SNPQCstatus { font-size: 20px; font-weight: bold; color: #853717;}"),
                          verbatimTextOutput("QCresult"),
                          tags$style("#QCresult { font-size: 14px;}"),
                          uiOutput("download_snpQC"),
                          tags$hr(),
                          uiOutput("progressUI"),
                          textOutput("missing1"),
                          tags$style("#missing1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          div(id = "missingStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tableOutput("missing2"),
                          plotOutput("missing3", width = "800px", height = "350px"),
                          tags$hr(),
                          textOutput("maf1"),
                          tags$style("#maf1 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          width = 9)
                      ))
           )),
  ##### Page 3: Data Conversion #####
  tabPanel("Data Conversion",
           tabsetPanel(
             tags$br(),
             tabPanel("Data Conversion", 
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("fileSelection3"),
                          verbatimTextOutput("CfileInfo"),
                          tags$hr(),
                          tags$h5("1. Convert VCF to  data.frame"),
                          actionButton("Cvcf2df", "Convert to data.frame",
                                       style = "color: #fff; background-color: #007ACC"),
                          tags$hr(),
                          tags$h5("2. Convert data.frame to  genind"),
                          uiOutput("groupfile1"),
                          actionButton("Cdf2gi", "Convert to genind",
                                       style = "color: #fff; background-color: #007ACC; margin-bottom: 5px"),
                          tags$hr(),
                          tags$h5("3. Convert genind to  genlight"),
                          actionButton("Cgi2gl", "Convert to genlight",
                                       style = "color: #fff; background-color: #007ACC"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_C"),
                          tags$hr(),
                          textOutput("Cstatus1"),
                          tags$style("#Cstatus1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          uiOutput("progressUI"),
                          verbatimTextOutput("CTable1"),
                          uiOutput("download_df"),
                          uiOutput("download_snpInfo"),
                          tags$br(),
                          textOutput("Cstatus2"),
                          tags$style("#Cstatus2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          div(id = "giStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          verbatimTextOutput("CTable2"),
                          uiOutput("download_gi"),
                          tags$br(),
                          textOutput("Cstatus3"),
                          tags$style("#Cstatus3 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          div(id = "glStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          verbatimTextOutput("CTable3"),
                          uiOutput("download_gl"),
                          width = 9)
                      )
             ),
             tabPanel("Input Converted Data", 
                      sidebarLayout(
                        sidebarPanel(
                          tags$h5("Input data.frame File"),
                          uiOutput("uploaddf"),
                          actionButton("inputdf", "Input",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetdf", "Reset"),
                          tags$hr(),
                          tags$h5("Input genind File"),
                          uiOutput("uploadgi"),
                          actionButton("inputgi", "Input",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetgi", "Reset"),
                          tags$hr(),
                          tags$h5("Input genlight File"),
                          uiOutput("uploadgl"),
                          actionButton("inputgl", "Input",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetgl", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_input2"),
                          div(id = "input2Status", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          textOutput("dfstatus"),
                          tags$style("#dfstatus { font-size: 20px; font-weight: bold; color: #853717;}"),
                          verbatimTextOutput("dfinfo"),
                          tags$br(),
                          textOutput("gistatus"),
                          tags$style("#gistatus { font-size: 20px; font-weight: bold; color: #853717;}"),
                          verbatimTextOutput("giinfo"),
                          tags$br(),
                          textOutput("glstatus"),
                          tags$style("#glstatus { font-size: 20px; font-weight: bold; color: #853717;}"),
                          verbatimTextOutput("glinfo"),
                          width = 9)
                      ))
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
                          actionButton("runPCA", "Run PCA",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetPCA", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_PCA"),
                          div(id = "PCAStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6, 
                                   textOutput("PCAtitle1"),
                                   tags$style("#PCAtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("PCAplot", width = "500px", height = "500px"),
                                   uiOutput("pc1"),
                                   uiOutput("pc2"),
                                   uiOutput("groupfile4"),
                                   uiOutput("download_PCA_plot")
                            ),
                            column(6, 
                                   textOutput("PCAtitle2"),
                                   tags$style("#PCAtitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          sliderInput("npca", "The number of PC axes retained", min = 1, max = 1000, value = 100, step = 1),
                          sliderInput("Maxgrp", "Maximum number of clusters ", min = 3, max = 35, value = 16, step = 1),
                          actionButton("runDAPC1", "Run STEP I",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetDAPC1", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: DAPC Analysis"),
                          sliderInput("grp", "Number of cluster (K)", min = 3, max = 35, value = 5, step = 1),
                          actionButton("runDAPC2", "Run STEP II",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetDAPC2", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_DAPC"),
                          div(id = "DAPCStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(5, 
                                   textOutput("DAPCtitle1"),
                                   tags$style("#DAPCtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("BICplot", width = "400px", height = "270px"),
                                   uiOutput("download_BIC_plot"),
                                   textOutput("DAPCtitle2"),
                                   tags$style("#DAPCtitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("DF1plot", width = "400px", height = "270px"),
                                   uiOutput("download_DF1_plot"),
                                   textOutput("DAPCtitle3"),
                                   tags$style("#DAPCtitle3 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("DF2plot", width = "400px", height = "270px"),
                                   uiOutput("download_DF2_plot")
                            ),
                            column(7, 
                                   uiOutput("download_DAPC_pop"),
                                   uiOutput("download_DAPC_transformed"),
                                   uiOutput("download_DAPC_result"),
                                   tags$hr(),
                                   textOutput("DAPCtitle4"),
                                   tags$style("#DAPCtitle4 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("DAPCplot", width = "600px", height = "600px"),
                                   uiOutput("download_DAPC_plot")
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
                          sliderInput("sample", "Number of bootstrap replicates", min = 10, max = 1000, value = 100, step = 10),
                          actionButton("runUPGMA", "Run UPGMA",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetUPGMA", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_UPGMA"),
                          div(id = "UPGMAStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          textOutput("UPGMAtitle1"),
                          tags$style("#UPGMAtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          actionButton("runNJ", "Run NJ",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetNJ", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_NJ"),
                          div(id = "NJStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          textOutput("NJtitle1"),
                          tags$style("#NJtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          actionButton("runKinship", "Run Kinship",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetKinship", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_Kinship"),
                          div(id = "KinshipStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          textOutput("Kinshiptitle1"),
                          tags$style("#Kinshiptitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          plotOutput("Kinship", width = "800px", height = "800px"),
                          uiOutput("download_Kinship_plot"),
                          uiOutput("download_Kinship_result"),
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
                          actionButton("runGD", "Analysis",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetGD", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_GD"),
                          div(id = "GDStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          textOutput("GDtitle1"),
                          tags$style("#GDtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          plotOutput("GDplot", width = "950px", height = "350px"),
                          uiOutput("Type"),
                          uiOutput("Parameter"),
                          uiOutput("download_GD_plot"),
                          tags$hr(),
                          textOutput("GDtitle2"),
                          tags$style("#GDtitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          DT::dataTableOutput("GDresults"),
                          uiOutput("download_GD_site"),
                          uiOutput("download_GD"),
                          tags$hr(),
                          textOutput("GDtitle3"),
                          tags$style("#GDtitle3 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          actionButton("runSW", "Run Sliding Window",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetSW", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: Circos Plot"),
                          uiOutput("Chr_Info"), # Track 1
                          selectInput("Track1", "Track 1 & 2: Chromosome Info.", choices = NULL), # Track 1
                          uiOutput("Track3"), # Track 3-
                          actionButton("addTrack", "Add Track", style = "background-color: #deddc6"),
                          actionButton("runCircos", "Run Circos Plot",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetCircos", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_Circos"),
                          div(id = "CircosStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          uiOutput("progressUI"),
                          textOutput("Circostitle1"),
                          tags$style("#Circostitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          DT::dataTableOutput("SWresults"),
                          uiOutput("download_SW"),
                          tags$hr(),
                          textOutput("Circostitle2"),
                          tags$style("#Circostitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          actionButton("runGT", "Run Genetic Distance",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetGT", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_GT"),
                          div(id = "GTStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6, 
                                   textOutput("GTtitle1"),
                                   tags$style("#GTtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("GTplot", width = "400px", height = "400px"),
                                   uiOutput("download_GT_plot")
                            ),
                            column(6,
                                   textOutput("GTtitle2"),
                                   tags$style("#GTtitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          actionButton("runAMOVA", "Run AMOVA",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetAMOVA", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: Permutation Test"),
                          sliderInput("nperm", "Number of permutations", min = 10, max = 1000, value = 99, step = 1),
                          actionButton("runTest", "Run Permutation Test",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetTest", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_AMOVA"),
                          div(id = "AMOVAStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6, 
                                   textOutput("AMOVAtitle1"),
                                   tags$style("#AMOVAtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("AMOVAvarplot", width = "450px", height = "600px"),
                                   uiOutput("download_AMOVA_plot")
                            ),
                            column(6,
                                   textOutput("AMOVAtitle2"),
                                   tags$style("#AMOVAtitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("AMOVAtestplot", width = "450px", height = "600px"),
                                   uiOutput("download_AMOVA_test_plot")
                            )
                          ),
                          tags$hr(),
                          textOutput("AMOVAtitle3"),
                          tags$style("#AMOVAtitle3 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          tags$h4("Genome Scans for Selection based on Principal Component Analysis (pcadapt)"),
                          tags$br(),
                          uiOutput("fileSelection_pcadapt"),
                          verbatimTextOutput("pcadaptfileInfo"),
                          tags$style("#pcadaptfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info2"),
                          sliderInput("pcadapt_PC", "The number of PC axes retained", min = 1, max = 35, value = 5, step = 1),
                          actionButton("SNPthin", "SNP thinning", style = "background-color: #deddc6"),
                          uiOutput("SNPthin_size"),
                          uiOutput("SNPthin_thr"),
                          actionButton("runpcadapt", "Run pcadapt",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetpcadapt", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_pcadapt"),
                          div(id = "pcadaptStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(4, 
                                   uiOutput("pcadapt_adj"),
                                   uiOutput("pcadapt_alpha")
                            ),
                            column(8,
                                   verbatimTextOutput("pcadapt_result")
                            )
                          ),
                          textOutput("pcadapttitle1"), # Manhattan plot
                          tags$style("#pcadapttitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          plotOutput("pcadaptplot1", width = "950px", height = "350px"),
                          uiOutput("download_pcadapt_plot1"),
                          tags$hr(),
                          fluidRow(
                            column(4, 
                                   textOutput("pcadapttitle2"), # QQ plot
                                   tags$style("#pcadapttitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("pcadaptplot2", width = "350px", height = "350px"),
                                   uiOutput("download_pcadapt_plot2")
                            ),
                            column(4,
                                   textOutput("pcadapttitle3"), # Histogram of p-values
                                   tags$style("#pcadapttitle3 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("pcadaptplot3", width = "350px", height = "350px"),
                                   uiOutput("download_pcadapt_plot3")
                            ),
                            column(4,
                                   textOutput("pcadapttitle4"), # Histogram of test statistics
                                   tags$style("#pcadapttitle4 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("pcadaptplot4", width = "350px", height = "350px"),
                                   uiOutput("download_pcadapt_plot4")
                            )
                          ),
                          tags$hr(),
                          textOutput("pcadapttitle5"), # Table of sign. SNPs
                          tags$style("#pcadapttitle5 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          DT::dataTableOutput("pcadapt_Sign_SNP"),
                          uiOutput("download_pcadapt_results"),
                          width = 9)
                      )),
             tabPanel("OutFLANK", 
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Identifies loci under selection per population (OutFLANK)"),
                          tags$br(),
                          uiOutput("fileSelection_OutFLANK"),
                          verbatimTextOutput("OutFLANKfileInfo"),
                          tags$style("#OutFLANKfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info3"),
                          actionButton("runOutFLANK", "Run OutFLANK",
                                       style = "color: #fff; background-color: #007ACC"),
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
                            ),
                            column(8,
                                   verbatimTextOutput("OutFLANK_result")
                            )
                          ),
                          textOutput("OutFLANKtitle1"), # Manhattan plot
                          tags$style("#OutFLANKtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          plotOutput("OutFLANKplot1", width = "950px", height = "350px"), # p-value
                          uiOutput("download_OutFLANK_plot1"),
                          tags$br(),
                          plotOutput("OutFLANKplot2", width = "950px", height = "350px"), # Fst
                          uiOutput("download_OutFLANK_plot2"),
                          tags$hr(),
                          fluidRow(
                            column(4, 
                                   textOutput("OutFLANKtitle2"), # QQ plot
                                   tags$style("#OutFLANKtitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("OutFLANKplot3", width = "350px", height = "350px"),
                                   uiOutput("download_OutFLANK_plot3")
                            ),
                            column(4,
                                   textOutput("OutFLANKtitle3"), # Histogram of p-values
                                   tags$style("#OutFLANKtitle3 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("OutFLANKplot4", width = "350px", height = "350px"),
                                   uiOutput("download_OutFLANK_plot4")
                            ),
                            column(4,
                                   textOutput("OutFLANKtitle4"), # Histogram of Fst
                                   tags$style("#OutFLANKtitle4 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("OutFLANKplot5", width = "350px", height = "350px"),
                                   uiOutput("download_OutFLANK_plot5")
                            )
                          ),
                          tags$hr(),
                          textOutput("OutFLANKtitle5"), # Table of sign. SNPs
                          tags$style("#OutFLANKtitle5 { font-size: 20px; font-weight: bold; color: #853717;}"),
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
                          actionButton("runIBS", "Run IBS",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetIBS", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_IBS"),
                          div(id = "IBSStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          verbatimTextOutput("IBSres"),
                          textOutput("IBStitle1"), # Chromosome ideogram
                          tags$style("#IBStitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          plotOutput("IBSplot", width = "950px", height = "350px"),
                          uiOutput("download_IBS_plot"),
                          tags$hr(),
                          textOutput("IBStitle2"), # Sliding window data
                          tags$style("#IBStitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                          DT::dataTableOutput("IBS_SW"),
                          uiOutput("download_IBS_SW"),
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
                          actionButton("runCoreSample", "Run Core",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetCoreSample", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_CoreSample"),
                          div(id = "CoreSampleStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(4, 
                                   textOutput("CoreSampletitle1"),
                                   tags$style("#CoreSampletitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   verbatimTextOutput("CoreSampleres"),
                                   uiOutput("download_core_sample_coverage"),
                                   uiOutput("download_core_sample_dataset"),
                                   uiOutput("download_core_sample_info")
                                   
                            ),
                            column(8,
                                   textOutput("CoreSampletitle2"),
                                   tags$style("#CoreSampletitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("CoreSampleplot", width = "690px", height = "500px"),
                                   uiOutput("download_CoreSample_plot")
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
                          sliderInput("coverage2", "Coverage (%)", min = 90, max = 100, value = 95, step = 0.1),
                          selectInput("diff2", "Coverage differences between iterations", choices = c(1, 0.1, 0.01, 0.001), 
                                      selected = 0.001),
                          actionButton("runCoreSNP", "Run Core",
                                       style = "color: #fff; background-color: #007ACC"),
                          actionButton("resetCoreSNP", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_CoreSNP"),
                          div(id = "CoreSNPStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(4, 
                                   textOutput("CoreSNPtitle1"),
                                   tags$style("#CoreSNPtitle1 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   verbatimTextOutput("CoreSNPres"),
                                   uiOutput("download_core_SNP_coverage"),
                                   uiOutput("download_core_SNP_dataset"),
                                   uiOutput("download_core_SNP_info")
                                   
                            ),
                            column(8,
                                   textOutput("CoreSNPtitle2"),
                                   tags$style("#CoreSNPtitle2 { font-size: 20px; font-weight: bold; color: #853717;}"),
                                   plotOutput("CoreSNPplot", width = "690px", height = "500px"),
                                   uiOutput("download_CoreSNP_plot")
                            )
                          ),
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
                          tags$h4("1. Preliminary results", class = "custom-h4"),
                          textInput("AI_species", "Enter species of your SNP dataset:", value = "", placeholder = "Ex: Wild rice (Oryza rufipogon)"),
                          actionButton("autogenerate", "Auto-generate",
                                       style = "color: #fff; background-color: #00a595"),
                          tags$hr(class = "dashed-hr"),
                          actionButton("Input_autogenerate", "Or click here to upload...",
                                       style = "color: #292929; background-color: #deddc6"),
                          actionButton("Input_autogenerate_Reset", "Reset",
                                       style = "color: #292929; background-color: #f7f7ff")
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
                          tags$h4("2. AI-driven report", class = "custom-h4"),
                          selectInput("AI_model", "Choose AI model:",
                                      choices = names(AI_model_choice), selected = "GPT-4o mini"),
                          fileInput("AI_api_key", "OpenAI API key file:", multiple = F, accept = c(".txt")),
                          actionButton("runAIreport", "Get report",
                                       style = "color: #fff; background-color: #00a595"),
                          actionButton("AIreport_Reset", "Reset",
                                       style = "color: #292929; background-color: #f7f7ff"),
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
      
      .btn-file {  
        background-color: #deddc6; 
        color: white;
      }
      
      .progress-bar {
        background-color: #b68d4d;
      }
      
      .dashed-hr {
        border: none;
        border-top: 1px dashed #000;
        margin: 20px 0;
      }
    "))
  )
)