# Page_2_Data_QC
##### Page 2: Data QC #####
#' @title Page_2_Data_QC_UI
#' @export
Page_2_Data_QC_UI = function() {
  tabPanel("Data QC",
           tabsetPanel(
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
                          sliderInput("sampleThrMR", "Threshold of missing rate (remove > [threshold])", min = 0, max = 1, value = 0.05, step = 0.01),
                          sliderInput("sampleThrH", "Threshold of heterozygosity rate (remove > [threshold])", min = 0, max = 1, value = 0.10, step = 0.01),
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
                          div(class = "title-text-style", uiOutput("samplemissing1")),
                          div(id = "samplemissingStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
                          tableOutput("samplemissing2"),
                          plotOutput("samplemissing3", width = "800px", height = "350px"),
                          tags$hr(),
                          div(class = "title-text-style", uiOutput("sampleh1")),
                          div(id = "samplehStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
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
                          sliderInput("ThrMR", "Threshold of missing rate (remove > [threshold])", min = 0, max = 1, value = 0.05, step = 0.01),
                          sliderInput("ThrMAF", "Threshold of MAF (remove < [threshold])", min = 0, max = 0.5, value = 0.05, step = 0.01),
                          sliderInput("ThrH0", "Threshold of heterozygosity rate (remove < [threshold])", min = 0, max = 1, value = 0.0, step = 0.01),
                          sliderInput("ThrH", "Threshold of heterozygosity rate (remove > [threshold])", min = 0, max = 1, value = 0.10, step = 0.01),
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
                          div(class = "title-text-style", uiOutput("missing1")),
                          div(id = "missingStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
                          tableOutput("missing2"),
                          plotOutput("missing3", width = "800px", height = "350px"),
                          tags$hr(),
                          div(class = "title-text-style", uiOutput("maf1")),
                          div(id = "mafStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
                          tableOutput("maf2"),
                          plotOutput("maf3", width = "800px", height = "350px"),
                          tags$hr(),
                          div(class = "title-text-style", uiOutput("h1")),
                          div(id = "hStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
                          tableOutput("h2"),
                          plotOutput("h3", width = "800px", height = "350px"),
                          tags$hr(),
                          div(class = "title-text-style", uiOutput("HWE1")),
                          div(id = "hweStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
                          tableOutput("HWE2"),
                          plotOutput("HWE3", width = "800px", height = "380px"),
                          tags$hr(),
                          width = 9)
                      )),
             tabPanel("SNP Density",
                      sidebarLayout(
                        sidebarPanel(
                          bslib::tooltip(
                            uiOutput("Site_Info0"),
                            "Upload: Site Info. (RDS)"
                          ),
                          bslib::tooltip(
                            uiOutput("Chr_Info0"),
                            "Upload: Chromosome Info. (CSV)"
                          ),
                          sliderInput("WindowSize0", "Window size (kb)", min = 0, max = 1000, value = 500, step = 10),
                          actionButton("SNPdensity", "Summary", class = "run-action-button"),
                          actionButton("resetSNPdensity", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_SNPdensity"),
                          uiOutput("progressUI"),
                          div(id = "SNPdensityStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
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
           ))
}
#' @title Page_2_Data_QC_Server
#' @export
Page_2_Data_QC_Server = function(input, output, session) {
  #### Page 2-1: Sample QC ####
  output$fileSelection1 = renderUI({
    choices = c(
      "Input VCF Data (in data.frame)" = "VCFdf",
      "SNP Post-QC Data" = "QCData"
    )
    selectInput("selectedFile", "Selecte a dataset for QC:", choices)
  })
  
  output$SampleQCfileInfo = renderText({
    req(VCFdf(), input$selectedFile)
    if (input$selectedFile == "QCData" && is.null(QCData())) {
      paste("Not available for 'SNP Post-QC Data'!")
    } else {
      data = if (input$selectedFile == "VCFdf") VCFdf() else QCData()
      prefix = if (input$selectedFile == "VCFdf") "" else "Post-QC Data (Updated)\n"
      paste0(prefix,
             "Number of samples: ", nrow(data), "\n",
             "Number of SNPs: ", ncol(data), "\n",
             "Type: data.frame")
    }
  })
  
  observeEvent(input$sampleQCmissing, {
    data = if (input$selectedFile == "VCFdf") VCFdf() else QCData()
    req(data)
    shinyjs::show("samplemissingStatus")
    rate = rowSums(is.na(data)) / ncol(data)
    samplemissingrate(rate)
    samplemissing1("Sample Missing Rate")

    if (all(rate == 0, na.rm = TRUE)) {
      samplemissing1("Sample Missing Rate \n*all values are 0")
    } else{
      samplemissing1("Sample Missing Rate")
    }
    
    samplemissing2(stat2summary(samplemissingrate()))
    shinyjs::hide("samplemissingStatus")
    showNotification("Run Successfully - Sample Missing Rate", type = "message")
  })

  observeEvent(input$sampleQCH, {
    data = if (input$selectedFile == "VCFdf") VCFdf() else QCData()
    req(data)
    shinyjs::show("samplehStatus")
    mat = as.matrix(data)
    rate = rowSums(mat == 1, na.rm = TRUE) / (ncol(mat) - rowSums(is.na(mat)))
    sampleh(rate)
    if (all(rate == 0, na.rm = TRUE)) {
      sampleh1("Sample Heterozygosity Rate \n*all values are 0")
    } else{
      sampleh1("Sample Heterozygosity Rate")
    }

    sampleh2(stat2summary(sampleh()))
    shinyjs::hide("samplehStatus")
    showNotification("Run Successfully - Sample Heterozygosity Rate", type = "message")
  })
  
  observeEvent(input$sampleQC, {
    if (is.null(Site_Info())) {
      showNotification("Site Info is required. Please upload it using any 'Site Info' upload box.",
                       type = "error")
      return(NULL)
    }
    req(samplemissingrate(), sampleh())

    data = if (input$selectedFile == "VCFdf") VCFdf() else QCData()
    rm.sample = union(which(samplemissingrate() > input$sampleThrMR),
                      which(sampleh() > input$sampleThrH))
    if (length(rm.sample) > 0) {
      data = data[-rm.sample, ]
    }
    QCData(data)
    df(data)
    SampleQC_sample(nrow(data))
    SampleQC_SNP(ncol(data))
    
    Site_Info(Site_Info())
    guide_sampleQC("Sample quality control is complete. \nYou will receive the Post-QC Data (in data.frame) when you download the file.")
    sampleQCstatus("Post-QC Data (in data.frame)")
    pre_results = pre_results()
    pre_results[[6]] = "## Data QC"
    pre_results[[7]] = "### Sample QC"
    pre_results[[8]] = paste0("Removed samples with ", "missing rate > ", input$sampleThrMR ," and heterozygosity rate > ", input$sampleThrH)
    pre_results[[9]] = paste0("Number of samples: ", nrow(QCData()))
    pre_results[[10]] = paste0("Number of SNPs: ", ncol(QCData()))
    pre_results(pre_results)
    
    showNotification("Run Successfully", type = "message")
    
    output$DsampleQC = downloadHandler(
      filename = function() {
        paste("data.frame_", SampleQC_sample(), "_", SampleQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        shinyjs::show("samplemissingStatus")
        saveRDS(QCData(), file)
        shinyjs::hide("samplemissingStatus")
      })
    
    output$DsampleQCSite = downloadHandler(
      filename = function() {
        paste("Site_Info_", SampleQC_sample(), "_", SampleQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        shinyjs::show("samplemissingStatus")
        saveRDS(Site_Info(), file)
        shinyjs::hide("samplemissingStatus")
      })
  })
  
  observeEvent(input$resetsampleQC, {
    QCData(NULL)
    SampleQC_sample(0)
    SampleQC_SNP(0)
    sampleQCstatus("")
    showNotification("Data have been reset.")
    guide_sampleQC("1️⃣ Need to obtain the summary statistics first! Then, scroll down to review the results. \n2️⃣ Adjust the thresholds and click the 'Sample QC by Thresholds' button.")
    })
  
  output$sampleQCresult = renderText({
    req(SampleQC_sample(), SampleQC_SNP())
    if (sampleQCstatus() == "Post-QC Data (in data.frame)") {
      paste("Removed samples with ", "missing rate > ", input$sampleThrMR ," and heterozygosity rate > ", input$sampleThrH, "\n",
            "File name: ", "data.frame_", SampleQC_sample(), "_", SampleQC_SNP(), "SNPs", "\n",
            "Number of samples: ", SampleQC_sample(), "\n",
            "Number of SNPs: ", SampleQC_SNP(), "\n",
            "Type: data.frame",
            sep = "")
    }
  })
  
  output$download_sampleQC = renderUI({
    if (sampleQCstatus() == "Post-QC Data (in data.frame)") {
      downloadButton("DsampleQC", "Download data.frame File")
    }
  })
  
  output$download_sampleQC_Site_info = renderUI({
    if (sampleQCstatus() == "Post-QC Data (in data.frame)") {
      downloadButton("DsampleQCSite", "Download Site Info.")
    }
  })
  
  output$sampleQCstatus = renderText({
    sampleQCstatus()
  })
  
  output$guide_sampleQC = renderUI({
    div(class = "guide-text-block", guide_sampleQC())
  })
  output$samplemissing1 = renderText({ samplemissing1() })
  
  output$samplemissing2 = renderTable({ samplemissing2() })
  
  output$samplemissing3 = renderPlot({
    req(samplemissingrate())
    Sampleplot(samplemissingrate())
  })
  
  output$sampleh1 = renderText({ sampleh1() })
  
  output$sampleh2 = renderTable({ sampleh2() })
  
  output$sampleh3 = renderPlot({
    req(sampleh())
    Sampleplot(sampleh())
  })
  
  #### Page 2-2: SNP QC ####
  output$fileSelection2 = renderUI({
    choices = c(
      "Sample Post-QC Data" = "QCData",
      "Input VCF Data (in data.frame)" = "VCFdf"
    )
    selectInput("selectedFile2", "Selecte a dataset for QC:", choices)
  })
  
  output$SNPQCfileInfo = renderText({
    req(VCFdf(), input$selectedFile2)
    if (input$selectedFile2 == "QCData" && is.null(QCData())) {
      paste("Not available for 'Sample Post-QC Data'!")
    } else {
      data = if (input$selectedFile2 == "VCFdf") VCFdf() else QCData()
      prefix = if (input$selectedFile2 == "VCFdf") "" else "Post-QC Data (Updated)\n"
      paste0(prefix,
             "Number of samples: ", nrow(data), "\n",
             "Number of SNPs: ", ncol(data), "\n",
             "Type: data.frame")
    }
  })
  
  observeEvent(input$QCmissing, {
    data = if (input$selectedFile2 == "VCFdf") VCFdf() else QCData()
    req(data)
    shinyjs::show("missingStatus")
    rate = colSums(is.na(data)) / nrow(data)
    missingrate(rate)

    if (all(rate == 0, na.rm = TRUE)) {
      missing1("SNP Missing Rate \n*all values are 0")
    } else{
      missing1("SNP Missing Rate")
    }
    missing2(stat2summary(missingrate()))
    shinyjs::hide("missingStatus")
    showNotification("Run Successfully - SNP Missing Rate", type = "message")
  })
  
  observeEvent(input$QCMAF, {
    data = if (input$selectedFile2 == "VCFdf") VCFdf() else QCData()
    req(data)
    shinyjs::show("mafStatus")
    mat = as.matrix(data)
    valid = nrow(mat) - colSums(is.na(mat))
    rate = colSums(mat, na.rm = TRUE) / (2 * valid)
    rate = pmin(rate, 1 - rate)
    maf(rate)
    maf1("SNP Minor Allele Frequency (MAF)")
    maf2(stat2summary(maf()))
    shinyjs::hide("mafStatus")
    showNotification("Run Successfully - SNP MAF", type = "message")
  })
  
  observeEvent(input$QCH, {
    data = if (input$selectedFile2 == "VCFdf") VCFdf() else QCData()
    req(data)
    shinyjs::show("hStatus")
    mat = as.matrix(data)
    valid = nrow(mat) - colSums(is.na(mat))
    rate = colSums(mat == 1, na.rm = TRUE) / valid
    h(rate)

    if (all(rate == 0, na.rm = TRUE)) {
      h1("SNP Heterozygosity Rate \n*all values are 0")
    } else{
      h1("SNP Heterozygosity Rate")
    }
    h2(stat2summary(h()))
    shinyjs::hide("hStatus")
    showNotification("Run Successfully - SNP Heterozygosity Rate", type = "message")
  })
  
  observeEvent(input$QCHWE, {
    data = if (input$selectedFile2 == "VCFdf") VCFdf() else QCData()
    req(data)
    shinyjs::show("hweStatus")
    hwe = hwe_test(data)
    HWE(hwe)

    if (all(hwe < 0.00001, na.rm = TRUE)) {
      HWE1("SNP p-value for Hardy-Weinberg equilibrium (HWE) \n*all values are 0")
    } else{
      HWE1("SNP p-value for Hardy-Weinberg equilibrium (HWE)")
    }
    HWE2(stat2summary_HWE(HWE()))
    shinyjs::hide("hweStatus")
    showNotification("Run Successfully - SNP HWE", type = "message")
  })
  
  output$doThrHWE = renderUI({
    if (input$doHWE == TRUE){
      sliderInput("ThrHWE", "Threshold of -log(p) for HWE (remove > [threshold])", min = 0, max = 30, value = 6)
    }
  })
  
  observeEvent(input$QC, {
    if (is.null(Site_Info())) {
      showNotification("Site Info is required. Please upload it using any 'Site Info' upload box.",
                       type = "error")
      return(NULL)
    }
    req(missingrate(), maf(), h(), HWE())

    data = if (input$selectedFile2 == "VCFdf") VCFdf() else QCData()
    if (input$doHWE == TRUE) {
      rm.loc = union(union(union(union(which(missingrate() > input$ThrMR),
                                       which(maf() < input$ThrMAF)),
                                 which(h() > input$ThrH)),
                           which(h() < input$ThrH0)),
                     which(-log10(HWE()) > input$ThrHWE))
    } else {
      rm.loc = union(union(union(which(missingrate() > input$ThrMR),
                                 which(maf() < input$ThrMAF)),
                           which(h() > input$ThrH)),
                     which(h() < input$ThrH0))
    }

    if (length(rm.loc) > 0){
      data = data[, -rm.loc]
    }
    QCData(data)
    df(data)
    SNPQC_sample(nrow(data))
    SNPQC_SNP(ncol(data))
    Site_Info = Site_Info()[-rm.loc, ]
    Site_Info(Site_Info)
    guide_QC("SNP quality control is complete. \nYou will receive the Post-QC Data (in data.frame) when you download the file.")
    SNPQCstatus("Post-QC Data (in data.frame)")
    pre_results = pre_results()
    pre_results[[6]] = "## Data QC"
    pre_results[[11]] = "### SNP QC"
    if (input$doHWE == TRUE) {
      pre_results[[12]] = paste0("Removed SNPs with ", "missing rate > ", input$ThrMR ,", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", heterozygosity rate > ", input$ThrH, ", and HWE -log10(p-value) > ", input$ThrHWE)
    } else{
      pre_results[[12]] = paste0("Removed SNPs with ", "missing rate > ", input$ThrMR ,", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", and heterozygosity rate > ", input$ThrH)
    }
    pre_results[[13]] = paste0("Number of samples: ", nrow(QCData()))
    pre_results[[14]] = paste0("Number of SNPs: ", ncol(QCData()))
    pre_results(pre_results)

    showNotification("Run Successfully", type = "message")
    
    output$DsnpQC = downloadHandler(
      filename = function() {
        paste("data.frame_", SNPQC_sample(), "_", SNPQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        shinyjs::show("missingStatus")
        saveRDS(QCData(), file)
        shinyjs::hide("missingStatus")
      })
  
    
    output$DSNPQCSite = downloadHandler(
      filename = function() {
        paste("Site_Info_", SNPQC_sample(), "_", SNPQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        shinyjs::show("missingStatus")
        saveRDS(Site_Info(), file)
        shinyjs::hide("missingStatus")
      })
  })
  
  observeEvent(input$resetSNPQC, {
    QCData(NULL)
    SNPQC_sample(0)
    SNPQC_SNP(0)
    SNPQCstatus("")
    showNotification("Data have been reset.")
    guide_QC("1️⃣ Need to obtain the summary statistics first! Then, scroll down to review the results. \n2️⃣ Adjust the thresholds and click the 'SNP QC by Thresholds' button.")
    })
  
  output$QCresult = renderText({
    req(SNPQC_sample(), SNPQC_SNP())
    if (SNPQCstatus() == "Post-QC Data (in data.frame)") {
      if (input$doHWE == TRUE) {
        paste("Removed SNPs with ", "missing rate > ", input$ThrMR , ", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", heterozygosity rate > ", input$ThrH, ", and HWE -log10(p-value) > ", input$ThrHWE, "\n",
              "File name: ", "data.frame_", SNPQC_sample(), "_", SNPQC_SNP(), "SNPs", "\n",
              "Number of samples: ", SNPQC_sample(), "\n",
              "Number of SNPs: ", SNPQC_SNP(), "\n",
              "Type: data.frame",
              sep = "")
      } else{
        paste("Removed SNPs with ", "missing rate > ", input$ThrMR , ", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", and heterozygosity rate > ", input$ThrH, "\n",
              "File name: ", "data.frame_", SNPQC_sample(), "_", SNPQC_SNP(), "SNPs", "\n",
              "Number of samples: ", SNPQC_sample(), "\n",
              "Number of SNPs: ", SNPQC_SNP(), "\n",
              "Type: data.frame",
              sep = "")
      }
    }
  })
  
  output$download_snpQC = renderUI({
    if (SNPQCstatus() == "Post-QC Data (in data.frame)") {
      downloadButton("DsnpQC", "Download data.frame File")
    }
  })
  
  output$download_SNPQC_Site_info = renderUI({
    if (SNPQCstatus() == "Post-QC Data (in data.frame)") {
      downloadButton("DSNPQCSite", "Download Site Info.")
    }
  })
  output$guide_QC = renderUI({ div(class = "guide-text-block", guide_QC()) })
  output$SNPQCstatus = renderText({ SNPQCstatus() })
  
  output$missing1 = renderText({ missing1() })
  output$missing2 = renderTable({ missing2() })
  output$missing3 = renderPlot({
    req(missingrate())
    SNPplot(missingrate())
  })
  
  output$maf1 = renderText({ maf1() })
  output$maf2 = renderTable({ maf2() })
  output$maf3 = renderPlot({
    req(maf())
    SNPplot(maf())
  })
  
  output$h1 = renderText({ h1() })
  output$h2 = renderTable({ h2()})
  output$h3 = renderPlot({
    req(h())
    SNPplot(h())
  })
  
  output$HWE1 = renderText({ HWE1() })
  output$HWE2 = renderTable({ HWE2()})
  output$HWE3 = renderPlot({
    req(HWE())
    SNPplot_HWE(-log10(HWE()))
  })
  
  #### Page 2-3: SNP Density ####
  output$Site_Info0 = renderUI({
    fileInput("Site_Info0", "Site Info.* (required)", multiple = F, accept = c(".rds"))
  })
  
  observeEvent(input$Site_Info0, {
    req(input$Site_Info0)
    tryCatch({
      data = readRDS(input$Site_Info0$datapath)
      if (!("Chr" %in% names(data))) stop("Site Info file is missing the required 'Chr' column.")
      # if (any(is.na(data$Chr))) stop("The 'Chr' column in Site Info contains missing (NA) values.")
      Site_Info(data)
      showNotification("Uploaded successfully", type = "message")
    }, error = function(e) {
      Site_Info(NULL)
      showNotification(paste("Fail: ", e$message), type = "error", duration = 10)
    })
  })
  
  output$Chr_Info0 = renderUI({
    fileInput("Chr_Info0", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$Chr_Info0, {
    req(input$Chr_Info0)
    tryCatch({
      data = read.csv(input$Chr_Info0$datapath, stringsAsFactors = FALSE)
      required_cols = c("Chr", "Start", "End")
      missing_cols = setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) stop(paste("Chromosome Info file is missing required columns:", paste(missing_cols, collapse = ", ")))
      if (!is.numeric(data$Start) || !is.numeric(data$End)) stop("The 'Start' and 'End' columns in Chromosome Info must be numeric.")
      if (any(is.na(data$Start)) || any(is.na(data$End))) stop("The 'Start' or 'End' column in Chromosome Info contains missing (NA) values.")
      Chr_Info(data)
      showNotification("Uploaded successfully", type = "message")
    }, error = function(e) {
      Chr_Info(NULL)
      showNotification(paste("Fail: ", e$message), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$SNPdensity, {
    req(Chr_Info(), Site_Info())
    
    # Defensive check: validate data structures
    tryCatch({
      Site_Info_data <- Site_Info()
      Chr_Info_data  <- Chr_Info()
      Site_Info_data$Chr = as.numeric(Site_Info_data$Chr)
      
      # Check required columns in Site_Info
      required_site_cols <- c("Chr")
      missing_site_cols <- setdiff(required_site_cols, names(Site_Info_data))
      if (length(missing_site_cols) > 0)
        stop(paste0("Site Info is missing required columns: ", paste(missing_site_cols, collapse = ", ")))
      
      # Check required columns in Chr_Info
      required_chr_cols <- c("Chr", "Start", "End")
      missing_chr_cols <- setdiff(required_chr_cols, names(Chr_Info_data))
      if (length(missing_chr_cols) > 0)
        stop(paste0("Chromosome Info is missing required columns: ", paste(missing_chr_cols, collapse = ", ")))
      
      # Type and NA checks
      if (!is.numeric(Site_Info_data$Chr) && !is.integer(Site_Info_data$Chr))
        stop("'Chr' in Site Info must be numeric or integer.")
      if (any(is.na(Site_Info_data$Chr)))
        stop("'Chr' in Site Info contains missing values.")
      
      if (!is.numeric(Chr_Info_data$Start) || !is.numeric(Chr_Info_data$End))
        stop("'Start' and 'End' in Chromosome Info must be numeric.")
      if (any(is.na(Chr_Info_data$Start)) || any(is.na(Chr_Info_data$End)))
        stop("'Start' or 'End' in Chromosome Info contains missing values.")
      
      if (nrow(Site_Info_data) == 0)
        stop("Site Info is empty.")
      if (nrow(Chr_Info_data) == 0)
        stop("Chromosome Info is empty.")
      
      # All checks passed, continue to analysis
      shinyjs::show("SNPdensityStatus")
      progressVal = reactiveVal(NULL)
      
      Chr_Info_data$Length = Chr_Info_data$End - Chr_Info_data$Start
      
      window_size = as.numeric(input$WindowSize0)*1000
      SNPdensity = density_analysis(Site_Info_data, Chr_Info_data, window_size)
      SNPdensityresult1(SNPdensity)
      
      space_chr = round(Chr_Info_data[, "Length"] / as.numeric(table(Site_Info_data$Chr)), 2) # average spacing bp/SNPs
      space_total = round(sum(Chr_Info_data[, "Length"]) / length(Site_Info_data$Chr), 2)
      
      snp_chr = round(as.numeric(table(Site_Info_data$Chr)) / Chr_Info_data[, "Length"] * 1000, 4) # average SNPs/1000bp
      snp_total = round(length(Site_Info_data$Chr) / sum(Chr_Info_data[, "Length"]) * 1000, 4)
      
      SNPdensityresults2_df = data.frame(
        "Chr" = Chr_Info_data$Chr,
        "bp_over_SNPs" = space_chr,
        "SNPs_over_1000bp" = snp_chr
      )
      SNPdensityresults2_df[nrow(SNPdensityresults2_df) + 1, 1:3] = c("Total", space_total, snp_total)
      SNPdensityresults2(SNPdensityresults2_df)
      
      shinyjs::hide("SNPdensityStatus")
      SNPdensity1("SNP Density Plot")
      SNPdensity2("SNP Density across All Chromosome")
      guide_SNPdensity("The SNP density analysis is complete.")
      showNotification("Run Successfully", type = "message")
    }, error = function(e) {
      shinyjs::hide("SNPdensityStatus")
      showNotification(paste("Fail: ", e$message), type = "error")
      return(NULL)
    })
  })
  
  observeEvent(input$resetSNPdensity, {
    SNPdensity1("")
    SNPdensity2("")
    SNPdensityresult1(NULL)
    SNPdensityresults2(NULL)
    densityplot(NULL)
    showNotification("Data have been reset.")
    output$Site_Info0 = renderUI({
      fileInput("Site_Info0", "Site Info.* (required)", multiple = F, accept = c(".rds"))
    })
    output$Chr_Info0 = renderUI({
      fileInput("Chr_Info0", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
    })
    guide_SNPdensity("Need to upload the ▶️ Site Info file (in RDS) and ▶️ Chromosome Info file (in CSV). \nPlease select the optimal window size and step, then click the 'Summary' button.")
    })
  
  output$SNPdensity_result1 = renderText({
    req(SNPdensityresults2(), Chr_Info(), Site_Info())
    data = SNPdensityresults2()
    Chr_Info = Chr_Info()
    Site_Info = Site_Info()
    
    if (SNPdensity1() == "SNP Density Plot") {
      last_row = nrow(data)
      if (last_row == 0) {
        return("No data available to calculate SNP density.")
      }
      text = paste0("--- Reference Genome ---", "\n",
                    "Number of chromosomes: ", length(Chr_Info[,1]), "\n",
                    "Total length (bp): ", sum(Chr_Info[,3]), "\n",
                    "--- SNP Density ---", "\n",
                    "Number of SNPs: ", length(Site_Info[,1]), "\n",
                    "Average SNP spacing: ", data[last_row, 2], " bp", "\n",
                    "Average number of SNPs per 1000bp: ", data[last_row, 3], " SNPs", "\n", 
                    "SNP spacing across chromosomes, ", data[1,1], " to ", length(Chr_Info[,1]), ": ",
                    paste(as.numeric(data[1:last_row-1, 2]), collapse = ", "), "\n"
                    )
      pre_results = pre_results()
      pre_results[[2]] = "## Data Input"
      pre_results[[15]] = paste0("### SNP Density", "\n", text)
      pre_results(pre_results)
      paste0(text)
    }
  })
  
  
  output$SNPdensityplot = renderPlot({
    req(Chr_Info(), SNPdensityresult1())
    if (SNPdensity1() == "SNP Density Plot") {
      Chr_Info = Chr_Info()
      Chr_Info$Length = Chr_Info$End - Chr_Info$Start
      window_data = SNPdensityresult1()
      
      MB = seq(0, 300, by = 20)
      linewidth = c(32,32,32,32,32,29,25,20,19,17,
                    15,14,13,12,12,11,10,10,9,9,
                    9,8,8,8,8,7,7,7,7,7,
                    6,6,6,6,6,rep(5,10))
      nchr = length(unique(Chr_Info$Chr))
      
      plot = ggplot() +
        geom_bar(data = Chr_Info, aes(x = Chr, y = Length), stat = "identity", fill = "grey95", width = 0.5, alpha = 0.9) +
        scale_x_discrete(expand = c(0, 0.3)) +
        scale_y_continuous("Position (Mb)",
                           breaks = MB*10^6,
                           labels = MB,
                           expand = c(0, 0)) +
        labs(x = "") +
        theme_classic() +
        theme(axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 16, vjust = 2),
              axis.text.x = element_text(size = 12, angle = 60, vjust = 0.5),
              axis.text.y = element_text(size = 12),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(linewidth = 0.7),
              axis.ticks.y = element_line(linewidth = 0.7))
      
      title = paste0("Number of SNPs within ", input$WindowSize0, "kb window size")
      window_data$Count[window_data$Count == 0] = NA
      
      plot = plot +
        geom_rect(data = window_data, aes(xmin = Chr, xmax = Chr, ymin = Start, ymax = End, color = Count),
                  alpha = 0.5, linewidth = linewidth[nchr]) +
        scale_color_gradientn(name = title, colors = c("#7BB662", "#FFD301", "#E03C32"), na.value = "grey") +
        guides(color = guide_colorbar(barwidth = 15, barheight = 1, title.position = "top", title.vjust = 1,
                                      label.theme = element_text(size = 12))) +
        theme(legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.key.size = unit(1, "cm"),
              legend.position = "bottom")
      densityplot(plot)
      plot
    }
  })
  
  output$download_SNPdensity_plot = renderUI({
    if (SNPdensity1() == "SNP Density Plot") {
      actionButton(
        inputId = "show_download_SNPdensity_plot", 
        label = tagList(shiny::icon("download"), "Download Plot"), 
        class = "AI1-action-button"
      )
    }
  })
  
  observeEvent(input$show_download_SNPdensity_plot, {
    showModal(
      modalDialog(
        title = "Download Plot Settings",
        fluidRow(
          column(6,
                 numericInput("dl_snpdensity_width", "Width", value = 12, min = 3, max = 30, step = 0.5),
                 numericInput("dl_snpdensity_height", "Height", value = 5, min = 3, max = 15, step = 0.5),
                 selectInput("dl_snpdensity_unit", "Unit", choices = c("inches" = "in", "cm" = "cm"), selected = "in")
          ),
          column(6,
                 selectInput("dl_snpdensity_format", "File format", choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg"), selected = "pdf"),
                 conditionalPanel(
                   condition = "input.dl_snpdensity_format == 'png' || input.dl_snpdensity_format == 'jpeg'",
                   numericInput("dl_snpdensity_dpi", "Resolution (DPI)", value = 300, min = 72, max = 600, step = 10)
                 )
          )
        ),
        footer = tagList(
          downloadButton("DSNPdensity_plot", "Download"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  output$DSNPdensity_plot = downloadHandler(
    filename = function() {
      ext = input$dl_snpdensity_format
      paste0("SNP_Density_Plot-", input$WindowSize0, "kb.", ext)
    },
    content = function(file) {
      shinyjs::show("SNPdensityStatus")
      req(densityplot())
      
      width = input$dl_snpdensity_width
      height = input$dl_snpdensity_height
      units = input$dl_snpdensity_unit
      device = input$dl_snpdensity_format
      dpi = input$dl_snpdensity_dpi
      
      if (device == "pdf") {
        ggsave(file, plot = densityplot(), device = "pdf", width = width, height = height, units = units)
      } else if (device == "jpeg") {
        ggsave(file, plot = densityplot(), device = "jpeg", width = width, height = height, units = units, dpi = dpi)
      } else {
        ggsave(file, plot = densityplot(), device = "png", width = width, height = height, units = units, dpi = dpi)
      }
      shinyjs::hide("SNPdensityStatus")
      removeModal()
    }
  )
  
  
  output$SNPdensity_result2 = DT::renderDataTable({
    req(SNPdensityresults2())
    DT::datatable(SNPdensityresults2(), options = list(pageLength = 10))
  })
  
  output$download_SNPdensity_result2 = renderUI({
    if (SNPdensity1() == "SNP Density Plot") {
      downloadButton("DSNPdensity_result2", "Download Window Data")
    }
  })
  
  output$DSNPdensity_result2 = downloadHandler(
    filename = function() {
      paste0("SNP_Density.csv")
    },
    content = function(file) {
      shinyjs::show("SNPdensityStatus")
      write.csv(SNPdensityresults2(), file, row.names = FALSE)
      shinyjs::hide("SNPdensityStatus")
    }
  )
  
  output$guide_SNPdensity = renderUI({ div(class = "guide-text-block", guide_SNPdensity()) })
  output$SNPdensity1 = renderText({ SNPdensity1() })
  output$SNPdensity2 = renderText({ SNPdensity2() })
}