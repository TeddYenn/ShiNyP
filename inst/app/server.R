##### Server #####
server = function(input, output, session) {
  options(shiny.maxRequestSize = 10^5*1024^3) # Maximum size: 10^5 GB
  shinyjs::hide("inputStatus")
  shinyjs::hide("samplemissingStatus")
  shinyjs::hide("samplehStatus")
  shinyjs::hide("missingStatus")
  shinyjs::hide("mafStatus")
  shinyjs::hide("hStatus")
  shinyjs::hide("giStatus")
  shinyjs::hide("glStatus")
  shinyjs::hide("input2Status")
  shinyjs::hide("PCAStatus")
  shinyjs::hide("DAPCStatus")
  shinyjs::hide("UPGMAStatus")
  shinyjs::hide("NJStatus")
  shinyjs::hide("KinshipStatus")
  shinyjs::hide("GDStatus")
  shinyjs::hide("CircosStatus")
  shinyjs::hide("GTStatus")
  shinyjs::hide("AMOVAStatus")
  shinyjs::hide("pcadaptStatus")
  shinyjs::hide("OutFLANKStatus")
  shinyjs::hide("IBSStatus")
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
  
  ##### Home Page #####
  observeEvent(input$guide_button, {
    shinyjs::runjs("window.open('https://teddy-tw.notion.site/ShiNyP-68b21f9bd4d94152b85feda55452d2a3?pvs=4', '_blank');")
  })
  
  observeEvent(input$case_button, {
    shinyjs::runjs("window.open('https://www.example.com/case-studies', '_blank');")
  })
  ##### Page 1: Data Input #####
  data = reactiveVal(NULL)
  vcfData = reactiveVal(NULL)
  fileName = reactiveVal(NULL)
  fileInfo = reactiveVal(NULL)
  guide_input = reactiveVal("Waiting for upload VCF data... \nYou can upload: \n● VCF file from plink (recommended) or \n● VCF or gzipped VCF (vcf.gz) file from VCFtools or \n● Post-QC VCF file (in rds) from ShiNyP. \nAfter seeing 'Upload complete' on the progress bar, you can click the 'input' button.")
  uploadStatus = reactiveVal("")
  
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      data(input$file1)
      guide_input("Now, waiting for input VCF data... (please click 'input' button)")
    }
  })
  
  output$Uploaddata = renderUI({
    fileInput("file1", "", multiple = TRUE, accept = c(".vcf", ".gz", ".rds"))
  })
  
  observeEvent(input$resetInput, {
    output$Uploaddata = renderUI({
      fileInput("file1", "", multiple = TRUE, accept = c(".vcf", ".gz", ".rds"))
    })
    vcfData(NULL)
    uploadStatus("")
    showNotification("Data have been reset.")
    guide_input("Waiting for upload VCF data... \nYou can upload: \n● VCF file from plink (recommended) or \n● VCF or gzipped VCF (vcf.gz) file from VCFtools or \n● Post-QC VCF file (in rds) from ShiNyP. \nAfter seeing 'Upload complete' on the progress bar, you can click the 'input' button.")
  })
  
  observeEvent(input$Inputdata, {
    if (guide_input() == "Now, waiting for input VCF data... (please click 'input' button)"){
      shinyjs::show("inputStatus")
      req(input$file1)
      req(input$presample)
      if (grepl("\\.vcf$", input$file1$name)) {
        vcf = fread(input$file1$datapath, header = TRUE, sep = "\t")
      } else if (grepl("\\.gz$", input$file1$name)) {
        vcf = fread(input$file1$datapath, header = TRUE, sep = "\t")
      } else if (grepl("\\.rds$", input$file1$name)) {
        vcf = readRDS(input$file1$datapath)
      }
      names(vcf) = gsub("(.+?)_\\1", "\\1", names(vcf))
      vcf[ID == ".", ID := paste(`#CHROM`, POS, sep = ":")]
      vcf_process = function(x) { paste0(substr(x, 1, 1), "/", substr(x, 3, 3)) }
      vcf[, (names(vcf)[10:ncol(vcf)]) := lapply(.SD, vcf_process), .SDcols = names(vcf)[10:ncol(vcf)]]
      
      vcfData(as.data.frame(vcf))
      output$contents = DT::renderDataTable({
        selected_vcfData = vcfData()[, c(1:(input$presample+9))]
        DT::datatable(selected_vcfData)
      })
      guide_input("VCF data has been input! (•‿•) ")
      uploadStatus("Preview VCF data")
      fileName(tools::file_path_sans_ext(input$file1$name))
      shinyjs::hide("inputStatus")
      pre_results = pre_results()
      pre_results[[2]] = "# Data Input"
      pre_results[[4]] = paste0("Number of samples: ", dim(vcfData())[2]-9) # Data Input
      pre_results[[5]] = paste0("Number of SNPs: ", dim(vcfData())[1]) # Data Input
      pre_results(pre_results)
    }
  })
  
  output$guide_input = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_input())
  })
  
  output$uploadStatus = renderText({
    uploadStatus()
  })
  
  output$fileInfo = renderText({
    req(vcfData())
    if (guide_input() == "VCF data has been input! (•‿•) ") {
      paste0("File name: ", fileName(), "\n",
             "Number of samples: ", dim(vcfData())[2]-9, "\n",
             "Number of SNPs: ", dim(vcfData())[1], "\n",
             "Type: VCF")
    }
  })
  ##### Page 2-1: Sample QC #####
  df1 = reactiveVal(NULL)
  guide_sampleQC = reactiveVal("You have to obtain the summary statistics first! \nScroll down to check the results. \nThen, adjust the thresholds and click the 'Sample QC by Thresholds' button.")
  sampleQCstatus = reactiveVal("")
  QCData = reactiveVal(NULL)
  SampleQCData = reactiveVal(NULL)
  samplemissingrate = reactiveVal(NULL)
  samplemissing1 = reactiveVal(NULL)
  samplemissing2 = reactiveVal(NULL)
  samplemissing3 = reactiveVal(NULL)
  sampleh = reactiveVal(NULL)
  sampleh1 = reactiveVal(NULL)
  sampleh2 = reactiveVal(NULL)
  sampleh3 = reactiveVal(NULL)
  progressVal = reactiveVal(0)
  
  output$fileSelection1 = renderUI({
    choices = c(
      "Input VCF file" = "vcfData",
      "SNP post-QC file" = "QCData"
    )
    selectInput("selectedFile", "Selecte a dataset for QC:", choices)
  })
  
  output$SampleQCfileInfo = renderText({
    req(vcfData())
    if (sampleQCstatus() != "Post-QC VCF file"){
      if (input$selectedFile == "vcfData") {
        paste0("Number of samples: ", dim(vcfData())[2]-9, "\n",
               "Number of SNPs: ", dim(vcfData())[1], "\n",
               "Type: VCF")
      } else if (is.null(QCData())){
        paste("No available for 'SNP post-QC file'!")
      } else {
        req(QCData())
        paste0("Number of samples: ", dim(QCData())[2]-9, "\n",
               "Number of SNPs: ", dim(QCData())[1], "\n",
               "Type: VCF")
      }
    }
  })
  
  observeEvent(input$sampleQCmissing, {
    req(input$selectedFile)
    
    if (input$selectedFile == "vcfData") {
      req(vcfData())
      shinyjs::show("samplemissingStatus")
      data = vcfData()
    } else if (is.null(QCData())){
      samplemissing1("No available for 'SNP post-QC file'!")
      return()
    } else {
      req(QCData())
      shinyjs::show("samplemissingStatus")
      data = QCData()
    }
    if (is.null(df1())) {
      withProgress(message = 'Converting data...', value = 0, {
        df1(vcf2df(data)) 
      })
    } else {
      req(df1())
    }
    rate = rowSums(is.na(df1())) / dim(df1())[2]
    samplemissingrate(rate)
    samplemissing1("Summary of sample missing rate")
    samplemissing2(stat2summary(samplemissingrate()))
    shinyjs::hide("samplemissingStatus")
  })
  
  observeEvent(input$sampleQCH, {
    req(input$selectedFile)
    
    if (input$selectedFile == "vcfData") {
      req(vcfData())
      shinyjs::show("samplehStatus")
      data = vcfData()
    } else if (is.null(QCData())){
      sampleh1("No available for 'SNP post-QC file'!")
      return()
    } else {
      req(QCData())
      shinyjs::show("samplehStatus")
      data = QCData()
    }
    
    if (is.null(df1())) {
      req(data)
      withProgress(message = 'Converting data...', value = 0, {
        df1(vcf2df(data)) 
      })
    } else {
      req(df1())
    }
    rate = rowSums(df1() == 1, na.rm = TRUE)/(dim(df1())[2]-rowSums(is.na(df1())))
    sampleh(rate)
    sampleh1("Summary of sample heterozygosity rate")
    sampleh2(stat2summary(sampleh()))
    shinyjs::hide("samplehStatus")
  })
  
  observeEvent(input$sampleQC, {
    if (input$selectedFile == "vcfData") {
      data = vcfData()
    } else {
      data = QCData()
    }
    req(samplemissingrate())
    req(sampleh())
    rm.sample = union(which(samplemissingrate() > input$sampleThrMR), which(sampleh() > input$sampleThrH))
    if (length(rm.sample)>0){
      rm.sample = as.numeric(rm.sample)+9
      QCData = data[, -rm.sample]
    } else{
      QCData = data
    }
    QCData(QCData)
    SampleQCData(QCData)
    guide_sampleQC("Sample quality control is completed. \nYou will receive the post-QC VCF file (in rds) when you download the file. \nYou can conduct SNP QC immediately or restart the program and then input the post-QC VCF file.")
    sampleQCstatus("Post-QC VCF file")
    pre_results = pre_results()
    pre_results[[6]] = "# Data QC"
    pre_results[[7]] = "Sample QC"
    pre_results[[8]] = paste0("Removed samples with ", "missing rate > ", input$sampleThrMR ," and heterozygosity rate > ", input$sampleThrH)
    pre_results[[9]] = paste0("Number of samples: ", dim(SampleQCData())[2]-9)
    pre_results[[10]] = paste0("Number of SNPs: ", dim(SampleQCData())[1])
    pre_results(pre_results)
  })
  
  observeEvent(input$resetsampleQC, {
    SampleQCData(NULL)
    QCData(NULL)
    sampleQCstatus("")
    showNotification("Data have been reset.")
    guide_sampleQC("You have to obtain the summary statistics first! \nScroll down to check the results. \nThen, adjust the thresholds and click the 'Sample QC by Thresholds' button.")
  })
  
  output$sampleQCresult = renderText({
    req(SampleQCData())
    if (sampleQCstatus() == "Post-QC VCF file") {
      paste("Removed samples with ", "missing rate > ", input$sampleThrMR ," and heterozygosity rate > ", input$sampleThrH, "\n",
            "File name: SampleQC_vcf_", fileName(), "\n",
            "Number of samples: ", dim(SampleQCData())[2]-9, "\n",
            "Number of SNPs: ", dim(SampleQCData())[1], 
            sep = "")
    }
  })
  
  output$download_sampleQC = renderUI({
    if (sampleQCstatus() == "Post-QC VCF file") {
      downloadButton("DsampleQC", "Download Post-QC File")
    }
  })
  
  output$DsampleQC = downloadHandler(
    filename = function() {
      paste("SampleQC_vcf_", fileName(), ".rds", sep = "")},
    content = function(file) {
      saveRDS(QCData(), file)
    })
  
  output$sampleQCstatus = renderText({
    sampleQCstatus()
  })
  
  output$guide_sampleQC = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_sampleQC())
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
  
  ##### Page 2-2: SNP QC #####
  df2 = reactiveVal(NULL)
  guide_QC = reactiveVal("You have to obtain the summary statistics first! \nScroll down to check the results. \nThen, adjust the thresholds and click the 'SNP QC by Thresholds' button.")
  SNPQCstatus = reactiveVal("")
  SNPQCData = reactiveVal(NULL)
  missingrate = reactiveVal(NULL)
  missing1 = reactiveVal(NULL)
  missing2 = reactiveVal(NULL)
  missing3 = reactiveVal(NULL)
  maf = reactiveVal(NULL)
  maf1 = reactiveVal(NULL)
  maf2 = reactiveVal(NULL)
  maf3 = reactiveVal(NULL)
  h = reactiveVal(NULL)
  h1 = reactiveVal(NULL)
  h2 = reactiveVal(NULL)
  h3 = reactiveVal(NULL)
  progressVal = reactiveVal(0)
  
  output$fileSelection2 = renderUI({
    choices = c(
      "Sample post-QC file" = "QCData",
      "Input VCF file" = "vcfData"
    )
    selectInput("selectedFile2", "Selecte a dataset for QC:", choices)
  })
  
  output$SNPQCfileInfo = renderText({
    req(vcfData())
    if (SNPQCstatus() != "Post-QC VCF file"){
      if (input$selectedFile2 == "vcfData") {
        paste0("Number of samples: ", dim(vcfData())[2]-9, "\n",
               "Number of SNPs: ", dim(vcfData())[1],"\n",
               "Type: VCF")
      } else if (is.null(QCData())){
        paste("No available for 'SNP post-QC file'!")
      } else {
        req(QCData())
        paste0("Number of samples: ", dim(QCData())[2]-9, "\n",
               "Number of SNPs: ", dim(QCData())[1], "\n",
               "Type: VCF")
      }
    }
  })
  
  observeEvent(input$QCmissing, {
    req(input$selectedFile2)
    
    if (input$selectedFile2 == "vcfData") {
      req(vcfData())
      shinyjs::show("missingStatus")
      data = vcfData()
    } else if (is.null(QCData())){
      missing1("No available for 'Sample post-QC file'!")
      return()
    } else {
      req(QCData())
      shinyjs::show("missingStatus")
      data = QCData()
    }
    if (is.null(df2())) {
      req(data)
      withProgress(message = 'Converting data...', value = 0, {
        df2(vcf2df(data))
      })
    } else {
      req(df2())
    }
    rate = colSums(is.na(df2()))/(dim(df2())[1])
    missingrate(rate)
    missing1("Summary of SNP missing rate")
    missing2(stat2summary(missingrate()))
    shinyjs::hide("missingStatus")
  })
  
  observeEvent(input$QCMAF, {
    req(input$selectedFile2)
    
    if (input$selectedFile2 == "vcfData") {
      req(vcfData())
      shinyjs::show("mafStatus")
      data = vcfData()
    } else if (is.null(QCData())){
      maf1("No available for 'Sample post-QC file'!")
      return()
    } else {
      req(QCData())
      shinyjs::show("mafStatus")
      data = QCData()
    }
    if (is.null(df2())) {
      req(data)
      withProgress(message = 'Converting data...', value = 0, {
        df2(vcf2df(data))
      })
    } else {
      req(df2())
    }
    rate = (colSums(df2() == 1, na.rm = TRUE) + 2*colSums(df2() == 2, na.rm = TRUE))/(2*(dim(df2())[1]-colSums(is.na(df2()))))
    rate = pmin(rate, 1 - rate)
    maf(rate)
    maf1("Summary of SNP minor allele frequency")
    maf2(stat2summary(maf()))
    shinyjs::hide("mafStatus")
  })
  
  observeEvent(input$QCH, {
    req(input$selectedFile2)
    
    if (input$selectedFile2 == "vcfData") {
      req(vcfData())
      shinyjs::show("hStatus")
      data = vcfData()
    } else if (is.null(QCData())){
      h1("No available for 'Sample post-QC file'!")
      return()
    } else {
      req(QCData())
      shinyjs::show("hStatus")
      data = QCData()
    }
    if (is.null(df2())) {
      req(data)
      withProgress(message = 'Converting data...', value = 0, {
        df2(vcf2df(data))
      })
    } else {
      req(df2())
    }
    rate = colSums(df2() == 1, na.rm = TRUE)/(dim(df2())[1]-colSums(is.na(df2())))
    h(rate)
    h1("Summary of SNP heterozygosity rate")
    h2(stat2summary(h()))
    shinyjs::hide("hStatus")
  })
  
  observeEvent(input$QC, {
    if (input$selectedFile2 == "vcfData") {
      data = vcfData()
    } else {
      data = QCData()
    }
    req(missingrate())
    req(maf())
    req(h())
    rm.loc = union(union(union(which(missingrate() > input$ThrMR), 
                               which(maf() < input$ThrMAF)), 
                         which(h() > input$ThrH)), 
                   which(h() < input$ThrH0))
    if (length(rm.loc)>0){
      rm.sample = as.numeric(rm.loc)
      QCData = data[-as.numeric(rm.loc), ]
    } else{
      QCData = data
    }
    QCData(QCData)
    SNPQCData(QCData)
    guide_QC("SNP quality control is completed. \nYou will receive the post-QC VCF file (in rds) when you download the file. \nYou can conduct sample QC immediately or restart the program and then input the post-QC VCF file.")
    SNPQCstatus("Post-QC VCF file")
    pre_results = pre_results()
    pre_results[[11]] = "SNP QC"
    pre_results[[12]] = paste0("Removed SNPs with ", "missing rate > ", input$ThrMR ,", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", and heterozygosity rate > ", input$ThrH)
    pre_results[[13]] = paste0("Number of samples: ", dim(SNPQCData())[2]-9)
    pre_results[[14]] = paste0("Number of SNPs: ", dim(SNPQCData())[1])
    pre_results(pre_results)
  })
  
  observeEvent(input$resetSNPQC, {
    SNPQCData(NULL)
    QCData(NULL)
    SNPQCstatus("")
    showNotification("Data have been reset.")
    guide_QC("You have to obtain the summary statistics first! \nScroll down to check the results. \nThen, adjust the thresholds and click the 'SNP QC by Thresholds' button.")
  })
  
  output$QCresult = renderText({
    req(SNPQCData())
    if (SNPQCstatus() == "Post-QC VCF file") {
      paste("Removed SNPs with ", "missing rate > ", input$ThrMR ,", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", and heterozygosity rate > ", input$ThrH, "\n",
            "File name: SNPQC_vcf_", fileName(), "\n",
            "Number of samples: ", dim(SNPQCData())[2]-9, "\n",
            "Number of SNPs: ", dim(SNPQCData())[1], 
            sep = "")
    }
  })
  
  output$download_snpQC = renderUI({
    if (SNPQCstatus() == "Post-QC VCF file") {
      downloadButton("DsnpQC", "Download Post-QC File")
    }
  })
  
  output$DsnpQC = downloadHandler(
    filename = function() {
      paste("SNPQC_vcf_", fileName(), ".rds", sep = "")},
    content = function(file) {
      saveRDS(QCData(), file)
    })
  
  output$SNPQCstatus = renderText({ SNPQCstatus() })
  
  output$guide_QC = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_QC())
  })
  
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
  
  ##### Page 3-1: Data Conversion #####
  df = reactiveVal(NULL)
  gi = reactiveVal(NULL)
  gl = reactiveVal(NULL)
  groupInfo1 = reactiveVal(NULL)
  guide_C = reactiveVal("You have to convert the data step by step. \nWaiting for convert VCF to data.frame...")
  Site_Info = reactiveVal("")
  Cstatus1 = reactiveVal("")
  Cstatus2 = reactiveVal("")
  Cstatus3 = reactiveVal("")
  CTable1 = reactiveVal(NULL)
  CTable2 = reactiveVal(NULL)
  CTable3 = reactiveVal(NULL)
  progressVal = reactiveVal(0)
  
  output$fileSelection3 = renderUI({
    if (!is.null(SNPQCData()) && !is.null(SampleQCData())){
      choices = c(
        "SNP+Sample post-QC VCF file" = "SNPQCData",
        "Sample post-QC VCF file" = "SampleQCData",
        "Input VCF file" = "vcfData"
      )
    }else if (is.null(SNPQCData()) && !is.null(SampleQCData())){
      choices = c(
        "Sample post-QC VCF file" = "SampleQCData",
        "Input VCF file" = "vcfData"
      )
    }else if (!is.null(SNPQCData()) && is.null(SampleQCData())){
      choices = c(
        "SNP post-QC VCF file" = "SNPQCData",
        "Input VCF file" = "vcfData"
      )
    }else{
      choices = c(
        "Input VCF file" = "vcfData"
      )
    }
    selectInput("FileforDataConv", "Dataset for conversion:", choices)
  })
  
  output$CfileInfo = renderText({
    req(vcfData())
    if (input$FileforDataConv == "vcfData") {
      paste0("Number of samples: ", dim(vcfData())[2]-9, "\n",
             "Number of SNPs: ", dim(vcfData())[1], "\n",
             "Type: VCF")
    } else if (input$FileforDataConv == "SNPQCData"){
      req(SNPQCData())
      paste0("Number of samples: ", dim(SNPQCData())[2]-9, "\n",
             "Number of SNPs: ", dim(SNPQCData())[1], "\n",
             "Type: VCF")
    } else {
      req(SampleQCData())
      paste0("Number of samples: ", dim(SampleQCData())[2]-9, "\n",
             "Number of SNPs: ", dim(SampleQCData())[1], "\n",
             "Type: VCF")
    }
  })
  
  observeEvent(input$Cvcf2df, {
    if (input$FileforDataConv == "vcfData") {
      req(vcfData())
      guide_C("Converting data...")
      withProgress(message = 'Converting data...', value = 0, {
        df = vcf2df(vcfData())
        Site_Info = vcf2Site_Info(vcfData())
      })
    } else if (input$FileforDataConv == "SampleQCData"){
      req(SampleQCData())  # df data -> SampleQCData
      withProgress(message = 'Converting data...', value = 0, {
        df = vcf2df(SampleQCData())
        Site_Info = vcf2Site_Info(SampleQCData())
      })
    } else if (input$FileforDataConv == "SNPQCData"){
      req(SNPQCData())   # df data -> SNPQCData
      withProgress(message = 'Converting data...', value = 0, {
        df = vcf2df(SNPQCData())
        Site_Info = vcf2Site_Info(SNPQCData())
      })
    }
    df(df)
    Site_Info(Site_Info)
    Cstatus1("The conversion is completed (VCF to data.frame)!")
    guide_C("The data is converted to df. \nWaiting for convert df to genind...")
    pre_results = pre_results()
    pre_results[[17]] = paste0("-> Number of samples: ", dim(df())[1]) # Data Conversion
    pre_results[[18]] = paste0("-> Number of SNPs: ", dim(df())[2])
    pre_results(pre_results)
  })
  
  output$download_df = renderUI({
    if (Cstatus1() == "The conversion is completed (VCF to data.frame)!") {
      downloadButton("Ddf", "Download data.frame File")
    }
  })
  
  output$Ddf = downloadHandler(
    filename = function() {
      paste("df_", fileName(), ".rds", sep = "")},
    content = function(file) {
      saveRDS(df(), file)
    })
  
  output$download_snpInfo = renderUI({
    if (Cstatus1() == "The conversion is completed (VCF to data.frame)!") {
      downloadButton("DsnpInfo", "Download Site Info.")
    }
  })
  
  output$DsnpInfo = downloadHandler(
    filename = function() {
      paste("Site_Info_", fileName(), ".rds", sep = "")},
    content = function(file) {
      saveRDS(Site_Info(), file)
    })
  
  output$groupfile1 = renderUI({
    fileInput("groupfile1", "Group Info. (optional)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$groupfile1, {
    req(input$groupfile1)
    groupfile = read.csv(input$groupfile1$datapath)
    groupInfo1 = as.numeric(groupfile$Group)
    groupInfo1(groupInfo1)
  })
  
  observeEvent(input$Cdf2gi, {
    req(df())
    shinyjs::show("giStatus")
    df = df() %>% 
      mutate(across(everything(), ~ case_when(
        . == 0 ~ "11",
        . == 1 ~ "12",
        . == 2 ~ "22",
        TRUE ~ as.character(.)
      )))
    if (!is.null(groupInfo1())){
      req(groupInfo1())
      gi = df2genind(df, sep = NULL, ind.names = row.names(df()), ploidy = 2, ncode = 1, NA.char = NA, pop = groupInfo1())
    }else{
      gi = df2genind(df, sep = NULL, ind.names = row.names(df()), ploidy = 2, ncode = 1, NA.char = NA)
    }
    gi(gi)
    Cstatus2("The conversion is completed (data.frame to genind)!")
    shinyjs::hide("giStatus")
    guide_C("The data is converted to df and gi. \nWaiting for convert gi to genlight...")
  })
  
  output$download_gi = renderUI({
    if (Cstatus2() == "The conversion is completed (data.frame to genind)!") {
      downloadButton("Dgi", "Download genind File")
    }
  })
  
  output$Dgi = downloadHandler(
    filename = function() {
      paste("genind_", fileName(), ".rds", sep = "")},
    content = function(file) {
      saveRDS(gi(), file)
    })
  
  observeEvent(input$Cgi2gl, {
    req(gi())
    shinyjs::show("glStatus")
    gl = gi2gl(gi(), verbose = 0)
    gl(gl)
    Cstatus3("The conversion is completed (genind to genlight)!")
    shinyjs::hide("glStatus")
    guide_C("The data is converted to data.frame, genind, and genlight. \nEnjoy the downstream analysis! (•‿•) ")
  })
  
  output$download_gl = renderUI({
    if (Cstatus3() == "The conversion is completed (genind to genlight)!") {
      downloadButton("Dgl", "Download genlight File")
    }
  })
  
  output$Dgl = downloadHandler(
    filename = function() {
      paste("genlight_", fileName(), ".rds", sep = "")},
    content = function(file) {
      saveRDS(gl(), file)
    })
  
  output$guide_C = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_C())
  })
  
  output$Cstatus1 = renderText({ Cstatus1() })
  
  output$CTable1 = renderText({
    req(df())
    if (Cstatus1() == "The conversion is completed (VCF to data.frame)!"){
      paste0("Status: converted", "\n",
             "File name: df_", fileName(), "\n",
             "Number of samples: ", dim(df())[1], "\n",
             "Number of SNPs: ", dim(df())[2], "\n",
             "Type: ", class(df()), "\n",
             "Size: ", size2size(as.numeric(object.size(df()))))
    }
  })
  output$Cstatus2 = renderText({ Cstatus2() })
  
  output$CTable2 = renderText({
    req(gi())
    if (Cstatus2() == "The conversion is completed (data.frame to genind)!"){
      if (!is.null(groupInfo1())){
        group.info = "Added"
      }else{
        group.info = "NaN"
      }
      paste0("Status: converted", "\n",
             "File name: genind_", fileName(), "\n",
             "Number of samples: ", length(gi()@ploidy), "\n",
             "Number of SNPs: ", length(gi()@all.names), "\n",
             "Type: ", class(gi()), "\n",
             "Size: ", size2size(as.numeric(object.size(gi()))), "\n",
             "Group Info.: ", group.info
      )
    }
  })
  output$Cstatus3 = renderText({ Cstatus3() })
  
  output$CTable3 = renderText({
    req(gl())
    if (Cstatus3() == "The conversion is completed (genind to genlight)!"){
      paste0("Status: converted", "\n",
             "File name: genlight_", fileName(), "\n",
             "Number of samples: ", length(gl()@ind.names), "\n",
             "Number of SNPs: ", gl()@n.loc, "\n",
             "Type: ", class(gl()), "\n",
             "Size: ", size2size(as.numeric(object.size(gl()))))
    }
  })
  ##### Page 3-2: Input Converted Data #####
  groupInfo1 = reactiveVal(NULL)
  guide_input2 = reactiveVal("You can upload and input data.frame, genind, or genlight files (in rds) that have already been converted.")
  dfstatus = reactiveVal("")
  gistatus = reactiveVal("")
  glstatus = reactiveVal("")
  dfinfo = reactiveVal(NULL)
  giinfo = reactiveVal(NULL)
  glinfo = reactiveVal(NULL)
  
  output$uploaddf = renderUI({
    fileInput("input_df", "", multiple = F, accept = c(".rds"))
  })
  
  observeEvent(input$resetdf, {
    output$uploaddf = renderUI({
      fileInput("input_df", "", multiple = F, accept = c(".rds"))
    })
    dfstatus("")
    df(NULL)
    showNotification("Data have been reset.")
  })
  
  observeEvent(input$inputdf, {
    req(input$input_df)
    shinyjs::show("input2Status")
    df = readRDS(input$input_df$datapath)
    df(df)
    dfstatus("The data.frame file has been input!")
    shinyjs::hide("input2Status")
    pre_results = pre_results()
    pre_results[[17]] = paste0("-> Number of samples: ", dim(df())[1])
    pre_results[[18]] = paste0("-> Number of SNPs: ", dim(df())[2])
    pre_results(pre_results)
  })
  
  output$uploadgi = renderUI({
    fileInput("input_gi", "", multiple = F, accept = c(".rds"))
  })
  
  observeEvent(input$resetgi, {
    output$uploadgi = renderUI({
      fileInput("input_gi", "", multiple = F, accept = c(".rds"))
    })
    gistatus("")
    gi(NULL)
    showNotification("Data have been reset.")
  })
  
  observeEvent(input$inputgi, {
    req(input$input_gi)
    shinyjs::show("input2Status")
    gi = readRDS(input$input_gi$datapath)
    gi(gi)
    gistatus("The genind file has been input!")
    shinyjs::hide("input2Status")
    pre_results = pre_results()
    pre_results[[17]] = paste0("-> Number of samples: ", length(gi()@ploidy))
    pre_results[[18]] = paste0("-> Number of SNPs: ", length(gi()@all.names))
    pre_results(pre_results)
  })
  
  output$uploadgl = renderUI({
    fileInput("input_gl", "", multiple = F, accept = c(".rds"))
  })
  
  observeEvent(input$resetgl, {
    output$uploadgl = renderUI({
      fileInput("input_gl", "", multiple = F, accept = c(".rds"))
    })
    glstatus("")
    gl(NULL)
    showNotification("Data have been reset.")
  })
  
  observeEvent(input$inputgl, {
    req(input$input_gl)
    shinyjs::show("input2Status")
    gl = readRDS(input$input_gl$datapath)
    gl(gl)
    glstatus("The genlight file has been input!")
    shinyjs::hide("input2Status")
    pre_results = pre_results()
    pre_results[[17]] = paste0("-> Number of samples: ", length(gl()@ind.names))
    pre_results[[18]] = paste0("-> Number of SNPs: ", gl()@n.loc)
    pre_results(pre_results)
  })
  
  output$dfinfo = renderText({
    req(df())
    if (dfstatus() == "The data.frame file has been input!") {
      paste0("Status: input", "\n",
             "File name: ", tools::file_path_sans_ext(input$input_df$name), "\n",
             "Number of samples: ", dim(df())[1], "\n",
             "Number of SNPs: ", dim(df())[2], "\n",
             "Type: ", class(df()), "\n",
             "Size: ", size2size(as.numeric(object.size(df())))
      )
    }
  })
  
  output$giinfo = renderText({
    req(gi())
    if (!is.null(gi()@pop)){
      group.info = "Added"
    }else{
      group.info = "NaN"
    }
    if (gistatus() == "The genind file has been input!") {
      paste0("Status: input", "\n",
             "File name: ", tools::file_path_sans_ext(input$input_gi$name), "\n",
             "Number of samples: ", length(gi()@ploidy), "\n",
             "Number of SNPs: ", length(gi()@all.names), "\n",
             "Type: ", class(gi()), "\n",
             "Size: ", size2size(as.numeric(object.size(gi()))), "\n",
             "Group Info.: ", group.info
      )
    }
  })
  
  output$glinfo = renderText({
    req(gl())
    if (glstatus() == "The genlight file has been input!") {
      paste0("Status: input", "\n",
             "File name: ", tools::file_path_sans_ext(input$input_gl$name), "\n",
             "Number of samples: ", length(gl()@ind.names), "\n",
             "Number of SNPs: ", gl()@n.loc, "\n",
             "Type: ", class(gl()), "\n",
             "Size: ", size2size(as.numeric(object.size(gl())))
      )
    }
  })
  
  output$guide_input2 = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", 
        guide_input2())
  })
  output$dfstatus = renderText({ dfstatus() })
  output$gistatus = renderText({ gistatus() })
  output$glstatus = renderText({ glstatus() })
  ##### Page 4: Population Structure #####
  ##### PCA #####
  guide_PCA = reactiveVal("To run PCA, input data must be data.frame file. \nPlease click 'Run PCA' button")
  PCAfileInfo = reactiveVal("")
  PCAtitle1 = reactiveVal("")
  PCAtitle2 = reactiveVal("")
  groupfile4 = reactiveVal(NULL)
  PCA2Dplot = reactiveVal(NULL)
  PCAexpplot = reactiveVal(NULL)
  pca_result = reactiveVal(NULL)
  PCA_SD = reactiveVal(data.frame())
  PCA_Trans = reactiveVal(data.frame())
  PC_number = reactiveVal(NULL)
  
  output$fileSelection_PCA = renderUI({
    if (!is.null(df())){
      choices = c("data.frame file" = "df")
    } else {
      choices = ""
    }
    selectInput("FileforPCA", "Dataset for PCA:", choices)
  })
  
  observeEvent(input$runPCA, {
    req(input$FileforPCA)
    shinyjs::show("PCAStatus")
    pca_data = switch(input$FileforPCA, "df" = df())
    pca_data[] = lapply(pca_data, function(x) replace(x, is.na(x), 0))
    pca_result = prcomp(pca_data)
    pca_result(pca_result)  
    sd = pca_result$sdev
    total_variance = sum(sd)
    variance_percent = sd / total_variance * 100
    PCA_SD = data.frame(
      PC = paste0("PC", seq_along(sd)),
      Standard_deviations = sd,
      Proportion_of_explained_variance = variance_percent,
      Cumulative_proportion_of_explained_variance = c(0, cumsum(variance_percent)[1:length(sd)-1])
    )
    PCA_Trans(as.data.frame(pca_result$x)) 
    PCA_SD(PCA_SD)
    shinyjs::hide("PCAStatus")
    PCAtitle1("PCA scatter plot")
    PCAtitle2("PC explained variance plot")
    guide_PCA("The PCA is completed. \nPlease select the PCs for the X and Y axes of the 2D PCA plot.\nTry altering the number of PCs and observe the explained variance plot.")
    pre_results = pre_results()
    pre_results[[19]] = "# Population Structure"
    pre_results[[20]] = "Principal Component Analysis (PCA)"
    pre_results[[21]] = paste0("Top 10 PCs explained variance (%), PC 1 to PC 10: ", paste(round(PCA_SD$Proportion_of_explained_variance[1:10], 2), collapse = ", "))
    pre_results(pre_results)
  })
  
  observeEvent(input$resetPCA, {
    groupfile4(NULL)
    pca_result(NULL)
    PCA_SD(NULL)
    PCAtitle1("")
    PCAtitle2("")
    showNotification("Data have been reset.")
    guide_PCA("To run PCA, input data must be data.frame file. \nPlease click 'Run PCA' button.")
  })
  
  output$pc1 = renderUI({
    if (PCAtitle1() == "PCA scatter plot"){
      pca_result = pca_result()
      selectInput("pc1", "Select PC for X-axis:", choices = paste0("PC", seq_along(pca_result$sdev)), selected = "PC1")
    }
  })
  
  output$pc2 = renderUI({
    if (PCAtitle1() == "PCA scatter plot"){
      pca_result = pca_result()
      selectInput("pc2", "Select PC for Y-axis:", choices = paste0("PC", seq_along(pca_result$sdev)), selected = "PC2")
    }
  })
  
  output$groupfile4 = renderUI({
    if (PCAtitle1() == "PCA scatter plot"){
      fileInput("groupfile4", "Group or Core Sample Info.", multiple = F, accept = c(".csv"))
    }
  })
  
  output$PC = renderUI({
    if (PCAtitle1() == "PCA scatter plot"){
      pca_result = pca_result()
      sliderInput("PC", "Number of PCs:", min = 1, max = length(pca_result$sdev), value = length(pca_result$sdev)*0.1, step = 1) 
    }
  })
  
  observeEvent(input$groupfile4, {
    req(input$groupfile4)
    groupfile = read.csv(input$groupfile4$datapath)
    groupfile4 = as.numeric(groupfile[,2])
    groupfile4(groupfile4)
  })
  
  output$PCAfileInfo = renderText({
    req(df())
    paste("Number of samples: ", dim(df())[1], "\n",
          "Number of SNPs: ", dim(df())[2], "\n",
          "Type: ", class(df()),
          sep = "")
  })
  
  output$PCAplot = renderPlot({
    req(input$pc1, input$pc2, pca_result(), PCA_SD())
    if (PCAtitle1() == "PCA scatter plot") {
      PCA_SD = as.data.frame(PCA_SD())
      PCA_NewData = as.data.frame(pca_result()$x)
      A = sym(colnames(PCA_NewData)[as.numeric(str_extract(input$pc1, "\\d+"))])
      B = sym(colnames(PCA_NewData)[as.numeric(str_extract(input$pc2, "\\d+"))])
      
      if (is.null(groupfile4())){
        PCA2Dplot = ggplot(PCA_NewData, aes(x = !!A, y = !!B)) +
          geom_point(size = 4, alpha = 0.7, color = "#534b3b") +
          theme(legend.position = "none") 
      } else {
        PCA_NewData$Group = factor(groupfile4())
        colors = viridis::viridis(n = length(unique(PCA_NewData$Group)), alpha = 0.8, direction = -1, end = 0.95)
        PCA2Dplot = ggplot(PCA_NewData, aes(x = !!A, y = !!B, color = Group)) +
          geom_point(size = 4, alpha = 0.7) +
          scale_color_manual(values = colors) +
          theme(legend.position = "right")
      }
      PCA2Dplot = PCA2Dplot +
        labs(x = paste0(input$pc1, " (", round(PCA_SD[as.numeric(str_extract(input$pc1, "\\d+")),3],2), "%)"),
             y = paste0(input$pc2, " (", round(PCA_SD[as.numeric(str_extract(input$pc2, "\\d+")),3],2), "%)")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
      PCA2Dplot(PCA2Dplot)
      PCA2Dplot()
    }
  })
  
  output$PCAexpplot = renderPlot({
    req(input$PC, input$pc1, PCA_SD())
    if (PCAtitle2() == "PC explained variance plot") {
      PCA_SD = as.data.frame(PCA_SD())
      
      variance_data = data.frame(
        PCs = 1:input$PC,
        PV = PCA_SD[1:input$PC, 3], # Proportion of Variance
        CV = PCA_SD[1:input$PC, 4] # Cumulative Proportion
      )
      
      ylim.PV = c(0, max(variance_data$PV))
      ylim.CP = c(0, max(variance_data$CV))     
      b = diff(ylim.PV)/diff(ylim.CP)
      a = ylim.PV[1] - b*(ylim.CP)[1]
      
      PCAexpplot = ggplot(variance_data, aes(x = PCs, y = PV)) +
        geom_bar(stat = "identity", show.legend = FALSE, fill = "#8c7f63", width = 0.8) +
        geom_line(aes(y = a + CV*b), color = "#d95a25", lwd = 1.2) +
        geom_point(aes(y = a + CV*b), color = "#d95a25", size = 3, shape = 21, fill = "white") +
        xlab("Principal components axis")+
        scale_y_continuous("Proportion of variance (%)", limits = c(0, max(variance_data$PV)), sec.axis = sec_axis(~ (. - a)/b, name = "Cumulative proportion (%)")) +
        theme_classic() +
        theme(
          axis.title.x =   element_text(size = 18),
          axis.title.y.left = element_text(size = 18, color = "#534b3b"),
          axis.title.y.right = element_text(size = 18, color = "#b74c20"),
          axis.text.x = element_text(size = 14),
          axis.text.y.left = element_text(size = 13, color = "#534b3b"), 
          axis.text.y.right = element_text(size = 13, color = "#b74c20")) +
        theme(legend.position = "none")
      PCAexpplot(PCAexpplot)
      PCAexpplot()
    }
  })
  
  output$download_PCA_plot = renderUI({
    if (PCAtitle1() == "PCA scatter plot") {
      downloadButton("DPCAplot", "Download Plot")
    }
  })
  
  output$DPCAplot = downloadHandler(
    filename = function() {
      paste0("PCA_Scatter_Plot-", input$pc1, "_vs_",input$pc2, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 5.8)
      print(PCA2Dplot())
      dev.off()
    }
  )
  
  output$download_Expplot = renderUI({
    if (PCAtitle1() == "PCA scatter plot") {
      downloadButton("DExpplot", "Download Plot")
    }
  })
  
  output$DExpplot = downloadHandler(
    filename = function() {
      paste0("PCA_Explained_Variance_Plot-", "First_", input$PC, "_PCs", ".pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 5.8)
      print(PCAexpplot())
      dev.off()
    }
  )
  
  output$download_var = renderUI({
    if (PCAtitle1() == "PCA scatter plot") {
      downloadButton("Dvar", "Download Explained Variance")
    }
  })
  
  output$Dvar = downloadHandler(
    filename = "PCA_Explained_Variance.csv",
    content = function(file) {
      write.csv(PCA_SD(), file, row.names = FALSE)
    }
  )
  
  output$download_PCA_transformed = renderUI({
    if (PCAtitle1() == "PCA scatter plot") {
      downloadButton("DPCAtrans", "Download PCA Transformed Data")
    }
  })
  
  output$DPCAtrans = downloadHandler(
    filename = "PCA_Transformed_Data.csv",
    content = function(file) {
      write.csv(PCA_Trans(), file, row.names = FALSE)
    }
  )
  
  output$download_PCA_result = renderUI({
    if (PCAtitle1() == "PCA scatter plot") {
      downloadButton("DPCAres", "Download PCA All Results")
    }
  })
  
  output$DPCAres = downloadHandler(
    filename = "PCA_prcomp_Object.rds",
    content = function(file) {
      saveRDS(pca_result(), file)
    }
  )
  
  output$guide_PCA = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_PCA())
  })
  
  output$PCAtitle1 = renderText({
    PCAtitle1()
  })
  
  output$PCAtitle2 = renderText({
    PCAtitle2()
  })
  
  ##### DAPC #####
  guide_DAPC = reactiveVal("To run DAPC, input data must be genind file. \nPlease click 'Run STEP I' button first.")
  DAPCfileInfo = reactiveVal("")
  DAPC1 = reactiveVal(NULL)
  DAPC2 = reactiveVal(NULL)
  DAPCtitle1 = reactiveVal("")
  DAPCtitle2 = reactiveVal("")
  DAPCtitle3 = reactiveVal("")
  DAPCtitle4 = reactiveVal("")
  BICplot = reactiveVal(NULL)
  DAPC_pop = reactiveVal(NULL)
  DAPC_Trans = reactiveVal(NULL)
  
  output$fileSelection_DAPC = renderUI({
    if (!is.null(gi())){
      choices = c("genind file" = "gi")
      updateSliderInput(session, "npca", min = 1, max = length(gi()@ploidy), value = length(gi()@ploidy)*0.8, step = 1) # Number of PCs
    } else {
      choices = ""
    }
    selectInput("FileforDAPC", "Dataset for DAPC:", choices)
  })
  
  observeEvent(input$runDAPC1, {
    req(input$FileforDAPC)
    req(input$npca)
    req(input$Maxgrp)
    
    shinyjs::show("DAPCStatus")
    
    dapc_data = switch(input$FileforDAPC, "gi" = gi())
    
    DAPC1 = find.clusters(dapc_data, max.n = input$Maxgrp, n.pca = input$npca, scale = FALSE, choose = FALSE)
    DAPC1(DAPC1)
    
    lowest = as.numeric(which(DAPC1$Kstat == min(DAPC1$Kstat)))
    updateSliderInput(session, "grp", min = 3, max = 35, value = lowest, step = 1)
    
    shinyjs::hide("DAPCStatus")
    DAPCtitle1("BIC plot")
    guide_DAPC("The STEP I is completed. \nPlease choose number of cluster (K) for STEP II.")
  })
  
  observeEvent(input$runDAPC2, {
    req(input$FileforDAPC)
    req(input$npca)
    req(input$grp)
    
    shinyjs::show("DAPCStatus")
    
    dapc_data = switch(input$FileforDAPC, "gi" = gi())
    
    DAPC2 = find.clusters(dapc_data, n.clust = input$grp, n.pca = input$npca, scale = FALSE, choose = FALSE)
    DAPC2 = dapc(dapc_data, pop = DAPC2$grp, n.pca = input$grp-1, n.da = input$grp-1)
    DAPC2(DAPC2)
    DAPC_Trans = as.data.frame(DAPC2$tab)
    DAPC_Trans(DAPC_Trans)
    DAPC_pop = data.frame("ID" = row.names(DAPC_Trans), "Group" = DAPC2$assign)
    DAPC_pop(DAPC_pop)
    
    shinyjs::hide("DAPCStatus")
    DAPCtitle2("Density plot of first discriminant function")
    DAPCtitle3("Density plot of second discriminant function")
    DAPCtitle4("DAPC scatter plot")
    guide_DAPC("The DAPC STEP II is completed. \nPlease check the results.")
    pre_results = pre_results()
    pre_results[[19]] = "# Population Structure"
    pre_results[[22]] = "Discriminant Analysis of Principal Components (DAPC)"
    pre_results[[23]] = paste0("The samples were divided into ", length(table(DAPC2$grp)), " groups based on the BIC value at K = ", length(table(DAPC2$grp)))
    pre_results[[24]] = paste0("Group sizes, Group 1 to Group ", length(table(DAPC2$grp)), ": ", paste(as.numeric(table(DAPC2$grp)), collapse = ", "))
    pre_results[[25]] = paste0("The group centroid of each group at first discriminant function, Group 1 to Group ", length(table(DAPC2$grp)), ": ", paste(round(DAPC2$grp.coord[,1], 2), collapse = ", "))
    pre_results[[26]] = paste0("The group centroid of each group at second discriminant function, Group 1 to Group ", length(table(DAPC2$grp)), ": ", paste(round(DAPC2$grp.coord[,2], 2), collapse = ", "))
    pre_results(pre_results)
  })
  
  output$DAPCfileInfo = renderText({
    req(gi())
    paste0("Number of samples: ", length(gi()@ploidy), "\n",
           "Number of SNPs: ", length(gi()@all.names), "\n",
           "Type: ", class(gi())
    )
  })
  
  observeEvent(input$resetDAPC1, {
    DAPCtitle1("")
    DAPC1(NULL)
    BICplot(NULL)
    showNotification("Data have been reset.")
    guide_DAPC("To run DAPC, input data must be genind file.\nPlease click 'Run STEP I' button first.")
  })
  
  observeEvent(input$resetDAPC2, {
    DAPC1(NULL)
    DAPC2(NULL)
    DAPCtitle1("")
    DAPCtitle2("")
    DAPCtitle3("")
    DAPCtitle4("")
    showNotification("Data have been reset.")
    guide_DAPC("To run DAPC, input data must be genind file.\nPlease click 'Run STEP I' button first.")
  })
  
  output$BICplot = renderPlot({
    req(DAPC1())
    if (DAPCtitle1() == "BIC plot"){
      lowest = which(DAPC1()$Kstat == min(DAPC1()$Kstat))
      
      BIC_data = data.frame(
        Group = c(1:length(DAPC1()$Kstat)),
        BIC = DAPC1()$Kstat,
        fill = rep("#c19b73", length(DAPC1()$Kstat)))
      
      BIC_data[lowest, 3] = "#cd0000"
      
      
      BICplot = ggplot(BIC_data, aes(x = Group, y = BIC)) +
        geom_line(linetype = "twodash", lwd = 1.5) +
        xlab("Number of clusters (K)") +
        scale_y_continuous(name = "BIC") +
        geom_point(size = 5, color = BIC_data$fill) +
        theme_classic() +
        theme(
          axis.title.x =   element_text(size = 16),
          axis.title.y = element_text(size = 16, color = "grey10"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 13, color = "grey10")) +
        theme(legend.position = "none") 
      BICplot(BICplot)
      BICplot()
    }
  })
  
  output$DAPCplot = renderPlot({
    req(DAPC2())
    if (DAPCtitle4() == "DAPC scatter plot"){
      scatter(DAPC2(), bg = "white", scree.da = F, pch = 19, 
              posi.leg = "topright", legend = T, cex = 2.2, cellipse = 1, axesell = F)
    }
  })
  
  output$DF1plot = renderPlot({
    req(DAPC2())
    if (DAPCtitle4() == "DAPC scatter plot"){
      scatter(DAPC2(), 1, 1, bg = "white",
              scree.da = FALSE, legend = F, solid = .6)
    }
  }) 
  
  output$DF2plot = renderPlot({
    req(DAPC2())
    if (DAPCtitle4() == "DAPC scatter plot"){
      scatter(DAPC2(), 2, 2, bg = "white",
              scree.da = FALSE, legend = F, solid = .6)
    }
  }) 
  
  output$download_BIC_plot = renderUI({
    if (DAPCtitle1() == "BIC plot") {
      downloadButton("DBICplot", "Download Plot")
    }
  })
  
  output$DBICplot = downloadHandler(
    filename = "DAPC_BIC_Plot.pdf",
    content = function(file) {
      pdf(file, width = 6, height = 4)
      plot(BICplot())
      dev.off()
    }
  )
  
  output$download_DF1_plot = renderUI({
    if (DAPCtitle4() == "DAPC scatter plot") {
      downloadButton("DDF1plot", "Download Plot")
    }
  })
  
  output$DDF1plot = downloadHandler(
    filename = "DAPC_First_Discriminant_Function_Plot.pdf",
    content = function(file) {
      pdf(file, width = 8, height = 4)
      scatter(DAPC2(), 1, 1, bg = "white",
              scree.da = FALSE, legend = F, solid = .6)
      dev.off()
    }
  )
  
  output$download_DF2_plot = renderUI({
    if (DAPCtitle4() == "DAPC scatter plot") {
      downloadButton("DDF2plot", "Download Plot")
    }
  })
  
  output$DDF2plot = downloadHandler(
    filename = "DAPC_Second_Discriminant_Function_Plot.pdf",
    content = function(file) {
      pdf(file, width = 8, height = 4)
      scatter(DAPC2(), 2, 2, bg = "white",
              scree.da = FALSE, legend = F, solid = .6)
      dev.off()
    }
  )
  
  output$download_DAPC_plot = renderUI({
    if (DAPCtitle4() == "DAPC scatter plot") {
      downloadButton("DDAPCplot", "Download Plot")
    }
  })
  
  output$DDAPCplot = downloadHandler(
    filename = "DAPC_Scatter_Plot.pdf",
    content = function(file) {
      pdf(file, width = 12, height = 9)
      scatter(DAPC2(), bg = "white", scree.da = F, pch = 19, 
              posi.leg = "topright", legend = T, cex = 2.2, cellipse = 1, axesell = F)
      dev.off()
    }
  )
  
  output$download_DAPC_pop = renderUI({
    if (DAPCtitle4() == "DAPC scatter plot") {
      downloadButton("DDAPCpop", "Download Group Info.")
    }
  })
  
  output$DDAPCpop = downloadHandler(
    filename = function() { 
      "DAPC_Group_Info.csv" 
    },
    content = function(file) {
      write.csv(DAPC_pop(), file, row.names = FALSE)
    }
  )
  
  output$download_DAPC_transformed = renderUI({
    if (DAPCtitle4() == "DAPC scatter plot") {
      downloadButton("DDAPCtrans", "Download Transformed Data")
    }
  })
  
  output$DDAPCtrans = downloadHandler(
    filename = function() { 
      "DAPC_Transformed_Data.csv" 
    },
    content = function(file) {
      write.csv(DAPC_Trans(), file, row.names = TRUE)
    }
  )
  
  output$download_DAPC_result = renderUI({
    if (DAPCtitle4() == "DAPC scatter plot") {
      downloadButton("DDAPCres", "Download DAPC All Results")
    }
  })
  
  output$DDAPCres = downloadHandler(
    filename = function() { 
      "DAPC_dapc_Object.rds" 
    },
    content = function(file) {
      saveRDS(DAPC2(), file)
    }
  )
  
  output$guide_DAPC = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", 
        guide_DAPC())
  })
  
  output$DAPCtitle1 = renderText({ DAPCtitle1() })
  
  output$DAPCtitle2 = renderText({ DAPCtitle2() })
  
  output$DAPCtitle3 = renderText({ DAPCtitle3() })
  
  output$DAPCtitle4 = renderText({ DAPCtitle4() })
  
  ##### UPGMA #####
  guide_UPGMA = reactiveVal("To run UPGMA phylogenetic tree, input data must be genlight file. \nPlease click 'Run UPGMA' button")
  UPGMAfileInfo = reactiveVal("")
  UPGMAtitle1 = reactiveVal("")
  tree = reactiveVal(NULL)
  UPGMA = reactiveVal(NULL)
  
  output$fileSelection_UPGMA = renderUI({
    if (!is.null(gl())){
      choices = c("genlight file" = "gl")
    } else {
      choices = ""
    }
    selectInput("FileforUPGMA", "Dataset for UPGMA:", choices)
  })
  
  output$UPGMAfileInfo = renderText({
    req(gl())
    paste0("Number of samples: ", length(gl()@ind.names), "\n",
           "Number of SNPs: ", gl()@n.loc, "\n",
           "Type: ", class(gl())
    )
  })
  
  observeEvent(input$runUPGMA, {
    req(input$FileforUPGMA)
    req(input$sample)
    shinyjs::show("UPGMAStatus")
    
    UPGMA_data = switch(input$FileforUPGMA, "gl" = gl())
    tree = aboot(UPGMA_data, tree = "upgma", 
                 distance = bitwise.dist, 
                 sample = input$sample,
                 showtree = F)
    tree(tree)
    
    shinyjs::hide("UPGMAStatus")
    UPGMAtitle1("UPGMA phylogenetic tree")
    guide_UPGMA("The UPGMA is completed. \nTry altering the layout style and observe the UPGMA phylogenetic tree..")
  })
  
  observeEvent(input$resetUPGMA, {
    UPGMAtitle1("")
    tree(NULL)
    showNotification("Data have been reset.")
    guide_UPGMA("To run UPGMA phylogenetic tree, input data must be genlight file. \nPlease click 'Run UPGMA' button.")
  })
  
  output$Layout = renderUI({
    if (UPGMAtitle1() == "UPGMA phylogenetic tree"){
      selectInput("Layout", "Layout style", choices = c("phylogram", "cladogram", "fan", "unrooted", "radial", "tidy"),
                  selected = "fan")
    }
  })
  
  output$UPGMA = renderPlot({
    req(tree())
    req(input$Layout)
    if (UPGMAtitle1() == "UPGMA phylogenetic tree") {
      plot.phylo(tree(), type = input$Layout, show.tip = T)
    }
  })
  
  output$download_UPGMA_plot = renderUI({
    if (UPGMAtitle1() == "UPGMA phylogenetic tree") {
      downloadButton("DUPGMAplot", "Download Plot")
    }
  })
  
  output$DUPGMAplot = downloadHandler(
    filename = function() {
      paste0("UPGMA_Plot-", input$sample, "bootstraps-Layout_", input$Layout, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 10, height = 10)
      plot.phylo(tree(), type = input$Layout, show.tip = T)
      dev.off()
    }
  )
  
  output$download_UPGMA_result = renderUI({
    if (UPGMAtitle1() == "Download UPGMA Results") {
      downloadButton("DUPGMAres", "Download Plot")
    }
  })
  
  output$DUPGMAres = downloadHandler(
    filename = "UPGMA_phylo_Object.rds",
    content = function(file) {
      saveRDS(tree(), file)
    }
  )
  
  output$guide_UPGMA = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", 
        guide_UPGMA())
  })
  
  output$UPGMAtitle1 = renderText({ UPGMAtitle1() })
  
  ##### NJ #####
  guide_NJ = reactiveVal("To run NJ phylogenetic tree, input data must be genlight file. \nPlease click 'Run NJ' button")
  NJfileInfo = reactiveVal("")
  NJtree = reactiveVal(NULL)
  NJ = reactiveVal(NULL)
  NJtitle1 = reactiveVal("")
  
  output$fileSelection_NJ = renderUI({
    if (!is.null(gl())){
      choices = c("genlight file" = "gl")
    } else {
      choices = ""
    }
    selectInput("FileforNJ", "Dataset for NJ:", choices)
  })
  
  output$NJfileInfo = renderText({
    req(gl())
    paste0("Number of samples: ", length(gl()@ind.names), "\n",
           "Number of SNPs: ", gl()@n.loc, "\n",
           "Type: ", class(gl())
    )
  })
  
  observeEvent(input$runNJ, {
    req(input$FileforNJ)
    shinyjs::show("NJStatus")
    
    NJ_data = switch(input$FileforNJ, "gl" = gl())
    
    NJtree = nj(dist.gene(as.matrix(NJ_data))) 
    NJtree(NJtree)
    
    shinyjs::hide("NJStatus")
    NJtitle1("NJ phylogenetic tree")
    guide_NJ("The NJ tree is completed. \nTry altering the layout style and observe the NJ phylogenetic tree..")
  })
  
  observeEvent(input$resetNJ, {
    NJtree(NULL)
    NJtitle1("")
    showNotification("Data have been reset.")
    guide_NJ("To run NJ phylogenetic tree, input data must be genlight file. \nPlease click 'Run NJ' button.")
  })
  
  output$NJLayout = renderUI({
    if (NJtitle1() == "NJ phylogenetic tree"){
      selectInput("NJLayout", "Layout style", choices = c("phylogram", "cladogram", "fan", "unrooted", "radial", "tidy"),
                  selected = "fan")
    }
  })
  
  output$NJ = renderPlot({
    req(NJtree())
    req(input$NJLayout)
    if (NJtitle1() == "NJ phylogenetic tree") {
      plot.phylo(NJtree(), type = input$NJLayout, show.tip = T)
    }
  })
  
  output$download_NJ_plot = renderUI({
    if (NJtitle1() == "NJ phylogenetic tree") {
      downloadButton("DNJplot", "Download Plot")
    }
  })
  
  output$DNJplot = downloadHandler(
    filename = function() {
      paste0("NJ_Plot-Layout_", input$NJLayout, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 10, height = 10)
      plot.phylo(NJtree(), type = input$NJLayout, show.tip = T)
      dev.off()
    }
  )
  
  output$download_NJ_result = renderUI({
    if (NJtitle1() == "NJ phylogenetic tree") {
      downloadButton("DNJres", "Download NJ Results")
    }
  })
  
  output$DNJres = downloadHandler(
    filename = "NJ_phylo_Object.rds",
    content = function(file) {
      saveRDS(NJtree(), file)
    }
  )
  
  output$guide_NJ = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", 
        guide_NJ())
  })
  
  output$NJtitle1 = renderText({ NJtitle1() })
  
  ##### Kinship #####
  guide_Kinship = reactiveVal("To run kinship matrix, input data must be data.frame file. \n'DAPC_Group_Info.' in csv file from DAPC analysis is optional. \nPlease click 'Run Kinship' button")
  KinshipfileInfo = reactiveVal("")
  groupInfo2 = reactiveVal(NULL)
  KinshipMatrix = reactiveVal(NULL)
  Kinshiptitle1 = reactiveVal("")
  
  output$fileSelection_Kinship = renderUI({
    if (!is.null(df())){
      choices = c("data.frame file" = "df")
    } else {
      choices = ""
    }
    selectInput("FileforKinship", "Dataset for kinship analysis:", choices)
  })
  
  output$KinshipfileInfo = renderText({
    req(df())
    paste0("Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2], "\n",
           "Type: ", class(df())
    )
  })
  
  output$groupfile2 = renderUI({
    fileInput("groupfile2", "Group Info. (optional)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$groupfile2, {
    req(input$groupfile2)
    groupfile = read.csv(input$groupfile2$datapath)
    groupInfo2 = as.numeric(groupfile$Group)
    groupInfo2(groupInfo2)
  })
  
  observeEvent(input$runKinship, {
    req(input$FileforKinship)
    req(input$Kinship_method)
    shinyjs::show("KinshipStatus")
    
    Kinship_data = switch(input$FileforKinship, "df" = df())
    Kinship_data[] = lapply(Kinship_data, function(x) replace(x, is.na(x), 0))
    Kinship_data = as.matrix(Kinship_data)
    KinshipMatrix = kinship(Kinship_data, method = input$Kinship_method, MAF = NULL, denominator = NULL)
    
    if (!is.null(groupInfo2())){
      group_pos = lapply(sort(unique(groupInfo2())), function(x) which(groupInfo2() == x))
      rank = unlist(group_pos)
      KinshipMatrix = KinshipMatrix[, rank]
      KinshipMatrix = KinshipMatrix[rank, ]
    }
    KinshipMatrix(KinshipMatrix)
    
    shinyjs::hide("KinshipStatus")
    Kinshiptitle1("Kinship matrix")
    guide_Kinship("The kinship analysis is completed.")
  })
  
  observeEvent(input$resetKinship, {
    KinshipMatrix(NULL)
    Kinshiptitle1("")
    output$groupfile2 = renderUI({
      fileInput("groupfile2", "Group Info. (optional)", multiple = F, accept = c(".csv"))
    })
    showNotification("Data have been reset.")
    guide_Kinship("To run kinship matrix, input data must be data.frame file. \n'Group Info.' file from DAPC analysis is optional. \nPlease click 'Run Kinship' button")
  })
  
  
  output$Kinship = renderPlot({
    req(KinshipMatrix())
    if (Kinshiptitle1() == "Kinship matrix") {
      plot_popkin(KinshipMatrix(), titles = "", names = F, ylab = "", 
                  col_n = 100, oma = 0.5, mar_pad = 0.1, leg_width = 0.1, leg_title = "")
    }
  })
  
  output$download_Kinship_plot = renderUI({
    if (Kinshiptitle1() == "Kinship matrix") {
      downloadButton("DKinshipplot", "Download Plot")
    }
  })
  
  output$DKinshipplot = downloadHandler(
    filename = function() {
      paste0("Kinship_Matrix_Plot-Method_", input$Kinship_method, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 10, height = 10)
      plot_popkin(KinshipMatrix(), titles = "Kinship matrix", names = F, ylab = "", 
                  col_n = 100, oma = 0.5, mar_pad = 0.1, leg_width = 0.1, leg_title = "")
      dev.off()
    }
  )
  
  output$download_Kinship_result = renderUI({
    if (Kinshiptitle1() == "Kinship matrix") {
      downloadButton("DKinshipres", "Download Kinship Matrix")
    }
  })
  
  output$DKinshipres = downloadHandler(
    filename = paste0("Kinship_Matrix_Object-Method_", input$Kinship_method, ".rds"),
    content = function(file) {
      saveRDS(KinshipMatrix(), file)
    }
  )
  
  output$guide_Kinship = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", 
        guide_Kinship())
  })
  
  output$Kinshiptitle1 = renderText({ Kinshiptitle1() })
  ##### Page 5: Genetic Diversity #####
  ##### Diversity Parameters #####
  guide_GD = reactiveVal("To analysis genetic diversity, input data must be data.frame file. \nYou need to upload a Site Info file (in rds), which can be downloaded from the 'Data Conversion' tab after you have convert the VCF to data.frame file.\nPlease click 'Analysis' button")
  GDtitle1 = reactiveVal("")
  GDtitle2 = reactiveVal("")
  GDtitle3 = reactiveVal("")
  GDfileInfo = reactiveVal("")
  groupInfo3 = reactiveVal(NULL)
  popgen = reactiveVal(NULL)
  group_stat = reactiveVal("")
  fst_matrix = reactiveVal("")
  site_stat = reactiveVal(NULL)
  Site_Info = reactiveVal("")
  GDplot = reactiveVal(NULL)
  
  output$fileSelection_GD = renderUI({
    if (!is.null(df())){ choices = c("data.frame file" = "df") } else { choices = "" }
    selectInput("FileforGD", "Dataset for analysis:", choices)
  })
  
  output$GDfileInfo = renderText({
    req(df())
    paste("Number of samples: ", dim(df())[1], "\n",
          "Number of SNPs: ", dim(df())[2], "\n",
          "Type: ", class(df()),
          sep = "")
  })
  
  output$Site_Info1 = renderUI({
    fileInput("Site_Info1", "Site Info.* (required)", multiple = F, accept = c(".rds"))
  })
  
  output$groupfile3 = renderUI({
    fileInput("groupfile3", "Group Info. (optional)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$groupfile3, {
    req(input$groupfile3)
    groupfile = read.csv(input$groupfile3$datapath)
    groupInfo3 = groupfile$Group
    groupInfo3(groupInfo3)
  })
  
  observeEvent(input$Site_Info1, {
    req(input$Site_Info1)
    Site_Info = readRDS(input$Site_Info1$datapath)
    Site_Info(Site_Info)
  })
  
  observeEvent(input$runGD, {
    req(df(), Site_Info())
    shinyjs::show("GDStatus")
    
    GD_data = switch(input$FileforGD, "df" = df())
    if (!is.null(groupInfo3())){
      popgen = popgen2(GD_data, groupInfo3())
      
      if (!is.null(Site_Info())){
        site_stat = cbind(Site_Info(), popgen$whole$Markers, popgen$bygroup$F.stats$Markers)
      } else{
        site_stat = cbind(popgen$whole$Markers, popgen$bygroup$F.stats$Markers)
      }
      site_stat$Fst = ifelse(site_stat$Fst < 0, 0, site_stat$Fst)
      site_stat(site_stat)
      
      ngroup = sort(as.character(unique(groupInfo3())))
      GD_names = c("Sample_size", "Nei", "PIC", "Pi", "He", "Ho", "Fis", "Exclusive_allele", "Fixed_allele")
      group_stat = matrix(nrow = length(GD_names), ncol = length(ngroup)+1, 
                          dimnames = list(GD_names, c("Overall", paste("Group", ngroup))))
      group_stat[, 1] = c(length(popgen$whole$Genotypes)/2, popgen$whole$Population[, 1], NA, NA)
      for (i in seq_along(ngroup)) {
        group_stat[1, i + 1] = length(popgen$bygroup[[ngroup[i]]]$Genotypes) / 2
        group_stat[2:7, i + 1] = popgen$bygroup[[ngroup[i]]]$Population[, 1]
        group_stat[8, i + 1] = if (!is.na(popgen$bygroup[[ngroup[i]]]$exclusive[1])) {length(popgen$bygroup[[ngroup[i]]]$exclusive)} else {0}
        group_stat[9, i + 1] = if (!is.na(popgen$bygroup[[ngroup[i]]]$fixed[1])) {length(popgen$bygroup[[ngroup[i]]]$fixed)} else {0}
      }
      group_stat = t(group_stat)
      group_stat = group_stat[, c(1, 5:6, 2:4, 7:9)]
      group_stat(group_stat)
      
      f_data = as.data.frame(popgen$bygroup$F.stats$Genotypes)
      f_data$Comparison = rownames(f_data)
      f_data = f_data[-1,]
      f_data = f_data %>%
        separate(Comparison, into = c("Comparison1", "Comparison2"), sep = "-", fill = "right")
      fst_data = f_data[,c(2,4,5)]
      
      fst_matrix = matrix(0, nrow = length(ngroup), ncol = length(ngroup))
      rownames(fst_matrix) = colnames(fst_matrix) = paste("Group", ngroup)
      for (i in 1:nrow(fst_data)) {
        row = as.numeric(fst_data$Comparison1[i])
        col = as.numeric(fst_data$Comparison2[i])
        value = fst_data$Fst[i]
        fst_matrix[row, col] = value
        fst_matrix[col, row] = value
      }
      fst_matrix(fst_matrix)
      output$Type = renderUI({
        selectInput("Type", "By each group:", choices = c("Statistics per site", "Statistics by group"), selected = "Statistics per site")
      })
      output$GDgroupresults = DT::renderDataTable({
        DT::datatable(group_stat())
      })
      GDtitle3("Genetic diversity statistics by group")
      pre_results = pre_results()
      pre_results[[32]] = paste0("The average observed heterozygosity (Ho) of each group, Group 1 to Group ", length(group_stat[,3]), ": ", paste(group_stat[,3], collapse = ", "))
      pre_results[[32]] = paste0("The average expected heterozygosity (He) of each group, Group 1 to Group ", length(group_stat[,2]), ": ", paste(group_stat[,2], collapse = ", "))
      pre_results[[33]] = paste0("The average unbiased π diversity (Pi) of each group, Group 1 to Group ", length(group_stat[,6]), ": ", paste(group_stat[,6], collapse = ", "))
      pre_results[[34]] = paste0("The number of exclusive allele of each group, Group 1 to Group ", length(group_stat[,2]), ": ", paste(group_stat[-1,8], collapse = ", "))
      pre_results[[35]] = paste0("The number of fixed allele of each group, Group 1 to Group ", length(group_stat[,2]), ": ", paste(group_stat[-1,9], collapse = ", "))
      pre_results(pre_results)
    } else{
      popgen = popgen2(GD_data)
      if (!is.null(Site_Info())){
        site_stat = cbind(Site_Info(), popgen$whole$Markers)
      } else{
        site_stat = popgen$whole$Markers
      }
      site_stat(site_stat)
      output$Type = renderUI({
        selectInput("Type", "By per site:", choices = c("Statistics per site"), selected = "Statistics per site")
      })
    }
    popgen(popgen)
    shinyjs::hide("GDStatus")
    guide_GD("The analysis of genetic diversity is completed. \nPlease check the results.")
    GDtitle2("Genetic diversity statistics by per site")
    output$GDresults = DT::renderDataTable({
      DT::datatable(site_stat(), options = list(pageLength = 5))
    })
    pre_results = pre_results()
    pre_results[[30]] = "# Genetic Diversity"
    pre_results[[31]] = paste0("Across all chromosomes, the average missing rate, minor allele frequency (MAF), and nucleotide diversity π were ", 
                               round(mean(popgen$whole$Markers[,8])*100,4), "%, ", round(mean(popgen$whole$Markers[,1]),4), ", and ", round(mean(popgen$whole$Markers[,7]),4), ", respectively.")
    pre_results(pre_results)
  })
  
  observeEvent(input$resetGD, {
    GDfileInfo = reactiveVal("")
    groupInfo3 = reactiveVal("")
    group_stat = reactiveVal("")
    fst_matrix = reactiveVal("")
    Site_Info = reactiveVal("")
    popgen = reactiveVal(NULL)
    site_stat = reactiveVal(NULL)
    GDplot = reactiveVal(NULL)
    output$groupfile3 = renderUI({
      fileInput("groupfile3", "Group Info. (optional)", multiple = F, accept = c(".csv"))
    })
    output$Site_Info1 = renderUI({
      fileInput("Site_Info1", "Site Info. (required)", multiple = F, accept = c(".rds"))
    })
    GDtitle1("")
    GDtitle2("")
    output$GDresults = DT::renderDataTable({ DT::datatable(NULL) })
    GDtitle3("")
    output$GDgroupresults = DT::renderDataTable({ DT::datatable(NULL) })
    showNotification("Data have been reset.")
    guide_GD("To analysis genetic diversity, input data must be data.frame file. \nPlease click 'Analysis' button")
  })
  
  observeEvent(input$Type, {
    req(site_stat())
    if (input$Type == "Statistics per site") {
      if (!is.null(groupInfo3())) {
        output$Parameter = renderUI({
          selectInput("Parameter", "Select a parameter:", choices = c("Minor allele frequency", "Major allele frequency", "Expected heterozygosity (He)", 
                                                                      "Observed heterozygosity (Ho)", "Nei's genetic diversity (Nei)", "Polymorphism information content (PIC)", 
                                                                      "Unbiased π diversity (Pi)", "Missing rate", "HWE p-value (pval)", 
                                                                      "Inbreeding coefficient within individuals (Fis)", "Fixation index among populations (Fst)", "Total inbreeding coefficient (Fit)"), selected = "Unbiased π diversity (Pi)")
        })
      } else{
        output$Parameter = renderUI({
          selectInput("Parameter", "Select a parameter:", choices = c("Minor allele frequency", "Major allele frequency", "Expected heterozygosity (He)", 
                                                                      "Observed heterozygosity (Ho)", "Nei's genetic diversity (Nei)", "Polymorphism information content (PIC)", 
                                                                      "Unbiased π diversity (Pi)", "Missing rate"), selected = "Unbiased π diversity (Pi)")
        })
      }
    } else if (input$Type == "Statistics by group") {
      output$Parameter = renderUI({
        selectInput("Parameter", "Select a parameter:", choices = c("Sample size", "Expected heterozygosity (He)", "Observed heterozygosity (Ho)", 
                                                                    "Nei's genetic diversity (Nei)", "Polymorphism information content (PIC)", "Unbiased π diversity (Pi)", 
                                                                    "Inbreeding coefficient within individuals (Fis)", "Exclusive allele", "Fixed allele"), selected = "Unbiased π diversity (Pi)")
      })
    }
  })
  
  observeEvent(input$Parameter, {
    if (input$Type == "Statistics per site") {
      req(site_stat())
      if (input$Parameter == "Minor allele frequency"){
        GDplot = GDsiteplot(site_stat(), "Minor", "Minor allele frequency")
      } else if (input$Parameter == "Major allele frequency"){
        GDplot = GDsiteplot(site_stat(), "Major", "Major allele frequency")
      } else if (input$Parameter == "Expected heterozygosity (He)"){
        GDplot = GDsiteplot(site_stat(), "He", "Expected heterozygosity (He)")
      } else if (input$Parameter == "Observed heterozygosity (Ho)"){
        GDplot = GDsiteplot(site_stat(), "Ho", "Observed heterozygosity (Ho)")
      } else if (input$Parameter == "Nei's genetic diversity (Nei)"){
        GDplot = GDsiteplot(site_stat(), "Nei", "Nei's genetic diversity")
      } else if (input$Parameter == "Polymorphism information content (PIC)"){
        GDplot = GDsiteplot(site_stat(), "PIC", "Polymorphism information content")
      } else if (input$Parameter == "Unbiased π diversity (Pi)"){
        GDplot = GDsiteplot(site_stat(), "Pi", "Unbiased π diversity")
      } else if (input$Parameter == "Missing rate"){
        GDplot = GDsiteplot(site_stat(), "Miss", "Missing rate")
      } else if (input$Parameter == "HWE p-value (pval)"){
        GDplot = GDsiteplot(site_stat(), "pval", "HWE p-value")
      } else if (input$Parameter == "Inbreeding coefficient within individuals (Fis)"){
        GDplot = GDsiteplot(site_stat(), "Fis", "Inbreeding coefficient within individuals (Fis)")
      } else if (input$Parameter == "Fixation index among populations (Fst)"){
        GDplot = GDsiteplot(site_stat(), "Fst", "Fixation index among populations (Fst)")
      } else if (input$Parameter == "Total inbreeding coefficient (Fit)"){
        GDplot = GDsiteplot(site_stat(), "Fit", "Total inbreeding coefficient (Fit)")
      }
      GDplot(GDplot)
      GDtitle1("Plot of genetic diversity statistics per site")
    } else if (input$Type == "Statistics by group") {
      req(group_stat())
      if (input$Parameter == "Sample size"){
        GDplot = GDgroupplot(group_stat(), "Sample_size", "Sample size")
      } else if (input$Parameter == "Expected heterozygosity (He)"){
        GDplot = GDgroupplot(group_stat(), "He", "Expected heterozygosity (He)")
      } else if (input$Parameter == "Observed heterozygosity (Ho)"){
        GDplot = GDgroupplot(group_stat(), "Ho", "Observed heterozygosity (Ho)")
      } else if (input$Parameter == "Nei's genetic diversity (Nei)"){
        GDplot = GDgroupplot(group_stat(), "Nei", "Nei's genetic diversity")
      } else if (input$Parameter == "Polymorphism information content (PIC)"){
        GDplot = GDgroupplot(group_stat(), "PIC", "Polymorphism information content")
      } else if (input$Parameter == "Unbiased π diversity (Pi)"){
        GDplot = GDgroupplot(group_stat(), "Pi", "Unbiased Pi diversity")
      } else if (input$Parameter == "Inbreeding coefficient within individuals (Fis)"){
        GDplot = GDgroupplot(group_stat(), "Fis", "Inbreeding coefficient within individuals (Fis)")
      } else if (input$Parameter == "Exclusive allele"){
        GDplot = GDgroupplot(group_stat(), "Exclusive_allele", "Exclusive allele")
      } else if (input$Parameter == "Fixed allele"){
        GDplot = GDgroupplot(group_stat(), "Fixed_allele", "Fixed allele")
      }
      GDplot(GDplot)
      GDtitle1("Plot of genetic diversity statistics by group")
    }
  })
  
  output$download_GD_plot = renderUI({
    if (GDtitle2() == "Genetic diversity statistics by per site") {
      downloadButton("DGDplot", "Download Plot")
    }
  })
  
  output$DGDplot = downloadHandler(
    filename = function() {
      paste0("Genetic_Diversity_", input$Type, "-", input$Parameter, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 15.5, height = 5.8)
      print(GDplot())
      dev.off()
    }
  )
  
  output$download_GD_site = renderUI({
    if (GDtitle2() == "Genetic diversity statistics by per site") {
      downloadButton("DGD_site", "Download Diversity (per site)")
    }
  })
  
  output$DGD_site = downloadHandler(
    filename = "Genetic_Diversity_by_Site.rds",
    content = function(file) {
      saveRDS(site_stat(), file)
    }
  )
  
  output$download_GD_group = renderUI({
    if (GDtitle3() == "Genetic diversity statistics by group") {
      downloadButton("DGD_group", "Download Diversity (group)")
    }
  })
  
  output$DGD_group = downloadHandler(
    filename = "Genetic_Diversity_by_Group.csv",
    content = function(file) {
      write.csv(group_stat(), file, row.names = TRUE)
    }
  )
  
  output$download_Fst = renderUI({
    if (GDtitle3() == "Genetic diversity statistics by group") {
      downloadButton("D_Fst", "Download Fst Matrix (group)")
    }
  })
  
  output$D_Fst = downloadHandler(
    filename = "Genetic_Diversity_Fst_Matrix.csv",
    content = function(file) {
      write.csv(fst_matrix(), file, row.names = TRUE)
    }
  )
  
  output$download_GD = renderUI({
    if (GDtitle2() == "Genetic diversity statistics by per site") {
      downloadButton("D_GD", "Download All Diversity Results")
    }
  })
  
  output$D_GD = downloadHandler(
    filename = "Genetic_Diversity_All_Results.rds",
    content = function(file) {
      saveRDS(popgen(), file)
    }
  )
  
  output$guide_GD = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", 
        guide_GD())
  })
  
  output$GDplot = renderPlot({
    req(GDplot())
    if (GDtitle2() == "Genetic diversity statistics by per site") {
      GDplot()
    }
  })
  
  output$GDtitle1 = renderText({ GDtitle1() })
  
  output$GDtitle2 = renderText({ GDtitle2() })
  
  output$GDtitle3 = renderText({ GDtitle3() })
  
  ##### Circos Plot #####
  guide_Circos = reactiveVal("To run sliding window, you need to run 'Diversity Parameter' first! \nPlease choose optimal window size and step, then click 'Run Sliding Window' button.")
  Circostitle1 = reactiveVal("")
  Circostitle2 = reactiveVal("")
  GDInfo = reactiveVal("")
  Chr_Info = reactiveVal(NULL)
  SW_data = reactiveVal(NULL)
  n_SelePara = reactiveVal(0)
  
  observeEvent(site_stat(), {
    if (!is.null(site_stat())) {
      updateSelectInput(session, "SelePara", 
                        choices = colnames(site_stat())[-c(1:3, 13)], 
                        selected = c("Minor", "Ho", "Pi"))
    }
  })
  
  output$GDInfo = renderText({
    if (!is.null(site_stat())){
      paste0("Data is ready!", "\n",
             "Number of SNPs: ", dim(site_stat())[1], "\n",
             "Number of Chromosomes: ", length(unique(site_stat()[,1])), "\n",
             "Number of Parameters: ", dim(site_stat())[2]-3, "\n",
             "Parameters: ", paste(colnames(site_stat())[-c(1:3)], collapse = ", ")
      )
    } else{
      paste0("No available data!")
    }
  })
  
  observeEvent(input$runSW, {
    req(site_stat())
    shinyjs::show("CircosStatus")
    
    data = site_stat()
    window = as.numeric(input$WindowSize * 1000)
    step = as.numeric(input$StepSize * 1000)
    SelePara = as.character(input$SelePara)
    
    SW_data = data.frame(matrix(ncol = length(SelePara) + 4, nrow = 0))
    colnames(SW_data) = c("Chr", "Start", "End", "Count", SelePara)
    
    nchr = length(unique(data[, 1]))
    
    r = 1
    progressVal = reactiveVal(NULL)
    withProgress(message = "Processing Data", value = 0, {
      for (i in 1:nchr) {
        shiny::setProgress(value = i / nchr, message = sprintf("Processing Chromosome %d of %d", i, nchr))
        
        chr = as.numeric(data[, 1])
        CHR = data[chr == i, ]
        start_pos = seq(0, max(CHR$Pos), by = step)
        
        for (j in seq_along(start_pos)) {
          loc = which(CHR$Pos >= start_pos[j] & CHR$Pos <= start_pos[j] + window)
          
          if (length(loc) != 0) {
            SW_data[r, 1] = paste0("Chr", i)
            SW_data[r, 2] = start_pos[j]
            SW_data[r, 3] = start_pos[j] + window
            SW_data[r, 4] = length(loc)
            SW_data[r, 5:(length(SelePara) + 4)] = round(colMeans(CHR[loc, SelePara], na.rm = TRUE), 4)
            r = r + 1
          }
        }
      }
    })
    SW_data$Chr = sapply(SW_data$Chr, chromosome)
    SW_data(SW_data)
    
    shinyjs::hide("CircosStatus")
    guide_Circos("Analysis of 'Sliding Window' is completed. \nPlease choose parameter for each track, then click 'Run Circos plot' button.")
    Circostitle1("Sliding window data")
    
    output$SWresults = DT::renderDataTable({
      DT::datatable(SW_data(), options = list(pageLength = 5))
    })
  })
  
  output$download_SW = renderUI({
    if (Circostitle1() == "Sliding window data") {
      downloadButton("D_SW", "Download Sliding Window Data")
    }
  })
  
  output$D_SW = downloadHandler(
    filename = "Sliding_Window_Data.csv",
    content = function(file) {
      write.csv(SW_data(), file, row.names = TRUE)
    }
  )
  
  observeEvent(input$resetSW, {
    SW_data = reactiveVal(NULL)
    Circostitle1("")
    output$SWresults = DT::renderDataTable({ DT::datatable(NULL) })
    showNotification("Data have been reset.")
    guide_Circos("To run sliding window, you need to run 'Diversity Parameter' first! \nPlease choose optimal window size and step, then click 'Run Sliding Window' button.")
  })
  
  output$Chr_Info = renderUI({
    fileInput("Chr_Info", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$Chr_Info, {
    Chr_Info = read.csv(input$Chr_Info$datapath)
    Chr_Info(Chr_Info)
    if (!is.null(Chr_Info)){
      updateSelectInput(session, "Track1", choices = "Chromosome Info.", selected = "Chromosome Info.")
    }
  })
  
  observeEvent(input$addTrack, {
    if (n_SelePara() < 4) {
      n_SelePara(n_SelePara() + 1)
    }
  })
  
  output$Track3 = renderUI({
    req(SW_data())
    lapply(seq_len(n_SelePara()), function(i) {
      selectInput(paste0("Track", i+2), paste("Track ", i+2 ,": Parameter"), choices = c(input$SelePara), selected = input$SelePara[i])
    })
  })
  
  observeEvent(input$runCircos, {
    req(SW_data(), Chr_Info(), input$Track3)
    shinyjs::show("CircosStatus")
    
    pdf_path = tempfile(fileext = ".pdf")
    generateCircosPlot(Chr_Info(), SW_data(), pdf_path, input$Track3, input$Track4, input$Track5, input$Track6)
    
    output$downloadCircosplot = renderUI({
      if (Circostitle2() == "Circos plot"){
        downloadButton("Circosplot", "Download Plot")
      }
    })
    
    output$Circosplot = downloadHandler(
      filename = "Circos_Plot.pdf",
      content = function(file) {
        file.copy(pdf_path, file)
      }
    )
    
    shinyjs::hide("CircosStatus")
    Circostitle2("Circos plot")
    guide_Circos("Circos plot is completed! \nPlease download the Circos plot and check the results.")
  })
  
  observeEvent(input$resetCircos, {
    output$Chr_Info = renderUI({ fileInput("Chr_Info", "Chromosome Info.* (required)", multiple = F, accept = c(".csv")) })
    Chr_Info(NULL)
    n_SelePara(0)
    lapply(seq_len(n_SelePara()), function(i) {
      selectInput(paste0("Track", i+2), paste("Track ", i+2 ,": Parameter"), choices = c(input$SelePara), selected = input$SelePara[i])
    })
    showNotification("Data have been reset.")
    Circostitle2("")
    guide_Circos("Analysis of 'Sliding Window' is completed. \nPlease choose parameter for each track, then click 'Run Circos plot' button.")
  })
  
  output$Circosplotinfo = renderText({
    if (Circostitle2() == "Circos plot"){
      paste0("Track 1 & 2: ", input$Track1, "\n",
             "Track 3: ", input$Track3, "\n",
             "Track 4: ", input$Track4, "\n",
             "Track 5: ", input$Track5, "\n",
             "Track 6: ", input$Track6, "\n")
    }
  })
  
  output$guide_Circos = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", 
        guide_Circos())
  })
  
  output$Circostitle1 = renderText({ Circostitle1() })
  
  output$Circostitle2 = renderText({ Circostitle2() })
  
  ##### Genetic Distance #####
  guide_GT = reactiveVal("To run genetic distance, input data must be genind file with 'Group Info.'. \nYou can get this genind file from the 'Data Conversion' tab after you have data.frame and Group Info. files.")
  GTfileInfo = reactiveVal("")
  GTmatrix = reactiveVal(NULL)
  GTdf = reactiveVal(NULL)
  GTtitle1 = reactiveVal("")
  GTtitle2 = reactiveVal("")
  GTplot = reactiveVal(NULL)
  
  output$fileSelection_GT = renderUI({
    if (!is.null(gi())){ choices = c("genind file" = "gi") } else { choices = "" }
    selectInput("FileforGT", "Dataset for genetic distance:", choices)
  })
  
  output$GTfileInfo = renderText({
    req(gi())
    group_info = ifelse(is.null(gi()@pop), "NaN \n**Warning** Not available!", "Added")
    paste0(
      "Number of samples: ", length(gi()@ploidy), "\n",
      "Number of SNPs: ", length(gi()@all.names), "\n",
      "Type: ", class(gi()), "\n",
      "Group Info.: ", group_info
    )
  })
  
  observeEvent(input$runGT, {
    req(input$FileforGT, gi()@pop)
    shinyjs::show("GTStatus")
    GT_data = switch(input$FileforGT, "gi" = gi())
    strata(GT_data) = data.frame(pop = pop(GT_data))
    GT = genet.dist(GT_data, diploid = TRUE, method = GT_method_choice[input$GT_method])
    GT.mat = as.matrix(GT) %>% round(digits = 3)
    GTmatrix = GT.mat[order(as.numeric(rownames(GT.mat))), order(as.numeric(colnames(GT.mat)))]
    rownames(GTmatrix) = colnames(GTmatrix) = paste("Group", colnames(GTmatrix))
    GTmatrix(GTmatrix)
    ind = which(upper.tri(GTmatrix, diag = FALSE), arr.ind = TRUE)
    GTdf = data.frame(
      Pair1 = colnames(GTmatrix)[ind[, 2]],
      Pair2 = rownames(GTmatrix)[ind[, 1]],
      GeneticDistance = GTmatrix[ind]
    )
    GTdf(GTdf)
    GTdf$Pair1 = factor(GTdf$Pair1, levels = unique(GTdf$Pair1))
    GTdf$Pair2 = factor(GTdf$Pair2, levels = unique(GTdf$Pair2))
    GTdf$GeneticDistance[GTdf$GeneticDistance < 0] = 0
    shinyjs::hide("GTStatus")
    GTtitle1("Genetic distance plot")
    GTtitle2("Genetic distance matrix")
    guide_GT("Genetic distance analysis is completed.")
    output$GTresults = renderTable({ GTmatrix() }, rownames = TRUE)
    pre_results = pre_results()
    pre_results[[30]] = "# Genetic Diversity"
    pre_results[[37]] = paste0("The genetic distance matrix between pairs of groups:")
    combined = apply(GTdf[, c("Pair1", "Pair2", "GeneticDistance")], 1, function(row) {
      paste0(row[1], "-", row[2], ": ", row[3])
    })
    pre_results[[38]] = paste(combined, collapse = "; ")
    pre_results(pre_results)
  })
  
  observeEvent(input$resetGT, {
    GTmatrix(NULL)
    GTdf(NULL)
    showNotification("Data have been reset.")
    output$GTresults = renderTable({ NULL })
    GTtitle1("")
    GTtitle2("")
    guide_GT("To run genetic distance, input data must be genind file with 'Group Info.'. \nYou can get this genind file from the 'Data Conversion' tab after you have data.frame and Group Info. files.")
  })
  
  output$GTplot = renderPlot({
    req(GTdf())
    if (guide_GT() == "Genetic distance analysis is completed.") {
      mid = (max(GTdf()[, 3]) - min(GTdf()[, 3])) / 2
      GTplot = ggplot(GTdf(), aes(x = Pair1, y = Pair2, fill = GeneticDistance)) +
        geom_tile() +
        geom_text(aes(label = GeneticDistance), color = "black", size = 4) +
        scale_fill_gradient2(low = "white", mid = "#f19372", high = "#b53c12", midpoint = mid, name = input$GT_method) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0), position = "right") +
        theme(
          axis.text = element_text(colour = "black", size = 14),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        ) +
        guides(fill = guide_colorbar(barwidth = 15, barheight = 1, title.position = "top"))
      GTplot(GTplot)
      GTplot()
    }
  })
  
  output$download_GT_plot = renderUI({
    if (!is.null(GTdf())) {
      downloadButton("DGTplot", "Download Plot")
    }
  })
  
  output$DGTplot = downloadHandler(
    filename = function() {
      paste0("Genetic_Distance_Plot-", input$GT_method, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 5, height = 5)
      print(GTplot())
      dev.off()
    }
  )
  
  output$download_GT_result = renderUI({
    if (GTtitle2() == "Genetic distance matrix") {
      downloadButton("DGTresult", "Download Genetic distance matrix")
    }
  })
  
  output$DGTresult = downloadHandler(
    filename = paste0("Genetic_Distance_Matrix-", input$GT_method, ".csv"),
    content = function(file) {
      write.csv(GTmatrix(), file, row.names = TRUE)
    }
  )
  
  output$guide_GT = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
        guide_GT()
    )
  })
  
  output$GTtitle1 = renderText({ GTtitle1() })
  output$GTtitle2 = renderText({ GTtitle2() })
  
  ##### AMOVA #####
  guide_AMOVA = reactiveVal("To run AMOVA, input data must be genind file with 'Group Info.'. \nYou can get this genind file from the 'Data Conversion' tab after you have data.frame and Group Info. files.")
  AMOVAfileInfo = reactiveVal("")
  amova.result = reactiveVal(NULL)
  amova.test = reactiveVal(NULL)
  AMOVA_res = reactiveVal(NULL)
  AMOVAvarplot = reactiveVal(NULL)
  AMOVAtitle1 = reactiveVal("")
  AMOVAtitle2 = reactiveVal("")
  AMOVAtitle3 = reactiveVal("")
  
  output$fileSelection_AMOVA = renderUI({
    if (!is.null(gi())){ choices = c("genind file" = "gi") } else { choices = "" }
    selectInput("FileforAMOVA", "Dataset for genetic distance:", choices)
  })
  
  output$AMOVAfileInfo = renderText({
    req(gi())
    group_info = ifelse(is.null(gi()@pop), "NaN \n**Warning** Not available!", "Added")
    paste0(
      "Number of samples: ", length(gi()@ploidy), "\n",
      "Number of SNPs: ", length(gi()@all.names), "\n",
      "Type: ", class(gi()), "\n",
      "Group Info.: ", group_info
    )
  })
  
  observeEvent(input$runAMOVA, {
    req(input$FileforAMOVA, gi()@pop)
    shinyjs::show("AMOVAStatus")
    AMOVA_data = switch(input$FileforAMOVA, "gi" = gi())
    strata(AMOVA_data) = data.frame(pop = pop(AMOVA_data))
    result = poppr.amova(AMOVA_data, ~pop)
    amova.result(result)
    name = c("Group-total: ", "Samples-group: ", "Samples-total: ", "")
    AMOVA_res = data.frame(
      Source_of_variance = c("Among groups", "Among individual within groups", "Within individuals", "Total"),
      df = result$results$Df,
      Sum_of_squares = round(result$results$`Sum Sq`, 2),
      Variance_components = round(result$componentsofcovariance$Sigma, 2),
      Variance_percentage = round(result$componentsofcovariance$`%`, 2),
      Phi_statistics = paste0(name, c(round(result$statphi$Phi, 4)[c(3,2,1)], ""))
    )
    row.names(AMOVA_res) = NULL
    AMOVA_res(AMOVA_res)
    shinyjs::hide("AMOVAStatus")
    AMOVAtitle1("AMOVA variance")
    AMOVAtitle3("AMOVA results")
    guide_AMOVA("AMOVA is completed, you can choose the number of permutations for test.")
    output$AMOVAresults = renderTable({ AMOVA_res() }, rownames = FALSE)
    pre_results = pre_results()
    pre_results[[30]] = "# Genetic Diversity"
    pre_results[[39]] = paste0("The Analysis of Molecular Variance (AMOVA) results:")
    pre_results[[40]] = paste0("Estimated variance percentage (%): " ,
                               "Among groups: ", AMOVA_res$Variance_percentage[1], 
                               "; Among individual within groups: ", AMOVA_res$Variance_percentage[2], 
                               "; Within individuals: ", AMOVA_res$Variance_percentage[3])
    pre_results(pre_results)
  })
  
  observeEvent(input$resetAMOVA, {
    AMOVA_res(NULL)
    showNotification("Data have been reset.")
    amova.result(NULL)
    AMOVAtitle1("")
    AMOVAtitle3("")
    output$AMOVAresults = renderTable({ NULL }, rownames = FALSE)
    guide_AMOVA("To run AMOVA, input data must be a genind file with 'Group Info.'. \nYou can get this genind file from the 'Data Conversion' tab after you have data.frame and Group Info. files.")
  })
  
  observeEvent(input$runTest, {
    req(amova.result())
    shinyjs::show("AMOVAStatus")
    test = randtest(amova.result(), nrepet = input$nperm)
    amova.test(test)
    AMOVA_res = cbind(AMOVA_res(), p_value = c(paste("<", test$pvalue), ""))
    AMOVA_res(AMOVA_res)
    shinyjs::hide("AMOVAStatus")
    AMOVAtitle2("AMOVA test")
    guide_AMOVA("AMOVA is completed.")
    output$AMOVAresults = renderTable({ AMOVA_res() }, rownames = FALSE)
    pre_results = pre_results()
    pre_results[[40]] = paste0("Estimated variance percentage (%) and p-value of population strata: " ,
                               "Among groups: ", AMOVA_res$Variance_percentage[1], ", p-value: ", AMOVA_res$p_value[1],
                               "; Among individual within groups: ", AMOVA_res$Variance_percentage[2], ", p-value: ", AMOVA_res$p_value[2],
                               "; Within individuals: ", AMOVA_res$Variance_percentage[3], ", p-value: ", AMOVA_res$p_value[3])
    pre_results(pre_results)
  })
  
  observeEvent(input$resetTest, {
    AMOVA_res(NULL)
    amova.result(NULL)
    amova.test(NULL)
    showNotification("Data have been reset.")
    AMOVAtitle1("")
    AMOVAtitle2("")
    AMOVAtitle3("")
    output$AMOVAresults = renderTable({ NULL }, rownames = FALSE)
    guide_AMOVA("To run AMOVA, input data must be a genind file with 'Group Info.'. \nYou can get this genind file from the 'Data Conversion' tab after you have data.frame and Group Info. files.")
  })
  
  output$AMOVAvarplot = renderPlot({
    req(AMOVA_res())
    if (AMOVAtitle1() == "AMOVA variance") {
      plot_data = as.data.frame(AMOVA_res())
      plot = ggplot(plot_data[1:3,], aes(x = "", y = Variance_percentage, fill = Source_of_variance)) +
        geom_col(color = "grey") +
        geom_label_repel(aes(label = paste0(Variance_percentage, "%")), position = position_stack(vjust = 0.5), size = 8, show.legend = F) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = c("#FCDC94", "#e7e6e5", "#D5ED9F")) +
        theme_void() +
        guides(fill = guide_legend(title = "", position = "bottom", nrow = 3)) +
        theme(legend.text = element_text(size = 18))
      AMOVAvarplot(plot)
      plot
    }
  })
  
  output$AMOVAtestplot = renderPlot({
    req(amova.test())
    if (AMOVAtitle2() == "AMOVA test") {
      plot(amova.test())
    }
  })
  
  output$download_AMOVA_plot = renderUI({
    if (AMOVAtitle1() == "AMOVA variance") {
      downloadButton("varplot", "Download Plot")
    }
  })
  
  output$varplot = downloadHandler(
    filename = "AMOVA_Variance_Plot.pdf",
    content = function(file) {
      pdf(file, width = 6, height = 8)
      print(AMOVAvarplot())
      dev.off()
    }
  )
  
  output$download_AMOVA_test_plot = renderUI({
    if (AMOVAtitle2() == "AMOVA test") {
      downloadButton("testplot", "Download Plot")
    }
  })
  
  output$testplot = downloadHandler(
    filename = "AMOVA_Test_Plot.pdf",
    content = function(file) {
      pdf(file, width = 6, height = 8)
      plot(amova.test())
      dev.off()
    }
  )
  
  output$download_AMOVA_results = renderUI({
    if (AMOVAtitle3() == "AMOVA results") {
      downloadButton("AMOVAResults", "Download AMOVA Results")
    }
  })
  
  output$AMOVAResults = downloadHandler(
    filename = "AMOVA_Results.csv",
    content = function(file) {
      write.csv(AMOVA_res(), file, row.names = FALSE)
    }
  )
  
  output$guide_AMOVA = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_AMOVA())
  })
  
  output$AMOVAtitle1 = renderText({ AMOVAtitle1() })
  output$AMOVAtitle2 = renderText({ AMOVAtitle2() })
  output$AMOVAtitle3 = renderText({ AMOVAtitle3() })
  
  ##### Page 6: Selection Sweep #####
  ##### pcadapt #####
  guide_pcadapt = reactiveVal("To run pcadapt, input data must be data.frame file. \nYou need to upload a Site Info file (in rds), which can be downloaded from the 'Data Conversion' tab after you have convert the VCF to data.frame file. \nPlease click 'Run pcadapt' button.")
  pcadaptfileInfo = reactiveVal("")
  pcadapttitle1 = reactiveVal("")
  pcadapttitle2 = reactiveVal("")
  pcadapttitle3 = reactiveVal("")
  pcadapttitle4 = reactiveVal("")
  pcadapttitle5 = reactiveVal("")
  SNP_Info = reactiveVal(NULL)
  pcadapt_data = reactiveVal(NULL)
  pcadapt_data2 = reactiveVal(NULL)
  pcadaptplot1 = reactiveVal(NULL)
  pcadaptplot2 = reactiveVal(NULL)
  pcadaptplot3 = reactiveVal(NULL)
  pcadaptplot4 = reactiveVal(NULL) 
  
  output$fileSelection_pcadapt = renderUI({
    if (!is.null(df())){
      choices = c("data.frame file" = "df")
    } else {
      choices = ""
    }
    selectInput("Fileforpcadapt", "Dataset for pcadapt:", choices)
  })
  
  output$pcadaptfileInfo = renderText({
    req(df())
    paste("Number of samples: ", dim(df())[1], "\n",
          "Number of SNPs: ", dim(df())[2], "\n",
          "Type: ", class(df()),
          sep = "")
  })
  
  output$Site_Info2 = renderUI({
    fileInput("Site_Info2", "Site Info.* (required)", multiple = F, accept = c(".rds"))
  })
  
  observeEvent(input$Site_Info2, {
    req(input$Site_Info2)
    Site_Info = readRDS(input$Site_Info2$datapath)
    
    for (i in 1:length(unique(Site_Info$Chr))) {
      if (i>1){
        end = max(Site_Info[Site_Info$Chr == i-1, 2])
        Site_Info[Site_Info$Chr == i, 2] = Site_Info[Site_Info$Chr == i, 2] + end
      }
    }
    SNP_Info(Site_Info)
    Chr_axis = SNP_Info() %>% group_by(Chr) %>% summarize(center = (max(Pos) + min(Pos)) / 2)
    Chr_axis(Chr_axis)
  })
  
  observeEvent(input$SNPthin, {
    output$SNPthin_size = renderUI({
      sliderInput("pcadapt_size", "Window size (number of SNPs)", min = 0, max = 1000, value = 200, step = 10)
    })
    output$SNPthin_thr = renderUI({
      sliderInput("pcadapt_thr", HTML("r&sup2; threshold"), min = 0, max = 1, value = 0.1, step = 0.05)
    })
  })
  
  observeEvent(input$runpcadapt, {
    req(input$Fileforpcadapt)
    shinyjs::show("pcadaptStatus")
    data = switch(input$Fileforpcadapt, "df" = df())
    data = read.pcadapt(data, type = "lfmm")
    if (!is.null(input$pcadapt_size)){
      pcadapt_res = pcadapt(input = data, K = input$pcadapt_PC, LD.clumping = list(size = input$pcadapt_size, thr = input$pcadapt_thr))
    } else{
      pcadapt_res = pcadapt(input = data, K = input$pcadapt_PC)
    }
    
    pvalue = pcadapt_res$pvalues
    chi2.stat = pcadapt_res$chi2.stat
    pvalue[is.na(pvalue)] = 1
    chi2.stat[is.na(chi2.stat)] = mean(chi2.stat, na.rm = T)
    
    pcadapt_data = data.frame(
      pvalue = pvalue,
      observed = -log10(sort(pvalue)),
      expected = -log10(ppoints(length(pvalue))),
      statistic = chi2.stat)
    pcadapt_data(pcadapt_data)
    
    pcadaptplot2 = ggplot(pcadapt_data, aes(x = expected, y = observed)) +
      geom_point(size = 1, color = "#186da9") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.7) +
      labs(x = expression(Expected -log[10](italic(p))), y = expression(Observed -log[10](italic(p)))) +
      theme_classic() +
      theme(
        axis.title.x =   element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
    pcadaptplot2(pcadaptplot2)
    
    pcadaptplot3 = ggplot(pcadapt_data, aes(x = pvalue)) +
      geom_histogram(binwidth = 0.02, fill = "#186da9", color = "grey70", size = 0.2, alpha = 0.8) +
      theme_classic() +
      labs(x = expression(italic(p)), y = "Frequency") +
      scale_x_continuous(expand = c(0, 0.01), 
                         breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                         labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
      scale_y_continuous(expand = c(0.01, 0.01)) +
      theme(
        axis.title.x =   element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
    pcadaptplot3(pcadaptplot3)
    
    pcadaptplot4 = ggplot(pcadapt_data, aes(x = statistic)) +
      geom_histogram(aes(y = ..density..), binwidth = max(pcadapt_data$statistic)/50, fill = "#186da9", color = "grey70", size = 0.2, alpha = 0.8) +
      geom_density(color = "#cb1d2c", size = 1) +
      labs(x = "Test statistic", y = "Density") +
      theme_classic() +
      scale_x_continuous(expand = c(0.01, 0.01)) +
      scale_y_continuous(expand = c(0.01, 0)) +
      theme(
        axis.title.x =   element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
    pcadaptplot4(pcadaptplot4)
    
    shinyjs::hide("pcadaptStatus")
    pcadapttitle1("Manhattan plot")
    pcadapttitle2("QQ plot of p-values")
    pcadapttitle3("Histogram of p-values")
    pcadapttitle4("Histogram of test statistics")
    pcadapttitle5("Significant SNPs")
    guide_pcadapt("The pcadapt analysis is completed.")
  })
  
  observeEvent(input$resetpcadapt, {
    output$SNPthin_size = renderUI({ NULL })
    output$SNPthin_thr = renderUI({ NULL })
    pcadapt_data(NULL)
    pcadapt_data2(NULL)
    SNP_Info(NULL)
    pcadapttitle1("")
    pcadapttitle2("")
    pcadapttitle3("")
    pcadapttitle4("")
    pcadapttitle5("")
    showNotification("Data have been reset.")
    output$Site_Info2 = renderUI({
      fileInput("Site_Info2", "Site Info.* (required)", multiple = F, accept = c(".rds"))
    })
    guide_pcadapt("To run pcadapt, input data must be data.frame file. \nPlease click 'Run pcadapt' button.")
  })
  
  output$pcadaptplot1 = renderPlot({
    req(pcadapt_data(), input$pcadapt_adj)
    if (pcadapttitle1() == "Manhattan plot") {
      data = SNP_Info()
      pcadapt_data = pcadapt_data()
      Chr_axis = Chr_axis()
      
      method = pval_adj_method_choice[input$pcadapt_adj]
      
      padj = p.adjust(pcadapt_data$pvalue, method = as.character(method))
      outliers = which(padj < as.numeric(input$pcadapt_alpha))
      
      data = data %>% 
        mutate(pvalue = -log10(pcadapt_data$pvalue)) %>% 
        mutate(adjust_pvalue = padj) %>% 
        mutate(signif = ifelse(row_number() %in% outliers, "Yes", "No"))
      
      pcadaptplot1 = ggplot(data, aes(x = Pos, y = pvalue)) +
        geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
        scale_color_manual(values = rep(c("#cbbc1d", "#5e929d"), 50)) +
        scale_x_continuous(label = Chr_axis$Chr, breaks = Chr_axis$center, expand = c(0.01, 0)) +
        scale_y_continuous(expand = c(0.01, 0)) +
        theme_classic() +
        xlab("Chromosome") +
        ylab(expression(-log[10](italic(p)))) +
        theme(legend.position = "none",
              panel.border = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_text(size = 14)
        ) 
      data2 = subset(data, signif == "Yes")
      
      if (dim(data2)[1]>0){
        pcadaptplot1 = pcadaptplot1 +
          geom_point(data = data2, aes(Pos, pvalue), color = "red", size = 1, alpha = 0.9) +
          geom_hline(yintercept = min(data2$pvalue), color = "#ff4500", linetype = "dashed", linewidth = 0.6)
        data2$pvalue = 10^(-data2$pvalue)
      }
      pcadaptplot1(pcadaptplot1)
      pcadapt_data2 = data2
      pcadapt_data2(pcadapt_data2)
      pre_results = pre_results()
      pre_results[[45]] = "# Selection Sweep"
      pre_results[[46]] = paste0("The results of 'Genome Scans for Selection based on Principal Component Analysis (pcadapt)': ")
      pre_results[[47]] = paste0(dim(pcadapt_data2)[1] , " significant selection signatures (SNP loci) were detected across ", 
                                 length(table(pcadapt_data2[,1])), " chromosomes with the ", 
                                 input$pcadapt_adj, " P-value adjustment method at α = ", input$pcadapt_alpha)
      pre_results(pre_results)
      pcadaptplot1()
    }
  })
  
  output$pcadapt_adj = renderUI({
    if (pcadapttitle1() == "Manhattan plot") {
      selectInput("pcadapt_adj", "P-value adjustment method", choices = names(pval_adj_method_choice), selected = "Benjamini & Hochberg (FDR)")
    }
  })
  
  output$pcadapt_alpha = renderUI({
    if (pcadapttitle1() == "Manhattan plot") {
      selectInput("pcadapt_alpha", "Level of significance (α)", choices = c(0.1, 0.05, 0.01, 0.005, 0.001), selected = 0.1)
    }
  })
  
  output$pcadapt_result = renderText({
    req(pcadapt_data2())
    chr = table(pcadapt_data2()[,1])
    out = c()
    for (i in seq_len(length(chr))) {
      out[i] = paste0("Chr ", names(chr)[i], ": ", as.numeric(chr[i]), " significant SNPs", "\n")
    }
    out = paste(out, collapse = "")
    paste("Number of significant SNPs: ", dim(pcadapt_data2())[1], "\n",
          "---------------", "\n",
          out,
          sep = "")
  })
  
  output$download_pcadapt_plot1 = renderUI({
    if (pcadapttitle1() == "Manhattan plot") {
      downloadButton("Dpcadapt_plot1", "Download Plot")
    }
  })
  
  output$Dpcadapt_plot1 = downloadHandler(
    filename = "pcadapt_Manhattan_Plot.pdf",
    content = function(file) {
      pdf(file, width = 10, height = 4)
      print(pcadaptplot1())
      dev.off()
    }
  )
  
  output$pcadaptplot2 = renderPlot({
    req(pcadapt_data())
    if (pcadapttitle2() == "QQ plot of p-values") {
      pcadaptplot2()
    }
  })
  
  output$download_pcadapt_plot2 = renderUI({
    if (pcadapttitle2() == "QQ plot of p-values") {
      downloadButton("Dpcadapt_plot2", "Download Plot")
    }
  })
  
  output$Dpcadapt_plot2 = downloadHandler(
    filename = "pcadapt_QQ_Plot.pdf",
    content = function(file) {
      pdf(file, width = 4, height = 4)
      print(pcadaptplot2())
      dev.off()
    }
  )
  
  output$pcadaptplot3 = renderPlot({
    req(pcadapt_data())
    if (pcadapttitle2() == "QQ plot of p-values") {
      pcadaptplot3()
    }
  })
  
  output$download_pcadapt_plot3 = renderUI({
    if (pcadapttitle2() == "QQ plot of p-values") {
      downloadButton("Dpcadapt_plot3", "Download Plot")
    }
  })
  
  output$Dpcadapt_plot3 = downloadHandler(
    filename = "pcadapt_Histogram_of_pvalue.pdf",
    content = function(file) {
      pdf(file, width = 4, height = 4)
      print(pcadaptplot3())
      dev.off()
    }
  )
  
  output$pcadaptplot4 = renderPlot({
    req(pcadapt_data())
    if (pcadapttitle2() == "QQ plot of p-values") {
      pcadaptplot4()
    }
  })
  
  output$download_pcadapt_plot4 = renderUI({
    if (pcadapttitle2() == "QQ plot of p-values") {
      downloadButton("Dpcadapt_plot4", "Download Plot")
    }
  })
  
  output$Dpcadapt_plot4 = downloadHandler(
    filename = "pcadapt_Histogram_of_Test_Statistics.pdf",
    content = function(file) {
      pdf(file, width = 4, height = 4)
      print(pcadaptplot4())
      dev.off()
    }
  )
  
  output$pcadapt_Sign_SNP = DT::renderDataTable({
    req(pcadapt_data())
    DT::datatable(pcadapt_data2(), options = list(pageLength = 10))
  })
  
  output$download_pcadapt_results = renderUI({
    if (pcadapttitle2() == "QQ plot of p-values") {
      downloadButton("Dpcadapt_results", "Download Table")
    }
  })
  
  output$Dpcadapt_results = downloadHandler(
    filename = "pcadapt_Significant_SNPs.csv",
    content = function(file) {
      write.csv(pcadapt_data2(), file, row.names = FALSE)
    }
  )
  
  output$guide_pcadapt = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_pcadapt())
  })
  
  output$pcadapttitle1 = renderText({ pcadapttitle1() })
  output$pcadapttitle2 = renderText({ pcadapttitle2() })
  output$pcadapttitle3 = renderText({ pcadapttitle3() })
  output$pcadapttitle4 = renderText({ pcadapttitle4() })
  output$pcadapttitle5 = renderText({ pcadapttitle5() })
  
  ##### OutFLANK #####
  guide_OutFLANK = reactiveVal("To run OutFLANK, input data must be genind file with 'Group Info.'. \nYou need to upload a Site Info file (in rds), which can be downloaded from the 'Data Conversion' tab after you have convert the VCF to data.frame file. \nPlease click 'Run OutFLANK' button.")
  OutFLANKfileInfo = reactiveVal("")
  OutFLANKtitle1 = reactiveVal("")
  OutFLANKtitle2 = reactiveVal("")
  OutFLANKtitle3 = reactiveVal("")
  OutFLANKtitle4 = reactiveVal("")
  OutFLANKtitle5 = reactiveVal("")
  outflank = reactiveVal(NULL)
  outflank_data2 = reactiveVal(NULL)
  outflank_data3 = reactiveVal(NULL)
  Chr_axis = reactiveVal(NULL)
  OutFLANKplot1 = reactiveVal(NULL)
  OutFLANKplot2 = reactiveVal(NULL)
  OutFLANKplot3 = reactiveVal(NULL)
  OutFLANKplot4 = reactiveVal(NULL) 
  OutFLANKplot5 = reactiveVal(NULL)
  
  output$fileSelection_OutFLANK = renderUI({
    if (!is.null(gi())){ choices = c("genind file" = "gi") } else { choices = "" }
    selectInput("FileforOutFLANK", "Dataset for OutFLANK:", choices)
  })
  
  output$OutFLANKfileInfo = renderText({
    req(gi())
    group_info = ifelse(is.null(gi()@pop), "NaN \n**Warning** Not available!", "Added")
    paste0(
      "Number of samples: ", length(gi()@ploidy), "\n",
      "Number of SNPs: ", length(gi()@all.names), "\n",
      "Type: ", class(gi()), "\n",
      "Group Info.: ", group_info
    )
  })
  
  output$Site_Info3 = renderUI({
    fileInput("Site_Info3", "Site Info.* (required)", multiple = F, accept = c(".rds"))
  })
  
  observeEvent(input$Site_Info3, {
    req(input$Site_Info3)
    Site_Info = readRDS(input$Site_Info3$datapath)
    
    for (i in 1:length(unique(Site_Info$Chr))) {
      if (i>1){
        end = max(Site_Info[Site_Info$Chr == i-1, 2])
        Site_Info[Site_Info$Chr == i, 2] = Site_Info[Site_Info$Chr == i, 2] + end
      }
    }
    SNP_Info(Site_Info)
    Chr_axis = SNP_Info() %>% group_by(Chr) %>% summarize(center = (max(Pos) + min(Pos)) / 2)
    Chr_axis(Chr_axis)
  })
  
  observeEvent(input$runOutFLANK, {
    req(input$FileforOutFLANK, gi()@pop, SNP_Info())
    shinyjs::show("OutFLANKStatus")
    data = switch(input$FileforOutFLANK, "gi" = gi())
    outflank = gl.outflank(data, plot = FALSE)
    outflank(outflank)
    
    pvalue = outflank$outflank$results$pvaluesRightTail
    FST = outflank$outflank$results$FST
    pvalue[is.na(pvalue)] = 1
    FST[is.na(FST)] = mean(FST, na.rm = T)
    
    outflank_data = data.frame(
      pvalue = pvalue,
      observed = -log10(sort(pvalue)),
      expected = -log10(ppoints(length(pvalue))),
      FST = FST)
    
    OutFLANKplot3 = ggplot(outflank_data, aes(x = expected, y = observed)) +
      geom_point(size = 1, color = "#186da9") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.7) +
      labs(x = expression(Expected -log[10](italic(p))), y = expression(Observed -log[10](italic(p)))) +
      theme_classic() +
      theme(
        axis.title.x =   element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
    OutFLANKplot3(OutFLANKplot3)
    
    OutFLANKplot4 = ggplot(outflank_data, aes(x = pvalue)) +
      geom_histogram(binwidth = 0.02, fill = "#186da9", color = "grey70", size = 0.2, alpha = 0.8) +
      theme_classic() +
      labs(x = expression(italic(p)), y = "Frequency") +
      scale_x_continuous(expand = c(0, 0.01), 
                         breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                         labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
      scale_y_continuous(expand = c(0.01, 0.01)) +
      theme(
        axis.title.x =   element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
    OutFLANKplot4(OutFLANKplot4)
    
    OutFLANKplot5 = ggplot(outflank_data, aes(x = FST)) +
      geom_histogram(binwidth = 0.02, fill = "#186da9", color = "grey70", size = 0.2, alpha = 0.8) +
      theme_classic() +
      labs(x = expression(F[ST]), y = "Frequency") +
      scale_x_continuous(expand = c(0, 0.01), 
                         breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                         labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
      scale_y_continuous(expand = c(0.01, 0.01)) +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
    OutFLANKplot5(OutFLANKplot5)
    
    
    shinyjs::hide("OutFLANKStatus")
    OutFLANKtitle1("Manhattan plot")
    OutFLANKtitle2("QQ plot of p-values")
    OutFLANKtitle3("Histogram of p-values")
    OutFLANKtitle4("Histogram of FST")
    OutFLANKtitle5("Significant SNPs")
    guide_OutFLANK("The OutFLANK analysis is completed.")
  })
  
  observeEvent(input$resetOutFLANK, {
    guide_OutFLANK("To run OutFLANK, input data must be genind file with 'Group Info.'. \nPlease click 'Run OutFLANK' button.")
    OutFLANKfileInfo("")
    OutFLANKtitle1("")
    OutFLANKtitle2("")
    OutFLANKtitle3("")
    OutFLANKtitle4("")
    OutFLANKtitle5("")
    outflank(NULL)
    outflank_data2(NULL)
    outflank_data3(NULL)
    Chr_axis(NULL)
    OutFLANKplot1(NULL)
    OutFLANKplot2(NULL)
    OutFLANKplot3(NULL)
    OutFLANKplot4(NULL) 
    OutFLANKplot5(NULL)
    showNotification("Data have been reset.")
    output$Site_Info3 = renderUI({
      fileInput("Site_Info3", "Site Info.* (required)", multiple = F, accept = c(".rds"))
    })
  })
  
  output$OutFLANKplot1 = renderPlot({
    req(outflank(), Chr_axis(), input$OutFLANK_adj)
    if (OutFLANKtitle1() == "Manhattan plot") {
      data = SNP_Info()
      outflank = outflank()
      Chr_axis = Chr_axis()
      
      method = pval_adj_method_choice[input$OutFLANK_adj]
      padj = p.adjust(outflank$outflank$results$pvaluesRightTail, method = as.character(method))
      outliers = which(padj < as.numeric(input$OutFLANK_alpha))
      
      outflank_data3 = data %>% 
        mutate(pvalue = -log10(outflank$outflank$results$pvaluesRightTail)) %>% 
        mutate(adjust_pvalue = padj) %>% 
        mutate(FST = outflank$outflank$results$FST) %>% 
        mutate(signif = ifelse(row_number() %in% outliers, "Yes", "No"))
      outflank_data3$FST <- ifelse(outflank_data3$FST < 0, 0, outflank_data3$FST)
      outflank_data3(outflank_data3)
      
      OutFLANKplot1 = ggplot(outflank_data3, aes(x = Pos, y = pvalue)) +
        geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
        scale_color_manual(values = rep(c("#cbbc1d", "#5e929d"), 50)) +
        scale_x_continuous(label = Chr_axis$Chr, breaks = Chr_axis$center, expand = c(0.01, 0)) +
        scale_y_continuous(expand = c(0.01, 0)) +
        theme_classic() +
        xlab("Chromosome") +
        ylab(expression(-log[10](italic(p)))) +
        theme(legend.position = "none",
              panel.border = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_text(size = 14)
        ) 
      data2 = subset(outflank_data3, signif == "Yes")
      
      if (dim(data2)[1]>0){
        OutFLANKplot1 = OutFLANKplot1 +
          geom_point(data = data2, aes(Pos, pvalue), color = "red", size = 1, alpha = 0.9) +
          geom_hline(yintercept = min(data2$pvalue), color = "#ff4500", linetype = "dashed", linewidth = 0.6)
        data2$pvalue = 10^(-data2$pvalue)
      }
      OutFLANKplot1(OutFLANKplot1)
      outflank_data2 = data2
      outflank_data2(outflank_data2)
      pre_results = pre_results()
      pre_results[[45]] = "# Selection Sweep"
      pre_results[[49]] = paste0("The results of 'Identifies loci under selection per population (OutFLANK)': ")
      pre_results[[50]] = paste0(dim(outflank_data2)[1] , " significant selection signatures (SNP loci) were detected across ", 
                                 length(table(outflank_data2[,1])), " chromosomes with the ", 
                                 input$OutFLANK_adj, " P-value adjustment method at α = ", input$OutFLANK_alpha)
      pre_results(pre_results)
      OutFLANKplot1()
    }
  })
  
  output$OutFLANKplot2 = renderPlot({
    req(outflank_data3(), Chr_axis())
    data = outflank_data3()
    Chr_axis = Chr_axis()
    OutFLANKplot2 = ggplot(data, aes(x = Pos, y = FST)) +
      geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
      scale_color_manual(values = rep(c("#cbbc1d", "#5e929d"), 50)) +
      scale_x_continuous(label = Chr_axis$Chr, breaks = Chr_axis$center, expand = c(0.01, 0)) +
      scale_y_continuous(expand = c(0.01, 0)) +
      theme_classic() +
      xlab("Chromosome") +
      ylab(expression(F[ST])) +
      theme(legend.position = "none",
            panel.border = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14)
      )
    OutFLANKplot2(OutFLANKplot2)
    OutFLANKplot2()
  })
  
  output$OutFLANK_adj = renderUI({
    if (OutFLANKtitle1() == "Manhattan plot") {
      selectInput("OutFLANK_adj", "P-value adjustment method", choices = names(pval_adj_method_choice), selected = "Benjamini & Hochberg (FDR)")
    }
  })
  
  output$OutFLANK_alpha = renderUI({
    if (OutFLANKtitle1() == "Manhattan plot") {
      selectInput("OutFLANK_alpha", "Level of significance (α)", choices = c(0.1, 0.05, 0.01, 0.005, 0.001), selected = 0.1)
    }
  })
  
  output$OutFLANK_result = renderText({
    req(outflank_data2())
    chr = table(outflank_data2()[,1])
    out = c()
    for (i in seq_len(length(chr))) {
      out[i] = paste0("Chr ", names(chr)[i], ": ", as.numeric(chr[i]), " significant SNPs", "\n")
    }
    out = paste(out, collapse = "")
    paste("Number of significant SNPs: ", dim(outflank_data2())[1], "\n",
          "---------------", "\n",
          out,
          sep = "")
  })
  
  output$download_OutFLANK_plot1 = renderUI({
    if (OutFLANKtitle1() == "Manhattan plot") {
      downloadButton("DOutFLANK_plot1", "Download Plot")
    }
  })
  
  output$DOutFLANK_plot1 = downloadHandler(
    filename = "OutFLANK_Manhattan_Plot_pvalue.pdf",
    content = function(file) {
      pdf(file, width = 10, height = 4)
      print(OutFLANKplot1())
      dev.off()
    }
  )
  
  output$download_OutFLANK_plot2 = renderUI({
    if (OutFLANKtitle1() == "Manhattan plot") {
      downloadButton("DOutFLANK_plot2", "Download Plot")
    }
  })
  
  output$DOutFLANK_plot2 = downloadHandler(
    filename = "OutFLANK_Manhattan_Plot_FST.pdf",
    content = function(file) {
      pdf(file, width = 10, height = 4)
      print(OutFLANKplot2())
      dev.off()
    }
  )
  
  output$OutFLANKplot3 = renderPlot({
    req(outflank())
    if (OutFLANKtitle2() == "QQ plot of p-values") {
      OutFLANKplot3()
    }
  })
  
  output$OutFLANKplot4 = renderPlot({
    req(outflank())
    if (OutFLANKtitle2() == "QQ plot of p-values") {
      OutFLANKplot4()
    }
  })
  
  output$OutFLANKplot5 = renderPlot({
    req(outflank())
    if (OutFLANKtitle2() == "QQ plot of p-values") {
      OutFLANKplot5()
    }
  })
  
  output$download_OutFLANK_plot3 = renderUI({
    if (OutFLANKtitle2() == "QQ plot of p-values") {
      downloadButton("DOutFLANK_plot3", "Download Plot")
    }
  })
  
  output$DOutFLANK_plot3 = downloadHandler(
    filename = "OutFLANK_QQ_Plot.pdf",
    content = function(file) {
      pdf(file, width = 4, height = 4)
      print(OutFLANKplot3())
      dev.off()
    }
  )
  
  output$download_OutFLANK_plot4 = renderUI({
    if (OutFLANKtitle2() == "QQ plot of p-values") {
      downloadButton("DOutFLANK_plot4", "Download Plot")
    }
  })
  
  output$DOutFLANK_plot4 = downloadHandler(
    filename = "OutFLANK_Histogram_of_pvalue.pdf",
    content = function(file) {
      pdf(file, width = 4, height = 4)
      print(OutFLANKplot4())
      dev.off()
    }
  )
  
  output$download_OutFLANK_plot5 = renderUI({
    if (OutFLANKtitle2() == "QQ plot of p-values") {
      downloadButton("DOutFLANK_plot5", "Download Plot")
    }
  })
  
  output$DOutFLANK_plot5 = downloadHandler(
    filename = "OutFLANK_Histogram_of_FST.pdf",
    content = function(file) {
      pdf(file, width = 4, height = 4)
      print(OutFLANKplot5())
      dev.off()
    }
  )
  
  output$OutFLANK_Sign_SNP = DT::renderDataTable({
    req(outflank())
    DT::datatable(outflank_data2(), options = list(pageLength = 10))
  })
  
  output$download_OutFLANK_Sign_SNP = renderUI({
    if (OutFLANKtitle2() == "QQ plot of p-values") {
      downloadButton("DOutFLANK_Sign_SNP", "Download Table")
    }
  })
  
  output$DOutFLANK_Sign_SNP = downloadHandler(
    filename = "OutFLANK_Significant_SNPs.csv",
    content = function(file) {
      write.csv(outflank_data2(), file, row.names = FALSE)
    }
  )
  
  output$guide_OutFLANK = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_OutFLANK())
  })
  
  output$OutFLANKtitle1 = renderText({ OutFLANKtitle1() })
  output$OutFLANKtitle2 = renderText({ OutFLANKtitle2() })
  output$OutFLANKtitle3 = renderText({ OutFLANKtitle3() })
  output$OutFLANKtitle4 = renderText({ OutFLANKtitle4() })
  output$OutFLANKtitle5 = renderText({ OutFLANKtitle5() })
  
  ##### IBS #####
  guide_IBS = reactiveVal("To run IBS, input data must be data.frame file. \nYou need to upload a Site Info file (in rds), which can be downloaded from the 'Data Conversion' tab after you have convert the VCF to data.frame file. \nPlease click 'Run IBS' button")
  IBSfileInfo = reactiveVal("")
  IBStitle1 = reactiveVal("")
  IBStitle2 = reactiveVal("")
  IBS_result = reactiveVal()
  IBSplot = reactiveVal()
  
  output$fileSelection_IBS = renderUI({
    if (!is.null(df())){
      choices = c("data.frame file" = "df")
      updateSelectInput(session, "REF", choices = row.names(df()), selected = row.names(df())[1])
      updateSelectInput(session, "COMPAR", choices = row.names(df()), selected = row.names(df())[2])
    } else {
      choices = ""
    }
    selectInput("FileforIBS", "Dataset for IBS:", choices)
  })
  
  output$IBSfileInfo = renderText({
    req(df())
    paste("Number of samples: ", dim(df())[1], "\n",
          "Number of SNPs: ", dim(df())[2], "\n",
          "Type: ", class(df()),
          sep = "")
  })
  
  output$Site_Info4 = renderUI({
    fileInput("Site_Info4", "Site Info.* (required)", multiple = F, accept = c(".rds"))
  })
  
  observeEvent(input$Site_Info4, {
    req(input$Site_Info4)
    Site_Info = readRDS(input$Site_Info4$datapath)
    Site_Info(Site_Info)
  })
  
  output$Chr_Info2 = renderUI({
    fileInput("Chr_Info2", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$Chr_Info2, {
    Chr_Info = read.csv(input$Chr_Info2$datapath)
    Chr_Info(Chr_Info)
  })
  
  
  observeEvent(input$runIBS, {
    req(input$FileforIBS, Site_Info())
    shinyjs::show("IBSStatus")
    data = switch(input$FileforIBS, "df" = df())
    progressVal = reactiveVal(NULL)
    IBS_result = IBS_analysis(data, Site_Info(), input$REF, input$COMPAR, Sliding.window = TRUE, 
                              window.size = input$WindowSize2*1000, step.size = input$StepSize2*1000, remove_RM = input$rmH)
    IBS_result(IBS_result)
    shinyjs::hide("IBSStatus")
    IBStitle1("Chromosome ideogram")
    IBStitle2("Sliding window data")
    guide_IBS("The IBS analysis is completed.")
  })
  
  output$IBSres = renderText({
    req(IBS_result())
    if (IBStitle1() == "Chromosome ideogram") {
      IBS_result = IBS_result()
      paste0("Reference: ", input$REF, "\n",
             "Comparison: ", input$COMPAR, "\n",
             "---------------------", "\n",
             "Missing SNPs: ", IBS_result$Num_NA_SNPs, "\n",
             "Different SNPs: ", IBS_result$Num_Diff_SNPs, " (",round(IBS_result$Num_Diff_SNPs/IBS_result$Num_Total_SNPs, 4)*100, "%)" ,"\n",
             "Identical SNPs (IBS): ", IBS_result$Num_Ide_SNPs, " (",round(IBS_result$Num_Ide_SNPs/IBS_result$Num_Total_SNPs, 4)*100, "%)"
      )
    }
  })
  
  output$IBSplot = renderPlot({
    req(IBS_result(), Chr_Info())
    if (IBStitle1() == "Chromosome ideogram") {
      Chr_Info = Chr_Info()
      IBS_data = IBS_result()
      
      Chr_Info$Length = Chr_Info$End - Chr_Info$Start
      window_data = IBS_data$window_data
      MB = seq(0, 300, by = 20)
      linewidth = c(32,32,32,32,32,29,25,20,19,17,
                    15,14,13,12,12,11,10,10,9,9,
                    9,8,8,8,8,7,7,7,7,7,
                    6,6,6,6,6,rep(5,10))
      nchr = length(unique(Chr_Info$Chr))
      
      IBSplot = ggplot() +
        geom_bar(data = Chr_Info, aes(x = Chr, y = Length), stat = "identity", fill = "grey90", width = 0.5, alpha = 0.9) +
        scale_x_discrete(expand = c(0, 0.3)) +
        scale_y_continuous("Position (Mb)", 
                           breaks = MB*10^6,
                           labels = MB,
                           expand = c(0, 0)) +
        labs(x = "Chromosome") +
        theme_classic() +
        theme(axis.title.x =  element_text(size = 14),
              axis.title.y = element_text(size = 14, vjust = 2),
              axis.text.x = element_text(size = 12, angle = 60, vjust = 0.5),
              axis.text.y = element_text(size = 12),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(linewidth = 0.7),
              axis.ticks.y = element_line(linewidth = 0.7)) 
      IBSplot = IBSplot +
        geom_rect(data = window_data, aes(xmin = Chr, xmax = Chr, ymin = Start, ymax = End, color = IBS_ratio),
                  alpha = 0.5, linewidth = linewidth[nchr]) +
        scale_color_gradient(name = "IBS Ratio", low = "#ffd9d9", high = "darkred") +
        guides(color = guide_colorbar(title.position = "top", title.vjust = 1, label.theme = element_text(size = 12))) +
        theme(legend.title = element_text(size = 14))
      IBSplot(IBSplot)
      IBSplot
    }
  })
  
  observeEvent(input$resetIBS, {
    IBS_result(NULL)
    IBStitle1("")
    IBStitle2("")
    showNotification("Data have been reset.")
    output$Site_Info4 = renderUI({
      fileInput("Site_Info4", "Site Info.* (required)", multiple = F, accept = c(".rds"))
    })
    output$Chr_Info2 = renderUI({
      fileInput("Chr_Info2", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
    })
    guide_IBS = reactiveVal("To run IBS, input data must be data.frame file. \nPlease click 'Run IBS' button")
  })
  
  output$download_IBS_plot = renderUI({
    if (IBStitle1() == "Chromosome ideogram") {
      downloadButton("DIBS_plot", "Download Plot")
    }
  })
  
  output$DIBS_plot = downloadHandler(
    filename = "IBS_Chromosome_Ideogram.pdf",
    content = function(file) {
      pdf(file, width = 12, height = 5)
      print(IBSplot())
      dev.off()
    }
  )
  
  output$IBS_SW = DT::renderDataTable({
    req(IBS_result())
    DT::datatable(IBS_result()$window_data, options = list(pageLength = 10))
  })
  
  output$download_IBS_SW = renderUI({
    if (IBStitle1() == "Chromosome ideogram") {
      downloadButton("DIBS_SW", "Download Table")
    }
  })
  
  output$DIBS_SW = downloadHandler(
    filename = "IBS_Sliding_Window.csv",
    content = function(file) {
      write.csv(IBS_result()$window_data, file, row.names = FALSE)
    }
  )
  
  output$guide_IBS = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_IBS())
  })
  
  output$IBStitle1 = renderText({ IBStitle1() })
  output$IBStitle2 = renderText({ IBStitle2() })
  
  ##### Page 7: Core Collection #####
  ##### Core Sample Set #####
  guide_CoreSample = reactiveVal("To run core sample set, input data must be data.frame file. \nPlease click 'Run Core' button.")
  CoreSamplefileInfo = reactiveVal("")
  CoreSampletitle1 = reactiveVal("")
  CoreSampletitle2 = reactiveVal("")
  core_sample_coverage = reactiveVal(NULL)
  core_sample_dataset = reactiveVal(NULL)
  core_sample_info = reactiveVal(NULL)
  CoreSampleplot = reactiveVal(NULL)
  
  output$fileSelection_CoreSample = renderUI({
    if (!is.null(df())){
      choices = c("data.frame file" = "df")
    } else {
      choices = ""
    }
    selectInput("FileforCoreSample", "Dataset for core sample set:", choices)
  })
  
  observeEvent(input$runCoreSample, {
    req(input$FileforCoreSample)
    shinyjs::show("CoreSampleStatus")
    data = switch(input$FileforCoreSample, "df" = df())
    data = as.data.frame(t(data))
    core_sample = core.set(data, coverage = as.numeric(input$coverage), difference = as.numeric(input$diff))
    core_sample_coverage(core_sample$coverage.table)
    core_sample_dataset(t(core_sample$coreset))
    
    core_sample_info = data.frame("ID" = colnames(data),
                                  "Core_sample" = ifelse(colnames(data) %in% core_sample_coverage()[,2], "TRUE", "FALSE"))
    core_sample_info(core_sample_info)
    
    shinyjs::hide("CoreSampleStatus")
    CoreSampletitle1("Core sample set")
    CoreSampletitle2("Coverage plot of core sample set")
    guide_CoreSample("The core sample set is completed.")
    
    pre_results = pre_results()
    pre_results[[51]] = "# Core Collection"
    pre_results[[52]] = "Core Sample Set"
    pre_results[[53]] = paste0("Number of core samples: ", length(core_sample_coverage()[,2]), " (", round(length(core_sample_coverage()[,2])/dim(df())[1], 4)* 100, "%)")
    pre_results[[54]] = paste0("Total coverage: ", max(as.numeric(core_sample_coverage()[,3])), "%")
    pre_results(pre_results)
  })
  
  observeEvent(input$resetCoreSample, {
    core_sample_coverage(NULL)
    core_sample_dataset(NULL)
    CoreSampletitle1("")
    CoreSampletitle2("")
    showNotification("Data have been reset.")
    guide_CoreSample("To run core sample set, input data must be data.frame file. \nPlease click 'Run Core' button.")
  })
  
  output$CoreSamplefileInfo = renderText({
    req(df())
    paste("Number of samples: ", dim(df())[1], "\n",
          "Number of SNPs: ", dim(df())[2], "\n",
          "Type: ", class(df()),
          sep = "")
  })
  
  output$CoreSampleres = renderText({
    req(core_sample_coverage())
    if (CoreSampletitle2() == "Coverage plot of core sample set") {
      paste0("Number of core samples: ", length(core_sample_coverage()[,2]), " (", round(length(core_sample_coverage()[,2])/dim(df())[1], 4)* 100, "%)", "\n",
             "Total coverage: ", max(as.numeric(core_sample_coverage()[,3])), "%", "\n",
             "Core sample IDs: ", paste(as.character(core_sample_coverage()[,2]), collapse = "; "))
    }
  })
  
  output$CoreSampleplot = renderPlot({
    req(input$coverage, input$diff, core_sample_coverage())
    if (CoreSampletitle2() == "Coverage plot of core sample set") {
      data = core_sample_coverage()
      data$Iteration = as.numeric(data$Iteration)
      data$Coverage = as.numeric(data$Coverage)
      data$Difference = as.numeric(data$Difference)
      ylim.cov = c(0, 100)
      ylim.diff = c(0, max(data$Difference))     
      b = diff(ylim.diff)/diff(ylim.cov) 
      a = ylim.diff[1] - b*(ylim.cov)[1]
      
      CoreSampleplot = ggplot(data, aes(x = Iteration, y = Difference)) +
        geom_bar(stat = "identity", show.legend = FALSE, fill = "#173B45", width = 0.8) +
        geom_line(aes(y = a + Coverage*b), color = "#FF8225", lwd = 2) +
        geom_point(aes(y = a + Coverage*b), color = "#FF8225", size = 4, shape = 21, fill = "white") +
        scale_y_continuous("Difference (%)", limits = c(0, max(data$Difference)), sec.axis = sec_axis(~ (. - a)/b, name = "Coverage (%)")) +
        theme_classic() +
        scale_x_continuous(
          breaks = data$Iteration,
          labels = paste0(data$Iteration, ": ", data$ID)
        ) +
        labs(x = "Iteration: Sample ID", color = "", fill = "") +
        theme(
          axis.title.x =   element_text(size = 18),
          axis.title.y.left = element_text(size = 18, color = "#173B45"),
          axis.title.y.right = element_text(size = 18, color = "#d65b00"),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y.left = element_text(size = 13, color = "#173B45"), 
          axis.text.y.right = element_text(size = 13, color = "#d65b00")) +
        theme(legend.position = "none")
      
      CoreSampleplot(CoreSampleplot)
      CoreSampleplot()
    }
  })
  
  output$download_CoreSample_plot = renderUI({
    if (CoreSampletitle2() == "Coverage plot of core sample set") {
      downloadButton("DCoreSample_plot", "Download Plot")
    }
  })
  
  output$DCoreSample_plot = downloadHandler(
    filename = function() {
      paste0("Core_Sample_Plot-", "coverage",input$coverage, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 10, height = 8)
      print(CoreSampleplot())
      dev.off()
    }
  )
  
  output$download_core_sample_coverage = renderUI({
    if (CoreSampletitle1() == "Core sample set") {
      downloadButton("Dcore_sample_coverage", "Download Coverage Data")
    }
  })
  
  output$Dcore_sample_coverage = downloadHandler(
    filename = "Core_Sample_Coverage_Data.csv",
    content = function(file) {
      write.csv(core_sample_coverage(), file, row.names = FALSE)
    }
  )
  
  output$download_core_sample_dataset = renderUI({
    if (CoreSampletitle1() == "Core sample set") {
      downloadButton("Dcore_sample_dataset", "Download Core Set in data.frame")
    }
  })
  
  output$Dcore_sample_dataset = downloadHandler(
    filename = "df_Core_Sample_Core_Set.rds",
    content = function(file) {
      saveRDS(core_sample_dataset(), file)
    }
  )
  
  output$download_core_sample_info = renderUI({
    if (CoreSampletitle1() == "Core sample set") {
      downloadButton("D_core_sample_info", "Download Core Sample Info.")
    }
  })
  
  output$D_core_sample_info = downloadHandler(
    filename = "Core_Sample_Info.csv",
    content = function(file) {
      write.csv(core_sample_info(), file, row.names = FALSE)
    }
  )
  
  output$guide_CoreSample = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_CoreSample())
  })
  
  output$CoreSampletitle1 = renderText({ CoreSampletitle1() })
  
  output$CoreSampletitle2 = renderText({ CoreSampletitle2() })
  
  ##### Core SNP Set #####
  guide_CoreSNP = reactiveVal("To run core SNP set, input data must be data.frame file. \nPlease click 'Run Core' button.")
  CoreSNPfileInfo = reactiveVal("")
  CoreSNPtitle1 = reactiveVal("")
  CoreSNPtitle2 = reactiveVal("")
  core_SNP_coverage = reactiveVal(NULL)
  core_SNP_dataset = reactiveVal(NULL)
  core_SNP_info = reactiveVal(NULL)
  CoreSNPplot = reactiveVal(NULL)
  
  output$fileSelection_CoreSNP = renderUI({
    if (!is.null(df())){
      choices = c("data.frame file" = "df")
    } else {
      choices = ""
    }
    selectInput("FileforCoreSNP", "Dataset for core SNP set:", choices)
  })
  
  observeEvent(input$runCoreSNP, {
    req(input$FileforCoreSNP)
    shinyjs::show("CoreSNPStatus")
    guide_CoreSNP("Running...")
    data = switch(input$FileforCoreSNP, "df" = df())
    core_SNP= core.set(data, coverage = input$coverage2, difference = as.numeric(input$diff2))
    core_SNP_coverage(core_SNP$coverage.table)
    core_SNP_dataset(core_SNP$coreset)
    
    core_SNP_info = data.frame("ID" = colnames(data),
                               "Core_SNP" = ifelse(colnames(data) %in% core_SNP_coverage()[,2], "TRUE", "FALSE"))
    core_SNP_info(core_SNP_info)
    
    shinyjs::hide("CoreSNPStatus")
    CoreSNPtitle1("Core SNP set")
    CoreSNPtitle2("Coverage plot of core SNP set")
    guide_CoreSNP("The core SNP set is completed.")
    
    pre_results = pre_results()
    pre_results[[51]] = "# Core Collection"
    pre_results[[55]] = "Core SNPs set"
    pre_results[[56]] = paste0("Number of core SNPs: ", length(core_SNP_coverage()[,2]), " (", round(length(core_SNP_coverage()[,2])/dim(df())[1], 4)* 100, "%)")
    pre_results[[57]] = paste0("Total coverage: ", max(as.numeric(core_SNP_coverage()[,3])), "%")
    pre_results(pre_results)
  })
  
  observeEvent(input$resetCoreSNP, {
    core_SNP_coverage(NULL)
    core_SNP_dataset(NULL)
    CoreSNPtitle1("")
    CoreSNPtitle2("")
    showNotification("Data have been reset.")
    guide_CoreSNP("To run core SNP set, input data must be data.frame file. \nPlease click 'Run Core' button.")
  })
  
  output$CoreSNPfileInfo = renderText({
    req(df())
    paste("Number of samples: ", dim(df())[1], "\n",
          "Number of SNPs: ", dim(df())[2], "\n",
          "Type: ", class(df()),
          sep = "")
  })
  
  output$CoreSNPres = renderText({
    req(core_SNP_coverage())
    if (CoreSNPtitle2() == "Coverage plot of core SNP set") {
      paste0("Number of core SNPs: ", length(core_SNP_coverage()[,2]), " (", round(length(core_SNP_coverage()[,2])/dim(df())[2], 4)* 100, "%)", "\n",
             "Total coverage: ", max(as.numeric(core_SNP_coverage()[,3])), "%", "\n",
             "Core SNP IDs: ", paste(as.character(core_SNP_coverage()[,2]), collapse = "; "))
    }
  })
  
  output$CoreSNPplot = renderPlot({
    req(input$coverage2, input$diff2, core_SNP_coverage())
    if (CoreSNPtitle2() == "Coverage plot of core SNP set") {
      data = core_SNP_coverage()
      data$Iteration = as.numeric(data$Iteration)
      data$Coverage = as.numeric(data$Coverage)
      data$Difference = as.numeric(data$Difference)
      ylim.cov = c(0, 100)
      ylim.diff = c(0, max(data$Difference))     
      b = diff(ylim.diff)/diff(ylim.cov) 
      a = ylim.diff[1] - b*(ylim.cov)[1]
      
      CoreSNPplot = ggplot(data, aes(x = Iteration, y = Difference)) +
        geom_bar(stat = "identity", show.legend = FALSE, fill = "#173B45", width = 0.8) +
        geom_line(aes(y = a + Coverage*b), color = "#FF8225", lwd = 2) +
        geom_point(aes(y = a + Coverage*b), color = "#FF8225", size = 4, shape = 21, fill = "white") +
        scale_y_continuous("Difference (%)", limits = c(0, max(data$Difference)), sec.axis = sec_axis(~ (. - a)/b, name = "Coverage (%)")) +
        theme_classic() +
        scale_x_continuous(
          breaks = data$Iteration,
          labels = paste0(data$Iteration, ": ", data$ID)
        ) +
        labs(x = "Iteration: SNP ID", color = "", fill = "") +
        theme(
          axis.title.x =   element_text(size = 18),
          axis.title.y.left = element_text(size = 18, color = "#173B45"),
          axis.title.y.right = element_text(size = 18, color = "#d65b00"),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y.left = element_text(size = 13, color = "#173B45"), 
          axis.text.y.right = element_text(size = 13, color = "#d65b00")) +
        theme(legend.position = "none")
      
      CoreSNPplot(CoreSNPplot)
      CoreSNPplot()
    }
  })
  
  output$download_CoreSNP_plot = renderUI({
    if (CoreSNPtitle2() == "Coverage plot of core SNP set") {
      downloadButton("DCoreSNP_plot", "Download Plot")
    }
  })
  
  output$DCoreSNP_plot = downloadHandler(
    filename = function() {
      paste0("Core_SNP_Plot-", "coverage",input$coverage2, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 10, height = 8)
      print(CoreSNPplot())
      dev.off()
    }
  )
  
  output$download_core_SNP_coverage = renderUI({
    if (CoreSNPtitle1() == "Core SNP set") {
      downloadButton("Dcore_SNP_coverage", "Download Coverage Data")
    }
  })
  
  output$Dcore_SNP_coverage = downloadHandler(
    filename = "Core_SNP_Coverage_Data.csv",
    content = function(file) {
      write.csv(core_SNP_coverage(), file, row.names = FALSE)
    }
  )
  
  output$download_core_SNP_dataset = renderUI({
    if (CoreSNPtitle1() == "Core SNP set") {
      downloadButton("Dcore_SNP_dataset", "Download Core Set in data.frame")
    }
  })
  
  output$Dcore_SNP_dataset = downloadHandler(
    filename = "df_Core_SNP_Core_Set.rds",
    content = function(file) {
      saveRDS(core_SNP_dataset(), file)
    }
  )
  
  output$download_core_SNP_info = renderUI({
    if (CoreSNPtitle1() == "Core SNP set") {
      downloadButton("D_core_SNP_info", "Download Core SNP Info.")
    }
  })
  
  output$D_core_SNP_info = downloadHandler(
    filename = "Core_SNP_Info.csv",
    content = function(file) {
      write.csv(core_SNP_info(), file, row.names = FALSE)
    }
  )
  
  output$guide_CoreSNP = renderUI({
    div(style = "white-space: pre-wrap; font-size: 16px; color: #333333; background: linear-gradient(145deg, #f0f4f8, #e0e6ed); padding: 15px;  border: 1px solid #d0d9e3; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);", guide_CoreSNP())
  })
  
  output$CoreSNPtitle1 = renderText({ CoreSNPtitle1() })
  output$CoreSNPtitle2 = renderText({ CoreSNPtitle2() })
  
  ##### Page 8: AI Report #####
  guide_AI = reactiveVal("This page allows you to retrieve your preliminary results from prior analysis, input your OpenAI API key, select an AI model, and generate an AI-powered report.")
  AItitle1 = reactiveVal("")
  AItitle2 = reactiveVal("")
  pre_results = reactiveVal(list())
  preliminary_results = reactiveVal(NULL)
  AI_report = reactiveVal(NULL)
  
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
  
  observeEvent(input$Cancel, {
    removeModal()
  })
  
  observeEvent(input$Upload_preliminary_results, {
    req(input$Upload_preliminary_results)
    data = readLines(input$Upload_preliminary_results$datapath, warn = FALSE)
    data = paste(data, collapse = "\n")
    preliminary_results(data)
    AItitle1("Your preliminary results (as the prompts of AI-driven report)")
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
    preliminary_results[[16]] = paste("->", input$AI_species, "SNP dataset for downstream analysis")
    preliminary_results = grep("NULL", preliminary_results, invert = TRUE, value = TRUE)
    preliminary_results = paste(preliminary_results, collapse = "\n")
    preliminary_results(preliminary_results)
    AItitle1("Your preliminary results (as the prompts of AI-driven report)")
  })
  
  output$AI_response1 = renderText({
    req(preliminary_results())
    if (AItitle1() == "Your preliminary results (as the prompts of AI-driven report)"){
      preliminary_results()
    }
  })
  
  output$download_AI_autogenerate = renderUI({
    if (AItitle1() == "Your preliminary results (as the prompts of AI-driven report)") {
      downloadButton("DAI_autogenerate", "Download",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
  })
  
  output$DAI_autogenerate = downloadHandler(
    filename = "AI_Preliminary_Results.txt",
    content = function(file) {
      write.table(preliminary_results(), file, row.names = FALSE, col.names = FALSE)
    }
  )
  
  ##### STEP 2
  observeEvent(input$runAIreport, {
    req(preliminary_results(), input$AI_api_key$datapath)
    shinyjs::show("AIStatus")
    
    result = tryCatch({
      key = readLines(input$AI_api_key$datapath, warn = FALSE)
      Sys.setenv(OPENAI_API_KEY = key)
      client = OpenAI()
      model = AI_model_choice[input$AI_model]
      
      Role = "You are a professional researcher, proficient in interpreting biological informatics and statistical results to deliver biologically meaningful insights."
      Start = "Here is my SNP dataset preliminary results, please help me to craft a academic report."
      message = paste(Role, Start, preliminary_results())
      
      completion = client$chat$completions$create(
        model = model,
        messages = list(list("role" = "user", "content" = message))
      )
      AI_report = paste0("----- Successful Request -----", "\n",
                         "OpenAI Model: ", input$AI_model, "\n",
                         "Total Tokens Used: ", completion$usage$total_tokens, "\n",
                         "Prompt Tokens: ", completion$usage$prompt_tokens, "\n",
                         "Completion Tokens: ", completion$usage$completion_tokens, "\n", "\n",
                         "----- AI-driven report -----", "\n", 
                         completion$choices[[1]]$message$content, "\n", "\n",
                         "## WARNING: ", "\n",
                         "## This report was generated with the assistance of OpenAI model and is for informational purposes only.", "\n",
                         "## It should not be considered as professional advice or a basis for decision-making.", "\n",
                         "## Please review and validate the content thoroughly before use.")
      
      AI_report(AI_report)
      AItitle2("Here's your AI report!")
      shinyjs::hide("AIStatus")
      NULL
    }, error = function(e) {
      shinyjs::hide("AIStatus")
      shinyjs::alert(paste("An error occurred:", e$message))
      NULL
    })
  })
  
  output$AI_response2 = renderText({
    req(AI_report())
    if (AItitle2() == "Here's your AI report!"){
      AI_report()
    }
  })
  
  observeEvent(input$AIreport_Reset, {
    AI_report(NULL)
    AItitle2("")
    showNotification("Data have been reset.")
  })
  
  output$download_AI_report = renderUI({
    if (AItitle2() == "Here's your AI report!") {
      downloadButton("DAI_report", "Download",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
  })
  
  output$DAI_report = downloadHandler(
    filename = "AI_AI_Report.txt",
    content = function(file) {
      write.table(AI_report(), file, row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  )
  
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