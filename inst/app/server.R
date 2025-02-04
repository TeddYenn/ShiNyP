##### Server #####
server = function(input, output, session) {
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
  #observeEvent(input$guide_button, {
  #  shinyjs::runjs("window.open('https://teddyenn.github.io/ShiNyP-guide', '_blank');")
  #})

  #observeEvent(input$case_button, {
  #  shinyjs::runjs("window.open('https://teddy-tw.notion.site/ShiNyP-Case-Studies-a8117cb6e8c44280a25686f9cdddca1c?pvs=4', '_blank');")
  #})
  ##### Page 1-1: VCF #####
  vcfData = reactiveVal(NULL)
  VCFdf = reactiveVal(NULL)
  Site_Info = reactiveVal(NULL)
  fileName = reactiveVal(NULL)
  fileInfo = reactiveVal(NULL)
  fileInfo2 = reactiveVal(NULL)
  guide_input = reactiveVal("Waiting for input VCF file... \nYou can upload: \n▷ A VCF file from PLINK (recommended), or \n▷ A VCF or gzipped VCF file (vcf.gz) from VCFtools, or \n▷ VCF file in RDS from ShiNyP. \nOnce you see 'Upload complete' on the progress bar, click the 'Input VCF file' button.")
  input1 = reactiveVal("")
  input2 = reactiveVal("")
  input3 = reactiveVal("")

  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      guide_input("Now waiting for input VCF file... (Please click the 'Input VCF file' button)")
    }
  })

  output$Uploaddata = renderUI({
    fileInput("file1", "", multiple = TRUE, accept = c(".vcf", ".gz", ".rds"))
  })

  observeEvent(input$demo_data, {
    showModal(modalDialog(
      title = "Demo dataset",
      selectInput("demoFile", "Select a VCF dataset:", choices = c("Chicken", "Wild rice", "Yeast", "Human #HGDP")),

      footer = tagList(
        actionButton("select_demo", "Select"),
        actionButton("close_demo", "Close")
      )
    ))
  })

  observeEvent(input$select_demo, {
    removeModal()
    if (input$demoFile == "Chicken") {
      showModal(modalDialog(
        title = "Data Source",
        p("Tan, X., Zhang, J., Dong, J., Huang, M., Li, Q., Wang, H., ... & Wang, D. (2024). Whole-genome variants dataset of 209 local chickens from China. Scientific Data, 11(1), 169.", br(),
          tags$a(href = "https://doi.org/10.1038/s41597-024-02995-w", "https://doi.org/10.1038/s41597-024-02995-w")
        ),
        footer = tagList(
          modalButton("Close")
        )
      ))
    } else if (input$demoFile == "Wild rice") {
      showModal(modalDialog(
        title = "Data Source",
        p("Kajiya-Kanegae, H., Ohyanagi, H., Ebata, T., Tanizawa, Y., Onogi, A., Sawada, Y., ... & Sato, Y. (2021). OryzaGenome2.1: Database of diverse genotypes in wild Oryza species. Rice, 14, 1-8.", br(),
          tags$a(href = "https://doi.org/10.1186/s12284-021-00468-x", "https://doi.org/10.1186/s12284-021-00468-x")
        ),
        footer = tagList(
          modalButton("Close")
        )
      ))
    } else if (input$demoFile == "Human #HGDP") {
      showModal(modalDialog(
        title = "Data Source",
        p("Bergström, A., McCarthy, S. A., Hui, R., Almarri, M. A., Ayub, Q., Danecek, P., ... & Tyler-Smith, C. (2020). Insights into human genetic variation and population history from 929 diverse genomes. Science, 367(6484), eaay5012.", br(),
          tags$a(href = "https://doi.org/10.1126/science.aay5012", "https://doi.org/10.1126/science.aay5012")
        ),
        footer = tagList(
          modalButton("Close")
        )
      ))
    } else if (input$demoFile == "Yeast") {
      showModal(modalDialog(
        title = "Data Source",
        p("Peter, J., De Chiara, M., Friedrich, A., Yue, J. X., Pflieger, D., Bergström, A., ... & Schacherer, J. (2018). Genome evolution across 1,011 Saccharomyces cerevisiae isolates. Nature, 556(7701), 339-344.", br(),
          tags$a(href = "https://doi.org/10.1038/s41586-018-0030-5", "https://doi.org/10.1038/s41586-018-0030-5")
        ),
        footer = tagList(
          modalButton("Close")
        )
      ))
    }
  })

  observeEvent(input$select_demo, {
    removeModal()
    shinyjs::show("inputStatus")
    withProgress(message = 'Processing data...', value = 0.05, {
      if (input$demoFile == "Chicken"){
        vcf = fread("data/Chicken/Chicken_10k_209.vcf", header = TRUE, sep = "\t")
        fileName("Chicken")
      } else if (input$demoFile == "Wild rice"){
        vcf = fread("data/Wild Rice/Wildrice_13k_446.vcf", header = TRUE, sep = "\t")
        fileName("Wildrice")
      } else if (input$demoFile == "Human #HGDP"){
        vcf = fread("data/Human_HGDP/Human_10k_929.vcf", header = TRUE, sep = "\t")
        fileName("Human")
      } else if (input$demoFile == "Yeast"){
        vcf = fread("data/Yeast/Yeast_10k_1011.vcf", header = TRUE, sep = "\t")
        fileName("Yeast")
      }
      incProgress(0.1, message = "Processing data...")
      names(vcf) = gsub("(.+?)_\\1", "\\1", names(vcf))
      vcf[, `#CHROM` := gsub("[^0-9]", "", `#CHROM`)]
      incProgress(0.15, message = "Processing data...")
      vcf[ID == ".", ID := paste(`#CHROM`, POS, sep = ":")]
      incProgress(0.4, message = "Processing data...")
      vcfData(as.data.frame(vcf))
    })
    guide_input("VCF file has been input!\nNow waiting to transform to a data.frame... (Please click the 'Transform to data.frame' button)")
    input1("VCF Data")
    input3("Preview VCF Data")
    shinyjs::hide("inputStatus")
  })

  observeEvent(input$close_demo, {
    removeModal()
  })

  observeEvent(input$Inputdata, {
    if (guide_input() == "Now waiting for input VCF file... (Please click the 'Input VCF file' button)"){
      shinyjs::show("inputStatus")
      req(input$file1)
      withProgress(message = 'Processing data...', value = 0.05, {
        if (grepl("\\.vcf$", input$file1$name)) {
          vcf = fread(input$file1$datapath, header = TRUE, sep = "\t")
        } else if (grepl("\\.gz$", input$file1$name)) {
          vcf = fread(input$file1$datapath, header = TRUE, sep = "\t")
        } else if (grepl("\\.rds$", input$file1$name)) {
          vcf = readRDS(input$file1$datapath)
          vcf = as.data.table(vcf)
        }
        incProgress(0.1, message = "Processing data...")
        names(vcf) = gsub("(.+?)_\\1", "\\1", names(vcf))
        vcf[, `#CHROM` := gsub("[^0-9]", "", `#CHROM`)]
        incProgress(0.15, message = "Processing data...")
        vcf[ID == ".", ID := paste(`#CHROM`, POS, sep = ":")]
        incProgress(0.2, message = "Processing data...")
        if (input$FVCFtools == TRUE){
          vcf_process = function(x) { paste0(substr(x, 1, 1), "/", substr(x, 3, 3)) }
          vcf[, (names(vcf)[10:ncol(vcf)]) := lapply(.SD, vcf_process), .SDcols = names(vcf)[10:ncol(vcf)]]
        }
        incProgress(0.4, message = "Processing data...")
        vcfData(as.data.frame(vcf))
      })
      guide_input("VCF file has been input!\nNow waiting to transform to a data.frame... (Please click the 'Transform to data.frame' button)")
      input1("VCF Data")
      input3("Preview VCF Data")

      fileName(tools::file_path_sans_ext(input$file1$name))
      shinyjs::hide("inputStatus")
    }
  })

  output$presample = renderUI({
    if (input1() == "VCF Data"){
      sliderInput("presample", "Preview number of samples", min = 1, max = 100, value = 5, step = 1)
    }
  })

  output$contents = DT::renderDataTable({
    req(vcfData())
    if (nrow(vcfData()) == 0) {
      return(NULL)
    } else {
      req(input$presample)
      if (ncol(vcfData()) < (input$presample + 9)) {
        showNotification("Not enough columns in the data.")
      } else{
        selected_vcfData = vcfData()[, c(1:(input$presample + 9))]
        DT::datatable(
          selected_vcfData,
          options = list(
            scrollX = TRUE,
            scrollY = '400px',
            paging = TRUE,
            searching = TRUE,
            lengthMenu = c(10, 25, 50, 100)
          ),
          rownames = FALSE
        )
      }
    }
  })

  observeEvent(input$resetInput, {
    output$Uploaddata = renderUI({
      fileInput("file1", "", multiple = TRUE, accept = c(".vcf", ".gz", ".rds"))
    })
    vcfData(NULL)
    VCFdf(NULL)
    Site_Info(NULL)
    input1("")
    input2("")
    input3("")
    showNotification("Data have been reset.")
    guide_input("Waiting for input VCF data... \nYou can upload: \n▷ A VCF file from PLINK (recommended), or \n▷ A VCF or gzipped VCF file (vcf.gz) from VCFtools, or \n▷ VCF file in RDS from ShiNyP. \nOnce you see 'Upload complete' on the progress bar, click the 'Input VCF file' button.")
  })

  observeEvent(input$vcf2df, {
    req(vcfData())
    shinyjs::show("inputStatus")
    withProgress(message = 'Converting data...', value = 0, {
      VCFdf(vcf2df(vcfData()))
      df(VCFdf())
    })
    Site_Info(vcf2Site_Info(vcfData()))
    input2("VCF Data in data.frame")
    shinyjs::hide("inputStatus")
    guide_input("VCF to data.frame is complete.")
    pre_results = pre_results()
    pre_results[[2]] = "# Data Input"
    pre_results[[4]] = paste0("Number of samples: ", dim(VCFdf())[1])
    pre_results[[5]] = paste0("Number of SNPs: ", dim(VCFdf())[2])
    pre_results(pre_results)

    output$Dinput = downloadHandler(
      filename = function() {
        paste("vcf_", fileName(), ".rds", sep = "")},
      content = function(file) {
        saveRDS(vcfData(), file)
      })

    output$Ddf = downloadHandler(
      filename = function() {
        paste("data.frame_", dim(VCFdf())[1], "_", dim(VCFdf())[2], "SNPs.rds", sep = "")},
      content = function(file) {
        saveRDS(VCFdf(), file)
      })

    output$DsnpInfo = downloadHandler(
      filename = function() {
        paste("Site_Info_", dim(VCFdf())[1], "_", dim(VCFdf())[2], "SNPs.rds", sep = "")},
      content = function(file) {
        saveRDS(Site_Info(), file)
      })
  })

  observeEvent(input$resetvcf2df, {
    VCFdf(NULL)
    df(NULL)
    Site_Info(NULL)
    input2("")
  })

  output$guide_input = renderUI({
    div(class = "guide-text-block", guide_input())
  })

  output$fileInfo = renderText({
    req(vcfData())
    if (input1() == "VCF Data") {
      paste0("File name: ", "vcf_",fileName(), "\n",
             "Number of samples: ", dim(vcfData())[2]-9, "\n",
             "Number of SNPs: ", dim(vcfData())[1], "\n",
             "Type: VCF", "\n",
             "Size: ", size2size(as.numeric(object.size(vcfData())))
      )
    }
  })

  output$fileInfo2 = renderText({
    req(VCFdf())
    if (input2() == "VCF Data in data.frame") {
      paste0("File name: ", "data.frame_", dim(VCFdf())[1], "_", dim(VCFdf())[2], "SNPs", "\n",
             "Number of samples: ", dim(VCFdf())[1], "\n",
             "Number of SNPs: ", dim(VCFdf())[2], "\n",
             "Type: data.frame", "\n",
             "Size: ", size2size(as.numeric(object.size(VCFdf())))
      )
    }
  })

  output$download_input = renderUI({
    if (input2() == "VCF Data in data.frame") {
      downloadButton("Dinput", "Download VCF Data in RDS")
    }
  })

  output$download_df = renderUI({
    if (input2() == "VCF Data in data.frame") {
      downloadButton("Ddf", "Download data.frame File")
    }
  })

  output$download_snpInfo = renderUI({
    if (input2() == "VCF Data in data.frame") {
      downloadButton("DsnpInfo", "Download Site Info.")
    }
  })

  output$input1 = renderText({ input1() })
  output$input2 = renderText({ input2() })
  output$input3 = renderText({ input3() })
  ##### Page 1-2: data.frame/genind/genlight #####
  groupInfo1 = reactiveVal(NULL)
  guide_input2 = reactiveVal("You can input data.frame, genind, or genlight files (in RDS) that have already been transformed.")
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
    VCFdf(df)
    dfstatus("data.frame")
    shinyjs::hide("input2Status")
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
    gistatus("genind")
    shinyjs::hide("input2Status")
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
    glstatus("genlight")
    shinyjs::hide("input2Status")
  })

  output$dfinfo = renderText({
    req(df())
    if (dfstatus() == "data.frame") {
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
    if (gistatus() == "genind") {
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
    if (glstatus() == "genlight") {
      paste0("Status: input", "\n",
             "File name: ", tools::file_path_sans_ext(input$input_gl$name), "\n",
             "Number of samples: ", length(gl()@ind.names), "\n",
             "Number of SNPs: ", gl()@n.loc, "\n",
             "Type: ", class(gl()), "\n",
             "Size: ", size2size(as.numeric(object.size(gl())))
      )
    }
  })

  output$guide_input2 = renderUI({ div(class = "guide-text-block", guide_input2()) })
  output$dfstatus = renderText({ dfstatus() })
  output$gistatus = renderText({ gistatus() })
  output$glstatus = renderText({ glstatus() })
  ##### Page 2-1: Sample QC #####
  guide_sampleQC = reactiveVal("You need to obtain the summary statistics first! \nScroll down to review the results. \nThen, adjust the thresholds and click the 'Sample QC by Thresholds' button.")
  sampleQCstatus = reactiveVal("")
  QCData = reactiveVal(NULL)
  SampleQC_sample = reactiveVal(0)
  SampleQC_SNP = reactiveVal(0)
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
      "Input VCF Data (in data.frame)" = "VCFdf",
      "SNP Post-QC Data" = "QCData"
    )
    selectInput("selectedFile", "Selecte a dataset for QC:", choices)
  })

  output$SampleQCfileInfo = renderText({
    req(VCFdf(), input$selectedFile)
    if (input$selectedFile == "VCFdf"){
      paste0("Number of samples: ", dim(VCFdf())[1], "\n",
             "Number of SNPs: ", dim(VCFdf())[2], "\n",
             "Type: data.frame")
    }else if (input$selectedFile == "QCData"){
      if (is.null(QCData())){
        paste("Not available for 'SNP Post-QC Data'!")
      }else {
        paste0("Post-QC Data (Updated)", "\n",
               "Number of samples: ", dim(QCData())[1], "\n",
               "Number of SNPs: ", dim(QCData())[2], "\n",
               "Type: data.frame")
      }
    }
  })

  observeEvent(input$sampleQCmissing, {
    if (input$selectedFile == "VCFdf") {
      req(VCFdf())
      shinyjs::show("samplemissingStatus")
      rate = rowSums(is.na(VCFdf())) / dim(VCFdf())[2]
    } else if (input$selectedFile == "QCData"){
      req(QCData())
      shinyjs::show("samplemissingStatus")
      rate = rowSums(is.na(QCData())) / dim(QCData())[2]
    }
    samplemissingrate(rate)
    samplemissing1("Summary of Sample Missing Rate")
    samplemissing2(stat2summary(samplemissingrate()))
    shinyjs::hide("samplemissingStatus")
  })

  observeEvent(input$sampleQCH, {
    if (input$selectedFile == "VCFdf") {
      req(VCFdf())
      shinyjs::show("samplehStatus")
      rate = rowSums(VCFdf() == 1, na.rm = TRUE)/(dim(VCFdf())[2]-rowSums(is.na(VCFdf())))
    } else if (input$selectedFile == "QCData"){
      req(QCData())
      shinyjs::show("samplehStatus")
      rate = rowSums(QCData() == 1, na.rm = TRUE)/(dim(QCData())[2]-rowSums(is.na(QCData())))
    }
    sampleh(rate)
    sampleh1("Summary of Sample Heterozygosity Rate")
    sampleh2(stat2summary(sampleh()))
    shinyjs::hide("samplehStatus")
  })

  observeEvent(input$sampleQC, {
    req(samplemissingrate(), sampleh(), Site_Info())
    if (input$selectedFile == "VCFdf") {
      data = VCFdf()
    } else {
      data = QCData()
    }
    rm.sample = union(which(samplemissingrate() > input$sampleThrMR), which(sampleh() > input$sampleThrH))
    if (length(rm.sample)>0){
      rm.sample = as.numeric(rm.sample)
      data = data[-rm.sample, ]
    } else{
      data = data
    }
    QCData(data)
    df(data)
    SampleQC_sample(dim(data)[1])
    SampleQC_SNP(dim(data)[2])

    Site_Info(Site_Info())
    guide_sampleQC("Sample quality control is complete. \nYou will receive the Post-QC Data (in data.frame) when you download the file.")
    sampleQCstatus("Post-QC Data (in data.frame)")
    pre_results = pre_results()
    pre_results[[6]] = "# Data QC"
    pre_results[[7]] = "Sample QC"
    pre_results[[8]] = paste0("Removed samples with ", "missing rate > ", input$sampleThrMR ," and heterozygosity rate > ", input$sampleThrH)
    pre_results[[9]] = paste0("Number of samples: ", dim(QCData())[1])
    pre_results[[10]] = paste0("Number of SNPs: ", dim(QCData())[2])
    pre_results(pre_results)

    output$DsampleQC = downloadHandler(
      filename = function() {
        paste("data.frame_", SampleQC_sample(), "_", SampleQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        saveRDS(QCData(), file)
      })

    output$DsampleQCSite = downloadHandler(
      filename = function() {
        paste("Site_Info_", SampleQC_sample(), "_", SampleQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        saveRDS(Site_Info(), file)
      })
  })

  observeEvent(input$resetsampleQC, {
    QCData(NULL)
    SampleQC_sample(0)
    SampleQC_SNP(0)
    sampleQCstatus("")
    showNotification("Data have been reset.")
    guide_sampleQC("You need to obtain the summary statistics first! \nScroll down to review the results. \nThen, adjust the thresholds and click the 'Sample QC by Thresholds' button.")
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

  ##### Page 2-2: SNP QC #####
  guide_QC = reactiveVal("You need to obtain the summary statistics first! \nScroll down to review the results. \nThen, adjust the thresholds and click the 'SNP QC by Thresholds' button.")
  SNPQCstatus = reactiveVal("")
  SNPQC_sample = reactiveVal(0)
  SNPQC_SNP = reactiveVal(0)
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
  HWE = reactiveVal(NULL)
  HWE1 = reactiveVal(NULL)
  HWE2 = reactiveVal(NULL)
  HWE3 = reactiveVal(NULL)
  progressVal = reactiveVal(0)

  output$fileSelection2 = renderUI({
    choices = c(
      "Sample Post-QC Data" = "QCData",
      "Input VCF Data (in data.frame)" = "VCFdf"
    )
    selectInput("selectedFile2", "Selecte a dataset for QC:", choices)
  })

  output$SNPQCfileInfo = renderText({
    req(VCFdf(), input$selectedFile2)
    if (input$selectedFile2 == "VCFdf"){
      paste0("Number of samples: ", dim(VCFdf())[1], "\n",
             "Number of SNPs: ", dim(VCFdf())[2], "\n",
             "Type: data.frame")
    }else if (input$selectedFile2 == "QCData"){
      if (is.null(QCData())){
        paste("Not available for 'Sample Post-QC Data'!")
      }else {
        paste0("Post-QC Data (Updated)", "\n",
               "Number of samples: ", dim(QCData())[1], "\n",
               "Number of SNPs: ", dim(QCData())[2], "\n",
               "Type: data.frame")
      }
    }
  })

  observeEvent(input$QCmissing, {
    if (input$selectedFile2 == "VCFdf") {
      req(VCFdf())
      shinyjs::show("missingStatus")
      rate = colSums(is.na(VCFdf()))/(dim(VCFdf())[1])
    } else if (input$selectedFile2 == "QCData"){
      req(QCData())
      shinyjs::show("missingStatus")
      rate = colSums(is.na(QCData()))/(dim(QCData())[1])
    }
    missingrate(rate)
    missing1("Summary of SNP Missing Rate")
    missing2(stat2summary(missingrate()))
    shinyjs::hide("missingStatus")
  })

  observeEvent(input$QCMAF, {
    if (input$selectedFile2 == "VCFdf") {
      req(VCFdf())
      shinyjs::show("mafStatus")
      rate = (colSums(VCFdf() == 1, na.rm = TRUE) + 2*colSums(VCFdf() == 2, na.rm = TRUE))/(2*(dim(VCFdf())[1]-colSums(is.na(VCFdf()))))
    } else if (input$selectedFile2 == "QCData"){
      req(QCData())
      shinyjs::show("mafStatus")
      rate = (colSums(QCData() == 1, na.rm = TRUE) + 2*colSums(QCData() == 2, na.rm = TRUE))/(2*(dim(QCData())[1]-colSums(is.na(QCData()))))
    }
    rate = pmin(rate, 1 - rate)
    maf(rate)
    maf1("Summary of SNP Minor Allele Frequency (MAF)")
    maf2(stat2summary(maf()))
    shinyjs::hide("mafStatus")
  })

  observeEvent(input$QCH, {
    if (input$selectedFile2 == "VCFdf") {
      req(VCFdf())
      shinyjs::show("hStatus")
      rate = colSums(VCFdf() == 1, na.rm = TRUE)/(dim(VCFdf())[1]-colSums(is.na(VCFdf())))
    } else if (input$selectedFile2 == "QCData"){
      req(QCData())
      shinyjs::show("hStatus")
      rate = colSums(QCData() == 1, na.rm = TRUE)/(dim(QCData())[1]-colSums(is.na(QCData())))
    }
    rate = pmin(rate, 1 - rate)
    h(rate)
    h1("Summary of SNP Heterozygosity Rate")
    h2(stat2summary(h()))
    shinyjs::hide("hStatus")
  })

  observeEvent(input$QCHWE, {
    if (input$selectedFile2 == "VCFdf") {
      req(VCFdf())
      shinyjs::show("hweStatus")
      hwe = hwe_test(VCFdf())
    } else if (input$selectedFile2 == "QCData"){
      req(QCData())
      shinyjs::show("hweStatus")
      hwe = hwe_test(QCData())
    }
    HWE(hwe)
    HWE1("Summary of SNP p-value for Hardy-Weinberg equilibrium (HWE)")
    HWE2(stat2summary_HWE(HWE()))
    shinyjs::hide("hweStatus")
  })

  output$doThrHWE = renderUI({
    if (input$doHWE == TRUE){
      sliderInput("ThrHWE", "Threshold of -log(p) for HWE (remove > [threshold])", min = 0, max = 30, value = 6)
    }
  })

  observeEvent(input$QC, {
    req(missingrate(), maf(), h(), HWE(), Site_Info())
    if (input$selectedFile2 == "VCFdf") {
      data = VCFdf()
    } else {
      data = QCData()
    }
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

    if (length(rm.loc)>0){
      rm.loc = as.numeric(rm.loc)
      data = data[, -rm.loc]
    } else{
      data = data
    }
    QCData(data)
    df(data)
    SNPQC_sample(dim(data)[1])
    SNPQC_SNP(dim(data)[2])
    Site_Info = Site_Info()[-rm.loc, ]
    Site_Info(Site_Info)
    guide_QC("SNP quality control is complete. \nYou will receive the Post-QC Data (in data.frame) when you download the file.")
    SNPQCstatus("Post-QC Data (in data.frame)")
    pre_results = pre_results()
    pre_results[[11]] = "SNP QC"
    if (input$doHWE == TRUE) {
      pre_results[[12]] = paste0("Removed SNPs with ", "missing rate > ", input$ThrMR ,", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", heterozygosity rate > ", input$ThrH, ", and HWE -log10(p-value) > ", input$ThrHWE)
    } else{
      pre_results[[12]] = paste0("Removed SNPs with ", "missing rate > ", input$ThrMR ,", MAF < ", input$ThrMAF, ", heterozygosity rate < ", input$ThrH0, ", and heterozygosity rate > ", input$ThrH)
    }
    pre_results[[13]] = paste0("Number of samples: ", dim(QCData())[1])
    pre_results[[14]] = paste0("Number of SNPs: ", dim(QCData())[2])
    pre_results(pre_results)

    output$DsnpQC = downloadHandler(
      filename = function() {
        paste("data.frame_", SNPQC_sample(), "_", SNPQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        saveRDS(QCData(), file)
      })

    output$DSNPQCSite = downloadHandler(
      filename = function() {
        paste("Site_Info_", SNPQC_sample(), "_", SNPQC_SNP(), "SNPs.rds", sep = "")},
      content = function(file) {
        saveRDS(Site_Info(), file)
      })
  })

  observeEvent(input$resetSNPQC, {
    QCData(NULL)
    SNPQC_sample(0)
    SNPQC_SNP(0)
    SNPQCstatus("")
    showNotification("Data have been reset.")
    guide_QC("You need to obtain the summary statistics first! \nScroll down to review the results. \nThen, adjust the thresholds and click the 'SNP QC by Thresholds' button.")
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

  ##### Page 2-3: SNP Density #####
  ##### Change Site #####
  guide_SNPdensity = reactiveVal("You need to upload the Site Info file (in RDS format) and Chromosome Info file (in CSV format). For more information, please visit: site.\nPlease select the optimal window size and step, then click the 'Summary' button.")
  SNPdensity1 = reactiveVal("")
  SNPdensity2 = reactiveVal("")
  SNPdensityresult1 = reactiveVal(NULL)
  SNPdensityresults2 = reactiveVal(NULL)
  densityplot = reactiveVal(NULL)
  progressVal = reactiveVal(0)

  output$Site_Info0 = renderUI({
    fileInput("Site_Info0", "Site Info.* (required)", multiple = F, accept = c(".rds"))
  })

  observeEvent(input$Site_Info0, {
    req(input$Site_Info0)
    Site_Info = readRDS(input$Site_Info0$datapath)
    Site_Info(Site_Info)
  })

  output$Chr_Info0 = renderUI({
    fileInput("Chr_Info0", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
  })

  observeEvent(input$Chr_Info0, {
    Chr_Info = read.csv(input$Chr_Info0$datapath)
    Chr_Info(Chr_Info)
  })

  observeEvent(input$SNPdensity, {
    req(Chr_Info(), Site_Info())
    shinyjs::show("SNPdensityStatus")
    progressVal = reactiveVal(NULL)

    Site_Info = Site_Info()
    Chr_Info = Chr_Info()
    Site_Info$Chr = as.numeric(Site_Info$Chr)
    Chr_Info$Length = Chr_Info$End - Chr_Info$Start

    window_size = as.numeric(input$WindowSize0)*1000
    SNPdensity = density_analysis(Site_Info, Chr_Info, window_size)
    SNPdensityresult1(SNPdensity)

    space_chr = round(Chr_Info[,3]/as.numeric(table(Site_Info$Chr)), 2) # average spacing bp/SNPs
    space_total = round(sum(Chr_Info[,3])/length(Site_Info$Chr), 2)

    snp_chr = round(as.numeric(table(Site_Info$Chr))/Chr_Info[,3]*1000, 4) # average spacing SNPs/1000bp
    snp_total = round(length(Site_Info$Chr)/sum(Chr_Info[,3])*1000, 4)

    SNPdensityresults2 = data.frame(
      "Chr" = Chr_Info[,1],
      "bp_over_SNPs" = space_chr,
      "SNPs_over_1000bp" = snp_chr
    )
    SNPdensityresults2[dim(SNPdensityresults2)[1]+1, 1:3] = c("Total", space_total, snp_total)
    SNPdensityresults2(SNPdensityresults2)

    shinyjs::hide("SNPdensityStatus")
    SNPdensity1("SNP Density Plot")
    SNPdensity2("SNP Density across All Chromosome")
    guide_SNPdensity("The SNP density analysis is complete.")

    output$DSNPdensity_plot = downloadHandler(
      filename = function() {
        paste0("SNP_Density_Plot", "-", input$WindowSize0, "kb.pdf")
      },
      content = function(file) {
        pdf(file, width = 12, height = 7)
        print(densityplot())
        dev.off()
      }
    )
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
    guide_SNPdensity("You need to upload the Site Info file (in RDS format) and Chromosome Info file (in CSV format). For more information, please visit: site.\nPlease select the optimal window size and step, then click the 'Summary' button.")
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
      text = paste0("# Summary of Reference Genome and SNP Density", "\n",
                    "Number of chromosomes: ", length(Chr_Info[,1]), "\n",
                    "Total length (bp): ", sum(Chr_Info[,3]), "\n",
                    "---------------------", "\n",
                    "Number of SNPs: ", length(Site_Info[,1]), "\n",
                    "Average SNP spacing: ", data[last_row, 2], " bp", "\n",
                    "Average number of SNPs per 1000bp: ", data[last_row, 3], " SNPs", "\n")
      pre_results = pre_results()
      pre_results[[15]] = text
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
              axis.title.y = element_text(size = 14, vjust = 2),
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
        scale_color_gradientn(name = title, colors = c("#185519", "#F6EACB", "#C7253E"), na.value = "grey") +
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
      downloadButton("DSNPdensity_plot", "Download Plot")
    }
  })

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
      write.csv(SNPdensityresults2(), file, row.names = FALSE)
    }
  )

  output$guide_SNPdensity = renderUI({ div(class = "guide-text-block", guide_SNPdensity()) })
  output$SNPdensity1 = renderText({ SNPdensity1() })
  output$SNPdensity2 = renderText({ SNPdensity2() })

  ##### Page 3: Data Conversion #####
  df = reactiveVal(NULL)
  gi = reactiveVal(NULL)
  gl = reactiveVal(NULL)
  groupInfo1 = reactiveVal(NULL)
  guide_C = reactiveVal("You need to transform the data step by step. \nWaiting for transform data.frame to genind...")
  Site_Info = reactiveVal("")
  Cstatus2 = reactiveVal("")
  Cstatus3 = reactiveVal("")
  CTable2 = reactiveVal(NULL)
  CTable3 = reactiveVal(NULL)
  progressVal = reactiveVal(0)

  output$fileSelection3 = renderUI({
    if (!is.null(QCData()) && !is.null(VCFdf())){
      choices = c(
        "Post-QC Data (in data.frame)" = "QCData",
        "Input VCF Data (in data.frame)" = "VCFdf"
      )
    }else if (!is.null(VCFdf())){
      choices = c(
        "Input VCF Data (in data.frame)" = "VCFdf"
      )
    }else {
      choices = ""
    }
    selectInput("FileforDataConv", "Dataset for Transformation:", choices)
  })

  output$CfileInfo = renderText({
    req(VCFdf(), input$FileforDataConv)
    if (input$FileforDataConv == "VCFdf") {
      df(VCFdf())
      paste0("Number of samples: ", dim(VCFdf())[1], "\n",
             "Number of SNPs: ", dim(VCFdf())[2], "\n",
             "Type: data.frame")
    } else if (input$FileforDataConv == "QCData"){
      req(QCData())
      df(QCData())
      paste0("Number of samples: ", dim(QCData())[1], "\n",
             "Number of SNPs: ", dim(QCData())[2], "\n",
             "Type: data.frame")
    }
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
    pre_results = pre_results()
    pre_results[[17]] = paste0("-> Number of samples: ", dim(df())[1])
    pre_results[[18]] = paste0("-> Number of SNPs: ", dim(df())[2])
    pre_results(pre_results)
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
    Cstatus2("data.frame to genind")
    shinyjs::hide("giStatus")
    guide_C("The data.frame has been transformed to genind format. \nNow waiting to transform genind to genlight...")
  })

  output$download_gi = renderUI({
    if (Cstatus2() == "data.frame to genind") {
      downloadButton("Dgi", "Download genind File")
    }
  })

  output$Dgi = downloadHandler(
    filename = function() {
      if (!is.null(groupInfo1())){
        paste("genind_group_", length(gi()@ploidy), "_", length(gi()@all.names), "SNPs.rds", sep = "")
      } else{
        paste("genind_", length(gi()@ploidy), "_", length(gi()@all.names), "SNPs.rds", sep = "")
      }
    },
    content = function(file) {
      saveRDS(gi(), file)
    })

  observeEvent(input$Cgi2gl, {
    req(gi())
    shinyjs::show("glStatus")
    gl = gi2gl(gi(), verbose = 0)
    gl(gl)
    Cstatus3("genind to genlight")
    shinyjs::hide("glStatus")
    guide_C("The data has been transformed to data.frame, genind, and genlight formats. \nEnjoy the downstream analysis! (•‿•)")
  })

  output$download_gl = renderUI({
    if (Cstatus3() == "genind to genlight") {
      downloadButton("Dgl", "Download genlight File")
    }
  })

  output$Dgl = downloadHandler(
    filename = function() {
      paste("genlight_", length(gl()@ind.names), "_", gl()@n.loc, "SNPs.rds", sep = "")},
    content = function(file) {
      saveRDS(gl(), file)
    })

  output$guide_C = renderUI({ div(class = "guide-text-block", guide_C()) })

  output$Cstatus2 = renderText({ Cstatus2() })

  output$CTable2 = renderText({
    req(gi())
    if (Cstatus2() == "data.frame to genind"){
      if (!is.null(gi()@pop)){
        group.info = "Added"
        file.name = "File name: genind_group_"
      }else{
        group.info = "NaN"
        file.name = "File name: genind_"
      }
      paste0("Status: transformed", "\n",
             file.name, length(gi()@ploidy), "_", length(gi()@all.names), "SNPs", "\n",
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
    if (Cstatus3() == "genind to genlight"){
      paste0("Status: transformed", "\n",
             "File name: genlight_", length(gl()@ind.names), "_", gl()@n.loc, "SNPs", "\n",
             "Number of samples: ", length(gl()@ind.names), "\n",
             "Number of SNPs: ", gl()@n.loc, "\n",
             "Type: ", class(gl()), "\n",
             "Size: ", size2size(as.numeric(object.size(gl()))))
    }
  })
  ##### Page 4: Population Structure #####
  ##### PCA #####
  guide_PCA = reactiveVal("To run PCA, the input data must be in data.frame format. \nPlease click the 'Run PCA' button")
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
    PCAtitle1("PCA Scatter Plot")
    PCAtitle2("PC Explained Variance Plot")
    guide_PCA("The PCA is complete. \nPlease select the PCs for the X and Y axes of the 2D PCA plot.\nTry adjusting the number of PCs and observe the explained variance plot.")
    pre_results = pre_results()
    pre_results[[19]] = "# Population Structure"
    pre_results[[20]] = "Principal Component Analysis (PCA)"
    pre_results[[21]] = paste0("Top 15 PCs explained variance (%), PC 1 to PC 15: ", paste(round(PCA_SD$Proportion_of_explained_variance[1:15], 2), collapse = ", "))
    pre_results(pre_results)

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
  })

  observeEvent(input$resetPCA, {
    groupfile4(NULL)
    pca_result(NULL)
    PCA_SD(NULL)
    PCAtitle1("")
    PCAtitle2("")
    showNotification("Data have been reset.")
    guide_PCA("To run PCA, the input data must be in data.frame format. \nPlease click the 'Run PCA' button")
  })

  output$pc1 = renderUI({
    if (PCAtitle1() == "PCA Scatter Plot"){
      pca_result = pca_result()
      selectInput("pc1", "Select PC for X-axis:", choices = paste0("PC", seq_along(pca_result$sdev)), selected = "PC1")
    }
  })

  output$pc2 = renderUI({
    if (PCAtitle1() == "PCA Scatter Plot"){
      pca_result = pca_result()
      selectInput("pc2", "Select PC for Y-axis:", choices = paste0("PC", seq_along(pca_result$sdev)), selected = "PC2")
    }
  })

  output$groupfile4 = renderUI({
    if (PCAtitle1() == "PCA Scatter Plot"){
      fileInput("groupfile4", "Group or Core Sample Info.", multiple = F, accept = c(".csv"))
    }
  })

  output$PC = renderUI({
    if (PCAtitle1() == "PCA Scatter Plot"){
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
    if (PCAtitle1() == "PCA Scatter Plot") {
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
        colors = colorRampPalette(custom_palette)(length(unique(PCA_NewData$Group)))
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
    if (PCAtitle2() == "PC Explained Variance Plot") {
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
        scale_y_continuous("Proportion of variance (%)", limits = c(0, max(variance_data$PV)), sec.axis = sec_axis(~ (. - a)/b, name = "Cumulative variance (%)")) +
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
    if (PCAtitle1() == "PCA Scatter Plot") {
      downloadButton("DPCAplot", "Download Plot")
    }
  })

  output$download_Expplot = renderUI({
    if (PCAtitle1() == "PCA Scatter Plot") {
      downloadButton("DExpplot", "Download Plot")
    }
  })

  output$download_var = renderUI({
    if (PCAtitle1() == "PCA Scatter Plot") {
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
    if (PCAtitle1() == "PCA Scatter Plot") {
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
    if (PCAtitle1() == "PCA Scatter Plot") {
      downloadButton("DPCAres", "Download PCA Object")
    }
  })

  output$DPCAres = downloadHandler(
    filename = "PCA_prcomp_Object.rds",
    content = function(file) {
      saveRDS(pca_result(), file)
    }
  )

  output$guide_PCA = renderUI({ div(class = "guide-text-block", guide_PCA()) })
  output$PCAtitle1 = renderText({ PCAtitle1() })
  output$PCAtitle2 = renderText({ PCAtitle2() })

  ##### DAPC #####
  guide_DAPC = reactiveVal("To run DAPC, the input data must be in genind format. \nPlease click the 'Run DAPC I' button first.")
  DAPCfileInfo = reactiveVal("")
  DAPC1 = reactiveVal(NULL)
  DAPC2 = reactiveVal(NULL)
  DAPCtitle1 = reactiveVal("")
  DAPCtitle2 = reactiveVal("")
  DAPCtitle3 = reactiveVal("")
  DAPCtitle4 = reactiveVal("")
  DAPCtitle5 = reactiveVal("")
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
    req(input$FileforDAPC, input$npca, input$Maxgrp)
    shinyjs::show("DAPCStatus")

    dapc_data = switch(input$FileforDAPC, "gi" = gi())

    DAPC1 = find.clusters(dapc_data, max.n = input$Maxgrp, n.pca = input$npca, scale = FALSE, choose = FALSE)
    DAPC1(DAPC1)

    lowest = as.numeric(which(DAPC1$Kstat == min(DAPC1$Kstat)))
    updateSliderInput(session, "grp", min = 3, max = 35, value = lowest, step = 1)

    shinyjs::hide("DAPCStatus")
    DAPCtitle1("Bayesian Information Criterion (BIC) Plot")
    guide_DAPC("STEP I is complete. \nPlease select the number of clusters (K) for STEP II.")
  })

  observeEvent(input$runDAPC2, {
    req(input$FileforDAPC, input$npca, input$grp)
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
    DAPCtitle2("Density Plot of First Discriminant Function")
    DAPCtitle3("Density Plot of Second Discriminant Function")
    DAPCtitle4("DAPC Scatter Plot")
    DAPCtitle5("DAPC Membership Probability Plot")
    guide_DAPC("DAPC is complete. \nPlease review the results.")
    pre_results = pre_results()
    pre_results[[22]] = "Discriminant Analysis of Principal Components (DAPC)"
    pre_results[[23]] = paste0("The samples were divided into ", length(table(DAPC2$assign)), " groups based on the BIC value at K = ", length(table(DAPC2$assign)))
    pre_results[[24]] = paste0("Group sizes, Group 1 to Group ", length(table(DAPC2$assign)), ": ", paste(as.numeric(table(DAPC2$assign)), collapse = ", "))
    pre_results[[25]] = paste0("The group centroid of each group at first discriminant function, Group 1 to Group ", length(table(DAPC2$assign)), ": ", paste(round(DAPC2$grp.coord[,1], 2), collapse = ", "))
    pre_results[[26]] = paste0("The group centroid of each group at second discriminant function, Group 1 to Group ", length(table(DAPC2$assign)), ": ", paste(round(DAPC2$grp.coord[,2], 2), collapse = ", "))
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
    guide_DAPC("To run DAPC, the input data must be in genind format. \nPlease click the 'Run DAPC I' button first.")
  })

  observeEvent(input$resetDAPC2, {
    DAPC1(NULL)
    DAPC2(NULL)
    DAPCtitle1("")
    DAPCtitle2("")
    DAPCtitle3("")
    DAPCtitle4("")
    DAPCtitle5("")
    showNotification("Data have been reset.")
    guide_DAPC("To run DAPC, input data must be genind file.\nPlease click 'Run DAPC I' button first.")
  })

  output$BICplot = renderPlot({
    req(DAPC1())
    if (DAPCtitle1() == "Bayesian Information Criterion (BIC) Plot"){
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
    if (DAPCtitle4() == "DAPC Scatter Plot"){
      ngroup = length(unique(DAPC2()$assign))
      colors = colorRampPalette(custom_palette)(ngroup)
      scatter(DAPC2(), grp = DAPC2()$assign, bg = "white", scree.da = F, pch = 19, col = colors,
              posi.leg = "topright", legend = T, cex = 2.2, cellipse = 1, axesell = F, txt.leg = paste("Group", 1:ngroup))
    }
  })

  output$DF1plot = renderPlot({
    req(DAPC2())
    if (DAPCtitle4() == "DAPC Scatter Plot"){
      ngroup = length(unique(DAPC2()$assign))
      colors = colorRampPalette(custom_palette)(ngroup)
      scatter(DAPC2(), grp = DAPC2()$assign, 1, 1, bg = "white", col = colors,
              scree.da = FALSE, legend = F, solid = .6)
    }
  })

  output$DF2plot = renderPlot({
    req(DAPC2())
    if (DAPCtitle4() == "DAPC Scatter Plot"){
      ngroup = length(unique(DAPC2()$assign))
      colors = colorRampPalette(custom_palette)(ngroup)
      scatter(DAPC2(), grp = DAPC2()$assign, 2, 2, bg = "white", col = colors,
              scree.da = FALSE, legend = F, solid = .6)
    }
  })

  output$probplot = renderPlot({
    req(DAPC2())
    if (DAPCtitle4() == "DAPC Scatter Plot"){
      ngroup = length(unique(DAPC2()$assign))
      population = DAPC2()$assign
      subset = c()
      for(i in 1:ngroup){
        subset = c(subset, which(population %in% i))
      }
      colors = colorRampPalette(custom_palette)(ngroup)
      compoplot(DAPC2(), subset = subset, col = colors, legend = F)
    }
  })

  output$download_BIC_plot = renderUI({
    if (DAPCtitle1() == "Bayesian Information Criterion (BIC) Plot") {
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
    if (DAPCtitle4() == "DAPC Scatter Plot") {
      downloadButton("DDF1plot", "Download Plot")
    }
  })

  output$DDF1plot = downloadHandler(
    filename = "DAPC_First_Discriminant_Function_Plot.pdf",
    content = function(file) {
      pdf(file, width = 8, height = 4)
      ngroup = length(unique(DAPC2()$assign))
      colors = colorRampPalette(custom_palette)(ngroup)
      scatter(DAPC2(), grp = DAPC2()$assign, 1, 1, bg = "white", col = colors,
              scree.da = FALSE, legend = F, solid = .6)
      dev.off()
    }
  )

  output$download_DF2_plot = renderUI({
    if (DAPCtitle4() == "DAPC Scatter Plot") {
      downloadButton("DDF2plot", "Download Plot")
    }
  })

  output$DDF2plot = downloadHandler(
    filename = "DAPC_Second_Discriminant_Function_Plot.pdf",
    content = function(file) {
      pdf(file, width = 8, height = 4)
      ngroup = length(unique(DAPC2()$assign))
      colors = colorRampPalette(custom_palette)(ngroup)
      scatter(DAPC2(), grp = DAPC2()$assign, 2, 2, bg = "white", col = colors,
              scree.da = FALSE, legend = F, solid = .6)
      dev.off()
    }
  )

  output$download_DAPC_plot = renderUI({
    if (DAPCtitle4() == "DAPC Scatter Plot") {
      downloadButton("DDAPCplot", "Download Plot")
    }
  })

  output$DDAPCplot = downloadHandler(
    filename = "DAPC_Scatter_Plot.pdf",
    content = function(file) {
      pdf(file, width = 12, height = 9)
      ngroup = length(unique(DAPC2()$assign))
      colors = colorRampPalette(custom_palette)(ngroup)
      scatter(DAPC2(), grp = DAPC2()$assign, bg = "white", scree.da = F, pch = 19, col = colors,
              posi.leg = "topright", legend = T, cex = 2.2, cellipse = 1, axesell = F, txt.leg = paste("Group", 1:ngroup))
      dev.off()
    }
  )

  output$download_prob_plot = renderUI({
    if (DAPCtitle4() == "DAPC Scatter Plot") {
      downloadButton("Dprobplot", "Download Plot")
    }
  })

  output$Dprobplot = downloadHandler(
    filename = "DAPC_Membership_Probability_Plot.pdf",
    content = function(file) {
      pdf(file, width = 8, height = 4)
      ngroup = length(unique(DAPC2()$assign))
      population = DAPC2()$assign
      subset = c()
      for(i in 1:ngroup){
        subset = c(subset, which(population %in% i))
      }
      colors = colorRampPalette(custom_palette)(ngroup)
      compoplot(DAPC2(), subset = subset, col = colors, legend = F)
      dev.off()
    }
  )

  output$download_DAPC_pop = renderUI({
    if (DAPCtitle4() == "DAPC Scatter Plot") {
      downloadButton("DDAPCpop", "Download DAPC Group Info.")
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
    if (DAPCtitle4() == "DAPC Scatter Plot") {
      downloadButton("DDAPCtrans", "Download DAPC Transformed Data")
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
    if (DAPCtitle4() == "DAPC Scatter Plot") {
      downloadButton("DDAPCres", "Download DAPC Object")
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

  output$guide_DAPC = renderUI({ div(class = "guide-text-block", guide_DAPC()) })
  output$DAPCtitle1 = renderText({ DAPCtitle1() })
  output$DAPCtitle2 = renderText({ DAPCtitle2() })
  output$DAPCtitle3 = renderText({ DAPCtitle3() })
  output$DAPCtitle4 = renderText({ DAPCtitle4() })
  output$DAPCtitle5 = renderText({ DAPCtitle5() })

  ##### UPGMA #####
  guide_UPGMA = reactiveVal("To run the UPGMA phylogenetic tree, the input data must be in genlight format. \nPlease click the 'Run UPGMA' button.")
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
    req(input$FileforUPGMA, input$sample)
    shinyjs::show("UPGMAStatus")

    UPGMA_data = switch(input$FileforUPGMA, "gl" = gl())
    tree = aboot(UPGMA_data, tree = "upgma",
                 distance = bitwise.dist,
                 sample = input$sample,
                 showtree = F)
    tree(tree)

    shinyjs::hide("UPGMAStatus")
    UPGMAtitle1("UPGMA Phylogenetic Tree")
    guide_UPGMA("The UPGMA analysis is complete. \nTry adjusting the layout style and observe the UPGMA phylogenetic tree.")

    output$DUPGMAplot = downloadHandler(
      filename = function() {
        paste0("UPGMA_Plot-", input$sample, "bootstraps-Layout_", input$Layout, ".pdf")
      },
      content = function(file) {
        Layout = Tree_layout_choice[input$Layout]
        ggsave(file, plot = ggtree(tree(), layout = Layout) + geom_tiplab(hjust = -0.1, align = TRUE, linesize = 0.5, size = 2), device = "pdf", width = 12, height = 12)
      }
    )
  })

  observeEvent(input$resetUPGMA, {
    UPGMAtitle1("")
    tree(NULL)
    showNotification("Data have been reset.")
    guide_UPGMA("To run the UPGMA phylogenetic tree, the input data must be in genlight format. \nPlease click the 'Run UPGMA' button.")
  })

  output$Layout = renderUI({
    if (UPGMAtitle1() == "UPGMA Phylogenetic Tree"){
      selectInput("Layout", "Layout style", choices = names(Tree_layout_choice), selected = "Circular")
    }
  })

  output$UPGMA = renderPlot({
    req(tree(), input$Layout)
    if (UPGMAtitle1() == "UPGMA Phylogenetic Tree") {
      Layout = Tree_layout_choice[input$Layout]
      ggtree(tree(), layout = Layout) + geom_tiplab(hjust = -0.1, align = TRUE, linesize = 0.5, size = 2)
    }
  })

  output$download_UPGMA_plot = renderUI({
    if (UPGMAtitle1() == "UPGMA Phylogenetic Tree") {
      downloadButton("DUPGMAplot", "Download Plot")
    }
  })

  output$download_UPGMA_result = renderUI({
    if (UPGMAtitle1() == "UPGMA Phylogenetic Tree") {
      downloadButton("DUPGMAres", "Download UPGMA Object")
    }
  })

  output$DUPGMAres = downloadHandler(
    filename = "UPGMA_phylo_Object.rds",
    content = function(file) {
      saveRDS(tree(), file)
    }
  )

  output$guide_UPGMA = renderUI({ div(class = "guide-text-block", guide_UPGMA()) })
  output$UPGMAtitle1 = renderText({ UPGMAtitle1() })

  ##### NJ #####
  guide_NJ = reactiveVal("To run the NJ phylogenetic tree, the input data must be in genlight format.\nPlease click the 'Run NJ' button.")
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
    NJtitle1("NJ Phylogenetic Tree")
    guide_NJ("The NJ tree is complete. \nTry adjusting the layout style and observe the NJ phylogenetic tree.")

    output$DNJplot = downloadHandler(
      filename = function() {
        paste0("NJ_Plot-Layout_", input$NJLayout, ".pdf")
      },
      content = function(file) {
        Layout = Tree_layout_choice[input$NJLayout]
        ggsave(file, plot = ggtree(NJtree(), layout = Layout) + geom_tiplab(hjust = -0.1, align = TRUE, linesize = 0.5, size = 2), device = "pdf", width = 12, height = 12)
      }
    )
  })

  observeEvent(input$resetNJ, {
    NJtree(NULL)
    NJtitle1("")
    showNotification("Data have been reset.")
    guide_NJ("To run the NJ phylogenetic tree, the input data must be in genlight format.\nPlease click the 'Run NJ' button.")
  })

  output$NJLayout = renderUI({
    if (NJtitle1() == "NJ Phylogenetic Tree"){
      selectInput("NJLayout", "Layout style", choices = names(Tree_layout_choice), selected = "Circular")
    }
  })

  output$NJ = renderPlot({
    req(NJtree(), input$NJLayout)
    if (NJtitle1() == "NJ Phylogenetic Tree") {
      Layout = Tree_layout_choice[input$NJLayout]
      ggtree(NJtree(), layout = Layout) + geom_tiplab(hjust = -0.1, align = TRUE, linesize = 0.5, size = 2)
    }
  })

  output$download_NJ_plot = renderUI({
    if (NJtitle1() == "NJ Phylogenetic Tree") {
      downloadButton("DNJplot", "Download Plot")
    }
  })

  output$download_NJ_result = renderUI({
    if (NJtitle1() == "NJ Phylogenetic Tree") {
      downloadButton("DNJres", "Download NJ Object")
    }
  })

  output$DNJres = downloadHandler(
    filename = "NJ_phylo_Object.rds",
    content = function(file) {
      saveRDS(NJtree(), file)
    }
  )

  output$guide_NJ = renderUI({ div(class = "guide-text-block", guide_NJ()) })
  output$NJtitle1 = renderText({ NJtitle1() })

  ##### Kinship #####
  guide_Kinship = reactiveVal("To run the kinship matrix, the input data must be in data.frame format. \nThe 'DAPC_Group_Info' CSV file from DAPC analysis is optional. \nPlease click the 'Run Kinship' button.")
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
    req(input$FileforKinship, input$Kinship_method)
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
    Kinshiptitle1("Kinship Matrix")
    guide_Kinship("The kinship analysis is complete.")

    output$DKinshipplot = downloadHandler(
      filename = function() {
        paste0("Kinship_Matrix_Plot-Method_", input$Kinship_method, ".pdf")
      },
      content = function(file) {
        pdf(file, width = 10, height = 10)
        plot_popkin(KinshipMatrix(), titles = "Kinship Matrix", names = F, ylab = "",
                    col_n = 100, oma = 0.5, mar_pad = 0.1, leg_width = 0.1, leg_title = "")
        dev.off()
      }
    )
  })

  observeEvent(input$resetKinship, {
    KinshipMatrix(NULL)
    Kinshiptitle1("")
    output$groupfile2 = renderUI({
      fileInput("groupfile2", "Group Info. (optional)", multiple = F, accept = c(".csv"))
    })
    showNotification("Data have been reset.")
    guide_Kinship("To run the kinship matrix, the input data must be in data.frame format. \nThe 'DAPC_Group_Info' CSV file from DAPC analysis is optional. \nPlease click the 'Run Kinship' button.")
  })


  output$Kinship = renderPlot({
    req(KinshipMatrix())
    if (Kinshiptitle1() == "Kinship Matrix") {
      plot_popkin(KinshipMatrix(), titles = "", names = F, ylab = "",
                  col_n = 100, oma = 0.5, mar_pad = 0.1, leg_width = 0.1, leg_title = "")
    }
  })

  output$download_Kinship_plot = renderUI({
    if (Kinshiptitle1() == "Kinship Matrix") {
      downloadButton("DKinshipplot", "Download Plot")
    }
  })

  output$download_Kinship_result = renderUI({
    if (Kinshiptitle1() == "Kinship Matrix") {
      downloadButton("DKinshipres", "Download Kinship Matrix")
    }
  })

  output$DKinshipres = downloadHandler(
    filename = paste0("Kinship_Matrix_Object-Method_", input$Kinship_method, ".rds"),
    content = function(file) {
      saveRDS(KinshipMatrix(), file)
    }
  )

  output$guide_Kinship = renderUI({ div(class = "guide-text-block", guide_Kinship()) })
  output$Kinshiptitle1 = renderText({ Kinshiptitle1() })

  ##### Scatter plot + #####
  shinyjs::hide("ScatterStatus")
  scatter_object = reactiveVal(NULL)
  scatter_file = reactiveVal("")
  scatter_data = reactiveVal(NULL)
  scatterInfo = reactiveVal(NULL)
  guide_scatter = reactiveVal("This page allows you to customize scatter plot \nYou can upload: \n▷ PCA Object (in RDS), or\n▷ DAPC Object (in RDS), and\n▷ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Scatter Plot' button.")
  scatter2D = reactiveVal("")
  scatter3D = reactiveVal("")
  Plot2D = reactiveVal(NULL)
  Plot3D = reactiveVal(NULL)

  output$scatter_Upload = renderUI({
    fileInput("scatterdata1", "", multiple = F, accept = c(".rds"))
  })

  observeEvent(input$scatterdata1, {
    data = readRDS(input$scatterdata1$datapath)
    scatter_object(data)
    req(input$Scatter_xvar, input$Scatter_yvar, input$Scatter_zvar)
    if (length(data) < 7){
      scatter_file("PCA")
      updateSelectInput(session, "Scatter_xvar", choices = paste0("PC", seq_along(data$sdev)), selected = "PC1")
      updateSelectInput(session, "Scatter_yvar", choices = paste0("PC", seq_along(data$sdev)), selected = "PC2")
      updateSelectInput(session, "Scatter_zvar", choices = paste0("PC", seq_along(data$sdev)), selected = "PC3")
      xvar = as.numeric(str_extract(input$Scatter_xvar, "\\d+"))
      yvar = as.numeric(str_extract(input$Scatter_yvar, "\\d+"))
      zvar = as.numeric(str_extract(input$Scatter_zvar, "\\d+"))
      scatter_data = data$x[, c(xvar, yvar, zvar)]
    }
    if (length(data) > 7){
      scatter_file("DAPC")
      updateSelectInput(session, "Scatter_xvar", choices = paste0("LD", seq_along(data$eig)), selected = "LD1")
      updateSelectInput(session, "Scatter_yvar", choices = paste0("LD", seq_along(data$eig)), selected = "LD2")
      updateSelectInput(session, "Scatter_zvar", choices = paste0("LD", seq_along(data$eig)), selected = "LD3")
      xvar = as.numeric(str_extract(input$Scatter_xvar, "\\d+"))
      yvar = as.numeric(str_extract(input$Scatter_yvar, "\\d+"))
      zvar = as.numeric(str_extract(input$Scatter_zvar, "\\d+"))
      if (is.na(xvar)) xvar = 1
      if (is.na(yvar)) yvar = 2
      if (is.na(zvar)) zvar = 3
      scatter_data = data$ind.coord[, c(xvar, yvar, zvar)]
    }
    scatter_data(scatter_data)
  })

  output$scatter_fileInfo = renderText({
    if (scatter_file() == "PCA") {
      paste0("-- PCA Object --", "\n",
             dim(scatter_object()$x)[1], " samples x ", dim(scatter_object()$x)[2], " PC axis")
    } else if (scatter_file() == "DAPC"){
      paste0("-- DAPC Object --", "\n",
             dim(scatter_object()$ind.coord)[1], " samples x ", dim(scatter_object()$ind.coord)[2], " LD axis")
    }
  })

  output$scatter_Upload2 = renderUI({
    fileInput("scatterdata2", "", multiple = F, accept = c(".csv"))
  })

  observeEvent(input$scatterdata2, {
    scatterInfo = read.csv(input$scatterdata2$datapath)
    scatterInfo(scatterInfo)
  })


  output$scatter_fileInfo2 = renderText({
    req(scatterInfo(), scatter_data())
    scatterInfo = scatterInfo()
    if (is.null(scatterInfo$ID)){
      paste0("**Warning**", "\n", "Data must contain a column named 'ID'!")
    } else if (dim(scatterInfo)[1] != dim(scatter_data())[1]){
      paste0("**Warning**", "\n", "Data must contain the same number of rows as the transformed data!")
    } else{
      updateSelectInput(session, "Scatter_colvar", choices = colnames(scatterInfo), selected = colnames(scatterInfo)[2])
      paste0("-- Group Info. & etc. --", "\n",
             "Names: ", paste(colnames(scatterInfo), collapse = ", "))
    }
  })

  observeEvent(input$runScatter, {
    req(scatter_object(), scatter_data(), scatterInfo())
    shinyjs::show("ScatterStatus")
    loc = which(colnames(scatterInfo()) == input$Scatter_colvar)

    xvar = as.numeric(str_extract(input$Scatter_xvar, "\\d+"))
    yvar = as.numeric(str_extract(input$Scatter_yvar, "\\d+"))
    zvar = as.numeric(str_extract(input$Scatter_zvar, "\\d+"))
    if (scatter_file() == "PCA") {
      scatter_data = scatter_object()$x[, c(xvar, yvar, zvar)]
      scatter_var = round(scatter_object()$sdev[c(xvar, yvar, zvar)]/sum(scatter_object()$sdev)*100, 2)
    } else if(scatter_file() == "DAPC") {
      scatter_data = scatter_object()$ind.coord[, c(xvar, yvar, zvar)]
      scatter_var = round(scatter_object()$pca.eig[c(xvar, yvar, zvar)]/sum(scatter_object()$pca.eig)*100, 2)
    }

    data = data.frame(
      xx = scatter_data[, 1],
      yy = scatter_data[, 2],
      zz = scatter_data[, 3],
      Group = as.character(scatterInfo()[, loc])
    )

    n_groups = n_distinct(scatterInfo()[, loc])
    colors = my_palette(input$Scatter_color, n_groups)

    text_content = paste0('"</br> ◉ ', colnames(scatterInfo()), ': ", scatterInfo()$', colnames(scatterInfo()), collapse = ', ')
    text = as.formula(paste("~paste(", text_content, ")"))

    if (scatter_file() == "PCA") {
      var1 = paste0(input$Scatter_xvar, " (", scatter_var[1], "%)")
      var2 = paste0(input$Scatter_yvar, " (", scatter_var[2], "%)")
      var3 = paste0(input$Scatter_zvar, " (", scatter_var[3], "%)")
    } else if(scatter_file() == "DAPC") {
      var1 = paste0(input$Scatter_xvar, " (", scatter_var[1], "%)")
      var2 = paste0(input$Scatter_yvar, " (", scatter_var[2], "%)")
      var3 = paste0(input$Scatter_zvar, " (", scatter_var[3], "%)")
    }

    if (input$Scatter_axis.title == "NULL"){
      var1 = ""
      var2 = ""
      var3 = ""
    }
    showline = TRUE
    zeroline = TRUE
    showticklabels = TRUE
    showlegend = TRUE
    if (input$Scatter_axis.line == "NULL"){ showline = FALSE }
    if (input$Scatter_zero.line == "NULL"){ zeroline = FALSE }
    if (input$Scatter_axis.tick.labels == "NULL"){ showticklabels = FALSE }
    if (input$Scatter_show.legend == "NULL"){ showlegend = FALSE }

    Plot2D = plot_ly(data, x = ~xx, y = ~yy,
                     type = "scatter", mode = "markers",
                     color = ~Group, colors = colors,
                     marker = list(size = input$Scatter_size, opacity = input$Scatter_opacity),
                     text = text) %>%
      layout(title = NA,
             xaxis = list(title = var1, showline = showline, showgrid = FALSE, zeroline = zeroline, zerolinecolor = "grey", showticklabels = showticklabels),
             yaxis = list(title = var2, showline = showline, showgrid = FALSE, zeroline = zeroline, zerolinecolor = "grey", showticklabels = showticklabels),
             legend = list(title = list(text = input$Scatter_colvar)), showlegend = showlegend)
    Plot2D(Plot2D)

    Plot3D = plot_ly(data, x = ~xx, y = ~yy, z = ~zz,
                     type = "scatter3d", mode = "markers",
                     color = ~Group, colors = colors,
                     marker = list(size = input$Scatter_size, opacity = input$Scatter_opacity),
                     text = text) %>%
      layout(title = NA,
             scene = list(
               xaxis = list(title = var1, showline = showline, showgrid = FALSE, zeroline = zeroline, zerolinecolor = "grey", showticklabels = showticklabels),
               yaxis = list(title = var2, showline = showline, showgrid = FALSE, zeroline = zeroline, zerolinecolor = "grey", showticklabels = showticklabels),
               zaxis = list(title = var3, showline = showline, showgrid = FALSE, zeroline = zeroline, zerolinecolor = "grey", showticklabels = showticklabels)),
             showlegend = showlegend,
             legend = list(title = list(text = input$Scatter_colvar)))
    Plot3D(Plot3D)
    scatter2D("2D Scatter Plot")
    scatter3D("3D Scatter Plot")
    shinyjs::hide("ScatterStatus")
    guide_scatter("You can customize the scatter plot and then click the 'Run Scatter Plot' button again.")
  })

  observeEvent(input$resetScatter, {
    scatter_object(NULL)
    scatter_file("")
    scatter_data(NULL)
    scatterInfo(NULL)
    Plot2D(NULL)
    Plot3D(NULL)
    scatter2D("")
    scatter3D("")
    guide_scatter("This page allows you to customize scatter plot \nYou can upload: \n▷ PCA Object (in RDS), or\n▷ DAPC Object (in RDS), and\n▷ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Scatter Plot' button.")
    output$scatter_Upload = renderUI({
      fileInput("scatterdata1", "", multiple = F, accept = c(".rds"))
    })
    output$scatter_Upload2 = renderUI({
      fileInput("scatterdata2", "", multiple = F, accept = c(".csv"))
    })
    showNotification("Data have been reset.")
  })

  output$scatter2DPlot = renderPlotly({
    Plot2D()
  })

  output$scatter3DPlot = renderPlotly({
    Plot3D()
  })

  output$download_scatter2DPlot_HTML = renderUI({
    if (scatter2D() == "2D Scatter Plot") {
      downloadButton("Dscatter2DPlot_HTML", "Download Plot")
    }
  })

  output$Dscatter2DPlot_HTML = downloadHandler(
    filename = "2D_Scatter_Plot.html",
    content = function(file) {
      htmlwidgets::saveWidget(Plot2D(), file)
    })

  output$download_scatter3DPlot_HTML = renderUI({
    if (scatter3D() == "3D Scatter Plot") {
      downloadButton("Dscatter3DPlot_HTML", "Download Plot")
    }
  })

  output$Dscatter3DPlot_HTML = downloadHandler(
    filename = "3D_Scatter_Plot.html",
    content = function(file) {
      htmlwidgets::saveWidget(Plot3D(), file)
    })

  output$guide_scatter = renderUI({ div(class = "guide-text-block", guide_scatter()) })

  output$scatter2D = renderText({ scatter2D() })
  output$scatter3D = renderText({ scatter3D() })

  ##### Tree Plot + #####
  shinyjs::hide("TreeStatus")
  tree_object = reactiveVal(NULL)
  treeInfo = reactiveVal(NULL)
  guide_Tree = reactiveVal("This page allows you to customize a phylogenetic tree plot. You can upload:\n▷ UPGMA Object (in RDS), or\n▷ NJ Object (in RDS), and\n▷ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Tree Plot' button.")
  TreePlot1 = reactiveVal("")
  TreePlot = reactiveVal(NULL)

  output$Tree_Upload = renderUI({
    fileInput("treedata1", "", multiple = F, accept = c(".rds"))
  })

  observeEvent(input$treedata1, {
    data = readRDS(input$treedata1$datapath)
    tree_object(data)
  })

  output$Tree_fileInfo = renderText({
    req(tree_object())
    data = tree_object()
    if (is.null(data$node.label)){
      paste0("-- NJ Tree Object --", "\n",
             length(data$tip.label), " samples")
    } else if (!is.null(data$node.label)){
      paste0("-- UPGMA Tree Object --", "\n",
             length(data$tip.label), " samples")
    }
  })

  output$Tree_Upload2 = renderUI({
    fileInput("treedata2", "", multiple = F, accept = c(".csv"))
  })

  observeEvent(input$treedata2, {
    treeInfo = read.csv(input$treedata2$datapath)
    treeInfo(treeInfo)
  })

  output$Tree_fileInfo2 = renderText({
    req(treeInfo(), tree_object())
    treeInfo = treeInfo()
    if (is.null(treeInfo$ID)){
      paste0("**Warning**", "\n", "Data must contain a column named 'ID'!")
    } else if (dim(treeInfo)[1] != length(tree_object()$tip.label)){
      paste0("**Warning**", "\n", "Data must contain the same number of samples as the object data!")
    } else{
      updateSelectInput(session, "Tree_str_color_var", choices = colnames(treeInfo), selected = colnames(treeInfo)[2])
      updateSelectInput(session, "Tree_taxa_color_var", choices = colnames(treeInfo), selected = colnames(treeInfo)[2])
      updateSelectInput(session, "Tree_sym_color_var", choices = colnames(treeInfo), selected = colnames(treeInfo)[2])
      updateSelectInput(session, "Tree_sym_shape_var", choices = colnames(treeInfo), selected = colnames(treeInfo)[2])
      paste0("-- Group Info. & etc. --", "\n",
             "Names: ", paste(colnames(treeInfo), collapse = ", "))
    }
  })

  observeEvent(input$runTree, {
    req(tree_object(), treeInfo())
    shinyjs::show("TreeStatus")
    tree = tree_object()
    data = treeInfo()
    Layout = Tree_layout_choice[input$Tree_str_layout]

    plot1 = ggtree(tree, layout = Layout)

    var0 = input$Tree_str_color_var
    var1 = input$Tree_taxa_color_var
    var2 = input$Tree_sym_color_var
    var3 = input$Tree_sym_shape_var

    str_color_var = split(data$ID, data[var0])
    taxa_color_var = split(data$ID, data[var1])
    sym_color_var = split(data$ID, data[var2])
    sym_shape_var = split(data$ID, data[var3])

    plot1 = groupOTU(plot1, str_color_var, "str_color")
    plot1 = groupOTU(plot1, taxa_color_var, "taxa_color")
    plot1 = groupOTU(plot1, sym_color_var, "sym_color")
    plot1 = groupOTU(plot1, sym_shape_var, "sym_shape")

    n_var0 = n_distinct(data[var0])
    n_var1 = n_distinct(data[var1])
    n_var2 = n_distinct(data[var2])
    colors0 = my_palette(input$Tree_str_color, n_var0)
    colors1 = my_palette(input$Tree_taxa_color, n_var1)
    colors2 = my_palette(input$Tree_sym_color, n_var2)

    if (n_distinct(data[var3]) < 5){
      shapes = sample(c(15, 16, 17, 18), n_distinct(data[var3]))
    } else {
      shapes = sample(c(1:18), n_distinct(data[var3]), prob = c(rep(0.05, 14), rep(0.9, 4)))
    }
    legend = Legend_choice[input$Tree_legend]

    plot2 = plot1 +
      geom_tree(aes(color = str_color), layout = Layout, size = input$Tree_str_size) +
      scale_color_manual(values = colors0) +
      guides(color = guide_legend(title = input$Tree_str_color_var)) +
      theme(legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 12),
            legend.position = legend)
    taxa_align = TRUE
    if (input$Tree_taxa_align == "FALSE"){ taxa_align = FALSE }
    if (input$Tree_taxa == "Show"){
      plot3 = plot2 +
        new_scale_color() +
        geom_tiplab(aes(color = taxa_color), hjust = -0.1, align = taxa_align, linesize = 0.5, size = input$Tree_taxa_size) +
        scale_color_manual(name = "taxa_color", values = colors1) +
        guides(color = guide_legend(title = input$Tree_taxa_color_var))
    } else {
      plot3 = plot2
    }
    if (input$Tree_sym == "Show"){
      plot4 = plot3 +
        new_scale_color() +
        geom_tippoint(aes(color = sym_color, shape = sym_shape), size = input$Tree_sym_size) +
        scale_color_manual(name = "sym_color", values = colors2) +
        scale_shape_manual(name = "sym_color", values = shapes) +
        guides(color = guide_legend(title = input$Tree_sym_color_var),
               shape = guide_legend(title = input$Tree_sym_shape_var))
    } else {
      plot4 = plot3
    }

    if (input$Tree_treescale == "Show"){ plot4 = plot4 + theme_tree2() }
    if (input$Tree_Bt == "Show"){ plot4 = plot4 + geom_label2(size = 2, aes(label = label, subset = !is.na(as.numeric(label)))) }
    TreePlot(plot4)
    TreePlot1("Phylogenetic Tree Plot")
    shinyjs::hide("TreeStatus")
    guide_Tree("You can customize the tree plot and then click the 'Run Tree Plot' button again.")
  })

  observeEvent(input$resetTree, {
    tree_object(NULL)
    treeInfo(NULL)
    TreePlot(NULL)
    TreePlot1("")
    guide_Tree("This page allows you to customize a phylogenetic tree plot. You can upload:\n▷ UPGMA Object (in RDS), or\n▷ NJ Object (in RDS), and\n▷ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Tree Plot' button.")
    output$Tree_Upload = renderUI({
      fileInput("treedata1", "", multiple = F, accept = c(".rds"))
    })
    output$Tree_Upload2 = renderUI({
      fileInput("treedata2", "", multiple = F, accept = c(".csv"))
    })
    showNotification("Data have been reset.")
  })

  output$TreePlot = renderPlot({
    req(TreePlot())
    TreePlot()
  })

  output$download_TreePlot = renderUI({
    if (TreePlot1() == "Phylogenetic Tree Plot") {
      downloadButton("DTreePlot", "Download Plot")
    }
  })

  output$DTreePlot = downloadHandler(
    filename = paste0("Phylogenetic_Tree.pdf"),
    content = function(file) {
      ggsave(file, plot = TreePlot(), device = "pdf", width = 12, height = 12)
    })

  output$TreePlot1 = renderText({ TreePlot1() })
  output$guide_Tree = renderUI({ div(class = "guide-text-block", guide_Tree()) })

  ##### Page 5: Genetic Diversity #####
  ##### Diversity Parameters #####
  guide_GD = reactiveVal("To analyze genetic diversity, the input data must be in data.frame format.\nYou also need to upload a Site Info file (in RDS format), which can be downloaded from the 'Data Input' page after transforming the VCF to a data.frame file.\nPlease click the 'Analysis' button.")
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
        selectInput("Type", "By each group:", choices = c("Statistics per site", "Statistics by group"), selected = "Statistics by group")
      })
      output$GDgroupresults = DT::renderDataTable({
        DT::datatable(group_stat())
      })
      GDtitle3("Genetic Diversity Statistics by Group")
      pre_results = pre_results()
      pre_results[[31]] = paste0("The average observed heterozygosity (Ho) of each group, Group 1 to Group ", length(group_stat[,2])-1, ": ", paste(group_stat[-1,3], collapse = ", "))
      pre_results[[32]] = paste0("The average expected heterozygosity (He) of each group, Group 1 to Group ", length(group_stat[,2])-1, ": ", paste(group_stat[-1,2], collapse = ", "))
      pre_results[[33]] = paste0("The average Unbiased pi diversity (Pi) of each group, Group 1 to Group ", length(group_stat[,2])-1, ": ", paste(group_stat[-1,6], collapse = ", "))
      pre_results[[34]] = paste0("The number of exclusive allele of each group, Group 1 to Group ", length(group_stat[,2])-1, ": ", paste(group_stat[-1,8], collapse = ", "))
      pre_results[[35]] = paste0("The number of fixed allele of each group, Group 1 to Group ", length(group_stat[,2])-1, ": ", paste(group_stat[-1,9], collapse = ", "))
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
    guide_GD("The analysis of genetic diversity is complete. \nPlease review the results.")
    GDtitle2("Genetic Diversity Statistics by per Site")
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
    guide_GD("To analyze genetic diversity, the input data must be in data.frame format.\nYou also need to upload a Site Info file (in RDS format), which can be downloaded from the 'Data Input' page after transforming the VCF to a data.frame file.\nPlease click the 'Analysis' button.")
  })

  observeEvent(input$Type, {
    req(site_stat())
    if (input$Type == "Statistics per site") {
      if (!is.null(groupInfo3())) {
        output$Parameter = renderUI({
          selectInput("Parameter", "Select a parameter:", choices = c("Minor allele frequency", "Major allele frequency", "Expected heterozygosity (He)",
                                                                      "Observed heterozygosity (Ho)", "Nei's genetic diversity (Nei)", "Polymorphism information content (PIC)",
                                                                      "Unbiased pi diversity (Pi)", "Missing rate", "HWE p-value (pval)",
                                                                      "Inbreeding coefficient within individuals (Fis)", "Fixation index among populations (Fst)", "Total inbreeding coefficient (Fit)"), selected = "Unbiased pi diversity (Pi)")
        })
      } else{
        output$Parameter = renderUI({
          selectInput("Parameter", "Select a parameter:", choices = c("Minor allele frequency", "Major allele frequency", "Expected heterozygosity (He)",
                                                                      "Observed heterozygosity (Ho)", "Nei's genetic diversity (Nei)", "Polymorphism information content (PIC)",
                                                                      "Unbiased pi diversity (Pi)", "Missing rate"), selected = "Unbiased pi diversity (Pi)")
        })
      }
    } else if (input$Type == "Statistics by group") {
      output$Parameter = renderUI({
        selectInput("Parameter", "Select a parameter:", choices = c("Sample size", "Expected heterozygosity (He)", "Observed heterozygosity (Ho)",
                                                                    "Nei's genetic diversity (Nei)", "Polymorphism information content (PIC)", "Unbiased pi diversity (Pi)",
                                                                    "Inbreeding coefficient within individuals (Fis)", "Exclusive allele", "Fixed allele"), selected = "Unbiased pi diversity (Pi)")
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
      } else if (input$Parameter == "Unbiased pi diversity (Pi)"){
        GDplot = GDsiteplot(site_stat(), "Pi", "Unbiased pi diversity")
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
      GDtitle1("Plot of Genetic Diversity Statistics per Site")
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
      } else if (input$Parameter == "Unbiased pi diversity (Pi)"){
        GDplot = GDgroupplot(group_stat(), "Pi", "Unbiased Pi diversity")
      } else if (input$Parameter == "Inbreeding coefficient within individuals (Fis)"){
        GDplot = GDgroupplot(group_stat(), "Fis", "Inbreeding coefficient within individuals (Fis)")
      } else if (input$Parameter == "Exclusive allele"){
        GDplot = GDgroupplot(group_stat(), "Exclusive_allele", "Exclusive allele")
      } else if (input$Parameter == "Fixed allele"){
        GDplot = GDgroupplot(group_stat(), "Fixed_allele", "Fixed allele")
      }
      GDplot(GDplot)
      GDtitle1("Plot of Genetic Diversity Statistics by Group")
    }
  })

  output$download_GD_plot = renderUI({
    if (GDtitle2() == "Genetic Diversity Statistics by per Site") {
      downloadButton("DGDplot", "Download Plot")
    }
  })

  output$DGDplot = downloadHandler(
    filename = function() {
      paste0("Genetic_Diversity_", input$Type, "-", input$Parameter, ".pdf")
    },
    content = function(file) {
      width = if (input$Type == "Statistics per site") 8 else 6
      pdf(file, width = width, height = 5.8)
      print(GDplot())
      dev.off()
    }
  )

  output$download_GD_site = renderUI({
    if (GDtitle2() == "Genetic Diversity Statistics by per Site") {
      downloadButton("DGD_site", "Download Genetic Diversity (per site)")
    }
  })

  output$DGD_site = downloadHandler(
    filename = "Genetic_Diversity_per_Site.rds",
    content = function(file) {
      saveRDS(site_stat(), file)
    }
  )

  output$download_GD_group = renderUI({
    if (GDtitle3() == "Genetic Diversity Statistics by Group") {
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
    if (GDtitle3() == "Genetic Diversity Statistics by Group") {
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
    if (GDtitle2() == "Genetic Diversity Statistics by per Site") {
      downloadButton("D_GD", "Download Diversity Object")
    }
  })

  output$D_GD = downloadHandler(
    filename = "Genetic_Diversity_Object.rds",
    content = function(file) {
      saveRDS(popgen(), file)
    }
  )

  output$GDplot = renderPlot({
    req(GDplot())
    if (GDtitle2() == "Genetic Diversity Statistics by per Site") {
      GDplot()
    }
  })

  output$guide_GD = renderUI({ div(class = "guide-text-block", guide_GD())})
  output$GDtitle1 = renderText({ GDtitle1() })
  output$GDtitle2 = renderText({ GDtitle2() })
  output$GDtitle3 = renderText({ GDtitle3() })

  ##### Circos Plot #####
  guide_Circos = reactiveVal("To run the sliding window analysis, you need to run 'Diversity Parameter' first! \nPlease select the optimal window size and step, then click the 'Run Sliding Window' button.")
  Circostitle1 = reactiveVal("")
  Circostitle2 = reactiveVal("")
  GDInfo = reactiveVal("")
  Track3 = reactiveVal(NULL)
  Track4 = reactiveVal(NULL)
  Track5 = reactiveVal(NULL)
  Track6 = reactiveVal(NULL)
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
    guide_Circos("The 'Sliding Window' analysis is complete. \nPlease select parameters for each track, then click the 'Run Circos Plot' button.")
    Circostitle1("Sliding Window Data")

    output$SWresults = DT::renderDataTable({
      DT::datatable(SW_data(), options = list(pageLength = 5))
    })

    output$D_SW = downloadHandler(
      filename = paste0("Diversity_Sliding_Window-Window_Size", input$WindowSize, "-Step_Size",input$StepSize,".csv"),
      content = function(file) {
        write.csv(SW_data(), file, row.names = FALSE)
      }
    )
  })

  output$download_SW = renderUI({
    if (Circostitle1() == "Sliding Window Data") {
      downloadButton("D_SW", "Download Sliding Window Data")
    }
  })

  observeEvent(input$resetSW, {
    SW_data = reactiveVal(NULL)
    Circostitle1("")
    output$SWresults = DT::renderDataTable({ DT::datatable(NULL) })
    showNotification("Data have been reset.")
    guide_Circos("To run the sliding window analysis, you need to run 'Diversity Parameter' first! \nPlease select the optimal window size and step, then click the 'Run Sliding Window' button.")
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
      if (i == 1){
        selectInput(paste0("Track", i+2), paste("Track ", i+2 ,": Parameter"), choices = c(input$SelePara, "SNP Density"), selected = "SNP Density")
      } else {
        selectInput(paste0("Track", i+2), paste("Track ", i+2 ,": Parameter"), choices = c(input$SelePara), selected = input$SelePara[i])
      }
    })
  })

  observeEvent(input$runCircos, {
    req(SW_data(), Chr_Info(), input$Track3)
    shinyjs::show("CircosStatus")

    Track3(input$Track3)
    Track4(input$Track4)
    Track5(input$Track5)
    Track6(input$Track6)

    pdf_path = tempfile(fileext = ".pdf")
    generateCircosPlot(Chr_Info(), SW_data(), pdf_path, Track3(), Track4(), Track5(), Track6())

    output$downloadCircosplot = renderUI({
      if (Circostitle2() == "Circos Plot"){
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
    Circostitle2("Circos Plot")
    guide_Circos("The Circos plot is complete!\nPlease download the Circos plot and review the results.")
  })

  observeEvent(input$resetCircos, {
    output$Chr_Info = renderUI({ fileInput("Chr_Info", "Chromosome Info.* (required)", multiple = F, accept = c(".csv")) })
    Chr_Info(NULL)
    Track3(NULL)
    Track4(NULL)
    Track5(NULL)
    Track6(NULL)
    n_SelePara(0)
    output$Track3 = renderUI({
      req(SW_data())
      lapply(seq_len(n_SelePara()), function(i) {
        if (i == 1){
          selectInput(paste0("Track", i+2), paste("Track ", i+2 ,": Parameter"), choices = c(input$SelePara, "SNP Density"), selected = "SNP Density")
        } else {
          selectInput(paste0("Track", i+2), paste("Track ", i+2 ,": Parameter"), choices = c(input$SelePara), selected = input$SelePara[i])
        }
      })
    })
    showNotification("Data have been reset.")
    Circostitle2("")
    guide_Circos("The 'Sliding Window' analysis is complete. \nPlease select parameters for each track, then click the 'Run Circos Plot' button.")
  })

  output$Circosplotinfo = renderText({
    if (Circostitle2() == "Circos Plot"){
      paste0("Track 1 & 2: ", input$Track1, "\n",
             "Track 3: ", Track3(), "\n",
             "Track 4: ", Track4(), "\n",
             "Track 5: ", Track5(), "\n",
             "Track 6: ", Track6(), "\n")
    }
  })
  output$guide_Circos = renderUI({ div(class = "guide-text-block",  guide_Circos()) })
  output$Circostitle1 = renderText({ Circostitle1() })
  output$Circostitle2 = renderText({ Circostitle2() })

  ##### Genetic Distance #####
  guide_GT = reactiveVal("To run the genetic distance analysis, the input data must be a genind file with 'Group Info.' \nYou can obtain this genind file from the 'Data Conversion' page after you have both the data.frame and Group Info. files.")
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
    GTtitle1("Genetic Distance Plot")
    GTtitle2("Genetic Distance Matrix")
    guide_GT("Genetic distance analysis is complete.")
    output$GTresults = renderTable({ GTmatrix() }, rownames = TRUE)
    pre_results = pre_results()
    pre_results[[30]] = "# Genetic Diversity"
    pre_results[[37]] = paste0("The Genetic Distance Matrix between pairs of groups:")
    combined = apply(GTdf[, c("Pair1", "Pair2", "GeneticDistance")], 1, function(row) {
      paste0(row[1], "-", row[2], ": ", row[3])
    })
    pre_results[[38]] = paste(combined, collapse = "; ")
    pre_results(pre_results)

    output$DGTplot = downloadHandler(
      filename = function() {
        paste0("Genetic_Distance_Plot-", input$GT_method, ".pdf")
      },
      content = function(file) {
        pdf(file, width = 7, height = 7)
        print(GTplot())
        dev.off()
      }
    )

    output$DGTresult = downloadHandler(
      filename = paste0("Genetic_Distance_Matrix-", input$GT_method, ".csv"),
      content = function(file) {
        write.csv(GTmatrix(), file, row.names = TRUE)
      }
    )
  })

  observeEvent(input$resetGT, {
    GTmatrix(NULL)
    GTdf(NULL)
    showNotification("Data have been reset.")
    output$GTresults = renderTable({ NULL })
    GTtitle1("")
    GTtitle2("")
    guide_GT("To run the genetic distance analysis, the input data must be a genind file with 'Group Info.' \nYou can obtain this genind file from the 'Data Conversion' page after you have both the data.frame and Group Info. files.")
  })

  output$GTplot = renderPlot({
    req(GTdf())
    if (guide_GT() == "Genetic distance analysis is complete.") {
      mid = (max(GTdf()[, 3]) - min(GTdf()[, 3])) / 2
      GTplot = ggplot(GTdf(), aes(x = Pair1, y = Pair2, fill = GeneticDistance)) +
        geom_tile() +
        geom_text(aes(label = GeneticDistance), color = "black", size = 4.5) +
        scale_fill_gradient2(low = "white", mid = "#f19372", high = "#b53c12", midpoint = mid, name = input$GT_method) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0), position = "right") +
        theme(
          axis.text = element_text(colour = "black", size = 12),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 11)
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

  output$download_GT_result = renderUI({
    if (GTtitle2() == "Genetic Distance Matrix") {
      downloadButton("DGTresult", "Download Genetic Distance Matrix")
    }
  })

  output$guide_GT = renderUI({ div(class = "guide-text-block", guide_GT()) })
  output$GTtitle1 = renderText({ GTtitle1() })
  output$GTtitle2 = renderText({ GTtitle2() })

  ##### AMOVA #####
  guide_AMOVA = reactiveVal("To run AMOVA, the input data must be a genind file with 'Group Info.' \nYou can obtain this genind file from the 'Data Conversion' page after you have both the data.frame and Group Info. files.")
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
    AMOVAtitle1("AMOVA Variance")
    AMOVAtitle3("AMOVA Table")
    guide_AMOVA("AMOVA is complete. You can now select the number of permutations for the test.")
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
    guide_AMOVA("To run AMOVA, the input data must be a genind file with 'Group Info.' \nYou can obtain this genind file from the 'Data Conversion' page after you have both the data.frame and Group Info. files.")
  })

  observeEvent(input$runTest, {
    req(amova.result())
    shinyjs::show("AMOVAStatus")
    test = randtest(amova.result(), nrepet = input$nperm)
    amova.test(test)
    AMOVA_res = cbind(AMOVA_res(), p_value = c(paste("<", test$pvalue), ""))
    AMOVA_res(AMOVA_res)
    shinyjs::hide("AMOVAStatus")
    AMOVAtitle2("AMOVA Test")
    guide_AMOVA("AMOVA is complete.")
    output$AMOVAresults = renderTable({ AMOVA_res() }, rownames = FALSE)
    pre_results = pre_results()
    pre_results[[40]] = paste0("Estimated variance percentage (%) and p-value of population strata: " ,
                               "Among groups: ", AMOVA_res$Variance_percentage[1], ", p-value: ", AMOVA_res$p_value[1],
                               "; Among individual within groups: ", AMOVA_res$Variance_percentage[2], ", p-value: ", AMOVA_res$p_value[2],
                               "; Within individuals: ", AMOVA_res$Variance_percentage[3], ", p-value: ", AMOVA_res$p_value[3])
    pre_results(pre_results)
  })

  observeEvent(input$resetTest, {
    amova.result(NULL)
    amova.test(NULL)
    showNotification("Data have been reset.")
    AMOVAtitle2("")
    AMOVAtitle3("")
    output$AMOVAresults = renderTable({ NULL }, rownames = FALSE)
    guide_AMOVA("To run AMOVA, the input data must be a genind file with 'Group Info.' \nYou can obtain this genind file from the 'Data Conversion' page after you have both the data.frame and Group Info. files.")
  })

  output$AMOVAvarplot = renderPlot({
    req(AMOVA_res())
    if (AMOVAtitle1() == "AMOVA Variance") {
      plot_data = as.data.frame(AMOVA_res())
      plot = ggplot(plot_data[1:3,], aes(x = "", y = Variance_percentage, fill = Source_of_variance)) +
        geom_col(color = "grey") +
        geom_label_repel(aes(label = paste0(Variance_percentage, "%")), position = position_stack(vjust = 0.5), size = 8, show.legend = F) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = c("#7bbbd6", "#ebe6e5", "#bbd67b")) +
        theme_void() +
        guides(fill = guide_legend(title = "", position = "bottom", nrow = 3)) +
        theme(legend.text = element_text(size = 18))
      AMOVAvarplot(plot)
      plot
    }
  })

  output$AMOVAtestplot = renderPlot({
    req(amova.test())
    if (AMOVAtitle2() == "AMOVA Test") {
      plot(amova.test())
    }
  })

  output$download_AMOVA_plot = renderUI({
    if (AMOVAtitle1() == "AMOVA Variance") {
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
    if (AMOVAtitle2() == "AMOVA Test") {
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
    if (AMOVAtitle3() == "AMOVA Table") {
      downloadButton("AMOVAResults", "Download AMOVA Table")
    }
  })

  output$AMOVAResults = downloadHandler(
    filename = "AMOVA_Table.csv",
    content = function(file) {
      write.csv(AMOVA_res(), file, row.names = FALSE)
    }
  )

  output$guide_AMOVA = renderUI({ div(class = "guide-text-block", guide_AMOVA()) })
  output$AMOVAtitle1 = renderText({ AMOVAtitle1() })
  output$AMOVAtitle2 = renderText({ AMOVAtitle2() })
  output$AMOVAtitle3 = renderText({ AMOVAtitle3() })

  ##### Page 6: Selection Sweep #####
  ##### pcadapt #####
  guide_pcadapt = reactiveVal("To run pcadapt, the input data must be in data.frame format. \nYou also need to upload a Site Info file (in RDS format), which can be downloaded from the 'Data Input' page after transforming the VCF to a data.frame file.\nPlease click the 'Run pcadapt' button.")
  pcadaptfileInfo = reactiveVal("")
  pcadapttitle1 = reactiveVal("")
  pcadapttitle2 = reactiveVal("")
  pcadapttitle3 = reactiveVal("")
  pcadapttitle4 = reactiveVal("")
  pcadapttitle5 = reactiveVal("")
  SNP_Info = reactiveVal(NULL)
  pcadapt_data = reactiveVal(NULL)
  pcadapt_data2 = reactiveVal(NULL)
  pcadapt_data3 = reactiveVal(NULL)
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
    Site_Info(Site_Info)

    Site_Info$Pos = as.numeric(Site_Info$Pos)
    Site_Info$Chr = as.character(Site_Info$Chr)

    for (i in 1:length(unique(Site_Info$Chr))) {
      if (i>1){
        end = max(Site_Info[Site_Info$Chr == i-1, 2])
        Site_Info[Site_Info$Chr == i, 2] = as.numeric(Site_Info[Site_Info$Chr == i, 2]) + end
      }
    }

    SNP_Info(Site_Info)
    Chr_axis = SNP_Info() %>%
      group_by(Chr) %>%
      summarise(center = (max(Pos, na.rm = TRUE) + min(Pos, na.rm = TRUE)) / 2, .groups = "drop")
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

    max_range = max(c(max(pcadapt_data$expected), max(pcadapt_data$observed)))
    pcadaptplot2 = ggplot(pcadapt_data, aes(x = expected, y = observed)) +
      geom_point(size = 1, color = "#186da9") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.7) +
      labs(x = expression(Expected -log[10](italic(p))), y = expression(Observed -log[10](italic(p)))) +
      theme_classic() +
      theme(
        axis.title.x =   element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
      scale_x_continuous(limits = c(0, max_range)) +
      scale_y_continuous(limits = c(0, max_range))
    pcadaptplot2(pcadaptplot2)

    pcadaptplot3 = ggplot(pcadapt_data, aes(x = pvalue)) +
      geom_histogram(binwidth = 0.02, fill = "#186da9", color = "grey70", linewidth = 0.2, alpha = 0.8) +
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
      geom_histogram(aes(y = ..density..), binwidth = max(pcadapt_data$statistic)/50, fill = "#186da9", color = "grey70", linewidth = 0.2, alpha = 0.8) +
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
    pcadapttitle1("Manhattan Plot")
    pcadapttitle2("QQ Plot of p-values")
    pcadapttitle3("Histogram of p-values")
    pcadapttitle4("Histogram of Test Statistics")
    pcadapttitle5("Significant SNPs")
    guide_pcadapt("The pcadapt analysis is complete.")
  })

  observeEvent(input$resetpcadapt, {
    output$SNPthin_size = renderUI({ NULL })
    output$SNPthin_thr = renderUI({ NULL })
    pcadapt_data(NULL)
    pcadapt_data2(NULL)
    pcadapt_data3(NULL)
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
    guide_pcadapt("To run pcadapt, the input data must be in data.frame format. \nYou also need to upload a Site Info file (in RDS format), which can be downloaded from the 'Data Input' page after transforming the VCF to a data.frame file.\nPlease click the 'Run pcadapt' button.")
  })

  output$pcadaptplot1 = renderPlot({
    req(pcadapt_data(), input$pcadapt_adj)
    if (pcadapttitle1() == "Manhattan Plot") {
      data = SNP_Info()
      pcadapt_data = pcadapt_data()
      Chr_axis = Chr_axis()

      method = pval_adj_method_choice[input$pcadapt_adj]

      padj = p.adjust(pcadapt_data$pvalue, method = as.character(method))
      outliers = which(padj < as.numeric(input$pcadapt_alpha))

      data = data %>%
        mutate(pvalue = pcadapt_data$pvalue) %>%
        mutate(adjust_pvalue = padj) %>%
        mutate(signif = ifelse(row_number() %in% outliers, "Yes", "No"))
      data$Chr = factor(data$Chr, levels = sort(unique(as.numeric(data$Chr))))
      n_chr = length(levels(data$Chr))
      colors = rep(c("#cbbc1d", "#5e929d"), length.out = n_chr)

      data2 = data
      Site_Info = Site_Info()
      data2[,1:3] = Site_Info[,1:3]
      pcadapt_data3(data2)


      pcadaptplot1 = ggplot(data, aes(x = Pos, y = -log10(pvalue))) +
        geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
        scale_color_manual(values = colors) +
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
      pcadapt_data2(data2)
      if (dim(data2)[1]>0){
        pcadaptplot1 = pcadaptplot1 +
          geom_point(data = data2, aes(Pos, -log10(pvalue)), color = "red", size = 1, alpha = 0.9) +
          geom_hline(yintercept = min(-log10(data2$pvalue)), color = "#ff4500", linetype = "dashed", linewidth = 0.6)
      }
      pcadaptplot1(pcadaptplot1)
      pre_results = pre_results()
      pre_results[[45]] = "# Selection Sweep"
      pre_results[[46]] = paste0("The results of 'PCA-based genome scan for selection (pcadapt)': ")
      pre_results[[47]] = paste0(dim(data2)[1] , " significant selection signatures (SNP loci) were detected across ",
                                 length(table(data2[,1])), " chromosomes with the ",
                                 input$pcadapt_adj, " P-value adjustment method at α = ", input$pcadapt_alpha)
      pre_results(pre_results)
      pcadaptplot1()
    }
  })

  output$pcadapt_adj = renderUI({
    if (pcadapttitle1() == "Manhattan Plot") {
      selectInput("pcadapt_adj", "P-value adjustment method", choices = names(pval_adj_method_choice), selected = "Benjamini & Hochberg (FDR)")
    }
  })

  output$pcadapt_alpha = renderUI({
    if (pcadapttitle1() == "Manhattan Plot") {
      selectInput("pcadapt_alpha", "Level of significance (alpha)", choices = c(0.1, 0.05, 0.01, 0.005, 0.001), selected = 0.1)
    }
  })

  output$pcadapt_result = renderText({
    req(pcadapt_data2())
    chr = table(pcadapt_data2()[,1])
    out = c()
    for (i in seq_len(length(chr))) {
      if (as.numeric(chr[i]) > 0){
        out[i] = paste0("Chr ", names(chr)[i], ": ", as.numeric(chr[i]), " significant SNPs", "\n")
      } else{
        out[i] = paste0("NA", "\n")
      }
    }
    out = out[!grepl("NA", out)]
    out = paste(out, collapse = "")
    paste("Number of significant SNPs: ", dim(pcadapt_data2())[1], "\n",
          "---------------", "\n",
          out,
          sep = "")
  })

  output$download_pcadapt_plot1 = renderUI({
    if (pcadapttitle1() == "Manhattan Plot") {
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

  output$download_pcadapt = renderUI({
    if (pcadapttitle1() == "Manhattan Plot") {
      downloadButton("Dpcadapt", "Download pcadapt p-value (per site)")
    }
  })

  output$Dpcadapt = downloadHandler(
    filename = "pcadapt_p-value_per_site.rds",
    content = function(file) {
      saveRDS(pcadapt_data3(), file)
    }
  )

  output$pcadaptplot2 = renderPlot({
    req(pcadapt_data())
    if (pcadapttitle2() == "QQ Plot of p-values") {
      pcadaptplot2()
    }
  })

  output$download_pcadapt_plot2 = renderUI({
    if (pcadapttitle2() == "QQ Plot of p-values") {
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
    if (pcadapttitle2() == "QQ Plot of p-values") {
      pcadaptplot3()
    }
  })

  output$download_pcadapt_plot3 = renderUI({
    if (pcadapttitle2() == "QQ Plot of p-values") {
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
    if (pcadapttitle2() == "QQ Plot of p-values") {
      pcadaptplot4()
    }
  })

  output$download_pcadapt_plot4 = renderUI({
    if (pcadapttitle2() == "QQ Plot of p-values") {
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
    if (pcadapttitle2() == "QQ Plot of p-values") {
      downloadButton("Dpcadapt_results", "Download Table")
    }
  })

  output$Dpcadapt_results = downloadHandler(
    filename = "pcadapt_Significant_SNPs.csv",
    content = function(file) {
      write.csv(pcadapt_data2(), file, row.names = FALSE)
    }
  )

  output$guide_pcadapt = renderUI({ div(class = "guide-text-block", guide_pcadapt()) })
  output$pcadapttitle1 = renderText({ pcadapttitle1() })
  output$pcadapttitle2 = renderText({ pcadapttitle2() })
  output$pcadapttitle3 = renderText({ pcadapttitle3() })
  output$pcadapttitle4 = renderText({ pcadapttitle4() })
  output$pcadapttitle5 = renderText({ pcadapttitle5() })

  ##### OutFLANK #####
  guide_OutFLANK = reactiveVal("To run OutFLANK, the input data must be a genind file with 'Group Info.' \nYou can obtain this genind file from the 'Data Conversion' page after you have both the data.frame and Group Info. files.\nYou also need to upload a Site Info file (in RDS format), which can be downloaded from the 'Data Input' page after transforming the VCF to a data.frame file.\nPlease click the 'Run OutFLANK' button.")
  OutFLANKfileInfo = reactiveVal("")
  OutFLANKtitle1 = reactiveVal("")
  OutFLANKtitle2 = reactiveVal("")
  OutFLANKtitle3 = reactiveVal("")
  OutFLANKtitle4 = reactiveVal("")
  OutFLANKtitle5 = reactiveVal("")
  outflank = reactiveVal(NULL)
  outflank_data2 = reactiveVal(NULL)
  outflank_data3 = reactiveVal(NULL)
  outflank_data4 = reactiveVal(NULL)
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

    Site_Info(Site_Info)

    Site_Info$Pos = as.numeric(Site_Info$Pos)
    Site_Info$Chr = as.character(Site_Info$Chr)

    for (i in 1:length(unique(Site_Info$Chr))) {
      if (i>1){
        end = max(Site_Info[Site_Info$Chr == i-1, 2])
        Site_Info[Site_Info$Chr == i, 2] = as.numeric(Site_Info[Site_Info$Chr == i, 2]) + end
      }
    }
    SNP_Info(Site_Info)

    Chr_axis = SNP_Info() %>%
      group_by(Chr) %>%
      summarise(center = (max(Pos, na.rm = TRUE) + min(Pos, na.rm = TRUE)) / 2, .groups = "drop")
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

    max_range = max(c(max(outflank_data$expected), max(outflank_data$observed)))
    OutFLANKplot3 = ggplot(outflank_data, aes(x = expected, y = observed)) +
      geom_point(size = 1, color = "#186da9") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.7) +
      labs(x = expression(Expected -log[10](italic(p))), y = expression(Observed -log[10](italic(p)))) +
      theme_classic() +
      theme(
        axis.title.x =   element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
      scale_x_continuous(limits = c(0, max_range)) +
      scale_y_continuous(limits = c(0, max_range))
    OutFLANKplot3(OutFLANKplot3)

    OutFLANKplot4 = ggplot(outflank_data, aes(x = pvalue)) +
      geom_histogram(binwidth = 0.02, fill = "#186da9", color = "grey70", linewidth = 0.2, alpha = 0.8) +
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
      geom_histogram(binwidth = 0.02, fill = "#186da9", color = "grey70", linewidth = 0.2, alpha = 0.8) +
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
    OutFLANKtitle1("Manhattan Plot")
    OutFLANKtitle2("QQ Plot of p-values")
    OutFLANKtitle3("Histogram of p-values")
    OutFLANKtitle4("Histogram of FST")
    OutFLANKtitle5("Significant SNPs")
    guide_OutFLANK("The OutFLANK analysis is complete.")
  })

  observeEvent(input$resetOutFLANK, {
    guide_OutFLANK("To run OutFLANK, the input data must be a genind file with 'Group Info.' \nYou can obtain this genind file from the 'Data Conversion' page after you have both the data.frame and Group Info. files.\nYou also need to upload a Site Info file (in RDS format), which can be downloaded from the 'Data Input' page after transforming the VCF to a data.frame file.\nPlease click the 'Run OutFLANK' button.")
    OutFLANKfileInfo("")
    OutFLANKtitle1("")
    OutFLANKtitle2("")
    OutFLANKtitle3("")
    OutFLANKtitle4("")
    OutFLANKtitle5("")
    outflank(NULL)
    outflank_data2(NULL)
    outflank_data3(NULL)
    outflank_data4(NULL)
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
    if (OutFLANKtitle1() == "Manhattan Plot") {
      data = SNP_Info()
      outflank = outflank()
      Chr_axis = Chr_axis()

      method = pval_adj_method_choice[input$OutFLANK_adj]
      padj = p.adjust(outflank$outflank$results$pvaluesRightTail, method = as.character(method))
      outliers = which(padj < as.numeric(input$OutFLANK_alpha))

      outflank_data3 = data %>%
        mutate(pvalue = outflank$outflank$results$pvaluesRightTail) %>%
        mutate(adjust_pvalue = padj) %>%
        mutate(FST = outflank$outflank$results$FST) %>%
        mutate(signif = ifelse(row_number() %in% outliers, "Yes", "No"))
      outflank_data3$Chr = factor(outflank_data3$Chr, levels = sort(unique(as.numeric(outflank_data3$Chr))))
      n_chr = length(levels(outflank_data3$Chr))
      colors = rep(c("#cbbc1d", "#5e929d"), length.out = n_chr)

      outflank_data3$FST = ifelse(outflank_data3$FST < 0, 0, outflank_data3$FST)
      outflank_data3(outflank_data3)

      Site_Info = Site_Info()
      outflank_data4 = outflank_data3
      outflank_data4[,1:3] = Site_Info[,1:3]
      outflank_data4(outflank_data4)

      OutFLANKplot1 = ggplot(outflank_data3, aes(x = Pos, y = -log10(pvalue))) +
        geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
        scale_color_manual(values = colors) +
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
      outflank_data2(data2)
      if (dim(data2)[1]>0){
        OutFLANKplot1 = OutFLANKplot1 +
          geom_point(data = data2, aes(Pos, -log10(pvalue)), color = "red", size = 1, alpha = 0.9) +
          geom_hline(yintercept = min(-log10(data2$pvalue)), color = "#ff4500", linetype = "dashed", linewidth = 0.6)
      }
      OutFLANKplot1(OutFLANKplot1)
      pre_results = pre_results()
      pre_results[[45]] = "# Selection Sweep"
      pre_results[[49]] = paste0("The results of 'Fst-based genome scan for selection (OutFLANK)': ")
      pre_results[[50]] = paste0(dim(data2)[1] , " significant selection signatures (SNP loci) were detected across ",
                                 length(table(data2[,1])), " chromosomes with the ",
                                 input$OutFLANK_adj, " P-value adjustment method at α = ", input$OutFLANK_alpha)
      pre_results(pre_results)
      OutFLANKplot1()
    }
  })

  output$OutFLANKplot2 = renderPlot({
    req(outflank_data3(), Chr_axis())
    data = outflank_data3()
    data$Chr = factor(data$Chr, levels = sort(unique(as.numeric(data$Chr))))
    n_chr = length(levels(data$Chr))
    colors = rep(c("#cbbc1d", "#5e929d"), length.out = n_chr)

    Chr_axis = Chr_axis()
    OutFLANKplot2 = ggplot(data, aes(x = Pos, y = FST)) +
      geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
      scale_color_manual(values = colors) +
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
    if (OutFLANKtitle1() == "Manhattan Plot") {
      selectInput("OutFLANK_adj", "P-value adjustment method", choices = names(pval_adj_method_choice), selected = "Benjamini & Hochberg (FDR)")
    }
  })

  output$OutFLANK_alpha = renderUI({
    if (OutFLANKtitle1() == "Manhattan Plot") {
      selectInput("OutFLANK_alpha", "Level of significance (alpha)", choices = c(0.1, 0.05, 0.01, 0.005, 0.001), selected = 0.1)
    }
  })

  output$OutFLANK_result = renderText({
    req(outflank_data2())
    chr = table(outflank_data2()[,1])
    out = c()
    for (i in seq_len(length(chr))) {
      if (as.numeric(chr[i]) > 0){
        out[i] = paste0("Chr ", names(chr)[i], ": ", as.numeric(chr[i]), " significant SNPs", "\n")
      } else{
        out[i] = paste0("NA", "\n")
      }
    }
    out = out[!grepl("NA", out)]
    out = paste(out, collapse = "")
    paste("Number of significant SNPs: ", dim(outflank_data2())[1], "\n",
          "---------------", "\n",
          out,
          sep = "")
  })

  output$download_OutFLANK_plot1 = renderUI({
    if (OutFLANKtitle1() == "Manhattan Plot") {
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

  output$download_OutFLANK = renderUI({
    if (OutFLANKtitle1() == "Manhattan Plot") {
      downloadButton("DOutFLANK", "Download OutFLANK p-value (per site)")
    }
  })

  output$DOutFLANK = downloadHandler(
    filename = "OutFLANK_p-value_per_site.rds",
    content = function(file) {
      saveRDS(outflank_data4(), file)
    }
  )

  output$download_OutFLANK_plot2 = renderUI({
    if (OutFLANKtitle1() == "Manhattan Plot") {
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
    if (OutFLANKtitle2() == "QQ Plot of p-values") {
      OutFLANKplot3()
    }
  })

  output$OutFLANKplot4 = renderPlot({
    req(outflank())
    if (OutFLANKtitle2() == "QQ Plot of p-values") {
      OutFLANKplot4()
    }
  })

  output$OutFLANKplot5 = renderPlot({
    req(outflank())
    if (OutFLANKtitle2() == "QQ Plot of p-values") {
      OutFLANKplot5()
    }
  })

  output$download_OutFLANK_plot3 = renderUI({
    if (OutFLANKtitle2() == "QQ Plot of p-values") {
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
    if (OutFLANKtitle2() == "QQ Plot of p-values") {
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
    if (OutFLANKtitle2() == "QQ Plot of p-values") {
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
    if (OutFLANKtitle2() == "QQ Plot of p-values") {
      downloadButton("DOutFLANK_Sign_SNP", "Download Table")
    }
  })

  output$DOutFLANK_Sign_SNP = downloadHandler(
    filename = "OutFLANK_Significant_SNPs.csv",
    content = function(file) {
      write.csv(outflank_data2(), file, row.names = FALSE)
    }
  )

  output$guide_OutFLANK = renderUI({ div(class = "guide-text-block", guide_OutFLANK()) })
  output$OutFLANKtitle1 = renderText({ OutFLANKtitle1() })
  output$OutFLANKtitle2 = renderText({ OutFLANKtitle2() })
  output$OutFLANKtitle3 = renderText({ OutFLANKtitle3() })
  output$OutFLANKtitle4 = renderText({ OutFLANKtitle4() })
  output$OutFLANKtitle5 = renderText({ OutFLANKtitle5() })

  ##### IBS #####
  guide_IBS = reactiveVal("To run IBS, the input data must be in data.frame format. \nYou also need to upload a Site Info file (in RDS format), which can be downloaded from the 'Data Input' page after transforming the VCF to a data.frame file. \nPlease click the 'Run IBS' button.")
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
    IBStitle1("Chromosome Ideogram")
    IBStitle2("Sliding Window Data")
    guide_IBS("The IBS analysis is complete.")

    output$DIBS_plot = downloadHandler(
      filename = function() {
        paste0("IBS_Chromosome_Ideogram-", input$REF, "_vs_", input$COMPAR, ".pdf")
      },
      content = function(file) {
        pdf(file, width = 12, height = 5)
        print(IBSplot())
        dev.off()
      }
    )

    output$DIBS_SW = downloadHandler(
      filename = function() {
        paste0("IBS_Sliding_Window-", input$REF, " vs ", input$COMPAR, ".csv")
      },
      content = function(file) {
        write.csv(IBS_result()$window_data, file, row.names = FALSE)
      }
    )
  })

  output$IBSres = renderText({
    req(IBS_result())
    if (IBStitle1() == "Chromosome Ideogram") {
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
    if (IBStitle1() == "Chromosome Ideogram") {
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
        geom_bar(data = Chr_Info, aes(x = Chr, y = Length), stat = "identity", fill = "grey80", width = 0.5, alpha = 0.9) +
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
    guide_IBS = reactiveVal("To run IBS, the input data must be in data.frame format. \nPlease click the 'Run IBS' button")
  })

  output$download_IBS_plot = renderUI({
    if (IBStitle1() == "Chromosome Ideogram") {
      downloadButton("DIBS_plot", "Download Plot")
    }
  })

  output$IBS_SW = DT::renderDataTable({
    req(IBS_result())
    DT::datatable(IBS_result()$window_data, options = list(pageLength = 10))
  })

  output$download_IBS_SW = renderUI({
    if (IBStitle1() == "Chromosome Ideogram") {
      downloadButton("DIBS_SW", "Download Sliding Window Data")
    }
  })

  output$guide_IBS = renderUI({ div(class = "guide-text-block", guide_IBS()) })
  output$IBStitle1 = renderText({ IBStitle1() })
  output$IBStitle2 = renderText({ IBStitle2() })


  ##### Manhattan Plot #####
  shinyjs::hide("ManhattanStatus")
  Manhattan_data = reactiveVal(NULL)
  Chr_Info = reactiveVal(NULL)
  guide_Manhattan = reactiveVal("This page allows you to customize a Manhattan plot. You can upload:\n▷ Genetic diversity per site (in RDS), or\n▷ pcadapt p-value per site (in RDS), or \n▷ OutFLANK p-value per site (in RDS), and\n▷ Chromosome Info.\nOnce your files are uploaded, click the 'Run Manhattan Plot' button.")
  ManhattanPlot1 = reactiveVal("")
  ManhattanPlot = reactiveVal(NULL)

  output$Manhattan_Upload = renderUI({
    fileInput("Manhattandata1", "", multiple = F, accept = c(".rds"))
  })

  observeEvent(input$Manhattandata1, {
    data = readRDS(input$Manhattandata1$datapath)
    Manhattan_data(data)
  })

  output$Manhattan_fileInfo = renderText({
    req(Manhattan_data())
    data = Manhattan_data()

    if (is.null(data$Chr) || is.null(data$Pos) || is.null(data$Marker)) {
      return(paste0("**Warning**", "\n", "Data must contain the columns named 'Chr', 'Pos', 'Marker'!"))
    } else {
      updateSelectInput(session, "Manhattan_hover_text", choices = colnames(data), selected = colnames(data)[1:3])
      updateSelectInput(session, "Manhattan_y_axis", choices = colnames(data)[-c(1:3)], selected = colnames(data)[4])
      return(paste0("-- Data for Manhattan Plot --", "\n",
                    length(data$Chr), " SNPs", "\n",
                    "Names: ", paste0(colnames(data), collapse = ", ")))
    }
  })

  output$Manhattan_Upload2 = renderUI({
    fileInput("Manhattandata2", "", multiple = F, accept = c(".csv"))
  })

  observeEvent(input$Manhattandata2, {
    Chr_Info = read.csv(input$Manhattandata2$datapath)
    Chr_Info(Chr_Info)
  })

  output$Manhattan_fileInfo2 = renderText({
    req(Manhattan_data(), Chr_Info())
    if (n_distinct(Manhattan_data()$Chr) != n_distinct(Chr_Info()[,1])){
      paste0("**Warning**", "\n", "Data must contain the same number of chromosomes as the data for Manhattan plot!")
    } else{
      paste0("-- Chromosome Info. --", "\n",
             "Number of chromosome: ", n_distinct(Chr_Info()[,1]))
    }
  })

  observeEvent(input$runManhattan, {
    req(Manhattan_data(), Chr_Info())
    shinyjs::show("ManhattanStatus")
    data = Manhattan_data()
    Chr_Info = Chr_Info()
    data$Chr = as.numeric(data$Chr)
    data$Pos = as.numeric(data$Pos)
    data$AdjPos = as.numeric(data$Pos)

    for (i in 1:length(unique(data$Chr))) {
      if (i>1){
        end = sum(Chr_Info$End[1:i-1])
        data$AdjPos[data$Chr == i] = as.numeric(data$AdjPos[data$Chr == i]) + end
      }
    }

    Chr_axis = data %>% group_by(Chr) %>% summarize(center = (max(AdjPos) + min(AdjPos)) / 2, .groups = "drop")

    sele_columns = input$Manhattan_hover_text
    text_content = apply(data, 1, function(row) {
      paste0(sapply(sele_columns, function(col) paste0(col, ": ", row[col])), collapse = "\n")
    })
    data$text = text_content

    loc = which(colnames(data) == input$Manhattan_y_axis)
    data$trans = data[, loc]
    if (input$Manhattan_y_axis_trans == "-log10"){
      data$trans = -log10(data$trans)
    } else if (input$Manhattan_y_axis_trans == "Standardization"){
      data$trans = (data$trans - mean(data$trans)) / sd(data$trans)
    } else if (input$Manhattan_y_axis_trans == "Mean-Centering"){
      data$trans = data$trans - mean(data$trans)
    }

    if (input$Manhattan_color == "Default"){
      data$Chr = factor(data$Chr, levels = sort(unique(as.numeric(data$Chr))))
      n_chr = length(levels(data$Chr))
      colors = rep(c("#cbbc1d", "#5e929d"), length.out = n_chr)
    } else {
      data$Chr = factor(data$Chr, levels = sort(unique(as.numeric(data$Chr))))
      n_chr = length(levels(data$Chr))
      colors = my_palette(input$Manhattan_color, 2)
      colors = rep(colors, length.out = n_chr)
    }

    ManhattanPlot = ggplot(data, aes(x = AdjPos, y = trans, text = text)) +
      geom_point(aes(color = as.factor(Chr)), alpha = input$Manhattan_opacity, size = input$Manhattan_size) +
      scale_color_manual(values = colors) +
      scale_x_continuous(label = Chr_axis$Chr, breaks = Chr_axis$center, expand = c(0.01, 0)) +
      scale_y_continuous(expand = c(0.01, 0)) +
      theme_classic() +
      theme(legend.position = "none",
            panel.border = element_blank(),
            axis.text.x = element_text(size = input$Manhattan_x_axis_text_size),
            axis.title.x = element_text(size = input$Manhattan_x_axis_title_size),
            axis.text.y = element_text(size = input$Manhattan_y_axis_text_size),
            axis.title.y = element_text(size = input$Manhattan_y_axis_title_size)
      )
    if (input$Manhattan_x_axis_title == "Show"){
      ManhattanPlot = ManhattanPlot + xlab("Chromosome")
    } else{
      ManhattanPlot = ManhattanPlot + xlab("")
    }
    if (input$Manhattan_y_axis_title == "Show"){
      ManhattanPlot = ManhattanPlot + ylab(input$Manhattan_y_axis)
    } else{
      ManhattanPlot = ManhattanPlot + ylab("")
    }
    if (!is.null(data$signif)){
      data2 = subset(data, signif == "Yes")
      if (input$Manhattan_y_threshold == "Show"){
        ManhattanPlot = ManhattanPlot +
          geom_hline(yintercept = min(-log10(data2$pvalue)), color = "#ff4500", linetype = "dashed", linewidth = 0.6)
      }
      if (input$Manhattan_highlight_color != "NULL"){
        ManhattanPlot = ManhattanPlot +
          geom_point(data = data2, aes(AdjPos, -log10(pvalue)), color = input$Manhattan_highlight_color, size = input$Manhattan_highlight_size, alpha = 0.9)
      }
    }

    ManhattanPlot = ggplotly(ManhattanPlot, tooltip = "text")
    ManhattanPlot(ManhattanPlot)

    ManhattanPlot1("Manhattan Plot")
    shinyjs::hide("ManhattanStatus")
    guide_Manhattan("You can customize the Manhattan plot and then click the 'Run Manhattan Plot' button again.")
  })

  observeEvent(input$resetManhattan, {
    Manhattan_data(NULL)
    Chr_Info(NULL)
    ManhattanPlot(NULL)
    ManhattanPlot1("")
    guide_Manhattan("This page allows you to customize a Manhattan plot. You can upload:\n▷ Genetic diversity per site (in RDS), or\n▷ pcadapt p-value per site (in RDS), or \n▷ OutFLANK p-value per site (in RDS), and\n▷ Chromosome Info. (in CSV).\nOnce your files are uploaded, click the 'Run Manhattan Plot' button.")
    output$Manhattan_Upload = renderUI({
      fileInput("Manhattandata1", "", multiple = F, accept = c(".rds"))
    })
    output$Manhattan_Upload2 = renderUI({
      fileInput("Manhattandata2", "", multiple = F, accept = c(".csv"))
    })
    showNotification("Data have been reset.")
  })

  output$ManhattanPlot = renderPlotly({
    ManhattanPlot()
  })

  output$download_ManhattanPlot = renderUI({
    if (ManhattanPlot1() == "Manhattan Plot") {
      downloadButton("DManhattanPlot_HTML", "Download Plot")
    }
  })

  output$DManhattanPlot_HTML = downloadHandler(
    filename = "Manhattan_Plot.html",
    content = function(file) {
      htmlwidgets::saveWidget(ManhattanPlot(), file)
    })

  output$ManhattanPlot1 = renderText({ ManhattanPlot1() })
  output$guide_Manhattan = renderUI({ div(class = "guide-text-block", guide_Manhattan()) })
  ##### Page 7: Core Collection #####
  ##### Core Sample Set #####
  guide_CoreSample = reactiveVal("To run core sample set, the input data must be in data.frame format. \nPlease click the 'Run Core Sample' button.")
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
    dataset = as.data.frame(t(core_sample$coreset))
    colnames(dataset) = row.names(data)
    row.names(dataset) = core_sample$coverage.table[,2]
    core_sample_dataset(dataset)

    core_sample_info = data.frame("ID" = colnames(data),
                                  "Core_sample" = ifelse(colnames(data) %in% core_sample_coverage()[,2], "TRUE", "FALSE"))
    core_sample_info(core_sample_info)

    shinyjs::hide("CoreSampleStatus")
    CoreSampletitle1("Core Sample Set")
    CoreSampletitle2("Coverage Plot of Core Sample Set")
    guide_CoreSample("The core sample set is completed.")

    pre_results = pre_results()
    pre_results[[51]] = "# Core Collection"
    pre_results[[52]] = "Core Sample Set"
    pre_results[[53]] = paste0("Number of core samples: ", length(core_sample_coverage()[,2]), " (", round(length(core_sample_coverage()[,2])/dim(df())[1], 4)* 100, "%)")
    pre_results[[54]] = paste0("Total coverage: ", max(as.numeric(core_sample_coverage()[,3])), "%")
    pre_results(pre_results)

    output$DCoreSample_plot = downloadHandler(
      filename = function() {
        paste0("Core_Sample_Plot-", "coverage", input$coverage, ".pdf")
      },
      content = function(file) {
        pdf(file, width = 10, height = 8)
        print(CoreSampleplot())
        dev.off()
      }
    )

    output$Dcore_sample_dataset = downloadHandler(
      filename = paste0("data.frame_", dim(core_sample_dataset())[1], "_", dim(core_sample_dataset())[2], "SNPs_", "Core_Sample_Set.rds"),
      content = function(file) {
        saveRDS(core_sample_dataset(), file)
      }
    )

  })

  observeEvent(input$resetCoreSample, {
    core_sample_coverage(NULL)
    core_sample_dataset(NULL)
    CoreSampletitle1("")
    CoreSampletitle2("")
    showNotification("Data have been reset.")
    guide_CoreSample("To run core sample set, the input data must be in data.frame format. \nPlease click the 'Run Core Sample' button.")
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
    if (CoreSampletitle2() == "Coverage Plot of Core Sample Set") {
      paste0("Number of core samples: ", length(core_sample_coverage()[,2]), " (", round(length(core_sample_coverage()[,2])/dim(df())[1], 4)* 100, "%)", "\n",
             "Total coverage: ", max(as.numeric(core_sample_coverage()[,3])), "%", "\n",
             "Core sample IDs: ", paste(as.character(core_sample_coverage()[,2]), collapse = "; "))
    }
  })

  output$CoreSampleplot = renderPlot({
    req(input$coverage, input$diff, core_sample_coverage())
    if (CoreSampletitle2() == "Coverage Plot of Core Sample Set") {
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
    if (CoreSampletitle2() == "Coverage Plot of Core Sample Set") {
      downloadButton("DCoreSample_plot", "Download Plot")
    }
  })

  output$download_core_sample_coverage = renderUI({
    if (CoreSampletitle1() == "Core Sample Set") {
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
    if (CoreSampletitle1() == "Core Sample Set") {
      downloadButton("Dcore_sample_dataset", "Download Core Samples Set in data.frame")
    }
  })

  output$download_core_sample_info = renderUI({
    if (CoreSampletitle1() == "Core Sample Set") {
      downloadButton("D_core_sample_info", "Download Core Sample Info.")
    }
  })

  output$D_core_sample_info = downloadHandler(
    filename = "Core_Sample_Info.csv",
    content = function(file) {
      write.csv(core_sample_info(), file, row.names = FALSE)
    }
  )

  output$guide_CoreSample = renderUI({ div(class = "guide-text-block", guide_CoreSample()) })
  output$CoreSampletitle1 = renderText({ CoreSampletitle1() })
  output$CoreSampletitle2 = renderText({ CoreSampletitle2() })

  ##### Core SNP Set #####
  guide_CoreSNP = reactiveVal("To run core SNP set, the input data must be in data.frame format. \nYou also need to upload the Site Info. and DAPC object files (in RDS format).\nPlease click the 'Run Core SNP' button.")
  CoreSNPfileInfo = reactiveVal("")
  CoreSNPtitle1 = reactiveVal("")
  CoreSNPtitle2 = reactiveVal("")
  core_SNP_dataset = reactiveVal(NULL)
  selected_Site_Info = reactiveVal(NULL)
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

  output$Site_Info5 = renderUI({
    fileInput("Site_Info5", "Site Info.* (required)", multiple = F, accept = c(".rds"))
  })

  observeEvent(input$Site_Info5, {
    req(input$Site_Info5)
    Site_Info = readRDS(input$Site_Info5$datapath)
    Site_Info(Site_Info)
  })

  output$Chr_Info3 = renderUI({
    fileInput("Chr_Info3", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
  })

  observeEvent(input$Chr_Info3, {
    Chr_Info = read.csv(input$Chr_Info3$datapath)
    Chr_Info(Chr_Info)
  })


  output$dapc_Upload = renderUI({
    fileInput("CoreSNPdata", "DAPC Object* (required)", multiple = F, accept = c(".rds"))
  })

  observeEvent(input$CoreSNPdata, {
    req(input$CoreSNPdata)
    DAPC2 = readRDS(input$CoreSNPdata$datapath)
    DAPC2(DAPC2)
  })

  observeEvent(input$runCoreSNP, {
    req(input$FileforCoreSNP, Site_Info(), DAPC2())

    shinyjs::show("CoreSNPStatus")
    guide_CoreSNP("Running...")
    data = switch(input$FileforCoreSNP, "df" = df())

    DAPC = DAPC2()
    Site_Info = Site_Info()
    loading = DAPC$var.contr
    ID = row.names(loading)
    ID_trim = substr(ID, 1, nchar(ID) - 2)
    ID_retain_loc = which(duplicated(ID_trim) == TRUE)
    row.names(loading) = ID_trim

    loading = loading[ID_retain_loc,]
    percent = DAPC$eig/sum(DAPC$eig)

    ratio = as.numeric(input$CoreSNPratio)/100
    num = ratio*dim(loading)[1]
    sel = round(num*percent, 0)
    select = list()
    for (i in 1:ncol(loading)) {
      select[[i]] = names(sort(abs(loading[,i]), decreasing = TRUE)[1:sel[i]])
    }

    selected_SNPs = unique(unlist(select))

    subset_data = data[, selected_SNPs]
    subset_data = lapply(subset_data, function(x) as.numeric(as.character(x)))
    subset_data = as.data.frame(subset_data)
    colnames(subset_data) = selected_SNPs
    core_SNP_dataset(subset_data)

    core_SNP_info = data.frame("ID" = colnames(data),
                               "Core_SNP" = ifelse(colnames(data) %in% selected_SNPs, "TRUE", "FALSE"))
    core_SNP_info(core_SNP_info)

    selected_Site_Info = Site_Info[which(Site_Info[,3] %in% selected_SNPs),]
    selected_Site_Info(selected_Site_Info)

    shinyjs::hide("CoreSNPStatus")
    CoreSNPtitle1("Core SNP Set")
    CoreSNPtitle2("Distribution of Core SNPs")
    guide_CoreSNP("The core SNP set is completed.\nYou can input the core set (as a data.frame file), run a PCA analysis, and then review the results.")

    pre_results = pre_results()
    pre_results[[51]] = "# Core Collection"
    pre_results[[55]] = "Core SNPs set"
    pre_results[[56]] = paste0("Number of core SNPs: ", length(selected_SNPs), " (", round(length(selected_SNPs)/dim(df())[2], 4)* 100, "%)")
    pre_results(pre_results)

    output$Dcore_SNP_dataset = downloadHandler(
      filename = paste0("data.frame_", dim(core_SNP_dataset())[1], "_", dim(core_SNP_dataset())[2], "SNPs_", "Core_SNP_Set.rds"),
      content = function(file) {
        saveRDS(core_SNP_dataset(), file)
      }
    )

    output$D_CoreSNP_site_info = downloadHandler(
      filename = paste0("Site_Info_", dim(core_SNP_dataset())[1], "_", dim(core_SNP_dataset())[2], "SNPs_", "Core_SNP_Set.rds"),
      content = function(file) {
        saveRDS(selected_Site_Info(), file)
      }
    )
  })

  observeEvent(input$resetCoreSNP, {
    core_SNP_dataset(NULL)
    CoreSNPtitle1("")
    CoreSNPtitle2("")
    showNotification("Data have been reset.")
    guide_CoreSNP("To run core SNP set, the input data must be in data.frame format. \nYou also need to upload the Site Info. and DAPC object files (in RDS format).\nPlease click the 'Run Core SNP' button.")
  })

  output$CoreSNPfileInfo = renderText({
    req(df())
    paste("Number of samples: ", dim(df())[1], "\n",
          "Number of SNPs: ", dim(df())[2], "\n",
          "Type: ", class(df()),
          sep = "")
  })

  output$CoreSNPres = renderText({
    req(core_SNP_dataset())
    if (CoreSNPtitle2() == "Distribution of Core SNPs") {
      paste0("Number of core SNPs: ", ncol(core_SNP_dataset()), " (", round(ncol(core_SNP_dataset())/dim(df())[2], 4)* 100, "%)",
             collapse = "; ")
    }
  })

  output$CoreSNPplot = renderPlot({
    req(selected_Site_Info())
    if (CoreSNPtitle2() == "Distribution of Core SNPs") {
      Chr_Info = Chr_Info()
      Chr_Info$Length = Chr_Info$End - Chr_Info$Start
      MB = seq(0, 300, by = 20)
      linewidth = c(32,32,32,32,32,29,25,20,19,17,
                    15,14,13,12,12,11,10,10,9,9,
                    9,8,8,8,8,7,7,7,7,7,
                    6,6,6,6,6,rep(5,10))
      nchr = length(unique(Chr_Info$Chr))
      CoreSNPplot = ggplot() +
        geom_bar(data = Chr_Info, aes(x = Chr, y = Length), stat = "identity", fill = "grey90", width = 0.5, alpha = 0.85) +
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
      selected_Site_Info = selected_Site_Info()
      selected_Site_Info$Start = selected_Site_Info$Pos-30000
      selected_Site_Info$End = selected_Site_Info$Pos+30000
      chromosome = function(chr) {
        num = as.numeric(gsub("\\D", "", chr))
        if (num < 10) {
          return(paste0("Chr0", num))
        } else {
          return(paste0("Chr", num))
        }
      }
      selected_Site_Info$Chr = sapply(selected_Site_Info$Chr, chromosome)

      CoreSNPplot = CoreSNPplot +
        geom_rect(data = selected_Site_Info,
                  aes(xmin = Chr, xmax = Chr, ymin = Start, ymax = End),
                  fill = "#fd6c00",color = "#fd6c00", alpha = 0.85, linewidth = linewidth[nchr])
      CoreSNPplot(CoreSNPplot)
      CoreSNPplot()
    }
  })


  output$download_CoreSNP_plot = renderUI({
    if (CoreSNPtitle2() == "Distribution of Core SNPs") {
      downloadButton("DCoreSNP_plot", "Download Plot")
    }
  })

  output$DCoreSNP_plot = downloadHandler(
    filename = function() {
      paste0("Core_SNP_Plot.pdf")
    },
    content = function(file) {
      pdf(file, width = 12, height = 5)
      print(CoreSNPplot())
      dev.off()
    }
  )

  output$download_core_SNP_dataset = renderUI({
    if (CoreSNPtitle1() == "Core SNP Set") {
      downloadButton("Dcore_SNP_dataset", "Download Core SNPs Set in data.frame")
    }
  })

  output$download_core_SNP_info = renderUI({
    if (CoreSNPtitle1() == "Core SNP Set") {
      downloadButton("D_core_SNP_info", "Download Core SNPs Info.")
    }
  })

  output$D_core_SNP_info = downloadHandler(
    filename = "Core_SNP_Info.rds",
    content = function(file) {
      saveRDS(core_SNP_info(), file)
    }
  )

  output$download_CoreSNP_site_info = renderUI({
    if (CoreSNPtitle1() == "Core SNP Set") {
      downloadButton("D_CoreSNP_site_info", "Download Site Info. of Core SNPs")
    }
  })

  output$guide_CoreSNP = renderUI({ div(class = "guide-text-block", guide_CoreSNP()) })
  output$CoreSNPtitle1 = renderText({ CoreSNPtitle1() })
  output$CoreSNPtitle2 = renderText({ CoreSNPtitle2() })

  ##### Page 8: AI Report #####
  guide_AI = reactiveVal("This page allows you to generate your preliminary results from prior analysis, input your OpenAI API key, select an AI model, and get an AI-driven report.")
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

  observeEvent(input$Upload_preliminary_results, {
    req(input$Upload_preliminary_results)
    data = readLines(input$Upload_preliminary_results$datapath, warn = FALSE)
    data = paste(data, collapse = "\n")
    preliminary_results(data)
    AItitle1("Your Preliminary Results (as the prompts of AI-driven report)")
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
    AItitle1("Your Preliminary Results (as the prompts of AI-driven report)")
  })

  output$AI_response1 = renderText({
    req(preliminary_results())
    if (AItitle1() == "Your Preliminary Results (as the prompts of AI-driven report)"){
      preliminary_results()
    }
  })

  output$download_AI_autogenerate = renderUI({
    if (AItitle1() == "Your Preliminary Results (as the prompts of AI-driven report)") {
      downloadButton("DAI_autogenerate", "Download",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
  })

  output$DAI_autogenerate = downloadHandler(
    filename = "Preliminary_Results.txt",
    content = function(file) {
      write.table(preliminary_results(), file, row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  )

  ##### STEP 2
  observeEvent(input$runAIreport, {
    req(preliminary_results(), input$AI_api_key$datapath)
    shinyjs::show("AIStatus")

    if (input$AI_prompt == "Summary Request"){
      Role = "You can act as my research assistant, helping me interpret data, summarize findings, and generate new research ideas based on my SNP analysis."
      Start = Summary_Request_Prompt
    } else if (input$AI_prompt == "Data Interpretation"){
      Role = "You are a professional researcher, proficient in interpreting biological informatics and statistical results to deliver biologically meaningful insights."
      Start = Data_Interpretation_Prompt
    } else if (input$AI_prompt == "Report Structuring"){
      Role = "You can assist in structuring and refining your report, ensuring it is clear, concise, and well-organized."
      Start = Report_Structuring_Prompt
    } else if (input$AI_prompt == "Idea Expansion"){
      Role = "You can engage in brainstorming sessions with me, generating ideas and exploring implications of my findings."
      Start = Idea_Expansion_Prompt
    }

    result = tryCatch({
      key = readLines(input$AI_api_key$datapath, warn = FALSE)
      Sys.setenv(OPENAI_API_KEY = key)
      client = OpenAI()
      model = AI_model_choice[input$AI_model]

      message = paste(Role, Start, preliminary_results())

      completion = client$chat$completions$create(
        model = model,
        messages = list(list("role" = "user", "content" = message))
      )
      AI_report = paste0("---------- ShiNyP ----------", "\n", "\n",
                         "----- Successful Request -----", "\n",
                         "OpenAI Model: ", input$AI_model, "\n",
                         "OpenAI Task: ", input$AI_prompt, "\n",
                         "Total Tokens Used: ", completion$usage$total_tokens, "\n",
                         "Prompt Tokens: ", completion$usage$prompt_tokens, "\n",
                         "Completion Tokens: ", completion$usage$completion_tokens, "\n", "\n",
                         "----- AI-driven report -----", "\n",
                         completion$choices[[1]]$message$content, "\n", "\n",
                         "#### WARNING: ", "\n",
                         "### This report was generated with the assistance of OpenAI model and is for informational purposes only.", "\n",
                         "### It should not be considered as professional advice or a basis for decision-making.", "\n",
                         "### Please review and validate the content thoroughly before use.")

      AI_report(AI_report)
      AItitle2("Here's Your AI Report!")
      shinyjs::hide("AIStatus")
      NULL
    }, error = function(e) {
      shinyjs::hide("AIStatus")
      shinyjs::alert(paste("An error occurred:", e$message))
      NULL
    })

    output$DAI_report = downloadHandler(
      filename = paste0("AI_Report-", input$AI_model, "-", input$AI_prompt,".txt"),
      content = function(file) {
        write.table(AI_report(), file, row.names = FALSE, col.names = FALSE, quote = FALSE)
      }
    )
  })

  output$AI_response2 = renderText({
    req(AI_report())
    if (AItitle2() == "Here's Your AI Report!"){
      AI_report()
    }
  })

  observeEvent(input$AIreport_Reset, {
    AI_report(NULL)
    AItitle2("")
    showNotification("Data have been reset.")
  })

  output$download_AI_report = renderUI({
    if (AItitle2() == "Here's Your AI Report!") {
      downloadButton("DAI_report", "Download",
                     style = "color: #f6f9f9; background-color: #00a595")
    }
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
