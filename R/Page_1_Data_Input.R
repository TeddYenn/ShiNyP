# Page_1_Data_Input
##### Page 1: Data Input #####
#' @title Page_1_Data_Input_UI
#' @export
Page_1_Data_Input_UI = function(input, output, session) {
  tabPanel("Data Input",
           useShinyjs(),
           tabsetPanel(
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
                          checkboxInput("FVCFdiploidize", "Applying Diploidization Processing", value = FALSE),
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
             tabPanel("data.frame/genlight",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h5("Input data.frame File"),
                          uiOutput("uploaddf"),
                          actionButton("inputdf", "Input", class = "run-action-button"),
                          actionButton("resetdf", "Reset"),
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
                          div(class = "title-text-style", textOutput("glstatus")),
                          verbatimTextOutput("glinfo"),
                          width = 9)
                      ))
           ))
}
#' @title Page_1_Data_Input_Server
#' @export
Page_1_Data_Input_Server = function(input, output, session) {
  ##### Page 1-1: VCF #####
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
        #vcf = fread("data/Chicken/Chicken_10k_209.vcf", header = TRUE, sep = "\t")
        path = system.file("demo_data/Chicken", "Chicken_10k_209.vcf", package = "ShiNyP")
        vcf = fread(path, header = TRUE, sep = "\t")
        fileName("Chicken")
      } else if (input$demoFile == "Wild rice"){
        #vcf = fread("data/Wild_Rice/Wildrice_13k_446.vcf", header = TRUE, sep = "\t")
        path = system.file("demo_data/Wild_Rice", "Wildrice_13k_446.vcf", package = "ShiNyP")
        vcf = fread(path, header = TRUE, sep = "\t")
        fileName("Wildrice")
      } else if (input$demoFile == "Human #HGDP"){
        #vcf = fread("data/Human_HGDP/Human_10k_929.vcf", header = TRUE, sep = "\t")
        system.file("demo_data/Human_HGDP", "Human_10k_929.vcf", package = "ShiNyP")
        vcf = fread(path, header = TRUE, sep = "\t")
        fileName("Human")
      } else if (input$demoFile == "Yeast"){
        #vcf = fread("data/Yeast/Yeast_10k_1011.vcf", header = TRUE, sep = "\t")
        path = system.file("demo_data/Yeast", "Yeast_10k_1011.vcf", package = "ShiNyP")
        vcf = fread(path, header = TRUE, sep = "\t")
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
          vcf_process = function(x) {
            x = ifelse(is.na(x) | x %in% c("./.", ".|."), NA_character_, x)
            genotype = sub(":.*", "", x)  
            return(genotype)
          }
          vcf[, (names(vcf)[10:ncol(vcf)]) := 
                lapply(.SD, vcf_process), 
              .SDcols = names(vcf)[10:ncol(vcf)]]
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
      sliderInput("presample", "Preview number of samples", min = 1, max = ncol(vcfData())-9, value = 5, step = 1)
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
    guide_input("Waiting for input VCF file. You can upload: \n✅ A VCF file from PLINK (recommended), or \n✅ A VCF or gzipped VCF file (vcf.gz) from VCFtools, or \n✅ VCF file in RDS from ShiNyP. \n Once you see 'Upload complete' on the progress bar, click the 'Input VCF file' button.")
  })
  
  observeEvent(input$vcf2df, {
    req(vcfData())
    shinyjs::show("inputStatus")
    diploidize = as.logical(input$FVCFdiploidize)
    VCFdf(vcf2df(vcfData(), diploidize))
    df(VCFdf())
    Site_Info(vcf2Site_Info(vcfData()))
    input2("VCF Data in data.frame")
    shinyjs::hide("inputStatus")
    guide_input("VCF to data.frame is complete.")
    pre_results = pre_results()
    pre_results[[2]] = "## Data Input"
    pre_results[[4]] = paste0("Number of samples: ", dim(VCFdf())[1])
    pre_results[[5]] = paste0("Number of SNPs: ", dim(VCFdf())[2])
    pre_results(pre_results)
    
    output$Dinput = downloadHandler(
      filename = function() {
        paste("vcf_", fileName(), ".rds", sep = "")},
      content = function(file) {
        shinyjs::show("inputStatus")
        saveRDS(vcfData(), file)
        shinyjs::hide("inputStatus")
      })
    
    output$Ddf = downloadHandler(
      filename = function() {
        paste("data.frame_", dim(VCFdf())[1], "_", dim(VCFdf())[2], "SNPs.rds", sep = "")},
      content = function(file) {
        shinyjs::show("inputStatus")
        saveRDS(VCFdf(), file)
        shinyjs::hide("inputStatus")
      })
    
    output$DsnpInfo = downloadHandler(
      filename = function() {
        paste("Site_Info_", dim(VCFdf())[1], "_", dim(VCFdf())[2], "SNPs.rds", sep = "")},
      content = function(file) {
        shinyjs::show("inputStatus")
        saveRDS(Site_Info(), file)
        shinyjs::hide("inputStatus")
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
      paste0("Type: VCF", "\n",
             "Number of samples: ", dim(vcfData())[2]-9, "\n",
             "Number of SNPs: ", dim(vcfData())[1], "\n",
             "File name: ", "vcf_",fileName(), "\n",
             "Size in RAM: ", size2size(as.numeric(object.size(vcfData())))
      )
    }
  })
  
  output$fileInfo2 = renderText({
    req(VCFdf())
    if (input2() == "VCF Data in data.frame") {
      paste0("Type: data.frame", "\n",
             "Number of samples: ", dim(VCFdf())[1], "\n",
             "Number of SNPs: ", dim(VCFdf())[2], "\n",
             "File name: ", "data.frame_", dim(VCFdf())[1], "_", dim(VCFdf())[2], "SNPs", "\n",
             "Size in RAM: ", size2size(as.numeric(object.size(VCFdf())))
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
  
  ##### Page 1-2: data.frame/genlight #####
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
      paste0("Type: ", class(df()), "\n",
             "Number of samples: ", dim(df())[1], "\n",
             "Number of SNPs: ", dim(df())[2], "\n",
             "File name: ", tools::file_path_sans_ext(input$input_df$name), "\n",
             "Size in RAM: ", size2size(as.numeric(object.size(df())))
      )
    }
  })
  
  output$glinfo = renderText({
    req(gl())
    if (glstatus() == "genlight") {
      paste0("Type: ", class(gl()), "\n",
             "Number of samples: ", nInd(gl()), "\n",
             "Number of SNPs: ", nLoc(gl()), "\n",
             "Group Info.: ", if (nPop(gl()) > 1) "Added" else "NaN", "\n",
             "File name: ", tools::file_path_sans_ext(input$input_gl$name), "\n",
             "Size in RAM: ", size2size(as.numeric(object.size(gl())))
      )
    }
  })
  
  output$guide_input2 = renderUI({ div(class = "guide-text-block", guide_input2()) })
  output$dfstatus = renderText({ dfstatus() })
  output$glstatus = renderText({ glstatus() })
}
