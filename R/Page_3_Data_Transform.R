# Page_3_Data_Transform
##### Page 3: Data Transform #####
#' @title Page_3_Data_Transform_UI
#' @export
Page_3_Data_Transform_UI = function() {
  tabPanel("Data Transform",
           sidebarLayout(
             sidebarPanel(
               uiOutput("fileSelection3"),
               verbatimTextOutput("CfileInfo"),
               tags$hr(),
               tags$h5("1. data.frame to genlight"),
               uiOutput("groupfile1"),
               actionButton("Cdf2gl", "Transform to genlight", class = "run-action-button"),
               tags$hr(),
               verbatimTextOutput("glfileInfo"),
               tags$style("#glfileInfo { font-size: 14px;}"),
               tags$hr(),
               tags$h5("2. From genlight to"),
               selectInput(
                 inputId = "Transform_method", 
                 label = "Export the genlight object to:",
                 choices = c("genlight with Group Info. (RDS)" = "gl2genlight_group",
                             "genind (RDS)" = "gl2genind",
                             "PLINK (PED & MAP)" = "gl2PLINK",
                             "GenAlEx (CSV)" = "gl2GenAlEx",
                             "LEA (GENO & LFMM)" = "gl2LEA",
                             "GDS (GDS)" = "gl2gds",
                             "STRUCTURE (STR)" = "gl2STRUCTURE",
                             "fastStructure (STR)" = "gl2fastStructure",
                             "PHYLIP (TXT)" = "gl2PHYLIP",
                             "Treemix (GZ)" = "gl2Treemix",
                             "BayeScan (TXT)" = "gl2BayeScan"),
                 selected = "gl2genind"),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2genlight_group'",
                 fileInput("T2_Group1", "Group Info. (required)", multiple = F, accept = c(".csv"))),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2genind'",
                 fileInput("T2_Group2", "Group Info. (optional)", multiple = F, accept = c(".csv"))),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2PLINK'",
                 textInput("T2_Path1", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2GenAlEx'",
                 textInput("T2_Path2", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2LEA'",
                 textInput("T2_Path3", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2gds'",
                 textInput("T2_Path4", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2STRUCTURE'",
                 textInput("T2_Path5", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2fastStructure'",
                 textInput("T2_Path6", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2PHYLIP'",
                 textInput("T2_Path7", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2Treemix'",
                 textInput("T2_Path8", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               conditionalPanel(
                 condition = "input.Transform_method == 'gl2BayeScan'",
                 textInput("T2_Path9", "Specify the output file path:", value = getwd(), placeholder = "Ex: C:/Program Files/R")),
               actionButton("Cgl2", "Transform", class = "run-action-button"),
               width = 3),
             mainPanel(
               uiOutput("guide_C"),
               tags$hr(),
               uiOutput("progressUI"),
               div(class = "title-text-style", textOutput("Cstatus2")),
               div(id = "glStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
               verbatimTextOutput("CTable2"),
               uiOutput("download_gl"),
               tags$br(),
               div(class = "title-text-style", textOutput("Cstatus3")),
               div(id = "gl2Status", style = "color: red; font-weight: bold;", "It may take a while..."),
               verbatimTextOutput("CTable3"),
               uiOutput("download_gl2"),
               width = 9)
           ))
}

#' @title Page_3_Data_Transform_Server
#' @export
Page_3_Data_Transform_Server = function(input, output, session) {
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
      paste0("Type: data.frame", "\n",
             "Number of samples: ", dim(VCFdf())[1], "\n",
             "Number of SNPs: ", dim(VCFdf())[2])
    } else if (input$FileforDataConv == "QCData"){
      req(QCData())
      df(QCData())
      paste0("Type: data.frame", "\n",
             "Number of samples: ", dim(QCData())[1], "\n",
             "Number of SNPs: ", dim(QCData())[2])
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
  
  observeEvent(input$Cdf2gl, {
    req(df())
    pre_results = pre_results()
    pre_results[[17]] = paste0("### Number of samples: ", dim(df())[1])
    pre_results[[18]] = paste0("### Number of SNPs: ", dim(df())[2])
    pre_results(pre_results)
    shinyjs::show("glStatus")
    if (!is.null(groupInfo1())){
      req(groupInfo1())
      gl = new("genlight", df())
      pop(gl) = as.vector(groupInfo1())
    }else{
      gl = new("genlight", df())
    }
    gl(gl)
    Cstatus2("data.frame to genlight")
    shinyjs::hide("glStatus")
    guide_C("The data.frame has been transformed to genlight format.")
  })
  
  output$download_gl = renderUI({
    if (Cstatus2() == "data.frame to genlight") {
      downloadButton("Dgl", "Download genlight File")
    }
  })
  
  output$Dgl = downloadHandler(
    filename = function() {
      if (!is.null(groupInfo1())){
        paste("genlight_group_", nInd(gl()) , "_", nLoc(gl()), "SNPs.rds", sep = "")
      } else{
        paste("genlight_", nInd(gl()) , "_", nLoc(gl()), "SNPs.rds", sep = "")
      }
    },
    content = function(file) {
      shinyjs::show("glStatus")
      saveRDS(gl(), file)
      shinyjs::hide("glStatus")
    })
  
  observeEvent(input$T2_Group1, {
    req(input$T2_Group1)
    groupfile = read.csv(input$T2_Group1$datapath)
    T2_Group1Info = as.numeric(groupfile$Group)
    T2_Group1Info(T2_Group1Info)
  })
  
  observeEvent(input$T2_Group2, {
    req(input$T2_Group2)
    groupfile = read.csv(input$T2_Group2$datapath)
    T2_Group2Info = as.numeric(groupfile$Group)
    T2_Group2Info(T2_Group2Info)
  })
  
  output$glfileInfo = renderText({
    req(gl())
    group_info = ifelse(nPop(gl()) > 1, "Added", "NaN")
    paste0("Type: ", class(gl()), "\n",
           "Number of samples: ", nInd(gl()) , "\n",
           "Number of SNPs: ", nLoc(gl()), "\n",
           "Group Info.: ", group_info
    )
  })
  
  observeEvent(input$Cgl2, {
    transform = input$Transform_method
    req(gl())
    shinyjs::show("gl2Status")
    guide_C("Running...")
    gl = gl()
    gl = gl.compliance.check(gl, verbose = 0)
    class(gl) = "genlight"
    gl(gl)
    
    if (transform == "gl2genlight_group"){
      req(T2_Group1Info())
      pop(gl) = as.vector(T2_Group1Info())
      gl(gl)
      Cstatus3("genlight with Group Info.")
      output$Cstatus3 = renderText({ Cstatus3() })
      output$CTable3 = renderText({
        if (Cstatus3() == "genlight with Group Info."){
          if (nPop(gl) > 1){
            group.info = "Added"
            file.name = "File name: genlight_group_"
          }else{
            group.info = "NaN"
            file.name = "File name: genlight_"
          }
          paste0("Type: ", class(gl()), "\n",
                 "Number of samples: ", nInd(gl()) , "\n",
                 "Number of SNPs: ", nLoc(gl()), "\n",
                 "Group Info.: ", group.info, "\n",
                 file.name, nInd(gl()) , "_", nLoc(gl()), "SNPs", "\n",
                 "Size in RAM: ", size2size(as.numeric(object.size(gl())))
          )
        }
      })
      output$download_gl2 = renderUI({
        if (Cstatus3() == "genlight with Group Info.") {
          downloadButton("Dgl2", "Download genlight File")
        }
      })
      output$Dgl2 = downloadHandler(
        filename = function() {
          paste("genlight_", nInd(gl()) , "_", nLoc(gl()), "SNPs.rds", sep = "")},
        content = function(file) {
          shinyjs::show("glStatus")
          saveRDS(gl(), file)
          shinyjs::hide("glStatus")
        })
      
    } else if (transform == "gl2genind"){
      if (!is.null(T2_Group2Info())){
        pop(gl) = as.vector(T2_Group2Info())
        gl(gl)
      }
      gi = gl2gi(gl(), probar = FALSE, verbose = 0)
      gi(gi)
      Cstatus3("genlight to genind")
      output$Cstatus3 = renderText({ Cstatus3() })
      output$CTable3 = renderText({
        if (Cstatus3() == "genlight to genind"){
          if (nPop(gi)>1){
            group.info = "Added"
            file.name = "File name: genind_group_"
          }else{
            group.info = "NaN"
            file.name = "File name: genind_"
          }
          paste0("Type: ", class(gi), "\n",
                 "Number of samples: ", nInd(gi), "\n",
                 "Number of SNPs: ", nLoc(gi), "\n",
                 "Group Info.: ", group.info, "\n",
                 file.name, nLoc(gi), "_", nInd(gi), "SNPs", "\n",
                 "Size in RAM: ", size2size(as.numeric(object.size(gi))))
        }
      })
      output$download_gl2 = renderUI({
        if (Cstatus3() == "genlight to genind") {
          downloadButton("Dgl2", "Download genind File")
        }
      })
      output$Dgl2 = downloadHandler(
        filename = function() {
          paste("genind_", length(gi@ploidy), "_", length(gi@loc.n.all), "SNPs.rds", sep = "")},
        content = function(file) {
          shinyjs::show("glStatus")
          saveRDS(gi, file)
          shinyjs::hide("glStatus")
        })
    } else if (transform == "gl2PLINK") {
      if (is.null(input$T2_Path1) || input$T2_Path1 == "") {
        showNotification("Please specify a valid output path.", type = "error")
      } else {
        gl2plink(gl(), outfile = "PLINK", outpath = input$T2_Path1, verbose = 0)
        Cstatus3("PLINK (PED & MAP) file has already been generated")
      }
    } else if (transform == "gl2GenAlEx"){
      gl2genalex(gl(), outfile = "GenAlEx.csv", outpath = input$T2_Path2, verbose = 0)
      Cstatus3("GenAlEx (CSV) file has already been generated")
    } else if (transform == "gl2LEA"){
      gl2geno(gl(), outfile = "geno", outpath = input$T2_Path3, verbose = 0)
      Cstatus3("LEA (GENO & LFMM) file has already been generated")
    } else if (transform == "gl2gds"){
      gl2gds(gl(), outfile = "GDS.GDS", outpath = input$T2_Path4, verbose = 0)
      Cstatus3("GDS (GDS) file has already been generated")
    } else if (transform == "gl2STRUCTURE"){
      gl2structure(gl(), outfile = "STRUCTURE.str", outpath = input$T2_Path5, verbose = 0)
      Cstatus3("STRUCTURE (STR) file has already been generated")
    } else if (transform == "gl2fastStructure"){
      gl2faststructure(gl(), outfile = "fastStructure.str", outpath = input$T2_Path6, verbose = 0)
      Cstatus3("fastStructure (STR) file has already been generated")
    } else if (transform == "gl2PHYLIP"){
      gl2phylip(gl(), outfile = "PHYLIP.txt", outpath = input$T2_Path7, verbose = 0)
      Cstatus3("PHYLIP (TXT) file has already been generated")
    } else if (transform == "gl2Treemix"){
      result = gl2treemix(gl(), outfile = "Treemix.gz", outpath = input$T2_Path8, verbose = 0)
      Cstatus3("Treemix (GZ) file has already been generated")
    } else if (transform == "gl2BayeScan"){
      gl2bayescan(gl(), outfile = "BayeScan.txt", outpath = input$T2_Path9, verbose = 0)
      Cstatus3("BayeScan (TXT) file has already been generated")
    }
    output$Cstatus3 = renderText({ Cstatus3() })
    shinyjs::hide("gl2Status")
    guide_C("The data has been transformed")
  })
  
  output$guide_C = renderUI({ div(class = "guide-text-block", guide_C()) })
  
  output$Cstatus2 = renderText({ Cstatus2() })
  
  output$CTable2 = renderText({
    req(gl())
    if (Cstatus2() == "data.frame to genlight"){
      if (nPop(gl())>1){
        group.info = "Added"
        file.name = "File name: genlight_group_"
      }else{
        group.info = "NaN"
        file.name = "File name: genlight_"
      }
      paste0("Type: ", class(gl()), "\n",
             "Number of samples: ", nInd(gl()) , "\n",
             "Number of SNPs: ", nLoc(gl()), "\n",
             "Group Info.: ", group.info, "\n",
             file.name, nInd(gl()) , "_", nLoc(gl()), "SNPs", "\n",
             "Size in RAM: ", size2size(as.numeric(object.size(gl())))
      )
    }
  })
}