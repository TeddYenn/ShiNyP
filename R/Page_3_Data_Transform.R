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
               bslib::tooltip(
                 uiOutput("groupfile1"),
                 "Upload: Group Info. (CSV)"
               ),
               actionButton("Cdf2gl", "Transform to genlight", class = "run-action-button"),
               tags$hr(),
               verbatimTextOutput("glfileInfo"),
               tags$style("#glfileInfo { font-size: 14px;}"),
               tags$hr(),
               tags$h5("2. From genlight to"),
               selectInput(
                 inputId = "Transform_method", 
                 label = "Export to:",
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
               div(id = "glStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
               verbatimTextOutput("CTable2"),
               uiOutput("download_gl"),
               tags$br(),
               div(class = "title-text-style", textOutput("Cstatus3")),
               div(id = "gl2Status", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
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
    tryCatch({
      data = df()
      if (!is.data.frame(data)) stop("Input is not a valid data.frame.")
      pre_results_data = pre_results()
      pre_results_data[[17]] = paste0("### Number of samples: ", nrow(data))
      pre_results_data[[18]] = paste0("### Number of SNPs: ", ncol(data))
      pre_results(pre_results_data)
      
      shinyjs::show("glStatus")
      
      if (!is.null(groupInfo1())) {
        group_vec = as.vector(groupInfo1())
        if (length(group_vec) != nrow(data)) {
          stop("Sample number in Group Info. does not match dataset")
        }
        gl_obj = new("genlight", data)
        pop(gl_obj) = group_vec
      } else {
        gl_obj = new("genlight", data)
      }
      if (!is.null(Site_Info())) {
        chrom_vec <- Site_Info()[,1]
        pos_vec   <- Site_Info()[,2]
        gl_obj$chromosome <- if(is.null(chrom_vec)) NULL else as.factor(chrom_vec)
        gl_obj$position   <- if(is.null(pos_vec)) NULL else as.integer(pos_vec)
      }
      gl(gl_obj)
      Cstatus2("data.frame to genlight")
      shinyjs::hide("glStatus")
      guide_C("The data.frame has been transformed to genlight format.")
      showNotification("Run Successfully", type = "message")
    }, error = function(e) {
      shinyjs::hide("glStatus")
      showNotification(paste("Fail: ", e$message), type = "error", duration = 10)
      Cstatus2("")
      guide_C("Please check the input data and try again.")
      gl(NULL)
    })
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
    req(gl())
    tryCatch({
      transform = input$Transform_method
      shinyjs::show("gl2Status")
      guide_C("Running...")
      
      gl_obj = gl()
      if (!inherits(gl_obj, "genlight")) stop("Input is not a valid 'genlight' object.")
      gl_obj = gl.compliance.check(gl_obj, verbose = 0)
      class(gl_obj) = "genlight"
      gl(gl_obj)
      
      if (transform == "gl2genlight_group") {
        req(T2_Group1Info())
        # Defensive: Group info length must match sample size
        group1 = as.vector(T2_Group1Info())
        if (length(group1) != nInd(gl_obj)) {
          stop("Sample number in Group Info. does not match dataset")
        }
        pop(gl_obj) = group1
        gl(gl_obj)
        Cstatus3("genlight with Group Info.")
        
        output$Cstatus3 = renderText({ Cstatus3() })
        output$CTable3 = renderText({
          if (Cstatus3() == "genlight with Group Info.") {
            group.info = if (nPop(gl_obj) > 1) "Added" else "NaN"
            file.name  = if (nPop(gl_obj) > 1) "File name: genlight_group_" else "File name: genlight_"
            paste0("Type: ", class(gl()), "\n",
                   "Number of samples: ", nInd(gl()), "\n",
                   "Number of SNPs: ", nLoc(gl()), "\n",
                   "Group Info.: ", group.info, "\n",
                   file.name, nInd(gl()), "_", nLoc(gl()), "SNPs", "\n",
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
            paste("genlight_", nInd(gl()), "_", nLoc(gl()), "SNPs.rds", sep = "")
          },
          content = function(file) {
            shinyjs::show("glStatus")
            saveRDS(gl(), file)
            shinyjs::hide("glStatus")
          }
        )
      } else if (transform == "gl2genind") {
        if (!is.null(T2_Group2Info())) {
          group2 = as.vector(T2_Group2Info())
          if (length(group2) != nInd(gl_obj)) {
            stop("Group information does not match the number of samples.")
          }
          pop(gl_obj) = group2
          gl(gl_obj)
        }
        gi_obj = gl2gi(gl(), probar = FALSE, verbose = 0)
        gi(gi_obj)
        Cstatus3("genlight to genind")
        output$Cstatus3 = renderText({ Cstatus3() })
        output$CTable3 = renderText({
          if (Cstatus3() == "genlight to genind") {
            group.info = if (nPop(gi_obj) > 1) "Added" else "NaN"
            file.name  = if (nPop(gi_obj) > 1) "File name: genind_group_" else "File name: genind_"
            paste0("Type: ", class(gi_obj), "\n",
                   "Number of samples: ", nInd(gi_obj), "\n",
                   "Number of SNPs: ", nLoc(gi_obj), "\n",
                   "Group Info.: ", group.info, "\n",
                   file.name, nLoc(gi_obj), "_", nInd(gi_obj), "SNPs", "\n",
                   "Size in RAM: ", size2size(as.numeric(object.size(gi_obj)))
            )
          }
        })
        output$download_gl2 = renderUI({
          if (Cstatus3() == "genlight to genind") {
            downloadButton("Dgl2", "Download genind File")
          }
        })
        output$Dgl2 = downloadHandler(
          filename = function() {
            paste("genind_", length(gi_obj@ploidy), "_", length(gi_obj@loc.n.all), "SNPs.rds", sep = "")
          },
          content = function(file) {
            shinyjs::show("glStatus")
            saveRDS(gi_obj, file)
            shinyjs::hide("glStatus")
          }
        )
      } else if (transform == "gl2PLINK") {
        if (is.null(input$T2_Path1) || input$T2_Path1 == "") {
          stop("Please specify a valid output path for PLINK files.")
        } else {
          gl2plink(gl(), outfile = "PLINK", outpath = input$T2_Path1, verbose = 0)
          Cstatus3("PLINK (PED & MAP) file has been generated.")
        }
      } else if (transform == "gl2GenAlEx") {
        if (is.null(input$T2_Path2) || input$T2_Path2 == "") {
          stop("Please specify a valid output path for GenAlEx files.")
        } else {
          gl2genalex(gl(), outfile = "GenAlEx.csv", outpath = input$T2_Path2, verbose = 0)
          Cstatus3("GenAlEx (CSV) file has been generated.")
        }
      } else if (transform == "gl2LEA") {
        if (is.null(input$T2_Path3) || input$T2_Path3 == "") {
          stop("Please specify a valid output path for LEA files.")
        } else {
          gl2geno(gl(), outfile = "geno", outpath = input$T2_Path3, verbose = 0)
          Cstatus3("LEA (GENO & LFMM) file has been generated.")
        }
      } else if (transform == "gl2gds") {
        if (is.null(input$T2_Path4) || input$T2_Path4 == "") {
          stop("Please specify a valid output path for GDS files.")
        } else {
          gl2gds(gl(), outfile = "GDS.GDS", outpath = input$T2_Path4, verbose = 0)
          Cstatus3("GDS (GDS) file has been generated.")
        }
      } else if (transform == "gl2STRUCTURE") {
        if (is.null(input$T2_Path5) || input$T2_Path5 == "") {
          stop("Please specify a valid output path for STRUCTURE files.")
        } else {
          gl2structure(gl(), outfile = "STRUCTURE.str", outpath = input$T2_Path5, verbose = 0)
          Cstatus3("STRUCTURE (STR) file has been generated.")
        }
      } else if (transform == "gl2fastStructure") {
        if (is.null(input$T2_Path6) || input$T2_Path6 == "") {
          stop("Please specify a valid output path for fastStructure files.")
        } else {
          gl2faststructure(gl(), outfile = "fastStructure.str", outpath = input$T2_Path6, verbose = 0)
          Cstatus3("fastStructure (STR) file has been generated.")
        }
      } else if (transform == "gl2PHYLIP") {
        if (is.null(input$T2_Path7) || input$T2_Path7 == "") {
          stop("Please specify a valid output path for PHYLIP files.")
        } else {
          gl2phylip(gl(), outfile = "PHYLIP.txt", outpath = input$T2_Path7, verbose = 0)
          Cstatus3("PHYLIP (TXT) file has been generated.")
        }
      } else if (transform == "gl2Treemix") {
        if (is.null(input$T2_Path8) || input$T2_Path8 == "") {
          stop("Please specify a valid output path for Treemix files.")
        } else {
          result = gl2treemix(gl(), outfile = "Treemix.gz", outpath = input$T2_Path8, verbose = 0)
          Cstatus3("Treemix (GZ) file has been generated.")
        }
      } else if (transform == "gl2BayeScan") {
        if (is.null(input$T2_Path9) || input$T2_Path9 == "") {
          stop("Please specify a valid output path for BayeScan files.")
        } else {
          gl2bayescan(gl(), outfile = "BayeScan.txt", outpath = input$T2_Path9, verbose = 0)
          Cstatus3("BayeScan (TXT) file has been generated.")
        }
      }
      output$Cstatus3 = renderText({ Cstatus3() })
      shinyjs::hide("gl2Status")
      guide_C("The data has been transformed.")
      showNotification("Run Successfully", type = "message")
    }, error = function(e) {
      shinyjs::hide("gl2Status")
      showNotification(paste("Fail: ", e$message), type = "error", duration = 10)
      Cstatus3("")
      guide_C("Please check the input and try again.")
    })
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