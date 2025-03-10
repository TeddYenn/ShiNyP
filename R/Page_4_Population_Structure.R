# Page_4_Population_Structure
##### Page 4: Population Structure #####
#' @title Page_4_Population_Structure_UI
#' @export
Page_4_Population_Structure_UI = function() {
  tabPanel("Population Structure",
           tabsetPanel(
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
           ))
}
#' @title Page_4_Population_Structure_Server
#' @export
Page_4_Population_Structure_Server = function(input, output, session) {
  ##### Page 4: Population Structure #####
  ##### PCA #####
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
    pca_data = pca_data[, apply(pca_data, 2, var) > 0]
    pca_result = prcomp(pca_data, scale = T)
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
    pre_results[[19]] = "## Population Structure"
    pre_results[[20]] = "### Principal Component Analysis (PCA)"
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
    guide_PCA("To run PCA, the input data must be in ✅ data.frame format. \nPlease click the 'Run PCA' button")
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
      fileInput("groupfile4", "Group Info. or Core Sample List", multiple = F, accept = c(".csv"))
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
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
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
  output$fileSelection_DAPC = renderUI({
    choices = list()
    if (!is.null(gi()) && !is.null(gl())) {
      choices = list("genind file" = "gi", "genlight file" = "gl")
    } else if (!is.null(gl())) {
      choices = list("genlight file" = "gl")
    } else if (!is.null(gi())) {
      choices = list("genind file" = "gi")
    }
    if (!is.null(gl())) {
      updateSliderInput(session, "npca", 
                        min = 1, 
                        max = length(gl()@ind.names), 
                        value = floor(length(gl()@ind.names) * 0.8), 
                        step = 1)
    } else if (!is.null(gi())) {
      updateSliderInput(session, "npca", 
                        min = 1, 
                        max = length(gi()@ploidy), 
                        value = floor(length(gi()@ploidy) * 0.8), 
                        step = 1)
    }
    selectInput("FileforDAPC", "Dataset for DAPC:", choices)
  })
  
  
  observeEvent(input$runDAPC1, {
    req(input$FileforDAPC, input$npca, input$Maxgrp)
    shinyjs::show("DAPCStatus")
    if (input$FileforDAPC == "gi"){
      DAPC1 = find.clusters(gi(), max.n = input$Maxgrp, n.pca = input$npca, scale = FALSE, choose = FALSE)
    }else if (input$FileforDAPC == "gl"){
      DAPC1 = find.clusters(gl(), max.n = input$Maxgrp, n.pca = input$npca, scale = FALSE, choose = FALSE)
    }
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
    req(input$FileforDAPC, input$npca, input$Maxgrp)
    shinyjs::show("DAPCStatus")
    if (input$FileforDAPC == "gi"){
      DAPC2 = find.clusters(gi(), n.clust = input$grp, n.pca = input$npca, scale = FALSE, choose = FALSE)
      DAPC2 = dapc(gi(), pop = DAPC2$grp, n.pca = input$grp-1, n.da = input$grp-1)
    }else if (input$FileforDAPC == "gl"){
      DAPC2 = find.clusters(gl(), n.clust = input$grp, n.pca = input$npca, scale = FALSE, choose = FALSE)
      DAPC2 = dapc(gl(), pop = DAPC2$grp, n.pca = input$grp-1, n.da = input$grp-1)
    }
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
    pre_results[[22]] = "### Discriminant Analysis of Principal Components (DAPC)"
    pre_results[[23]] = paste0("The samples were divided into ", length(table(DAPC2$assign)), " groups based on the BIC value at K = ", length(table(DAPC2$assign)))
    pre_results[[24]] = paste0("Group sizes, Group 1 to Group ", length(table(DAPC2$assign)), ": ", paste(as.numeric(table(DAPC2$assign)), collapse = ", "))
    pre_results[[25]] = paste0("The group centroid of each group at first discriminant function, Group 1 to Group ", length(table(DAPC2$assign)), ": ", paste(round(DAPC2$grp.coord[,1], 2), collapse = ", "))
    pre_results[[26]] = paste0("The group centroid of each group at second discriminant function, Group 1 to Group ", length(table(DAPC2$assign)), ": ", paste(round(DAPC2$grp.coord[,2], 2), collapse = ", "))
    pre_results(pre_results)
  })
  
  output$DAPCfileInfo = renderText({
    req(input$FileforDAPC)
    if (input$FileforDAPC == "gi"){
      req(gi())
      paste0("Type: ", class(gi()), "\n",
             "Number of samples: ", nInd(gi()), "\n",
             "Number of SNPs: ", nLoc(gi()))
    }else if (input$FileforDAPC == "gl"){
      req(gl())
      paste0("Type: ", class(gl()), "\n",
             "Number of samples: ", nInd(gl()), "\n",
             "Number of SNPs: ", nLoc(gl()))
    }
  })
  
  observeEvent(input$resetDAPC1, {
    DAPCtitle1("")
    DAPC1(NULL)
    BICplot(NULL)
    showNotification("Data have been reset.")
    guide_DAPC("To run DAPC, the input data must be in ✅ genlight or ✅ genind format. \nPlease click the 'Run DAPC I' button first.")
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
    guide_DAPC("To run DAPC, input data must be genlight or genind file.\nPlease click 'Run DAPC I' button first.")
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
    paste0("Type: ", class(gl()), "\n",
           "Number of samples: ", nInd(gl()), "\n",
           "Number of SNPs: ", nLoc(gl()))
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
    guide_UPGMA("To run the UPGMA phylogenetic tree, the input data must be in ✅ genlight format. \nPlease click the 'Run UPGMA' button.")
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
    paste0("Type: ", class(gl()), "\n",
           "Number of samples: ", nInd(gl()), "\n",
           "Number of SNPs: ", nLoc(gl()))
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
    guide_NJ("To run the NJ phylogenetic tree, the input data must be in ✅ genlight format.\nPlease click the 'Run NJ' button.")
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
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
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
    guide_Kinship("To run the kinship matrix, the input data must be in ✅ data.frame format. \nThe 'Group Info' CSV file from DAPC analysis is optional. \nPlease click the 'Run Kinship' button.")
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
    guide_scatter("This page allows you to customize scatter plot. You can upload: \n✅ PCA Object (in RDS), or\n✅ DAPC Object (in RDS), and\n✅ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Scatter Plot' button.")
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
    guide_Tree("This page allows you to customize a phylogenetic tree plot. You can upload:\n✅ UPGMA Object (in RDS), or\n✅ NJ Object (in RDS), and\n✅ Group and Other Info. (in CSV).\nOnce your files are uploaded, click the 'Run Tree Plot' button.")
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
}