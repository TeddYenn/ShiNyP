# Page_6_Selection_Sweep
##### Page 6: Selection Sweep #####
#' @title Page_6_Selection_Sweep_UI
#' @export
Page_6_Selection_Sweep_UI = function() {
  tabPanel("Selection Sweep",
           tabsetPanel(
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
           ))
}
#' @title Page_6_Selection_Sweep_Server
#' @export
Page_6_Selection_Sweep_Server = function(input, output, session) {
  ##### Page 6: Selection Sweep #####
  ##### pcadapt #####
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
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
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
    guide_pcadapt("To run pcadapt, the input data must be in ✅ data.frame format. \nYou also need to upload a ▶️ Site Info file (in RDS format). \nPlease click the 'Run pcadapt' button.")
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
      pre_results[[45]] = "## Selection Sweep"
      pre_results[[46]] = paste0("### pcadapt (PCA-based genome scan for selection)")
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
      shinyjs::show("pcadaptStatus")
      pdf(file, width = 10, height = 4)
      print(pcadaptplot1())
      dev.off()
      shinyjs::hide("pcadaptStatus")
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
      shinyjs::show("pcadaptStatus")
      saveRDS(pcadapt_data3(), file)
      shinyjs::hide("pcadaptStatus")
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
      shinyjs::show("pcadaptStatus")
      pdf(file, width = 4, height = 4)
      print(pcadaptplot2())
      dev.off()
      shinyjs::hide("pcadaptStatus")
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
      shinyjs::show("pcadaptStatus")
      pdf(file, width = 4, height = 4)
      print(pcadaptplot3())
      dev.off()
      shinyjs::hide("pcadaptStatus")
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
      shinyjs::show("pcadaptStatus")
      pdf(file, width = 4, height = 4)
      print(pcadaptplot4())
      dev.off()
      shinyjs::hide("pcadaptStatus")
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
      shinyjs::show("pcadaptStatus")
      write.csv(pcadapt_data2(), file, row.names = FALSE)
      shinyjs::hide("pcadaptStatus")
    }
  )
  
  output$guide_pcadapt = renderUI({ div(class = "guide-text-block", guide_pcadapt()) })
  output$pcadapttitle1 = renderText({ pcadapttitle1() })
  output$pcadapttitle2 = renderText({ pcadapttitle2() })
  output$pcadapttitle3 = renderText({ pcadapttitle3() })
  output$pcadapttitle4 = renderText({ pcadapttitle4() })
  output$pcadapttitle5 = renderText({ pcadapttitle5() })
  
  ##### OutFLANK #####
  output$fileSelection_OutFLANK = renderUI({
    if (!is.null(gl())){ choices = c("genlight file" = "gl") } else { choices = "" }
    selectInput("FileforOutFLANK", "Dataset for OutFLANK:", choices)
  })
  
  output$OutFLANKfileInfo = renderText({
    req(gl())
    group_info = ifelse(nPop(gl())>1, "Added", "NaN \n**Warning** Not available!")
    paste0("Type: ", class(gl()), "\n",
           "Number of samples: ", nInd(gl()), "\n",
           "Number of SNPs: ", nLoc(gl()), "\n",
           "Group Info.: ", group_info)
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
    req(input$FileforOutFLANK, gl()@pop, SNP_Info())
    shinyjs::show("OutFLANKStatus")
    data = switch(input$FileforOutFLANK, "gl" = gl())
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
    guide_OutFLANK("To run OutFLANK, the input data must be in ✅ genlight file with 'Group Info.' \nYou also need to upload a ▶️ Site Info file (in RDS format).\nPlease click the 'Run OutFLANK' button.")
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
      pre_results[[45]] = "## Selection Sweep"
      pre_results[[49]] = paste0("### OutFLANK (Fst-based genome scan for selection)")
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
      shinyjs::show("OutFLANKStatus")
      pdf(file, width = 10, height = 4)
      print(OutFLANKplot1())
      dev.off()
      shinyjs::hide("OutFLANKStatus")
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
      shinyjs::show("OutFLANKStatus")
      saveRDS(outflank_data4(), file)
      shinyjs::hide("OutFLANKStatus")
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
      shinyjs::show("OutFLANKStatus")
      pdf(file, width = 10, height = 4)
      print(OutFLANKplot2())
      dev.off()
      shinyjs::hide("OutFLANKStatus")
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
      shinyjs::show("OutFLANKStatus")
      pdf(file, width = 4, height = 4)
      print(OutFLANKplot3())
      dev.off()
      shinyjs::hide("OutFLANKStatus")
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
      shinyjs::show("OutFLANKStatus")
      pdf(file, width = 4, height = 4)
      print(OutFLANKplot4())
      dev.off()
      shinyjs::hide("OutFLANKStatus")
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
      shinyjs::show("OutFLANKStatus")
      pdf(file, width = 4, height = 4)
      print(OutFLANKplot5())
      dev.off()
      shinyjs::hide("OutFLANKStatus")
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
      shinyjs::show("OutFLANKStatus")
      write.csv(outflank_data2(), file, row.names = FALSE)
      shinyjs::hide("OutFLANKStatus")
    }
  )
  
  output$guide_OutFLANK = renderUI({ div(class = "guide-text-block", guide_OutFLANK()) })
  output$OutFLANKtitle1 = renderText({ OutFLANKtitle1() })
  output$OutFLANKtitle2 = renderText({ OutFLANKtitle2() })
  output$OutFLANKtitle3 = renderText({ OutFLANKtitle3() })
  output$OutFLANKtitle4 = renderText({ OutFLANKtitle4() })
  output$OutFLANKtitle5 = renderText({ OutFLANKtitle5() })
  
  ##### IBS #####
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
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
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
        shinyjs::show("IBSStatus")
        pdf(file, width = 12, height = 5)
        print(IBSplot())
        dev.off()
        shinyjs::hide("IBSStatus")
      }
    )
    
    output$DIBS_SW = downloadHandler(
      filename = function() {
        paste0("IBS_Sliding_Window-", input$REF, " vs ", input$COMPAR, ".csv")
      },
      content = function(file) {
        shinyjs::show("IBSStatus")
        write.csv(IBS_result()$window_data, file, row.names = FALSE)
        shinyjs::hide("IBSStatus")
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
    guide_IBS = reactiveVal("To run IBS, the input data must be in ✅ data.frame format. \nYou also need to upload the ▶️ Site Info file (in RDS format) and ▶️ Chromosome Info file (in CSV format). \nPlease click the 'Run IBS' button.")
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
    
    Chr_axis = data %>%
      group_by(Chr) %>%
      summarise(center = (max(AdjPos, na.rm = TRUE) + min(AdjPos, na.rm = TRUE)) / 2, .groups = "drop")
    
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
    guide_Manhattan("This page allows you to customize a Manhattan plot. You can upload:\n✅ Genetic diversity per site (in RDS), or\n✅ pcadapt p-value per site (in RDS), or \n✅ OutFLANK p-value per site (in RDS), and\n✅ Chromosome Info.\nOnce your files are uploaded, click the 'Run Manhattan Plot' button.")
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
      shinyjs::show("ManhattanStatus")
      htmlwidgets::saveWidget(ManhattanPlot(), file)
      shinyjs::hide("ManhattanStatus")
    })
  
  output$ManhattanPlot1 = renderText({ ManhattanPlot1() })
  output$guide_Manhattan = renderUI({ div(class = "guide-text-block", guide_Manhattan()) })
}