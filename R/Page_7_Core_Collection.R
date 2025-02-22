# Page_7_Core_Collection
##### Page 7: Core Collection #####
#' @title Page_7_Core_Collection_UI
#' @export
Page_7_Core_Collection_UI = function() {
  tabPanel("Core Collection",
           tabsetPanel(
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
                          tags$hr(),
                          selectInput(
                            inputId = "CoreSNPmethod", 
                            label = "Selection method",
                            choices = c("DAPC-based" = "dapc",
                                        "Random Sampling (percentage)" = "random_percentage",
                                        "Random Sampling (density)" = "random_density"),
                            selected = "random_percentage"),
                          conditionalPanel(
                            condition = "input.CoreSNPmethod == 'dapc'",
                            fileInput("CoreSNPdata", "DAPC Object* (required)", 
                                      multiple = FALSE, accept = c(".rds")),
                            sliderInput("CoreSNPratio", 
                                        "Core SNP Ratio (%)", 
                                        min = 0, max = 100, value = 10, step = 0.1)),
                          conditionalPanel(
                            condition = "input.CoreSNPmethod == 'random_percentage'",
                            sliderInput("random_percentage", 
                                        "Sampling percentage (%)", 
                                        min = 0, max = 100, value = 10, step = 0.1)),
                          conditionalPanel(
                            condition = "input.CoreSNPmethod == 'random_density'",
                            numericInput("random_density", "bp / SNP",
                                         value = 50000, min = 0, step = 1000,
                                         width = "200px")),
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
           ))
}
#' @title Page_7_Core_Collection_Server
#' @export
Page_7_Core_Collection_Server = function(input, output, session) {
  ##### Page 7: Core Collection #####
  ##### Core Sample Set #####
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
    pre_results[[51]] = "## Core Collection"
    pre_results[[52]] = "### Core Sample Set"
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
    guide_CoreSample("To run core sample set, the input data must be in ✅ data.frame format. \nPlease click the 'Run Core Sample' button.")
    })
  
  output$CoreSamplefileInfo = renderText({
    req(df())
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
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
  
  observeEvent(input$runCoreSNP, {
    method_chosen = input$CoreSNPmethod
    req(input$FileforCoreSNP, Site_Info())
    shinyjs::show("CoreSNPStatus")
    guide_CoreSNP("Running...")
    data = switch(input$FileforCoreSNP, "df" = df())
    
    selected_SNPs = character(0)
    
    if (method_chosen == "dapc") {
      req(input$CoreSNPdata) 
      DAPC = readRDS(input$CoreSNPdata$datapath)
      loading = DAPC$var.contr
      ID      = row.names(loading)
      ID_trim = substr(ID, 1, nchar(ID) - 2)
      ID_retain_loc = which(duplicated(ID_trim) == TRUE)
      row.names(loading) = ID_trim
      loading = loading[ID_retain_loc, ]
      percent = DAPC$eig / sum(DAPC$eig)
      
      ratio = as.numeric(input$CoreSNPratio)/100
      num   = ratio * nrow(loading)
      sel   = round(num * percent, 0)
      
      select = list()
      for (i in seq_len(ncol(loading))) {
        select[[i]] = names(sort(abs(loading[,i]), 
                                  decreasing = TRUE)[1:sel[i]])
      }
      selected_SNPs = unique(unlist(select))
      
    } else if (method_chosen == "random_percentage") {
      pct = input$random_percentage / 100
      
      total_snps  = ncol(data)
      sample_size = round(total_snps * pct)
      selected_SNPs = sample(colnames(data), sample_size)
      
    } else if (method_chosen == "random_density") {
      bp_per_snp = input$random_density
      req(Chr_Info()) 
      Chr_Info  = Chr_Info()
      Site_Info = Site_Info()
      
      chr_length = Chr_Info[,3]
      n_snp_target = floor(chr_length / bp_per_snp)
      site_info_by_chr = split(Site_Info, Site_Info[,1])
      n_snp_in_chr = sapply(site_info_by_chr, nrow)
      n_snp_in_chr_sorted = n_snp_in_chr[order(as.numeric(names(n_snp_in_chr)))]
      
      selected_chr_list = lapply(names(site_info_by_chr), function(chr) {
        df_chr = site_info_by_chr[[chr]] 
        n_snp_target_chr = n_snp_target[as.numeric(chr)]
        n_snp_target_chr = min(n_snp_target_chr, nrow(df_chr))
        sample(df_chr[,3], n_snp_target_chr)
      })
      selected_SNPs = unique(unlist(selected_chr_list))
    }
    subset_data = data[, selected_SNPs, drop = FALSE]
    subset_data = lapply(subset_data, function(x) as.numeric(as.character(x)))
    subset_data = as.data.frame(subset_data)
    colnames(subset_data) = selected_SNPs
    core_SNP_dataset(subset_data)
    
    core_SNP_info = data.frame(
      "ID"       = colnames(data),
      "Core_SNP" = ifelse(colnames(data) %in% selected_SNPs, "TRUE", "FALSE")
    )
    core_SNP_info(core_SNP_info)
    
    Site_Info = Site_Info()
    selected_Site_Info = Site_Info[which(Site_Info[,3] %in% selected_SNPs), ]
    selected_Site_Info(selected_Site_Info)
    
    shinyjs::hide("CoreSNPStatus")
    CoreSNPtitle1("Core SNP Set")
    CoreSNPtitle2("Distribution of Core SNPs")
    guide_CoreSNP("The core SNP set is completed.\nYou can input the core set (as a data.frame file), run a PCA analysis, and then review the results.")
    
    pre_results = pre_results()
    pre_results[[51]] = "## Core Collection"
    pre_results[[55]] = "### Core SNPs set"
    pre_results[[56]] = paste0("Number of core SNPs: ", 
                                length(selected_SNPs), " (", 
                                round(length(selected_SNPs)/ncol(data), 4)*100, "%)")
    pre_results(pre_results)
    
    output$Dcore_SNP_dataset = downloadHandler(
      filename = paste0("data.frame_", 
                        nrow(core_SNP_dataset()), "_", 
                        ncol(core_SNP_dataset()), 
                        "SNPs_", 
                        "Core_SNP_Set.rds"),
      content = function(file) {
        saveRDS(core_SNP_dataset(), file)
      }
    )
    
    output$D_CoreSNP_site_info = downloadHandler(
      filename = paste0("Site_Info_", 
                        nrow(core_SNP_dataset()), "_", 
                        ncol(core_SNP_dataset()), 
                        "SNPs_", 
                        "Core_SNP_Set.rds"),
      content = function(file) {
        saveRDS(selected_Site_Info(), file)
      }
    )
  })
  
  
  
  observeEvent(input$resetCoreSNP, {
    core_SNP_dataset(NULL)
    CoreSNPtitle1("")
    CoreSNPtitle2("")
    selected_Site_Info(NULL)
    showNotification("Data have been reset.")
    guide_CoreSNP("To run core SNP set, the input data must be in ✅ data.frame format. \nYou also need to upload the ▶️ Site Info. and ▶️ Chromosome Info file (in CSV format). \nPlease click the 'Run Core SNP' button.")
    })
  
  output$CoreSNPfileInfo = renderText({
    req(df())
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
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
  
}