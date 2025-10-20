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
                          bslib::tooltip(
                            sliderInput("coverage", "Coverage (%)", min = 80, max = 100, value = 95, step = 0.1),
                            "Minimal coverage"
                          ),
                          bslib::tooltip(
                            selectInput("diff", "Minimal differences", choices = c(1, 0.1, 0.01, 0.001),
                                        selected = 0.001),
                            "Minimal coverage differences between iterations"
                          ),
                          actionButton("runCoreSample", "Run Core Sample", class = "run-action-button"),
                          actionButton("resetCoreSample", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_CoreSample"),
                          div(id = "CoreSampleStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   div(class = "title-text-style", textOutput("CoreSampletitle1")),
                                   verbatimTextOutput("CoreSampleres"),
                                   uiOutput("download_core_sample_dataset"),
                                   uiOutput("download_core_sample_info"),
                                   uiOutput("download_core_sample_coverage")
                            ),
                            column(8,
                                   div(class = "title-text-style", textOutput("CoreSampletitle2")),
                                   plotOutput("CoreSampleplot", width = "690px", height = "500px"),
                                   uiOutput("download_core_sample_plot"),
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
                          bslib::tooltip(
                            uiOutput("Site_Info5"),
                            "Upload: Site Info. (RDS)"
                          ),
                          bslib::tooltip(
                            uiOutput("Chr_Info3"),
                            "Upload: Chromosome Info. (CSV)"
                          ),
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
                                        "Maximize ratio (%)", 
                                        min = 0, max = 100, value = 10, step = 0.1)),
                          conditionalPanel(
                            condition = "input.CoreSNPmethod == 'random_percentage'",
                            sliderInput("random_percentage", 
                                        "Sampling ratio (%)", 
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
                          div(id = "CoreSNPStatus", style = "color: red; font-weight: bold;", HTML("It may take a while <span class='loading-dots'><span>•</span><span>•</span><span>•</span></span>")),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("CoreSNPtitle1")),
                          verbatimTextOutput("CoreSNPres"),
                          uiOutput("download_core_SNP_dataset"),
                          uiOutput("download_core_SNP_site_info"),
                          uiOutput("download_core_SNP_info"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("CoreSNPtitle2")),
                          plotOutput("CoreSNPplot", width = "950px", height = "350px"),
                          uiOutput("download_core_SNP_plot"),
                          width = 9)
                      ))
           ))
}
#' @title Page_7_Core_Collection_Server
#' @export
Page_7_Core_Collection_Server = function(input, output, session) {
  
  #### Core Sample Set ####
  
  # ---- Select File ----
  
  output$fileSelection_CoreSample = renderUI({
    if (!is.null(df())){
      choices = c("data.frame file" = "df")
    } else {
      choices = ""
    }
    selectInput("FileforCoreSample", "Dataset for core sample set:", choices)
  })
  
  # ---- Core Functions ----
  
  observeEvent(input$runCoreSample, {
    tryCatch({
      req(input$FileforCoreSample)
      
      shinyjs::show("CoreSampleStatus")
      
      data = df()
      if (is.null(data) || !is.data.frame(data)) {
        shinyjs::hide("CoreSampleStatus")
        showNotification("Fail: The input data is not available or is not a data frame.", type = "error")
        return()
      }
      
      if (nrow(data) < 2 || ncol(data) < 2) {
        shinyjs::hide("CoreSampleStatus")
        showNotification("Fail: The input data must contain at least two samples and two SNPs.", type = "error")
        return()
      }
      
      core_sample = core.set(as.data.frame(t(data)), 
                              coverage = as.numeric(input$coverage), 
                              difference = as.numeric(input$diff))
      
      if (is.null(core_sample$coverage.table) || is.null(core_sample$coreset)) {
        shinyjs::hide("CoreSampleStatus")
        showNotification("Fail: Core set result is invalid or incomplete.", type = "error")
        return()
      }
      
      core_sample_coverage(core_sample$coverage.table)
      dataset = as.data.frame(t(core_sample$coreset))
      colnames(dataset) = colnames(data)
      row.names(dataset) = core_sample$coverage.table[,2]
      core_sample_dataset(dataset)
      
      core_sample_info = data.frame(
        "ID" = row.names(data),
        "Core_sample" = ifelse(row.names(data) %in% core_sample_coverage()[,2], "TRUE", "FALSE")
      )
      core_sample_info(core_sample_info)
      
      shinyjs::hide("CoreSampleStatus")
      CoreSampletitle1("Core Sample Set")
      CoreSampletitle2("Coverage Plot of Core Sample Set")
      guide_CoreSample("The core sample set is completed.")
      
      pre_results = pre_results()
      pre_results[[51]] = "## Core Collection"
      pre_results[[52]] = "### Core Sample Set"
      text = paste0(
        "Methodology: Establish a core collection that represents the genetic variation of the entire population. ",
        "This approach is a modified function from GenoCore (Jeong et al. 2017).\n",
        "Number of core samples: ", length(core_sample_coverage()[,2]), " (", 
        round(length(core_sample_coverage()[,2])/dim(data)[1], 4)* 100, "%)\n",
        "Total coverage: ", max(as.numeric(core_sample_coverage()[,3])), "%\n",
        "The top representative core samples are: ", paste0(head(core_sample_coverage())[,2], collapse = ", "), 
        ", with cumulative coverage values of ", paste(head(core_sample_coverage()[,3]), collapse = ", "), "%, respectively."
      )
      pre_results[[53]] = text
      pre_results(pre_results)
      showNotification("Run Successfully", type = "message")
    }, error = function(e) {
      shinyjs::hide("CoreSampleStatus")
      showNotification(paste("Fail:", e$message), type = "error")
      return()
    })
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
  
  # ---- Show Plot  ----
  
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
  
  # ---- Download Plot  ----
  
  output$download_core_sample_plot = renderUI({
    if (CoreSampletitle2() == "Coverage Plot of Core Sample Set") {
      actionButton(
        inputId = "show_download_core_sample_plot", 
        label = tagList(shiny::icon("download"), "Download Plot"), 
        class = "AI1-action-button"
      )
    }
  })
  
  observeEvent(input$show_download_core_sample_plot, {
    showModal(
      modalDialog(
        title = "Download Plot Settings",
        fluidRow(
          column(6,
                 numericInput("dl_core_width", "Width", value = 10, min = 1, max = 30, step = 1),
                 numericInput("dl_core_height", "Height", value = 6, min = 1, max = 30, step = 1),
                 selectInput("dl_core_unit", "Unit", choices = c("inches" = "in", "cm" = "cm"), selected = "in")
          ),
          column(6,
                 selectInput("dl_core_format", "File format", choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg"), selected = "pdf"),
                 conditionalPanel(
                   condition = "input.dl_core_format == 'png' || input.dl_core_format == 'jpeg'",
                   numericInput("dl_core_dpi", "Resolution (DPI)", value = 300, min = 72, max = 600, step = 10)
                 )
          )
        ),
        footer = tagList(
          downloadButton("DCoreSample_plot", "Download"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  output$DCoreSample_plot = downloadHandler(
    filename = function() {
      ext = input$dl_core_format
      cov = if (!is.null(input$coverage)) input$coverage else "coverage"
      paste0("Core_Sample_Plot-", cov, "%.", ext)
    },
    content = function(file) {
      shinyjs::show("CoreSampleStatus")
      req(CoreSampleplot())
      width = input$dl_core_width
      height = input$dl_core_height
      units = input$dl_core_unit
      device = input$dl_core_format
      dpi = input$dl_core_dpi
      
      if (device == "pdf") {
        ggsave(file, plot = CoreSampleplot(), device = "pdf", width = width, height = height, units = units)
      } else if (device == "jpeg") {
        ggsave(file, plot = CoreSampleplot(), device = "jpeg", width = width, height = height, units = units, dpi = dpi)
      } else {
        ggsave(file, plot = CoreSampleplot(), device = "png", width = width, height = height, units = units, dpi = dpi)
      }
      shinyjs::hide("CoreSampleStatus")
      removeModal()
    }
  )
  
  # ---- Download Table  ----
  
  output$download_core_sample_dataset = renderUI({
    if (CoreSampletitle1() == "Core Sample Set") {
      downloadButton("Dcore_sample_dataset", "Download data.frame of Core Samples Set")
    }
  })
  
  output$Dcore_sample_dataset = downloadHandler(
    filename = function(){
      paste0("data.frame_", dim(core_sample_dataset())[1], "_", dim(core_sample_dataset())[2], "SNPs_", "Core_Sample_Set.rds")
    },
    content = function(file) {
      shinyjs::show("CoreSampleStatus")
      saveRDS(core_sample_dataset(), file)
      shinyjs::hide("CoreSampleStatus")
    }
  )
  
  output$download_core_sample_coverage = renderUI({
    if (CoreSampletitle1() == "Core Sample Set") {
      downloadButton("Dcore_sample_coverage", "Download Coverage Data")
    }
  })
  
  output$Dcore_sample_coverage = downloadHandler(
    paste0("Core_Sample_Coverage_Data-", input$coverage, "%.csv"),
    content = function(file) {
      shinyjs::show("CoreSampleStatus")
      write.csv(core_sample_coverage(), file, row.names = FALSE)
      shinyjs::hide("CoreSampleStatus")
    }
  )
  
  output$download_core_sample_info = renderUI({
    if (CoreSampletitle1() == "Core Sample Set") {
      downloadButton("D_core_sample_info", "Download Core Sample List")
    }
  })
  
  output$D_core_sample_info = downloadHandler(
    filename = function(){
      paste0("Core_Sample_List-", input$coverage, "%.csv")
    },
    content = function(file) {
      shinyjs::show("CoreSampleStatus")
      write.csv(core_sample_info(), file, row.names = FALSE)
      shinyjs::hide("CoreSampleStatus")
    }
  )
  
  output$guide_CoreSample = renderUI({ div(class = "guide-text-block", guide_CoreSample()) })
  output$CoreSampletitle1 = renderText({ CoreSampletitle1() })
  output$CoreSampletitle2 = renderText({ CoreSampletitle2() })
  
  #### Core SNP Set ####
  
  # ---- Select File ----
  
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
    tryCatch({
      Site_Info_obj = readRDS(input$Site_Info5$datapath)
      if (!is.data.frame(Site_Info_obj)) stop("Not a data.frame file.")
      required_cols = c("Chr", "Pos", "Marker")
      missing_cols = setdiff(required_cols, names(Site_Info_obj))
      if (length(missing_cols) > 0) stop(paste("Site Info file is missing required columns:",
                                               paste(missing_cols, collapse = ", ")))
      Site_Info(Site_Info_obj)
      showNotification("Uploaded successfully", type = "message")
    }, error = function(e) {
      Site_Info(NULL)
      showNotification(paste("Fail: ", e$message), type = "error", duration = 10)
    })
  })
  
  output$Chr_Info3 = renderUI({
    fileInput("Chr_Info3", "Chromosome Info.* (required)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$Chr_Info3, {
    req(input$Chr_Info3)
    tryCatch({
      Chr_Info_obj = read.csv(input$Chr_Info3$datapath, stringsAsFactors = FALSE)
      if (!is.data.frame(Chr_Info_obj)) stop("Not a CSV data.frame.")
      if (!is.numeric(Chr_Info_obj$Start) || !is.numeric(Chr_Info_obj$End))
        stop("The 'Start' and 'End' columns in Chromosome Info must be numeric.")
      Chr_Info(Chr_Info_obj)
      showNotification("Uploaded successfully", type = "message")
    }, error = function(e) {
      Chr_Info(NULL)
      showNotification(paste("Fail: ", e$message), type = "error", duration = 10)
    })
  })
  
  output$dapc_Upload = renderUI({
    fileInput("CoreSNPdata", "DAPC Object* (required)", multiple = F, accept = c(".rds"))
  })
  
  # ---- Core Functions ----
  
  observeEvent(input$runCoreSNP, {
    tryCatch({
      # --- Basic requirement checks ---
      req(input$FileforCoreSNP, Site_Info())
      method_chosen = input$CoreSNPmethod
      df_data = df()
      site_info = Site_Info()
      
      shinyjs::show("CoreSNPStatus")
      guide_CoreSNP("Running...")
      
      # --- General data checks ---
      if (is.null(df_data) || !is.data.frame(df_data)) {
        shinyjs::hide("CoreSNPStatus")
        showNotification("Fail: Genotype data is not available or not in a valid data.frame format.", type = "error")
        return()
      }
      if (ncol(df_data) < 2 || nrow(df_data) < 2) {
        shinyjs::hide("CoreSNPStatus")
        showNotification("Fail: The input data must have at least two samples and two SNPs.", type = "error")
        return()
      }
      if (is.null(site_info) || !is.data.frame(site_info)) {
        shinyjs::hide("CoreSNPStatus")
        showNotification("Fail: Site Info. is missing or invalid.", type = "error")
        return()
      }
      selected_SNPs = character(0)
      text = ""
      
      # --- DAPC method ---
      if (method_chosen == "dapc") {
        req(input$CoreSNPdata)
        # Data check for DAPC input
        if (is.null(input$CoreSNPdata$datapath) || !file.exists(input$CoreSNPdata$datapath)) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: DAPC result file not found.", type = "error")
          return()
        }
        DAPC = readRDS(input$CoreSNPdata$datapath)
        if (is.null(DAPC$var.contr) || is.null(DAPC$eig)) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: DAPC object is incomplete or invalid.", type = "error")
          return()
        }
        loading = DAPC$var.contr
        ID = row.names(loading)
        ID_trim = substr(ID, 1, nchar(ID) - 2)
        ID_retain_loc = which(duplicated(ID_trim) == TRUE)
        if (length(ID_retain_loc) == 0) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: No duplicated SNP IDs found in DAPC loadings. Check the DAPC results.", type = "error")
          return()
        }
        row.names(loading) = ID_trim
        loading = loading[ID_retain_loc, ]
        percent = DAPC$eig / sum(DAPC$eig)
        ratio = as.numeric(input$CoreSNPratio) / 100
        num = ratio * nrow(loading)
        sel = round(num * percent, 0)
        if (any(sel < 1)) sel[sel < 1] = 1
        select = list()
        for (i in seq_len(ncol(loading))) {
          select[[i]] = names(sort(abs(loading[, i]), decreasing = TRUE)[1:sel[i]])
        }
        selected_SNPs = unique(unlist(select))
        text = "Methodology: DAPC-based selection method. ShiNyP first calculates the proportion of variance explained by each discriminant component. This proportion is then used to select the top SNPs for each component based on the absolute values of their loading scores. The final set of selected SNPs is obtained by taking the union of these individual marker sets."
        
        # --- Random percentage method ---
      } else if (method_chosen == "random_percentage") {
        if (is.null(input$random_percentage) || is.na(input$random_percentage)) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: Please specify the percentage for random SNP selection.", type = "error")
          return()
        }
        pct = input$random_percentage / 100
        total_snps = ncol(df_data)
        sample_size = round(total_snps * pct)
        if (sample_size < 1) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: The selected percentage results in zero SNPs.", type = "error")
          return()
        }
        selected_SNPs = sample(colnames(df_data), sample_size)
        text = "Methodology: Random percentage-based selection. A specified percentage of SNPs is randomly selected."
        
        # --- Random density method ---
      } else if (method_chosen == "random_density") {
        if (is.null(input$random_density) || is.na(input$random_density) || input$random_density <= 0) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: Please provide a valid SNP density value (bp per SNP).", type = "error")
          return()
        }
        req(Chr_Info())
        chr_info = Chr_Info()
        if (is.null(chr_info) || ncol(chr_info) < 3) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: Chr Info. is missing or invalid.", type = "error")
          return()
        }
        bp_per_snp = input$random_density
        chr_length = chr_info[, 3]
        n_snp_target = floor(chr_length / bp_per_snp)
        site_info_by_chr = split(site_info, site_info[, 1])
        n_snp_in_chr = sapply(site_info_by_chr, nrow)
        n_snp_in_chr_sorted = n_snp_in_chr[order(as.numeric(names(n_snp_in_chr)))]
        
        selected_chr_list = lapply(names(site_info_by_chr), function(chr) {
          df_chr = site_info_by_chr[[chr]] 
          n_snp_target_chr = n_snp_target[as.numeric(chr)]
          n_snp_target_chr = min(n_snp_target_chr, nrow(df_chr))
          sample(df_chr[,3], n_snp_target_chr)
        })
        selected_SNPs = unique(unlist(selected_chr_list))
        
        if (length(selected_SNPs) == 0) {
          shinyjs::hide("CoreSNPStatus")
          showNotification("Fail: No SNPs were selected by random density. Please check your parameters.", type = "error")
          return()
        }
        text = "Methodology: Random density-based selection. SNPs are selected at regular intervals based on their density within each chromosome."
        showNotification("Run Successfully", type = "message")
      } else {
        shinyjs::hide("CoreSNPStatus")
        showNotification("Error", type = "error")
        return()
      }
      
      # --- Subset and check selected SNPs ---
      if (length(selected_SNPs) == 0) {
        shinyjs::hide("CoreSNPStatus")
        showNotification("Fail: No SNPs selected. Please check your selection parameters.", type = "error")
        return()
      }
      subset_data = df_data[, selected_SNPs, drop = FALSE]
      # Data conversion
      subset_data = lapply(subset_data, function(x) as.numeric(as.character(x)))
      subset_data = as.data.frame(subset_data)
      colnames(subset_data) = selected_SNPs
      row.names(subset_data) = row.names(df_data)
      # Optional: sort columns if needed
      subset_data = subset_data[, order(as.numeric(gsub(":.*", "", colnames(subset_data))),
                                         as.numeric(gsub(".*:", "", colnames(subset_data))))]
      
      core_SNP_dataset(subset_data)
      
      core_SNP_info = data.frame(
        "ID"       = colnames(df_data),
        "Core_SNP" = ifelse(colnames(df_data) %in% selected_SNPs, "TRUE", "FALSE")
      )
      core_SNP_info(core_SNP_info)
      
      selected_Site_Info = site_info[which(site_info[, 3] %in% selected_SNPs), ]
      selected_Site_Info(selected_Site_Info)
      
      shinyjs::hide("CoreSNPStatus")
      CoreSNPtitle1("Core SNP Set")
      CoreSNPtitle2("Distribution of Core SNPs")
      guide_CoreSNP("The core SNP set is completed.\nYou can input the core set (as a data.frame file), run a PCA analysis, and then review the results.")
      
      pre_results = pre_results()
      pre_results[[51]] = "## Core Collection"
      pre_results[[55]] = "### Core SNPs set"
      text = paste0(text, "\n",
                     "Number of core SNPs: ", length(selected_SNPs), " (",
                     round(length(selected_SNPs)/ncol(df_data), 4)*100, "%)")
      pre_results[[56]] = text
      pre_results(pre_results)
    }, error = function(e) {
      shinyjs::hide("CoreSNPStatus")
      showNotification(paste("Fail:", e$message), type = "error")
      return()
    })
  })
  
  observeEvent(input$resetCoreSNP, {
    core_SNP_dataset(NULL)
    CoreSNPtitle1("")
    CoreSNPtitle2("")
    selected_Site_Info(NULL)
    showNotification("Data have been reset.")
    guide_CoreSNP("To run core SNP set, the input data must be in ✅ data.frame format. \nYou also need to upload the ▶️ Site Info. and ▶️ Chromosome Info file (in CSV). \nPlease click the 'Run Core SNP' button.")
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
  
  # ---- Show Plot  ----
  
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
  
  # ---- Download Plot  ----
  
  output$download_core_SNP_plot = renderUI({
    if (CoreSNPtitle2() == "Distribution of Core SNPs") {
      actionButton(
        inputId = "show_download_core_SNP_plot", 
        label = tagList(shiny::icon("download"), "Download Plot"), 
        class = "AI1-action-button"
      )
    }
  })
  
  observeEvent(input$show_download_core_SNP_plot, {
    showModal(
      modalDialog(
        title = "Download Plot Settings",
        fluidRow(
          column(6,
                 numericInput("dl_coreSNP_width", "Width", value = 12, min = 1, max = 30, step = 1),
                 numericInput("dl_coreSNP_height", "Height", value = 5, min = 1, max = 30, step = 1),
                 selectInput("dl_coreSNP_unit", "Unit", choices = c("inches" = "in", "cm" = "cm"), selected = "in")
          ),
          column(6,
                 selectInput("dl_coreSNP_format", "File format", choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg"), selected = "pdf"),
                 conditionalPanel(
                   condition = "input.dl_coreSNP_format == 'png' || input.dl_coreSNP_format == 'jpeg'",
                   numericInput("dl_coreSNP_dpi", "Resolution (DPI)", value = 300, min = 72, max = 600, step = 10)
                 )
          )
        ),
        footer = tagList(
          downloadButton("DCoreSNP_plot", "Download"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  output$DCoreSNP_plot = downloadHandler(
    filename = function() {
      ext = input$dl_coreSNP_format
      paste0("Core_SNP_Plot-", input$CoreSNPmethod, ext)
    },
    content = function(file) {
      shinyjs::show("CoreSNPStatus")
      req(CoreSNPplot())
      width = input$dl_coreSNP_width
      height = input$dl_coreSNP_height
      units = input$dl_coreSNP_unit
      device = input$dl_coreSNP_format
      dpi = input$dl_coreSNP_dpi
      
      if (device == "pdf") {
        ggsave(file, plot = CoreSNPplot(), device = "pdf", width = width, height = height, units = units)
      } else if (device == "jpeg") {
        ggsave(file, plot = CoreSNPplot(), device = "jpeg", width = width, height = height, units = units, dpi = dpi)
      } else {
        ggsave(file, plot = CoreSNPplot(), device = "png", width = width, height = height, units = units, dpi = dpi)
      }
      shinyjs::hide("CoreSNPStatus")
      removeModal()
    }
  )
  
  # ---- Download Table  ----
  
  output$download_core_SNP_dataset = renderUI({
    if (CoreSNPtitle1() == "Core SNP Set") {
      downloadButton("Dcore_SNP_dataset", "Download data.frame")
    }
  })
  
  output$Dcore_SNP_dataset = downloadHandler(
    filename = function(){
      paste0("data.frame_", 
             nrow(core_SNP_dataset()), "_", 
             ncol(core_SNP_dataset()),
             "SNPs_", 
             "Core_SNP_Set.rds")
    },
    content = function(file) {
      shinyjs::show("CoreSNPStatus")
      saveRDS(core_SNP_dataset(), file)
      shinyjs::hide("CoreSNPStatus")
    }
  )
  
  output$download_core_SNP_info = renderUI({
    if (CoreSNPtitle1() == "Core SNP Set") {
      downloadButton("D_core_SNP_info", "Download Core SNPs List")
    }
  })
  
  output$D_core_SNP_info = downloadHandler(
    filename = function(){
      paste0("Core_SNP_List-", input$CoreSNPmethod, ".rds")
    },
    content = function(file) {
      shinyjs::show("CoreSNPStatus")
      saveRDS(core_SNP_info(), file)
      shinyjs::hide("CoreSNPStatus")
    }
  )
  
  output$download_core_SNP_site_info = renderUI({
    if (CoreSNPtitle1() == "Core SNP Set") {
      downloadButton("D_CoreSNP_site_info", "Download Site Info.")
    }
  })
  
  output$D_CoreSNP_site_info = downloadHandler(
    filename = function(){
      paste0("Site_Info_", 
             nrow(core_SNP_dataset()), "_", 
             ncol(core_SNP_dataset()), 
             "SNPs_", 
             "Core_SNP_Set.rds")
    },
    content = function(file) {
      shinyjs::show("CoreSNPStatus")
      saveRDS(selected_Site_Info(), file)
      shinyjs::hide("CoreSNPStatus")
    }
  )
  
  output$guide_CoreSNP = renderUI({ div(class = "guide-text-block", guide_CoreSNP()) })
  output$CoreSNPtitle1 = renderText({ CoreSNPtitle1() })
  output$CoreSNPtitle2 = renderText({ CoreSNPtitle2() })
  
}