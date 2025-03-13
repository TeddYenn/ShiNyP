# Page_5_Genetic_Diversity
##### Page 5: Genetic Diversity #####
#' @title Page_5_Genetic_Diversity_UI
#' @export
Page_5_Genetic_Diversity_UI = function() {
  tabPanel("Genetic Diversity",
           tabsetPanel(
             tabPanel("Diversity Parameter",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Diversity Parameter"),
                          tags$br(),
                          uiOutput("fileSelection_GD"),
                          verbatimTextOutput("GDfileInfo"),
                          tags$style("#GDfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("Site_Info1"),
                          uiOutput("groupfile3"),
                          actionButton("runGD", "Run Diversity Analysis", class = "run-action-button"),
                          actionButton("resetGD", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_GD"),
                          div(id = "GDStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("GDtitle1")),
                          plotOutput("GDplot", width = "950px", height = "350px"),
                          uiOutput("Type"),
                          uiOutput("Parameter"),
                          uiOutput("download_GD_plot"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("GDtitle2")),
                          DT::dataTableOutput("GDresults"),
                          uiOutput("download_GD_site"),
                          uiOutput("download_GD"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("GDtitle3")),
                          DT::dataTableOutput("GDgroupresults"),
                          uiOutput("download_GD_group"),
                          uiOutput("download_Fst"),
                          width = 9),
                      )),
             tabPanel("Circos Plot",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Circos Plot"),
                          tags$br(),
                          verbatimTextOutput("GDInfo"),
                          tags$style("#GDInfo { font-size: 14px;}"),
                          tags$hr(),
                          tags$h5("STEP I: Sliding Window"),
                          selectInput("SelePara", "Select parameters:", choices = NULL, multiple = TRUE),
                          sliderInput("WindowSize", "Window size (kb)", min = 0, max = 1000, value = 500, step = 10),
                          sliderInput("StepSize", "Step size (kb)", min = 0, max = 500, value = 100, step = 5),
                          actionButton("runSW", "Run Sliding Window", class = "run-action-button"),
                          actionButton("resetSW", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: Circos Plot"),
                          uiOutput("Chr_Info"), # Track 1
                          selectInput("Track1", "Track 1 & 2: Chromosome Info.", choices = NULL), # Track 1
                          uiOutput("Track3"), # Track 3-
                          actionButton("addTrack", "Add Track", class = "S-action-button"),
                          actionButton("runCircos", "Run Circos Plot", class = "run-action-button"),
                          actionButton("resetCircos", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_Circos"),
                          div(id = "CircosStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          uiOutput("progressUI"),
                          div(class = "title-text-style", textOutput("Circostitle1")),
                          DT::dataTableOutput("SWresults"),
                          uiOutput("download_SW"),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("Circostitle2")),
                          verbatimTextOutput("Circosplotinfo"),
                          uiOutput("downloadCircosplot"),
                          width = 9),
                      )),
             tabPanel("Genetic Distance", # Genetic Distance
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Genetic Distance"),
                          tags$br(),
                          uiOutput("fileSelection_GT"),
                          verbatimTextOutput("GTfileInfo"),
                          tags$style("#GTfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          uiOutput("groupfile5"),
                          selectInput("GT_method", "Method",
                                      choices = names(GT_method_choice), selected = "Cavalli-Sforza's chord distance"),
                          actionButton("runGT", "Run Genetic Distance", class = "run-action-button"),
                          actionButton("resetGT", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_GT"),
                          div(id = "GTStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6,
                                   div(class = "title-text-style", textOutput("GTtitle1")),
                                   plotOutput("GTplot", width = "400px", height = "400px"),
                                   uiOutput("download_GT_plot")
                            ),
                            column(6,
                                   div(class = "title-text-style", textOutput("GTtitle2")),
                                   tableOutput("GTresults"),
                                   uiOutput("download_GT_result")
                            )
                          ),width = 9)
                      )),
             tabPanel("AMOVA",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Analysis of Molecular Variance (AMOVA)"),
                          tags$br(),
                          uiOutput("fileSelection_AMOVA"),
                          verbatimTextOutput("AMOVAfileInfo"),
                          tags$style("#AMOVAfileInfo { font-size: 14px;}"),
                          tags$hr(),
                          tags$h5("STEP I: AMOVA"),
                          actionButton("runAMOVA", "Run AMOVA", class = "run-action-button"),
                          actionButton("resetAMOVA", "Reset"),
                          tags$hr(),
                          tags$h5("STEP II: Permutation Test"),
                          sliderInput("nperm", "Number of permutations", min = 10, max = 1000, value = 99, step = 1),
                          actionButton("runTest", "Run Permutation Test", class = "run-action-button"),
                          actionButton("resetTest", "Reset"),
                          width = 3),
                        mainPanel(
                          uiOutput("guide_AMOVA"),
                          div(id = "AMOVAStatus", style = "color: red; font-weight: bold;", "It may take a while..."),
                          tags$hr(),
                          fluidRow(
                            column(6,
                                   div(class = "title-text-style", textOutput("AMOVAtitle1")),
                                   plotOutput("AMOVAvarplot", width = "450px", height = "600px"),
                                   uiOutput("download_AMOVA_plot")
                            ),
                            column(6,
                                   div(class = "title-text-style", textOutput("AMOVAtitle2")),
                                   plotOutput("AMOVAtestplot", width = "450px", height = "600px"),
                                   uiOutput("download_AMOVA_test_plot")
                            )
                          ),
                          tags$hr(),
                          div(class = "title-text-style", textOutput("AMOVAtitle3")),
                          tableOutput("AMOVAresults"),
                          uiOutput("download_AMOVA_results"),
                          width = 9)
                      ))
           ))
}
#' @title Page_5_Genetic_Diversity_Server
#' @export
Page_5_Genetic_Diversity_Server = function(input, output, session) {
  ##### Page 5: Genetic Diversity #####
  ##### Diversity Parameters #####
  output$fileSelection_GD = renderUI({
    if (!is.null(df())){ choices = c("data.frame file" = "df") } else { choices = "" }
    selectInput("FileforGD", "Dataset for analysis:", choices)
  })
  
  output$GDfileInfo = renderText({
    req(df())
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
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
    pre_results[[30]] = "## Genetic Diversity"
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
    guide_GD("To analyze genetic diversity, the input data must be in ✅ data.frame format.\nYou also need to upload a ▶️ Site Info file (in RDS format).\nThe 'Group Info' CSV file from DAPC analysis is optional. \nPlease click the 'Analysis' button.")
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
      shinyjs::show("GDStatus")
      width = if (input$Type == "Statistics per site") 8 else 6
      pdf(file, width = width, height = 5.8)
      print(GDplot())
      dev.off()
      shinyjs::hide("GDStatus")
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
      shinyjs::show("GDStatus")
      saveRDS(site_stat(), file)
      shinyjs::hide("GDStatus")
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
      shinyjs::show("GDStatus")
      write.csv(group_stat(), file, row.names = TRUE)
      shinyjs::hide("GDStatus")
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
      shinyjs::show("GDStatus")
      write.csv(fst_matrix(), file, row.names = TRUE)
      shinyjs::hide("GDStatus")
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
      shinyjs::show("GDStatus")
      saveRDS(popgen(), file)
      shinyjs::hide("GDStatus")
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
        shinyjs::show("CircosStatus")
        write.csv(SW_data(), file, row.names = FALSE)
        shinyjs::hide("CircosStatus")
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
        shinyjs::show("CircosStatus")
        file.copy(pdf_path, file)
        shinyjs::hide("CircosStatus")
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
  output$fileSelection_GT = renderUI({
    if (!is.null(df())){ choices = c("data.frame file" = "df") } else { choices = "" }
    selectInput("FileforGT", "Dataset for genetic distance:", choices)
  })
  
  output$GTfileInfo = renderText({
    req(df())
    paste0("Type: ", class(df()), "\n",
           "Number of samples: ", dim(df())[1], "\n",
           "Number of SNPs: ", dim(df())[2])
  })
  
  output$groupfile5 = renderUI({
    fileInput("groupfile5", "Group Info. (required)", multiple = F, accept = c(".csv"))
  })
  
  observeEvent(input$groupfile5, {
    req(input$groupfile5)
    groupfile = read.csv(input$groupfile5$datapath)
    groupInfo4 = groupfile$Group
    groupInfo4(groupInfo4)
  })
  
  observeEvent(input$runGT, {
    req(input$FileforGT, df(), groupInfo4())
    shinyjs::show("GTStatus")
    GT_data = cbind(groupInfo4(), df())
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
    pre_results[[30]] = "## Genetic Diversity"
    pre_results[[37]] = paste0("### Genetic Distance (between pairs of groups)")
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
        shinyjs::show("GTStatus")
        pdf(file, width = 7, height = 7)
        print(GTplot())
        dev.off()
        shinyjs::hide("GTStatus")
      }
    )
    
    output$DGTresult = downloadHandler(
      filename = paste0("Genetic_Distance_Matrix-", input$GT_method, ".csv"),
      content = function(file) {
        shinyjs::show("GTStatus")
        write.csv(GTmatrix(), file, row.names = TRUE)
        shinyjs::hide("GTStatus")
      }
    )
  })
  
  observeEvent(input$resetGT, {
    GTmatrix(NULL)
    GTdf(NULL)
    showNotification("Data have been reset.")
    output$GTresults = renderTable({ NULL })
    groupInfo4(NULL)
    output$groupfile5 = renderUI({
      fileInput("groupfile5", "Group Info. (required)", multiple = F, accept = c(".csv"))
    })
    GTtitle1("")
    GTtitle2("")
    guide_GT("To run the genetic distance analysis, the input data must be in ✅ data.frame format.\nYou also need to upload a ▶️ Group Info. file.")
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
  output$fileSelection_AMOVA = renderUI({
    if (!is.null(gl())){ choices = c("genlight file" = "gl") } else { choices = "" }
    selectInput("FileforAMOVA", "Dataset for AMOVA:", choices)
  })
  
  output$AMOVAfileInfo = renderText({
    req(gl())
    group_info = ifelse(nPop(gl())>1, "Added", "NaN \n**Warning** Not available!")
    paste0("Type: ", class(gl()), "\n",
           "Number of samples: ", nInd(gl()), "\n",
           "Number of SNPs: ", nLoc(gl()), "\n",
           "Group Info.: ", group_info)
  })
  
  observeEvent(input$runAMOVA, {
    req(input$FileforAMOVA, gl()@pop)
    shinyjs::show("AMOVAStatus")
    AMOVA_data = switch(input$FileforAMOVA, "gl" = gl())
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
    pre_results[[30]] = "## Genetic Diversity"
    pre_results[[39]] = paste0("### Analysis of Molecular Variance (AMOVA)")
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
    guide_AMOVA("To run AMOVA, the input data must be in ✅ genlight file with 'Group Info.' \nYou can obtain this genlight file from the 'Data Transform' page ")
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
    guide_AMOVA("To run AMOVA, the input data must be a genlight file with 'Group Info.' \nYou can obtain this genlight file from the 'Data Transform' page after you have both the data.frame and Group Info. files.")
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
      shinyjs::show("AMOVAStatus")
      pdf(file, width = 6, height = 8)
      print(AMOVAvarplot())
      dev.off()
      shinyjs::hide("AMOVAStatus")
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
      shinyjs::show("AMOVAStatus")
      pdf(file, width = 6, height = 8)
      plot(amova.test())
      dev.off()
      shinyjs::hide("AMOVAStatus")
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
      shinyjs::show("AMOVAStatus")
      write.csv(AMOVA_res(), file, row.names = FALSE)
      shinyjs::hide("AMOVAStatus")
    }
  )
  
  output$guide_AMOVA = renderUI({ div(class = "guide-text-block", guide_AMOVA()) })
  output$AMOVAtitle1 = renderText({ AMOVAtitle1() })
  output$AMOVAtitle2 = renderText({ AMOVAtitle2() })
  output$AMOVAtitle3 = renderText({ AMOVAtitle3() })
  
}