library(shiny)
library(googleVis)
library(ggpubr)
library(openxlsx)
library(table1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  titlePanel("OV-Strat: TCGA-OV Cohort Selection and Analysis"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 tableOutput("cohort1_tab_summary"),
                 actionButton("reset1", "Clear Cohort 1"), 
                 tableOutput("cohort2_tab_summary"),
                 actionButton("reset2", "Clear Cohort 2"), 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Select a Cohort", 
                 h1(strong("Generate 1-2 Cohorts"), style = "font-size:20px;"),
                 
                 # Filter criteria: residual tumor, adjuvant number min/max, PFI length min/max, standard
                 selectInput('residual','Residual Tumor after Treatment',choices = c("ResidTumor", "NoResidTumor", "All Patients")), 
                 numericInput('adjuvant_min', "Min number of adjuvant treatments", value = 0, min = 0), 
                 numericInput('adjuvant_max', "Max number of adjuvant treatments", value = 5, min = 0),
                 numericInput('PFI_min', "Min PFI", value = 0),
                 numericInput('PFI_max', "Max PFI", value = 1000),
                 #selectInput('standard', "Filter for standard treatment regimen", choices = c("Standard", "Non-Standard", "All Patients")),
                 
                 checkboxGroupInput("standard_group", "Patients with Standard Treatment Regimen",
                                    choices=c("Platinum-Refractory Patients" = "refractory",
                                      "Platinum-Resistant Patients" = "resistant",
                                      "Partially Platinum-Sensitive Patients" = "partially sensitive",
                                      "Fully Platinum-Sensitive Patients" = "fully sensitive",
                                      "Other Standard Patients" = "other"),
                                    selected=c("Platinum-Refractory Patients" = "refractory",
                                      "Platinum-Resistant Patients" = "resistant",
                                      "Partially Platinum-Sensitive Patients" = "partially sensitive",
                                      "Fully Platinum-Sensitive Patients" = "fully sensitive")),
                 checkboxGroupInput("nonstandard_group", "Patients with Non-Standard Treatment Regimen",
                                    c("All Non-Standard Patients" = "nonstandard")),
                 
                 #Action buttons to generate the cohort based on filter criteria above
                 actionButton("create_cohort1", "Generate Cohort 1", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 actionButton("create_cohort2", "Generate Cohort 2", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 
        ),
        tabPanel("View and Download", 
                 selectInput('tab2_cohort', "Select a Cohort", choices = c("Cohort 1", "Cohort 2")),
                 
                 h1(strong("Summary of Cohort Treatment Annotation"), style = "font-size:20px;"),
                 tableOutput("CohortAnnotation"), #TODO: create summary of annotation for selected cohort
                 #TODO: add error message if selected cohort is empty
                 
                 div(style="display: inline-block; vertical-align:bottom; width: 300px;",
                     h1(strong("Cohort Pharmaceutical Data"), style = "font-size:20px;")),
                 div(style="display: inline-block;vertical-align:bottom; width: 150px;", 
                     downloadButton('pharm_download', "Download", class = NULL, icon = shiny::icon("download"), 
                                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                 tableOutput("CohortPharmaceutical"),
                 #TODO: add error message if selected cohort is empty
                 
                 div(style="display: inline-block; vertical-align:bottom; width: 300px;",
                     h1(strong("Cohort Clinical Data"), style = "font-size:20px;")),
                 div(style="display: inline-block;vertical-align:bottom; width: 150px;", 
                     downloadButton('clinical_download', "Download", class = NULL, icon = shiny::icon("download"), 
                                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                 tableOutput("CohortClinical")
                 #TODO: add error message if selected cohort is empty
        ),
        tabPanel("Cohort Summaries",
                 fluidRow(
                   column(width = 6,
                          h1(strong("Cohort 1 Summary"), style = "font-size:20px;"),
                          tableOutput("cohort1_summary")),
                   column(width = 6,
                          h1(strong("Cohort 2 Summary"), style = "font-size:20px;"),
                          tableOutput("cohort2_summary"))
                 )
        ),
        tabPanel("Treatment Timelines",
                 h1(strong("Cohort Treatment Timelines"), style = "font-size:20px;"),
                 selectInput('tab4_cohort', "Select a Cohort", choices = c("Cohort 1", "Cohort 2")),
                 h3("Abbreviations:", style = "font-size:20px;"),
                 p("ATI: Adjuvant Treatment Interval"),
                 p("NATI: Non-Adjuvant Treatment Interval"),
                 p("PFI: Platinum Treatment Free Interval"),
                 p("NTE: New Tumor Event"),
                 #downloadButton('timeline_download', "Download pdf", class = NULL, 
                 #              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 htmlOutput("timeline")
        )
      )
    )
  )
)

server <- function(input, output,session) {
  ### TAB 1: GENERATE COHORTS ------------------------------------------------
  pharmaceutical <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRo7IGg7PeUv4krYMCtFMbiVAt7LlqRvtJJAGdcon0CD_73ARRJPxEgLRPgXSjT2J9Ki96emT1yJQwy/pub?gid=1330993528&single=true&output=csv", header=TRUE, stringsAsFactors = FALSE)
  #pharmaceutical <- read.csv("pharmaceutical_shiny.csv", header=TRUE, stringsAsFactors = FALSE) #n = 530
  #pharmaceutical = read.xlsx("https://dennismontoyacom.files.wordpress.com/2021/11/pharm-5.xlsx",sheet=1)
  clinical=read.xlsx("https://dennismontoyacom.files.wordpress.com/2021/12/clinical_new_calculation.xlsx",sheet=1)
  pharm1 <- pharmaceutical
  pharm2 <- pharmaceutical
  
  #Creating an object for storing "reactive values"  this will hold the cohort barcode_ids
  User_defined <- reactiveValues(data = NULL)
  User_filters <- reactiveValues(data = NULL) #hold filters applied by user
  #pharm1 <- reactiveValues(data = NULL)
  #pharm2 <- reactiveValues(data = NULL) #hold filtered data
  
  #Observe "Generate Cohort"
  observeEvent(input$create_cohort1,{
    #Store user_defined cohort 1 filters
    User_filters$residual1 <<- input$residual
    User_filters$adjuvant_min1 <<- input$adjuvant_min
    User_filters$adjuvant_max1 <<- input$adjuvant_max
    User_filters$PFI_min1 <<- input$PFI_min
    User_filters$PFI_max1 <<- input$PFI_max
    #User_filters$standard1 <<- input$standard
    User_filters$standard1 <<- input$standard_group
    User_filters$non_standard1 <<- input$nonstandard_group
    
    #Subset standard/nonstandard selections from pharm
    pharm1 <<- data.frame(matrix(ncol = ncol(pharmaceutical), nrow = 0))
    groups <- input$standard_group
    nsgroups<- input$nonstandard_group
    if (length(groups) > 0) {
      for (i in 1:length(groups)) {
        name <- ''
        if (groups[i] == "refractory") {
          name <- "Refractory"
        } else if (groups[i] == "resistant") {
          name <- "Resistant"
        } else if (groups[i] == "partially sensitive") {
          name <- "Partially Sensitive"
        } else if (groups[i] == "fully sensitive") {
          name <- "Fully Sensitive"
        } else if (groups[i] == "other") {
          name <- "Cannot Categorize"
        }
        pharm1 <<- rbind.data.frame(pharm1, pharmaceutical[which((pharmaceutical$response_type == name) & (pharmaceutical$adjuvant_type == "Standard Adjuvant")),])
      }
    }
    if (length(nsgroups != 0)) {
      pharm1 <<- rbind.data.frame(pharm1, pharmaceutical[which((pharmaceutical$adjuvant_type == "Non Standard Adjuvant") | (pharmaceutical$adjuvant_type == "No Adjuvant Treatments")),])
    }
    
    #Further filter df
    #pharm1 <<- pharmaceutical
    if (input$residual == "All Patients") {
    } else {
      pharm1 <<- pharm1[which(pharm1$residual_disease == input$residual),]
    }
    pharm1 <<- pharm1[which(pharm1$NumberTreatments >= input$adjuvant_min),]
    pharm1 <<- pharm1[which(pharm1$NumberTreatments <= input$adjuvant_max),]
    pharm1 <<- pharm1[which(pharm1$PFI >= input$PFI_min),]
    pharm1 <<- pharm1[which(pharm1$PFI <= input$PFI_max),]
    
    #if (input$standard == "All Patients") {
    #} else if (input$standard == "Standard") {
    #  pharm1 <<- pharm1[which(pharm1$adjuvant_type == "Standard Adjuvant"),]
    #} else if (input$standard == "Non Standard") {
    #  pharm1 <<- pharm1[which(pharm1$adjuvant_type != "Standard Adjuvant"),]
    #}
    
    #Store barcodes from cohort 1
    cohort1_barcodes <<- unique(pharm1$bcr_patient_barcode)
    User_defined$cohort1 <<- cohort1_barcodes
    #Redefine cohort1 df to have ALL rows 
    #(some progression events eliminated for not having adjuvant_type)
    data1 <<- data.frame(matrix(ncol = ncol(pharmaceutical), nrow = 0))
    for (i in 1:length(cohort1_barcodes)) {
      data1 <<- rbind.data.frame(data1, pharmaceutical[pharmaceutical$bcr_patient_barcode == cohort1_barcodes[i],])
    }
    pharm1 <<- data1
    
  })
  observeEvent(input$create_cohort2,{
    #Store user_defined cohort 2 filters
    User_filters$residual2 <<- input$residual
    User_filters$adjuvant_min2 <<- input$adjuvant_min
    User_filters$adjuvant_max2 <<- input$adjuvant_max
    User_filters$PFI_min2 <<- input$PFI_min
    User_filters$PFI_max2 <<- input$PFI_max
    #User_filters$standard2 <<- input$standard
    User_filters$standard2 <<- input$standard_group
    User_filters$non_standard2 <<- input$nonstandard_group
    
    #Subset standard/nonstandard selections from pharm
    pharm2 <<- data.frame(matrix(ncol = ncol(pharmaceutical), nrow = 0))
    groups <- input$standard_group
    nsgroups<- input$nonstandard_group
    if (length(groups) > 0) {
      for (i in 1:length(groups)) {
        name <- ''
        if (groups[i] == "refractory") {
          name <- "Refractory"
        } else if (groups[i] == "resistant") {
          name <- "Resistant"
        } else if (groups[i] == "partially sensitive") {
          name <- "Partially Sensitive"
        } else if (groups[i] == "fully sensitive") {
          name <- "Fully Sensitive"
        } else if (groups[i] == "other") {
          name <- "Cannot Categorize"
        }
        pharm2 <<- rbind.data.frame(pharm2, pharmaceutical[which((pharmaceutical$response_type == name) & (pharmaceutical$adjuvant_type == "Standard Adjuvant")),])
      }
    }
    if (length(nsgroups != 0)) {
      pharm2 <<- rbind.data.frame(pharm2, pharmaceutical[which((pharmaceutical$adjuvant_type == "Non Standard Adjuvant") | (pharmaceutical$adjuvant_type == "No Adjuvant Treatments")),])
    }
    
    #Further filter df
    if (input$residual == "All Patients") {
    } else {
      pharm2 <<- pharm2[which(pharm2$residual_disease == input$residual),]
    }
    pharm2 <<- pharm2[which(pharm2$NumberTreatments >= input$adjuvant_min),]
    pharm2 <<- pharm2[which(pharm2$NumberTreatments <= input$adjuvant_max),]
    pharm2 <<- pharm2[which(pharm2$PFI >= input$PFI_min),]
    pharm2 <<- pharm2[which(pharm2$PFI <= input$PFI_max),]
    #if (input$standard == "All Patients") {
    #} else if (input$standard == "Standard") {
    #  pharm2 <<- pharm2[which(pharm2$adjuvant_type == "Standard Adjuvant"),]
    #} else if (input$standard == "Non-Standard") {
    #  pharm2 <<- pharm2[which(pharm2$adjuvant_type != "Standard Adjuvant"),]
    #}
    #Store barcodes from cohort 2
    cohort2_barcodes <<- unique(pharm2$bcr_patient_barcode)
    User_defined$cohort2 <<- cohort2_barcodes
    #Redefine cohort2 df to have ALL rows 
    #(some progression events eliminated for not having adjuvant_type)
    data2 <<- data.frame(matrix(ncol = ncol(pharmaceutical), nrow = 0))
    for (i in 1:length(cohort2_barcodes)) {
      data2 <<- rbind.data.frame(data2, pharmaceutical[pharmaceutical$bcr_patient_barcode == cohort2_barcodes[i],])
    }
    pharm2 <<- data2
  })
  
  #Observe"Clear Cohorts"
  observeEvent(input$reset1, {
    User_defined$cohort1 <- NULL
    User_filters$residual1 <- NULL
    User_filters$adjuvant_min1 <- NULL
    User_filters$adjuvant_max1 <- NULL
    User_filters$PFI_min1 <- NULL
    User_filters$PFI_max1 <- NULL
    User_filters$standard1 <- NULL
    User_filters$non_standard1 <- NULL
  })  
  observeEvent(input$reset2, {
    User_defined$cohort2 <- NULL
    User_filters$residual2 <- NULL
    User_filters$adjuvant_min2 <- NULL
    User_filters$adjuvant_max2 <- NULL
    User_filters$PFI_min2 <- NULL
    User_filters$PFI_max2 <- NULL
    User_filters$standard2 <- NULL
    User_filters$non_standard2 <- NULL
  })  
  
  ##Outputs the Cohort name and number patients(viewed in sidepanel), lets user know cohort generated
  output$cohort1_tab_summary <- renderTable({
    if (is.null(User_defined$cohort1)) return()
    data.frame("Property" = c("Number Patients", "Tumor Status", "PFI Length", "Adjuvant Number", "Standard Patients", "Non Standard Patients"),
               "Value" = c(length(User_defined$cohort1), 
                           User_filters$residual1, 
                           paste(User_filters$PFI_min1, '-', User_filters$PFI_max1), 
                           paste(User_filters$adjuvant_min1, '-', User_filters$adjuvant_max1), 
                           paste(User_filters$standard1, collapse=", "), 
                           paste(User_filters$non_standard1, collapse=",")))
  })
  output$cohort2_tab_summary <- renderTable({
    if (is.null(User_defined$cohort2)) return()
    data.frame("Property" = c("Number Patients", "Tumor Status", "PFI Length", "Adjuvant Number", "Standard Patients", "Non Standard Patients"),
               "Value" = c(length(User_defined$cohort2), 
                           User_filters$residual2, 
                           paste(User_filters$PFI_min2, '-', User_filters$PFI_max2), 
                           paste(User_filters$adjuvant_min2, '-', User_filters$adjuvant_max2), 
                           paste(User_filters$standard2, collapse=", "), 
                           paste(User_filters$non_standard2, collapse=",")))
  })
  
  ### TAB 2: VIEW AND DOWNLOAD CLINICAL AND PHARM DATA -----------------------
  
  output$CohortAnnotation <- renderTable({
    #Create df for cohort pharmaceutical data
    pharm_output <- data.frame(matrix(nrow = 0, ncol = ncol(pharmaceutical)))
    if(input$tab2_cohort == "Cohort 1") {
      if(is.null(User_defined$cohort1)) return()
      pharm_output <- pharm1
    } else if (input$tab2_cohort == "Cohort 2") {
      if (is.null(User_defined$cohort2)) return()
      pharm_output <- pharm2
    }
    
    #Create df for summary of cohort treatment annotation
    treatment_counts <- data.frame(matrix(nrow = 1, ncol = 20))
    colnames(treatment_counts) <- c("Total_Num_Patients", "Total_Num_Treatments", 
                                    "Num_Adj_Treatments", "Num_Rec_Treatments", "Num_Prog_Treatments", 
                                    "Num_Pal_Treatments", "Num_Other_Treatments", "Num_Blank_NA_Treatments",
                                    
                                    "Unique_Adj_Patients", "Unique_Rec_Patients", "Unique_Prog_Patients",
                                    "Unique_Pal_Patients", "Unique_Other_Patients", "Unique_Blank_NA_Patients",
                                    
                                    "Adj_Per_Patient", "Rec_Per_Patient", "Prog_Per_Patient", 
                                    "Pal_Per_Patient", "Other_Per_Patient", "Blank_NA_Per_Patient")
    #Patients and treatments per cohort
    treatment_counts$Total_Num_Patients <- length(unique(pharm_output$bcr_patient_barcode))
    treatment_counts$Total_Num_Treatments <- length(pharm_output$bcr_patient_barcode)
    
    #Treatments per regimen indication
    treatment_counts$Num_Adj_Treatments <- length(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "ADJUVANT")])
    treatment_counts$Num_Rec_Treatments <- length(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "RECURRENCE")])
    treatment_counts$Num_Prog_Treatments <- length(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "PROGRESSION")])
    treatment_counts$Num_Pal_Treatments <- length(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "PALLIATIVE")])
    treatment_counts$Num_Other_Treatments <- length(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "OTHER, SPECIFY IN NOTES")])
    treatment_counts$Num_Blank_NA_Treatments <- length(pharm_output$bcr_patient_barcode[which(is.na(pharm_output$regimen_indication))])
    
    #Patients per regimen indication
    treatment_counts$Unique_Adj_Patients <- length(unique(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "ADJUVANT")]))
    treatment_counts$Unique_Rec_Patients <- length(unique(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "RECURRENCE")]))
    treatment_counts$Unique_Prog_Patients <- length(unique(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "PROGRESSION")]))
    treatment_counts$Unique_Pal_Patients <- length(unique(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "PALLIATIVE")]))
    treatment_counts$Unique_Other_Patients <- length(unique(pharm_output$bcr_patient_barcode[which(pharm_output$regimen_indication == "OTHER, SPECIFY IN NOTES")]))
    treatment_counts$Unique_Blank_NA_Patients <- length(unique(pharm_output$bcr_patient_barcode[which(is.na(pharm_output$regimen_indication))]))
    
    #Treatments per regimen indication per patient
    treatment_counts$Adj_Per_Patient <- treatment_counts$Num_Adj_Treatments[1] / treatment_counts$Total_Num_Patients[1]
    treatment_counts$Rec_Per_Patient <- treatment_counts$Num_Rec_Treatments[1] / treatment_counts$Total_Num_Patients[1]
    treatment_counts$Prog_Per_Patient <- treatment_counts$Num_Prog_Treatments[1] / treatment_counts$Total_Num_Patients[1]
    treatment_counts$Pal_Per_Patient <- treatment_counts$Num_Pal_Treatments[1] / treatment_counts$Total_Num_Patients[1]
    treatment_counts$Other_Per_Patient <- treatment_counts$Num_Other_Treatments[1] / treatment_counts$Total_Num_Patients[1]
    treatment_counts$Blank_NA_Per_Patient <- treatment_counts$Num_Blank_NA_Treatments[1] / treatment_counts$Total_Num_Patients[1]
    
    head(treatment_counts)
  })
  
  output$pharm_download <- downloadHandler(
    filename = "Cohort_Pharmaceutical_Data.csv",
    content = function(file) {
      #Create df for pharmaceutical data bind
      if(input$tab2_cohort == "Cohort 1") {
        if(is.null(User_defined$cohort1)) return()
        pharm_output <- pharm1
      } else if (input$tab2_cohort == "Cohort 2") {
        if (is.null(User_defined$cohort2)) return()
        pharm_output <- pharm2
      }
      write.csv(pharm_output, file)
    }
  )
  
  output$CohortPharmaceutical <- renderTable({
    #Create df for pharmaceutical data bind
    if(input$tab2_cohort == "Cohort 1") {
      if(is.null(User_defined$cohort1)) return()
      pharm_output <- pharm1
    } else if (input$tab2_cohort == "Cohort 2") {
      if (is.null(User_defined$cohort2)) return()
      pharm_output <- pharm2
    }
    head(pharm_output)
  })
  
  output$clinical_download <- downloadHandler(
    filename <- "Cohort_Clinical_Data.csv",
    content = function(file) {
      #Create df for clinical data bind
      clinical_output <- data.frame(matrix(nrow = 0, ncol = ncol(clinical)))
      if(input$tab2_cohort == "Cohort 1") {
        if(is.null(User_defined$cohort1)) return()
        for (i in 1:length(User_defined$cohort1)) {
          clinical_output <- rbind.data.frame(clinical_output, clinical[which(clinical$bcr_patient_barcode == User_defined$cohort1[i]),])
        }
      } else if (input$tab2_cohort == "Cohort 2") {
        if (is.null(User_defined$cohort2)) return()
        for (i in 1:length(User_defined$cohort2)) {
          clinical_output <- rbind.data.frame(clinical_output, clinical[which(clinical$bcr_patient_barcode == User_defined$cohort2[i]),])
        }
      }
      write.csv(clinical_output, file)
    }
  )
  
  output$CohortClinical <- renderTable({
    #Create df for clinical data bind
    clinical_output <- data.frame(matrix(nrow = 0, ncol = ncol(clinical)))
    if(input$tab2_cohort == "Cohort 1") {
      if(is.null(User_defined$cohort1)) return()
      for (i in 1:length(User_defined$cohort1)) {
        clinical_output <- rbind.data.frame(clinical_output, clinical[which(clinical$bcr_patient_barcode == User_defined$cohort1[i]),])
      }
    } else if (input$tab2_cohort == "Cohort 2") {
      if (is.null(User_defined$cohort2)) return()
      for (i in 1:length(User_defined$cohort2)) {
        clinical_output <- rbind.data.frame(clinical_output, clinical[which(clinical$bcr_patient_barcode == User_defined$cohort2[i]),])
      }
    }
    head(clinical_output)
  })
  
  ### TAB 3: OUTPUT COHORT SUMMARIES -----------------------------------------
  output$cohort1_summary <- renderTable({
    if (is.null(User_defined$cohort1)) return()
    pharm1_single_rows <- data.frame(matrix(nrow = 0, ncol = ncol(pharm1)))
    colnames(pharm1_single_rows) <- colnames(pharm1)
    for (i in 1:length(unique(pharm1$bcr_patient_barcode))) {
      single_patient <- pharm1[pharm1$bcr_patient_barcode == unique(pharm1$bcr_patient_barcode)[i],] #isolate single patient's data
      single_row <- single_patient[1,] #isolate first row only
      pharm1_single_rows <- rbind.data.frame(pharm1_single_rows, single_row) # bind first row only of each patient's data to count once in summary
    }
    table1(~ age + PFI + stage + race + days_to_death + days_to_last_follow_up + tumor_residual_disease, data = pharm1_single_rows, overall="Total")
  })
  
  output$cohort2_summary <- renderTable({
    if (is.null(User_defined$cohort2)) return()
    pharm2_single_rows <- data.frame(matrix(nrow = 0, ncol = ncol(pharm2)))
    colnames(pharm2_single_rows) <- colnames(pharm2)
    for (i in 1:length(unique(pharm2$bcr_patient_barcode))) {
      single_patient <- pharm2[pharm2$bcr_patient_barcode == unique(pharm2$bcr_patient_barcode)[i],] #isolate single patient's data
      single_row <- single_patient[1,] #isolate first row only
      pharm2_single_rows <- rbind.data.frame(pharm2_single_rows, single_row) # bind first row only of each patient's data to count once in summary
    }
    table1(~ age + PFI + stage + race + days_to_death + days_to_last_follow_up + tumor_residual_disease, data = pharm2_single_rows, overall="Total")
  })
  
  ### TAB 4: OUTPUT TIMELINES ------------------------------------------------
  
  output$timeline_download <- downloadHandler(
    filename <- "Cohort_Timelines.pdf",
    content = function(file) {
      ggsave(file, plot = plotI)
    }
  )
  
  output$timeline = renderGvis({ #Output patient timelines from OV sub-cohorts
    if(input$tab4_cohort == "Cohort 1") {
      if (is.null(User_defined$cohort1)) return()
      data <- pharm1
    } else if (input$tab4_cohort == "Cohort 2") {
      if (is.null(User_defined$cohort2)) return()
      data <- pharm2
    }
    data <- data[c('bcr_patient_barcode','drugname_corrected','days_to_drug_therapy_start',
                   'days_to_drug_therapy_end','stage', 'PFI', 'response_type', 'primary_therapy_outcome_success.y')]
    colnames(data) <- c("ID", "interval", "start", "end", "stage", "PFI", "response_type", "outcome")
    data <- data[which(!is.na(data$start)),]
    data <- data[which(!is.na(data$end)),]
    data <- data[which(!is.na(data$PFI)),]
    data <- data[which(!is.na(data$response_type)),]
    data <- data[which(data$PFI != "Cannot Calculate"),]
    data$start <- as.numeric(data$start)
    data$end <- as.numeric(data$end)
    data$PFI <- as.numeric(data$PFI)
    
    #Add primary response annotation
    for (i in 1:nrow(data)) {
      data$ID[i] <- paste(data$ID[i], data$outcome[i], sep = " | ")
      data$ID[i] <- paste(data$ID[i], data$stage[i], sep = " | ")
    }
    for (i in 1:nrow(data)) {
      if (isTRUE(data$start[i] > data$end[i])) {
        start_date <- data$end[i]
        data$end[i] <- data$start[i]
        data$start[i] <- start_date
      }
    }
    data$date1 <- ''
    data$date2 <- ''
    for (i in 1:nrow(data)) {
      x <- as.Date(as.numeric(data$start[i]), origin = "1900-01-01")
      y <- as.Date(as.numeric(data$end[i]), origin = "1900-01-01")
      data$date1[i] <- as.character(x)
      data$date2[i] <- as.character(y)
    }
    data$date1 <- as.Date(data$date1)
    data$date2 <- as.Date(data$date2)
    data <- data[-c(3, 4, 5, 6, 7)]
    #Create timeline
    gvisTimeline(data,
                 barlabel = "interval", 
                 rowlabel = "ID",
                 options = list(width=3000, height=1080),
                 start = "date1", 
                 end = "date2",
                 chartid = "Timelines")
  })
  
}

shinyApp(ui = ui, server = server)

