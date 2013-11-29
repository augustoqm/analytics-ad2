###############################################################################
# SOURCE()
###############################################################################
source("../ad2_functions/general.R")
source("../ad2_functions/mp1.R")
source("../ad2_functions/mp2.R")

###############################################################################
# FUNCTIONS
###############################################################################
GetMPData <- function(mp.dir, mp.number){
  switch(mp.number, 
         "1" = {
           list(phase1_test_target = read.csv(paste(mp.dir, "/hidden_data/phase1_test_target.csv", sep = "")),
                phase2_test_target = read.csv(paste(mp.dir, "/hidden_data/phase2_test_target.csv", sep = "")))
         },
         "2" = {
           list(wine_labels = read.csv(paste(mp.dir, "/hidden_data/mp2_fase2_rotulos_dos_vinhos.csv", sep = "")))
         })
}

GetStudentMPData <- function(student.dir, mp.number){
  CheckAndReadCsv <- function(csv.file){
    if (file.exists(csv.file)) {
      read.csv(csv.file)
    }else{
      data.frame()
    }
  }
  
  switch(mp.number, 
         "1" = {
           list(phase1_models_sqe = CheckAndReadCsv(paste(student.dir, "/phase1_models_sqe.csv", sep = "")), 
                phase1_predictions = CheckAndReadCsv(paste(student.dir, "/phase1_predictions.csv", sep = "")), 
                phase2_models_sqe = CheckAndReadCsv(paste(student.dir, "/phase2_models_sqe.csv", sep = "")), 
                phase2_predictions = CheckAndReadCsv(paste(student.dir, "/phase2_predictions.csv", sep = "")))
         },
         "2" = {
           list(model_validation = CheckAndReadCsv(paste(student.dir, "/mp2_validacao_modelos.csv", sep = "")), 
                prediction = CheckAndReadCsv(paste(student.dir, "/mp2_predicao.csv", sep = "")))
         })
}

###############################################################################
# SHINY SERVER
###############################################################################

# All session variables and settings
all.mini.projects <- c("MP 1 - Regressão", "MP 2 - Classificação")
data.dir <- "data"
theme_set(theme_bw(base_size=15))

# shinyServer function
shinyServer(function(input, output) {
  
  # ===========================================================================
  # REACTIVE SOURCEs and CONDUCTORs
  # ===========================================================================
  # ---------------------------------------------------------------------------
  # SIMPLE FUNCTIONS (called when necessary)
  # ---------------------------------------------------------------------------
  # Update the phase
  phase <- function(){
    input$phases
  }
  
  # ---------------------------------------------------------------------------
  # REACTIVE FUNCTIONS (conductors: called when its parents change)
  # ---------------------------------------------------------------------------
  mp.number <- reactive({
    as.numeric(strsplit(input$mini_project, " ")[[1]][2])
  })
  
  group.dir.name <- reactive({
    gsub(" ", "_", gsub(" / ", "-", input$student_group))
  })
  
  mp.dir <- reactive({
    paste(data.dir, "/mini_project_", mp.number(), sep = "")
  })
  
  mp.data <- reactive({
    GetMPData(mp.dir(), mp.number())
  })
  
  student.mp.data <- reactive({
    student.group.dir <- paste(mp.dir(), "/student_data/", group.dir.name(), sep = "")
    GetStudentMPData(student.group.dir, mp.number())
  })
  
  
  # ===========================================================================
  # REACTIVE ENDPOINTS
  # ===========================================================================
  # ---------------------------------------------------------------------------
  # Render INPUT
  # ---------------------------------------------------------------------------
  
  # List of groups
  output$student_group_list <- renderUI({
    student.data.dir <- paste(data.dir, "/mini_project_", mp.number(), "/student_data", sep = "")
    
    all.group.dir.names <- list.files(student.data.dir)
    
    # Remove the 101_Boot_Student directoriy name
    groups.with.submission <- all.group.dir.names[-c(1)]
    
    # Define the group.names to the list
    group.names <- aaply(groups.with.submission, 1, function(group){
      gsub("-", " / ", gsub("_", " ", group))
    })
    group.names <- as.character(group.names)
    
    selectInput('student_group', label="", group.names, group.names[1])
  })
  
  # PDF Download button
  output$download_mp_pdf_button <- downloadHandler(
    filename = function() {
      paste(data.dir, "/mini_project_", mp.number(), "/", 
            all.mini.projects[mp.number()], ".pdf", sep ="")
    },
    content = function(file) {
      mp.file <- paste(data.dir, "/mini_project_", mp.number(), "/", 
                       all.mini.projects[mp.number()], ".pdf", sep ="")
      
      if (file.exists(mp.file)){
        file.copy(mp.file, file)
      }else{
        boot.analysis.file <- paste(data.dir, "/mini_project_default/MP 0 - Default.pdf", sep ="")
        file.copy(boot.analysis.file, file)    
      }
    },
    contentType = "application/pdf"
  )
  
  # ---------------------------------------------------------------------------
  # Render OUTPUT: Mini-Project 1 (Phase 1)
  # ---------------------------------------------------------------------------
  
  output$phase1_ic_plot <- renderPlot({
    tryCatch(expr={
      # Plot Confidence Interval
      print(Mp1GetCI(student.mp.data()$phase1_models_sqe))
      
    }, error=function(e) {
      cat("Error: Wrong data >>", group.dir.name(),"(MP1 - Phase1: SQE values)\n")
    })
  })
  
  output$phase1_scatter_plots <- renderPlot({
    tryCatch(expr={
      # Plot the scatter plot
      print(Mp1GetScatterPlot(mp.data()$phase1_test_target, 
                              student.mp.data()$phase1_prediction))
      
    }, error=function(e){
      cat("Error: Wrong data >>", group.dir.name(), "(MP1 - Phase1: Predicted values)\n")
    })
  })
  
  output$phase1_download_analysis <- downloadHandler(
    filename = function() {
      paste(data.dir, "/mini_project_", mp.number(), "/student_data/", 
            group.dir.name(), "/", phase(), "_analysis.pdf", sep ="")
    },
    content = function(file) {
      analysis.file <- paste(data.dir, "/mini_project_", mp.number(), "/student_data/", 
                             group.dir.name(), "/", phase(), "_analysis.pdf", sep ="")
      if (file.exists(analysis.file)){
        file.copy(analysis.file, file)
      }else{
        boot.analysis.file <- paste(data.dir, "/mini_project_", mp.number(), 
                                    "/student_data/101_Boot_Student/", 
                                    phase(), "_analysis.pdf", sep ="")
        file.copy(boot.analysis.file, file)    
      }
    },
    contentType = "application/pdf"
  )
  
  # ---------------------------------------------------------------------------
  # Render OUTPUT: Mini-Project 1 (Phase 2)
  # ---------------------------------------------------------------------------
  
  output$phase2_ic_plot <- renderPlot({
    tryCatch(expr={
      # Plot Confidence Interval
      print(Mp1GetCI(student.mp.data()$phase2_models_sqe))
      
    }, error=function(e) {
      cat("Error: Wrong data >>", group.dir.name(),"(MP1 - Phase2: SQE values)\n")
    })
  })
  
  output$phase2_scatter_plots <- renderPlot({
    tryCatch(expr={
      # Plot the scatter plot
      print(Mp1GetScatterPlot(mp.data()$phase2_test_target, 
                              student.mp.data()$phase2_prediction))
      
    }, error=function(e){
      cat("Error: Wrong data >>", group.dir.name(),"(MP1 - Phase2: Predicted values)\n")
    })
  })
  
  output$phase2_download_analysis <- downloadHandler(
    filename = function() {
      paste(data.dir, "/mini_project_", mp.number(), "/student_data/", 
            group.dir.name(), "/", phase(), "_analysis.pdf", sep ="")
    },
    content = function(file) {
      analysis.file <- paste(data.dir, "/mini_project_", mp.number(), "/student_data/", 
                             group.dir.name(), "/", phase(), "_analysis.pdf", sep ="")
      if (file.exists(analysis.file)){
        file.copy(analysis.file, file)
      }else{
        boot.analysis.file <- paste(data.dir, "/mini_project_", mp.number(), 
                                    "/student_data/101_Boot_Student/", 
                                    phase(), "_analysis.pdf", sep ="")
        file.copy(boot.analysis.file, file)     
      }
    },
    contentType = "application/pdf"
  )
  
  # ---------------------------------------------------------------------------
  # Render OUTPUT: Mini-Project 2
  # ---------------------------------------------------------------------------
  
  output$mp2_download_analysis <- downloadHandler(
    filename = function() {
      paste(data.dir, "/mini_project_2/student_data/", 
            group.dir.name(), "/mp2_analise.pdf", sep ="")
    },
    content = function(file) {
      analysis.file <- paste(data.dir, "/mini_project_2/student_data/", 
                             group.dir.name(), "/mp2_analise.pdf", sep ="")
      
      if (file.exists(analysis.file)){
        file.copy(analysis.file, file)
      }else{
        boot.analysis.file <- paste(data.dir, 
                                    "/mini_project_2/student_data/101_Boot_Student/mp2_analise.pdf", sep ="")
        file.copy(boot.analysis.file, file)     
      }
    },
    contentType = "application/pdf"
  )
  
  output$mp2_ic_models_plot <- renderPlot({
    tryCatch(expr={
      # Plot Confidence Interval
      print(Mp2GetCIModels(student.mp.data()$model_validation))
      
    }, error=function(e) {
      cat("Error: Wrong data >>", group.dir.name(), "(MP2: Model Validation, CI)\n")
    })
  })
  
  output$mp2_confusion_matrix_bin <- renderPlot({
    tryCatch(expr={
      # Plot the Confusion Matrix
      print(Mp2GetConfMatrixBinary(student.mp.data()$prediction, mp.data()$wine_labels))
      
    }, error=function(e){
      cat("Error: Wrong data >>", group.dir.name(), "(MP2: Prediction, Binary Confusion Matrix)\n")
    })
  })
  
  output$mp2_confusion_matrix_mul <- renderPlot({
    tryCatch(expr={
      # Plot the Confusion Matrix
      print(Mp2GetConfMatrixMultiple(student.mp.data()$prediction, mp.data()$wine_labels))
      
    }, error=function(e){
      cat("Error: Wrong data >>", group.dir.name(), "(MP2: Prediction, Multiple Confusion Matrix)\n")
    })
  })
  
})