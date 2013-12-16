###############################################################################
# SOURCE()
###############################################################################
source("../ad2_functions/general.R")

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
         },
         "3" = {
           list(book_ratings_test = read.csv2(paste(mp.dir, "/hidden_data/mp3_book_crossing_teste_com_notas.csv", sep = "")))
         })
}

GetStudentMPData <- function(student.dir, mp.number){
  CheckAndReadCSV <- function(csv.file){
    if (file.exists(csv.file)) {
      read.csv(csv.file)
    }else{
      data.frame()
    }
  }
  
  switch(mp.number, 
         "1" = {
           list(phase1_models_sqe = CheckAndReadCSV(paste(student.dir, "/phase1_models_sqe.csv", sep = "")), 
                phase1_predictions = CheckAndReadCSV(paste(student.dir, "/phase1_predictions.csv", sep = "")), 
                phase2_models_sqe = CheckAndReadCSV(paste(student.dir, "/phase2_models_sqe.csv", sep = "")), 
                phase2_predictions = CheckAndReadCSV(paste(student.dir, "/phase2_predictions.csv", sep = "")))
         },
         "2" = {
           list(model_validation = CheckAndReadCSV(paste(student.dir, "/mp2_validacao_modelos.csv", sep = "")), 
                prediction = CheckAndReadCSV(paste(student.dir, "/mp2_predicao.csv", sep = "")))
         },
         "3" = {
           list(model_validation = CheckAndReadCSV(paste(student.dir, "/mp3_validacao_modelos_treino.csv", sep = ""))) 
           # prediction = CheckAndReadCSV(paste(student.dir, "/mp3_predicao_teste.csv", sep = "")))
         })
}

###############################################################################
# SHINY SERVER
###############################################################################

# All session variables and settings
all.mini.projects <- c("MP 1 - Regressão", "MP 2 - Classificação", "MP 3 - Recomendação")
data.dir <- "data"

theme_set(theme_bw(base_size=15))

# shinyServer function
shinyServer(function(input, output) {
   
  # ===========================================================================
  # REACTIVE SOURCEs and CONDUCTORs
  # ===========================================================================
  # ---------------------------------------------------------------------------
  # SESSION OBJECTS
  # ---------------------------------------------------------------------------
  session.objects <- list('mp1' = list(),
                          'mp2' = list(),
                          'mp3' = list(all_students_rmse_predictions = NULL))

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
  output$mini_project_tags <- renderUI({
    
    student.data.dir <- paste(data.dir, "/mini_project_", mp.number(), "/student_data", sep = "")
    
    all.group.dir.names <- list.files(student.data.dir)
    
    # Remove the 101_Boot_Student directoriy name
    groups.with.submission <- all.group.dir.names[-c(1)]
    
    # Define the group.names to the list
    group.names <- aaply(groups.with.submission, 1, function(group){
      gsub("-", " / ", gsub("_", " ", group))
    })
    group.names <- as.character(group.names)
    
    tags <- list(p(strong("Grupo"), em("(com submissão)")),
                 selectInput('student_group', label="", group.names, group.names[1]))
    
    if (mp.number() == "3"){
      # MP3 - Add the Prediction Comparison checkbox
      tags <- c(tags, 
                list(br(), br(), p(strong("Todos"), em("(com submissão)")),
                     checkboxInput("mp3_compare_predictions", "Comparar predições", F)))
    }
    return(tags)
  })
  
  # PDF Download button
  output$download_mp_pdf_button <- downloadHandler(
    filename = function() {
      paste(all.mini.projects[mp.number()], ".pdf", sep ="")
    },
    content = function(file) {
      mp.file <- paste(all.mini.projects[mp.number()], ".pdf", sep ="")
      
      if (file.exists(mp.file)){
        file.copy(mp.file, file)
      }else{
        boot.analysis.file <- paste("MP 0 - Default.pdf", sep ="")
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
  
  # ---------------------------------------------------------------------------
  # Render OUTPUT: Mini-Project 2
  # ---------------------------------------------------------------------------
  
  output$mp3_download_analysis <- downloadHandler(
    filename = function() {
      paste(data.dir, "/mini_project_3/student_data/", 
            group.dir.name(), "/mp3_analise.pdf", sep ="")
    },
    content = function(file) {
      analysis.file <- paste(data.dir, "/mini_project_3/student_data/", 
                             group.dir.name(), "/mp3_analise.pdf", sep ="")
      
      if (file.exists(analysis.file)){
        file.copy(analysis.file, file)
      }else{
        boot.analysis.file <- paste(data.dir, 
                                    "/mini_project_3/student_data/101_Boot_Student/mp3_analise.pdf", sep ="")
        file.copy(boot.analysis.file, file)     
      }
    },
    contentType = "application/pdf"
  )
  
  output$mp3_ic_models_validation_plot <- renderPlot({
    tryCatch(expr={
      print(MP3GetCIModels(student.mp.data()$model_validation))
    }, error=function(e) {
      cat("Error: Wrong data >>", group.dir.name(), "(MP3: Model Validation, CI)\n")
    })
  })
  
  output$mp3_ic_models_prediction_plot <- renderPlot({
    tryCatch(expr={
      if (mp.number() == 3){
        # Check the session object
        if (is.null(session.objects$mp3$all_students_rmse_predictions)){
          # Compare the RMSE value of the predictions between all the students 
          # Get the prediction from all students and bind them in a single 
          # model_validation data.frame (model_name, user, rmse)
          student.data.dir <- paste(data.dir, "/mini_project_", mp.number(), "/student_data", sep = "")
          
          # Remove the 101_Boot_Student directory name
          all.group.dirs <- list.files(student.data.dir)[-c(1)]
          
          all.students.rmse.per.user <- NULL
          for(group in all.group.dirs){
            
            student.prediction <- read.csv(paste(paste(student.data.dir, "/", group, sep = ""),
                                                 "/mp3_predicao_teste.csv", sep = ""))
            rmse.per.user <- mp.data()$book_ratings_test[,-2]
            rmse.per.user$Book.Rating <- sqrt((rmse.per.user$Book.Rating - student.prediction)^2)
            rmse.per.user <- ddply(rmse.per.user, .(User.ID),
                                   function(df) mean(df$Book.Rating, na.rm=T),
                                   .progress="text")
            colnames(rmse.per.user)[2] <- "rmse"
            rmse.per.user$nome_modelo <- rep(group, nrow(rmse.per.user))
            
            all.students.rmse.per.user <- rbind(all.students.rmse.per.user, 
                                                rmse.per.user)
          }
          
          # Update the session object
          session.objects$mp3$all_students_rmse_predictions <<- all.students.rmse.per.user
        }
        print(MP3GetCIModels(session.objects$mp3$all_students_rmse_predictions))
      }
    }, error=function(e) {        
      print(e)
      cat("Error: Wrong data >> All Students (MP3: Model Prediction, CI)\n")
    })
  })
  
})