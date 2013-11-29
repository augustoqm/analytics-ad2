###############################################################################
# SOURCE()
###############################################################################
source("../ad2_functions/general.R")
source("../ad2_functions/mp1.R")
source("../ad2_functions/mp2.R")

###############################################################################
# SHINY SERVER
###############################################################################

# All session variables and settings
all.mini.projects <- c("MP 1 - Regressão", "MP 2 - Classificação")
data.dir <- "data"

theme_set(theme_bw(base_size=15))
options(shiny.maxRequestSize=30*1024^2)

# shinyServer function
shinyServer(function(input, output) {
  
  # ===========================================================================
  # REACTIVE SOURCEs and CONDUCTORs
  # ===========================================================================
  # ---------------------------------------------------------------------------
  # REACTIVE FUNCTIONS (conductors: called when its parents change)
  # ---------------------------------------------------------------------------
  mp.number <- reactive({
    as.numeric(strsplit(input$mini_project, " ")[[1]][2])
  })
  
  # ===========================================================================
  # REACTIVE ENDPOINTS
  # ===========================================================================
  # PDF Download button
  output$download_mp_pdf_button <- downloadHandler(
    filename = function() {
      paste(data.dir, "/", all.mini.projects[mp.number()], ".pdf", sep ="")
    },
    content = function(file) {
      mp.file <- paste(data.dir, "/", all.mini.projects[mp.number()], ".pdf", sep ="")
      
      if (file.exists(mp.file)){
        file.copy(mp.file, file)
      }
    },
    contentType = "application/pdf"
  )

  # ---------------------------------------------------------------------------
  # Render HTML reactively 
  # ---------------------------------------------------------------------------
  
  # Upload data files buttons
  output$mp_upload_files <- renderUI({
    switch(mp.number(), 
           "1"=
             list(
               fileInput('mp1_sqe_file', em('1) Validação dos Modelos (faseX_sqe_modelos.csv)'),
                         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
               fileInput('mp1_prediction_file', em('2.1) Predição dos Atributos Meta (faseX_predicao.csv)'),
                         accept=c('text/csv', 'text/comma-separated-values', '.csv')),
               fileInput('mp1_prediction_correct_file', em('2.2) Valores reais dos Atributos Meta (faseX_predicao_real.csv)'),
                         accept=c('text/csv', 'text/comma-separated-values', '.csv'))
             ),
           "2"=
             list(
               fileInput('mp2_model_validation_file', em('1) Comparação dos Modelos (mp2_validacao_modelos.csv)'),
                         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
               fileInput('mp2_prediction_file', em("2.1) Predição da Qualidade dos Vinhos (mp2_predicao.csv)"),
                         accept=c('text/csv', 'text/comma-separated-values', '.csv')),
               fileInput('mp2_prediction_correct_file', em("2.2) Valores Reais da Qualidade dos Vinhos (mp2_predicao_real.csv)"),
                         accept=c('text/csv', 'text/comma-separated-values', '.csv'))
               ))
  })
  
  # ---------------------------------------------------------------------------
  # Mini-Project 1
  # ---------------------------------------------------------------------------
  
  output$ic_plots <- renderPlot({
    in.file.sqe <- input$mp1_sqe_file
    if (!is.null(in.file.sqe)){
      data.sqe <- read.csv(in.file.sqe$datapath, header=T)
      
      # Plot the CIs
      print(Mp1GetCI(data.sqe))
    }
  })
  
  output$scatter_plots <- renderPlot({
    in.file.prediction <- input$mp1_prediction_file
    in.file.prediction.correct <- input$mp1_prediction_correct_file
    
    if (!is.null(in.file.prediction) & !is.null(in.file.prediction.correct)){
      data.prediction <- read.csv(in.file.prediction$datapath, header=T)
      data.prediction.correct <- read.csv(in.file.prediction.correct$datapath, header=T)
      
      # Plot the Scatter Plots
      print(Mp1GetScatterPlot(data.prediction, data.prediction.correct))
    }
  })
  
  
  # ---------------------------------------------------------------------------
  # Mini-Project 2
  # ---------------------------------------------------------------------------
  
  output$mp2_ic_models_plot <- renderPlot({
    model.validation.file <- input$mp2_model_validation_file
    if (!is.null(model.validation.file)){
      model.validation <- read.csv(model.validation.file$datapath, header=T)
      
      # Plot the CIs
      print(Mp2GetCIModels(model.validation))
    }
  })
  
  output$mp2_confusion_matrix_bin <- renderPlot({
    in.file.prediction <- input$mp2_prediction_file
    in.file.prediction.correct <- input$mp2_prediction_correct_file
    
    if (!is.null(in.file.prediction) & !is.null(in.file.prediction.correct)){
      data.prediction <- read.csv(in.file.prediction$datapath, header=T)
      data.prediction.correct <- read.csv(in.file.prediction.correct$datapath, header=T)
      
      # Plot the Confusion Matrix
      print(Mp2GetConfMatrixBinary(data.prediction, data.prediction.correct))
    }
  })
  
  output$mp2_confusion_matrix_mul <- renderPlot({
    in.file.prediction <- input$mp2_prediction_file
    in.file.prediction.correct <- input$mp2_prediction_correct_file
    
    if (!is.null(in.file.prediction) & !is.null(in.file.prediction.correct)){
      data.prediction <- read.csv(in.file.prediction$datapath, header=T)
      data.prediction.correct <- read.csv(in.file.prediction.correct$datapath, header=T)
      
      # Plot the Confusion Matrix
      print(Mp2GetConfMatrixMultiple(data.prediction, data.prediction.correct))
    }
  })
  
})