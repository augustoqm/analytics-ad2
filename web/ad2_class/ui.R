###############################################################################
# SHINY UI
###############################################################################

# Global variables
all.mini.projects <- c("MP 1 - Regressão", "MP 2 - Classificação", "MP 3 - Recomendação")

title <- "Análise de Dados II"
windowTitle <- "Análise de Dados II - Sala de Aula"
subtitle <- "Visualizando os mini-projetos em sala de aula"

# shinyUI function
shinyUI(bootstrapPage(
  
  div(id="wrap", 
      div(id="content-bar", class="container", 
          # HEADER
          tagList(singleton(tags$head(tags$title(windowTitle))), 
                  div(class = "hero-unit", style = "text-align:center; padding: 25px 0px;", 
                      h1(title, 
                         br(), tags$small(subtitle)))),
          # BODY
          div(id="content", class="row-fluid", 
              # INPUT
              div(id="sidebar-input", class = "span3", 
                  tags$form(class = "well", 
                            strong("Mini-Projeto"),
                            selectInput('mini_project', label="", all.mini.projects, all.mini.projects[1]),
                            br(),
                            downloadButton(class="btn btn-primary btn-mini", 
                                           "download_mp_pdf_button", "Download PDF"),
                            tags$hr(),
                            
                            uiOutput("mini_project_tags"))),
              
              # MAIN 
              div(id="main-output", class = "span9", 
                  tagList(
                    conditionalPanel(condition=paste("input.mini_project == '", all.mini.projects[1], "'", sep =""),
                                     h3("Mini-Projeto 1 - Regressão", style="text-align:center"),
                                     tabsetPanel(id="phases",
                                       tabPanel(title="Fase 1", 
                                                h4("VALIDAÇÃO dos Modelos"),
                                                em("Intervalos de Confiança da Média (95%)"),
                                                plotOutput("phase1_ic_plot"),
                                                tags$hr(),
                                                h4("PREDIÇÃO dos Atributos-Meta"),
                                                em("Gráficos de Dispersão"),
                                                plotOutput("phase1_scatter_plots"),
                                                tags$hr(),
                                                h4("ANÁLISE Escrita"),
                                                downloadButton("phase1_download_analysis", "Download", 
                                                               class="btn btn-primary"),
                                                br(), br(),
                                                value = "phase1"),
                                       tabPanel(title="Fase 2", 
                                                h4("VALIDAÇÃO dos Modelos"),
                                                em("Intervalos de Confiança da Média (95%)"),
                                                plotOutput("phase2_ic_plot"),
                                                tags$hr(),
                                                h4("PREDIÇÃO dos Atributos-Meta"),
                                                em("Gráficos de Dispersão"),
                                                plotOutput("phase2_scatter_plots"),
                                                tags$hr(),
                                                h4("ANÁLISE Escrita"),
                                                downloadButton("phase2_download_analysis", "Download", 
                                                               class="btn btn-primary"),
                                                br(), br(),
                                                value = "phase2"))
                    ),
                    
                    conditionalPanel(condition=paste("input.mini_project == '", all.mini.projects[2], "'", sep =""),
                                     h3("Mini-Projeto 2 - Classificação", style="text-align:center"),
                                     tags$hr(),
                                     
                                     h4("Análise Escrita"),
                                     downloadButton("mp2_download_analysis", "Download", 
                                                    class="btn btn-primary"),
                                     tags$hr(),
                                     
                                     h4("Análise Visual - Comparação dos Modelos"),
                                     em("Intervalos de Confiança (95%) da Média - F-Measure"),
                                     plotOutput("mp2_ic_models_plot"),
                                     tags$hr(),
                                     
                                     h4("Análise Visual - Predição da Qualidade dos Vinhos!"),
                                     em("Matriz de Confusão - Qualidade Binária"),
                                     plotOutput("mp2_confusion_matrix_bin"),
                                     br(),
                                     em("Matriz de Confusão - Qualidade Múltipla"),
                                     plotOutput("mp2_confusion_matrix_mul"),
                                     br()),
                    
                    conditionalPanel(condition=paste("input.mini_project == '", all.mini.projects[3], "'", sep =""),
                                     h3("Mini-Projeto 3 - Recomendação", style="text-align:center"),
                                     tags$hr(),
                                     
                                     h4("Análise Escrita"),
                                     downloadButton("mp3_download_analysis", "Download", 
                                                    class="btn btn-primary"),
                                     tags$hr(),
                                     
                                     conditionalPanel(condition=paste("input.mp3_compare_predictions == false", sep =""),
                                                      h4("Análise Visual - Comparação de Modelos (validação)"),
                                                      em("Intervalos de Confiança (95%) da Média - RMSE"),
                                                      plotOutput("mp3_ic_models_validation_plot"),
                                                      tags$hr()),
                                     conditionalPanel(condition=paste("input.mp3_compare_predictions == true", sep =""),
                                                      h4("Análise Visual - Comparação de Modelos (predição)"),
                                                      em("Intervalos de Confiança (95%) da Média - RMSE"),
                                                      plotOutput("mp3_ic_models_prediction_plot"),
                                                      tags$hr()))
                  ))))
  )
)
)