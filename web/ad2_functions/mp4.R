################################################################################
# FUNCTIONS - Mini-Project 4
################################################################################

MP4GetBoxplotValidation <- function(model.validation){
  
  is.err.negative <- model.validation$erro_absoluto < 0
  model.validation$tipo_erro[!is.err.negative] <- 'positivo'
  model.validation$tipo_erro[is.err.negative] <- 'negativo'
  
  gg <- ggplot(model.validation, aes(x=nome_modelo, y=erro_absoluto, fill=tipo_erro)) +
    geom_boxplot(notch=T, outlier.shape=21) + 
    labs(list(x="", y="Erro Absoluto", fill="Sinal do Erro"))

  return(gg)  
}

MP4GetTemporalValidation <- function(model.validation, ts.interval=c(0,1)){

  # Select the validation date
  model.validation$dia_horizonte <- as.Date(model.validation$dia_horizonte)
  all.dates <- sort(model.validation$dia_horizonte)
  selected.dates <- all.dates[c(ts.interval[1] * length(all.dates) + 1, ts.interval[2] * length(all.dates))]
  model.validation <- subset(model.validation, dia_horizonte >= selected.dates[1] & dia_horizonte <= selected.dates[2])
  
  gg <- ggplot(model.validation, aes(x=dia_horizonte, y=erro_absoluto, col=nome_modelo)) +
    geom_line() + 
    facet_wrap(~nome_modelo, ncol=1) + 
    scale_x_date(labels=date_format("%d-%m-%Y")) + 
    labs(list(x="Tempo", y="Erro Absoluto")) + 
    theme(legend.position='none',
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(gg)  
}

MP4GetBoxplotPrediction <- function(student.prediction, real.juros){
  
  # Calculate the absolute error
  model.prediction <- merge(student.prediction, real.juros, by.x='dia_horizonte', by.y='data')
  model.prediction$erro_absoluto <- model.prediction$taxa_juros - model.prediction$saida_prevista
  
  # Remove the days without 'taxa de juros' (NA values)
  model.prediction <- model.prediction[!is.na(model.prediction$taxa_juros),]
  
  # Count the positive and negative errors
  is.err.negative <- model.prediction$erro_absoluto < 0
  model.prediction$tipo_erro <- 'positivo'
  model.prediction$tipo_erro[is.err.negative] <- 'negativo'
  
  # Define a name to the model
  model.prediction$nome_modelo <- rep("Melhor Modelo", nrow(model.prediction))
  
  gg <- ggplot(model.prediction, aes(x=nome_modelo, y=erro_absoluto, fill=tipo_erro)) +
    geom_boxplot(notch=T, outlier.shape=21) + 
    labs(list(x="", y="Erro Absoluto", fill="Sinal do Erro"))
  print(gg)
  
  return(gg)
}

MP4GetTSRealVsPrediction <- function(student.prediction, real.juros, ts.interval=c(0,1)){
  
  all.juros <- merge(student.prediction, real.juros, by.x='dia_horizonte', by.y='data')
  all.juros <- rename(all.juros, c("saida_prevista" = "Prevista",
                                   "taxa_juros" = "Real"))
  all.juros <- melt(all.juros, id.vars="dia_horizonte")
  all.juros$dia_horizonte <- as.Date(all.juros$dia_horizonte)
  
  # Select the TS interval
  all.dates <- sort(all.juros$dia_horizonte)
  selected.dates <- all.dates[c(ts.interval[1] * length(all.dates) + 1, ts.interval[2] * length(all.dates))]
  all.juros <- subset(all.juros, dia_horizonte >= selected.dates[1] & dia_horizonte <= selected.dates[2])
  
  gg <- ggplot(all.juros, aes(x=dia_horizonte, y=value, col=variable)) + 
    geom_line() + 
    scale_x_date(labels=date_format("%d-%m-%Y")) + 
    labs(list(x="Tempo", y="Taxa de Juros", col="Série")) +
    theme(legend.position="bottom")
  
  return (gg)
}
  