################################################################################
# FUNCTIONS - Mini-Project 4
################################################################################

MP4GetBoxplotValidation <- function(model.validation){
  
  is.err.negative <- model.validation[,3] < 0
  model.validation$tipo_erro[!is.err.negative] <- rep('positivo', sum(! is.err.negative))
  model.validation$tipo_erro[is.err.negative] <- rep('negativo', sum(is.err.negative))
  
  gg <- ggplot(model.validation, aes(x=nome_modelo, y=erro_absoluto, fill=tipo_erro)) +
    geom_boxplot(notch=T, outlier.shape=21) + 
    labs(list(x="Modelos", y="Erro Absoluto", fill="Sinal do Erro"))

  return(gg)  
}

MP4GetTemporalValidation <- function(model.validation, ts.interval=c(0,1)){

  # Select the validation date
  model.validation$data_horizonte <- as.Date(model.validation$data_horizonte)
  all.dates <- sort(model.validation$data_horizonte)
  selected.dates <- all.dates[c(ts.interval[1] * length(all.dates) + 1, ts.interval[2] * length(all.dates))]
  model.validation <- subset(model.validation, data_horizonte >= selected.dates[1] & data_horizonte <= selected.dates[2])
  
  gg <- ggplot(model.validation, aes(x=as.Date(data_horizonte), y=erro_absoluto, col=nome_modelo)) +
    geom_line() + 
    facet_wrap(~nome_modelo, ncol=1) + 
    scale_x_date(labels=date_format("%b-%Y")) + 
    labs(list(x="Tempo", y="Erro Absoluto")) + 
    theme(legend.position='none',
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(gg)  
}