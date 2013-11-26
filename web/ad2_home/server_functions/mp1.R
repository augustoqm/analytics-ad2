################################################################################
# FUNCTIONS - Mini-Project 2
################################################################################

Mp1GetCI <- function(student.sqe.data){
  # Calculate the CI for both SQEs
  ci.mean.data.y1 <- BootCI.Mean(data = student.sqe.data, 
                                 var = "sqe_aquecimento", .95)
  ci.mean.data.y1$model_name <- rep("Carga de Aquecimento", nrow(ci.mean.data.y1))
  
  ci.mean.data.y2 <- BootCI.Mean(data = student.sqe.data, 
                                 var = "sqe_refrigeracao", .95)
  ci.mean.data.y2$model_name <- rep("Carga de Refrigeração", nrow(ci.mean.data.y2))
  
  # Merge them in a single data.frame
  ci.data <- rbind(ci.mean.data.y1, ci.mean.data.y2)
  
  # Generate the Confidence Intervals in a unique ggplot
  limits <- aes(ymin=lower_ci, ymax=upper_ci)
  
  gg <- ggplot(ci.data, aes_string(x="model_name", y="mean_ci", col="model_name")) +
    # Add the error bar
    geom_errorbar(limits, size = 0.8) + 
    # Add the mean value of the CI
    geom_point(size = 3) + 
    # Configure the color, labels and legend
    scale_color_manual(values = c("Carga de Aquecimento" = "red", 
                                  "Carga de Refrigeração" = "blue")) +
    labs(list(y = "Soma do Quadrado dos Erros (SQE)", x = "Melhor Modelo", title = "")) +
    theme(legend.position = "none") 
  
  return(gg)
}

Mp1GetScatterPlot <- function(hidden.phase.target.test, student.phase.target.prediction){
  
  # Merge the data in a single data.frame
  data.y1 <- data.frame(student_test_target = student.phase.target.prediction[,"y_carga_aquecimento"], 
                        real_test_target = hidden.phase.target.test[,"y_carga_aquecimento"])
  data.y1$target_attribute = rep("Carga de Aquecimento", nrow(data.y1))
  
  data.y2 <- data.frame(student_test_target = student.phase.target.prediction[,"y_carga_refrigeracao"], 
                        real_test_target = hidden.phase.target.test[,"y_carga_refrigeracao"])
  data.y2$target_attribute = rep("Carga de Refrigeração", nrow(data.y2))
  
  data <- rbind(data.y1, data.y2)
  
  # Generate the Confidence Intervals in a unique ggplot
  gg <- ggplot(data, aes_string(x="student_test_target", y="real_test_target", colour = "target_attribute")) + 
    
    # Add the abline of the perfect predictions  
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1.1) +         
    
    # Add the point predictions
    geom_point(aes_string(fill = "target_attribute"), shape = 21, size = 5) + 
    geom_point(fill = "white", shape = 21, size = 3) + 
    
    # Split the predictions by target_attribute
    facet_wrap(~target_attribute, scales="free") + 
    
    # Configure the fill, colors, labels and legend
    scale_fill_manual(values = c("Carga de Aquecimento" = "red", 
                                 "Carga de Refrigeração" = "blue")) + 
    scale_color_manual(values = c("Carga de Aquecimento" = "red", 
                                  "Carga de Refrigeração" = "blue")) + 
    labs(list(x = "Valores Previstos", y = "Valores Reais", title = "")) + 
    theme(legend.position = "none") 
  
  return (gg)
}