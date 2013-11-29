################################################################################
# FUNCTIONS - Mini-Project 2
################################################################################

Mp2GetCIModels <- function(model.validation){
  
  CalculateFMeasure <- function(data){
    pred <- prediction(as.numeric(data$classe_prevista), 
                       as.numeric(data$classe_real))
    c(fmeasure=performance(pred, measure="f")@y.values[[1]][2])
  }
  
  # Calculate the FMeasure for each run
  binary.validations <- subset(model.validation, tipo_classificacao == "binaria")
  multi.validations <- subset(model.validation, tipo_classificacao == "multipla")
  
  binary.performance <- ddply(binary.validations, .(nome_modelo, execucao), 
                              function(data){
                                # Define the Main Binary class as "bom"
                                data$classe_prevista <- (data$classe_prevista == "bom") + 0
                                data$classe_real <- (data$classe_real == "bom") + 0
                                CalculateFMeasure(data)
                              })
  
  multi.performance <- ddply(multi.validations, .(nome_modelo, execucao), function(data){
    perf.quality <- adply(levels(multi.validations$classe_real), 1, function(quality.level){
      data$classe_prevista <- (data$classe_prevista == quality.level) + 0
      data$classe_real <- (data$classe_real == quality.level) + 0
      CalculateFMeasure(data)
    })
    c(fmeasure=mean(perf.quality$fmeasure))
  })
  
  # Calculate the CI for the Classification metric
  binary.ci <- ddply(binary.performance, .(nome_modelo), 
                     BootCI.Mean, "fmeasure", .95)
  binary.ci$class_type <- rep("Qualidade Binária", nrow(binary.ci))
  
  multi.ci <- ddply(multi.performance, .(nome_modelo), 
                    BootCI.Mean, "fmeasure", .95)
  multi.ci$class_type <- rep("Qualidade Múltipla", nrow(multi.ci))
  
  # Merge them in a single data.frame
  ci.data <- rbind(binary.ci, multi.ci)
  
  # Generate the Confidence Intervals in a unique ggplot
  limits <- aes(ymin=lower_ci, ymax=upper_ci)
  
  gg <- ggplot(ci.data, aes_string(x="nome_modelo", y="mean_ci", col="nome_modelo")) +
    # Add the error bar
    geom_errorbar(limits, size = 0.8) + 
    # Add the mean value of the CI
    geom_point(size = 3) + 
    # Split the view
    facet_wrap(~class_type, scales="free_x") + 
    # Configure the labels
    labs(list(y = "F-Measure", x = "Modelos", title = "", colour="Modelo"))
  
  return(gg)
}

Mp2GetConfMatrixBinary <- function (student.prediction, mp.labels){
  
  # Compare the tables with the same number of lines
  student.prediction <- student.prediction[1:nrow(mp.labels),]
  
  # Binary data
  bin.class.levels <- c("ruim", "bom")
  bin.confusion.data <- NULL
  for (i in bin.class.levels){
    for (j in bin.class.levels){
      bin.confusion.data <- rbind(bin.confusion.data, 
                                  data.frame(class1 = i, 
                                             class2 = j,
                                             values = sum((student.prediction$classe_prevista_binaria == i) & 
                                                            (mp.labels$qualidade_binaria == j))))
    }
  }
  
  bin.confusion.data$class1 <- factor(bin.confusion.data$class1, 
                                      levels=bin.class.levels)
  bin.confusion.data$class2 <- factor(bin.confusion.data$class2, 
                                      levels=bin.class.levels)
  
  # Plot the Confusion Matrix
  gg <- ggplot(bin.confusion.data, aes(x = class1, y = class2)) + 
    geom_tile(aes(fill = values), color = gray) + 
    geom_text(aes(fill = values, label = values), 
              colour = "black", size = 3) + 
    scale_fill_gradient(low = "yellow", high = "red") + 
    labs(list(x="Classe Prevista", y="Classe Real", fill="Matches")) + 
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(gg)
}

Mp2GetConfMatrixMultiple <- function (student.prediction, mp.labels){

  # Compare the tables with the same number of lines
  student.prediction <- student.prediction[1:nrow(mp.labels),]
  
  # Multiple data
  mul.class.levels <- c("muito_ruim", "ruim", "mais_ou_menos", "bom", "muito_bom")
  mul.confusion.data <- NULL
  for (i in mul.class.levels){
    for (j in mul.class.levels){
      mul.confusion.data <- rbind(mul.confusion.data, 
                                  data.frame(class1 = i, 
                                             class2 = j,
                                             values = sum((student.prediction$classe_prevista_multipla == i) & 
                                                            (mp.labels$qualidade_multipla == j))))
    }
  }
  
  mul.confusion.data$class1 <- factor(mul.confusion.data$class1, 
                                      levels=mul.class.levels)
  mul.confusion.data$class2 <- factor(mul.confusion.data$class2, 
                                      levels=mul.class.levels)
  
  # Plot the Confusion Matrix
  gg <- ggplot(mul.confusion.data, aes(x = class1, y = class2)) + 
    geom_tile(aes(fill = values), color = gray) + 
    geom_text(aes(fill = values, label = values), 
              colour = "black", size = 3) + 
    scale_fill_gradient(low = "yellow", high = "red") + 
    labs(list(x="Classe Prevista", y="Classe Real", fill="Matches")) + 
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(gg)
}
