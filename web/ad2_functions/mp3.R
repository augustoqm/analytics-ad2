################################################################################
# FUNCTIONS - Mini-Project 3
################################################################################

MP3GetCIModels <- function(model.validation){
 
  model.ci <- ddply(model.validation, .(nome_modelo), 
                    BootCI.Mean, "rmse", .95)
  
  # Generate the Confidence Intervals in a unique ggplot
  limits <- aes(ymin=lower_ci, ymax=upper_ci)
  
  gg <- ggplot(model.ci, aes(x=nome_modelo, y=mean_ci, col=nome_modelo)) +
    # Add the error bar
    geom_errorbar(limits, size = 0.8) + 
    # Add the mean value of the CI
    geom_point(size = 3) + 
    # Configure the labels
    labs(list(y = "RMSE", x = "Modelos", colour="Modelos"))
  
  return(gg)
}
