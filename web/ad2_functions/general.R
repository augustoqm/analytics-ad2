###############################################################################
# SOURCE() and LIBRARY()
###############################################################################
library(shiny, quietly=T)
library(plyr, quietly=T)
library(reshape, quietly=T)
library(ggplot2, quietly=T)
library(gridExtra, quietly=T)
library(boot, quietly=T)
library(ROCR, quietly=T)
library(scales, quietly=T)

source("../ad2_functions/mp1.R")
source("../ad2_functions/mp2.R")
source("../ad2_functions/mp3.R")
source("../ad2_functions/mp4.R")

###############################################################################
# FUNCTIONS
###############################################################################
BootCI.Mean <- function(data, var, conf.level=.95){
  
  meanFunc <- function(x, i){mean(x[i], na.rm=T)}
  
  data.boot <- boot(data[,var], statistic=meanFunc, R=1000)
  norm.ci <- boot.ci(data.boot, type="norm", conf=conf.level)$normal
  
  return(data.frame(mean_ci = norm.ci[2] + ((norm.ci[3] - norm.ci[2])/2), 
                    lower_ci=norm.ci[2], upper_ci=norm.ci[3]))
}
