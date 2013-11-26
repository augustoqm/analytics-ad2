###############################################################################
# SOURCE() and LIBRARY()
###############################################################################
library(plyr)
library(reshape)
library(ggplot2)
library(gridExtra)
library(boot)
library(ROCR)

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
