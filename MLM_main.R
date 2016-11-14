rm(list=ls())


#settings and libraries
source("functions_settings.R")
require(nlme)
require(ggplot2)
lCtr <- lmeControl(opt = "optim", msMaxIter = 500, gradHess = F)
outfile <- 'out/long.csv'
?lmeControl

### Data ###
rdata <- read.csv("data/data_mean_rev.csv")
catdata <- rdata;catdata$week <- factor(catdata$week) #make time into a categorical variable



### variables to include in analysis
varlist.pre <- as.vector(scales[['scale']])
varlist <- list()
i <- 1
for (var in varlist.pre){
  var.m <- sprintf("%s_mean", var)
  times <- length(unique(rdata[time][!is.na(rdata[var.m])]))
  if (times >= 2) {
    varlist[i] <- var
    i <- i + 1
  }
}
varlist <- unlist(varlist)


## model a and b and plots
n <- 0
for (var in varlist){
  var.m <- sprintf("%s_mean",var)
  center <- min(rdata[time][!is.na(rdata[var.m])])
  fdata <- rdata
  fdata[time] <- fdata[time] - center
  m <- model.a_b(var.m,time,fdata)
  if (n == 0){
    df <- as.data.frame(mlm(m$a,m$b,var.m))
    colnames(df) <- var
    n <- 1
  }
  else{
    df[var] <- as.data.frame(mlm(m$a,m$b,var.m))
  }  
  spagettiplot(var.m)
}


#export ICC, Slope, slope p
write.csv(t(df), file = outfile)


