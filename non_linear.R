source("functions_settings.R")


require(nlme)
rdata <- read.csv("data/data_mean_rev.csv")
catdata <- rdata;catdata[[time]] <- factor(catdata[[time]]) #make time into a categorical variable

lCtr <- lmeControl(opt = "optim", msMaxIter = 500, gradHess = F)
options(scipen=999)



varlist <- as.vector(scales[['scale']])
for (var in varlist){
  var.m <- sprintf("%s_mean", var)
  times <- length(unique(rdata[time][!is.na(rdata[var.m])]))
  ymax <- scales['rev_val'][scales['scale'] == var]
  if (times >= 2) {
    mf <- sprintf("%s ~ 1",var.m)
    mr <- sprintf("~1|%s",id)
    f <- formula(mf)
    r <- formula(mr)
    fdata <- rdata
    try(non_linear(var.m,time,id,rdata,catdata,f,r,times,ymax))
  } else {
    print(sprintf("%s only measured %s time", var,as.character(times)))
  }
}

