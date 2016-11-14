rm(list=ls())

require(psych)

source("functions_settings.R")
#Prepare data and get descriptives
### Data ###
rdata <- read.csv("C:/Users/jonaur/Desktop/DATA/intensiv_2016.csv", fileEncoding="UTF-8-BOM")
rdata[rdata == 999] <- NA
#rdata["Week"][rdata["Week"] == 166] <- 13
### VARS ###
outfile <- 'out/mean_sd_alpha_test_retest.csv'
file.create(outfile)


#scales <- scales[scales$scale == 'ex_org_com',]

## calculates new variable with mean value and reversed datafile
ll = list()
for (s in unlist(scales['scale'])) {
  items <- scales[scales['scale'] == s][-c(1:5)]
  items <- items[!is.na(items)]
  items <- items[items != ""]
  min_mean <- floor(length(items)/(3/2))
  #min_mean <- scales['min_mean'][scales['scale'] == s]
  if (min_mean < 2) {min_mean <- 2}
  rev <- scales['rev'][scales['scale'] == s]
  rev_val <- scales['rev_val'][scales['scale'] == s]
  if (is.na(rev) | rev == "") {
    rev <- NULL
    rev_val <- NULL
  } else {
    rev.n <- as.integer(unlist(strsplit(rev, ',')))
    rev.items <- items[rev.n]
    rev.items <- rev.items[!rev.items %in% ll]
    ll <- c(ll,rev.items)
    if (length(rev.items) > 0){
      print(paste(c('reversing',rev.items), collapse = " "))
      rdata[rev.items] <- rev_val - rdata[rev.items]
    }
  }
  if (length(items) < 2){min_mean <- 1}
  var_sav <- sprintf("%s_mean", s)
  print(var_sav)
  rdata[var_sav] <- mean.n(df = rdata[items], n = min_mean)
}

#Split datafile by time (must be done after calculation of new variable with mean value)
df <- split(rdata,rdata[time])
#executes script for all scales/subscales
for (s in unlist(scales['scale'])) {
  items <- scales[scales['scale'] == s][-c(1:5)]
  items <- items[!is.na(items)]
  items <- items[items != ""]
  print(items)
  stats <- get_stats(scale_name = s,items = items,df = df,time = time)
}

write.csv(rdata, file = "data/2016_data_mean_rev.csv")




