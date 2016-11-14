#reshapes longfile to wide
long.to.wide <- function(ldata, id, time, dropvar = NULL){
  ldata.sorted <- ldata[with(ldata, order(get(id), get(time))), ]
  w <- reshape(ldata.sorted,
               timevar = time,
               idvar = id,
               drop = dropvar,
               direction = "wide")
  return(w)
}

#Get ICC, Slope, Slope_p and residual reduction
mlm <- function(a,b,var){
  #summaries
  s.a <- summary(a)
  s.b <- summary(b)

  
  #slope and t-value
  slope <- s.b$tTable[2,"Value"]
  slope_p <- s.b$tTable[2,"p-value"]
  
  #variance
  vc.a <- VarCorr(a)
  vc.a_intercept <- as.numeric(vc.a[,1]['(Intercept)'])
  vc.a_residual <- as.numeric(vc.a[,1]['Residual'])
  vc.b <- VarCorr(b)
  vc.b_residual <- as.numeric(vc.b[,1]['Residual'])
  
  ICC <- vc.a_intercept/(vc.a_intercept+vc.a_residual)
  residual_reduction <- ((vc.a_residual-vc.b_residual)/vc.a_residual)*100
  
  
  names(ICC) <- "ICC"
  names(residual_reduction) <- "residual reduction"

  names(slope) <- "slope"
  names(slope_p) <- "slope p"
  return_obj <- round(c(ICC,slope,slope_p),3) # residual reduction is not returned
  return(return_obj)
}

#means from Fitzmaurice mean profile model
fitz_means <- function(c,var){
  s.c <- summary(c)
  means <- s.c$tTable[,1]
  means[2:length(means)] <- means[2:length(means)] + means[1]
  points <- unique(fdata[time][!is.na(fdata[var])])
  df <- data.frame(means)
  colnames(df) <- var
  df[time] <- points
  df <- df[c(2,1)]
  return(df)
}

#individual spagetti + trendline + confidance interval
spagettiplot <- function(var){
  ymax <- ceiling(max(rdata[var],na.rm = T)) + 1
  xmin <- floor(min(rdata[time], na.rm = T))
  xmax <- ceiling(max(rdata[time], na.rm = T))
  spagplot <- ggplot(data = rdata, aes(x = get(time), y = get(var), group = get(id))) +
    geom_smooth(method='lm',formula=y~x, se = F, size=.1, colour="black") + 
    scale_x_continuous(breaks=c(0,2,4,6,8,10,12), limits = c(xmin,xmax)) + 
    scale_y_continuous(breaks=c(0,2,4,6,8,10,12), limits = c(0,ymax)) + 
    labs(x = time, y = var) + theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_smooth(inherit.aes = F, formula = y~x, method = 'lm', aes(x= get(time),y = get(var)))
  ggsave(sprintf('out/%s_spagettiplot.png', var), dpi = 300, plot = spagplot)
}

#fitz mean plots
mean_plot <- function(var){
  ymax <- ceiling(max(rdata[var],na.rm = T)) + 1
  xmin <- floor(min(rdata[time], na.rm = T))
  xmax <- ceiling(max(rdata[time], na.rm = T))
  mean_plot <- ggplot(data = f_mean, aes(x = get(time), y = get(var))) + geom_point(colour = "blue", size = 2) +
    scale_x_continuous(breaks=c(0,2,4,6,8,10,12), limits = c(xmin,xmax)) + 
    scale_y_continuous(breaks=c(0,2,4,6,8,10,12), limits = c(0,ymax)) + labs(x = time, y = var) +
    theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none")
  ggsave(sprintf('out/%s_mean_plot.png', var), dpi = 300, plot = mean_plot)
}

#run mlm
model.a_b <- function(var,time,fdata){
  print(var)
  #Model A: Unconditional model
  f1 <- formula(paste(var, "1", sep = "~"))
  f2 <- formula(paste("~ 1", id, sep = " | "))
  m.a <- lme(fixed = f1, random= f2, method = "REML",
             data = fdata, control = lCtr, na.action = na.exclude)
  #names(m.a) <- varlist
  f1 <- formula(paste(var, time, sep = "~"))
  f2 <- formula(paste(paste("~",time, sep = ""), id, sep = " | "))
  m.b <- lme(fixed = f1, random= f2, method="REML",
             data = fdata, control = lCtr, na.action = na.exclude)
  return_list <- list("a" = m.a, "b" = m.b)
  return(return_list)
}

#model fitz_mean
model.fitz_mean <- function(var,time,catdata){
  print(var)
  f1 <- formula(paste(var, time, sep = "~"))
  f2 <- formula(paste(paste("~",time, sep = ""), id, sep = " | "))
  m.c <-   lme(fixed = f1, random = f2, method = "REML",
               data = catdata, control = lCtr, na.action = na.exclude)
  return(m.c)
}

#main function to get mean, sd, alpha stats and test-retest
get_stats <- function(scale_name, df,items, time = "time"){
  cols <- c(time,"N","mean","sd","Alpha","MIIC","ITC min","ITC max","tr.N vs prev","tr.R vs prev","tr.P vs prev")
  ret.df <- as.data.frame(setNames(replicate(length(cols),numeric(0), simplify = F), cols))
  var <- sprintf('%s_mean',scale_name)
  if (is.list(df)){
    tn <- names(df)
    for (n in tn){
      x <- df[[n]]
      if (!all(is.na(x[items]))){
        if (exists("t1")){
          t2 <- x[c(id,var)]
          tr.d <- merge(t1,t2,by=id)
          tr <- corr.test(tr.d[2:3])
          tr.N <- tr$n[1,2]
          tr.R <- tr$r[1,2]
          tr.P <- tr$p[1,2]
          t1 <- t2
        } else {
          t1 <- x[c(id,var)]
          tr.N <- NA
          tr.R <- NA
          tr.P <- NA
        }
        if (length(items) <= 1){
          alpha <- c(NA,NA,NA,NA)
        }else{
          alpha <- get_alpha(x,items)
        }
        m <- describe(x)[var,'mean']
        sd <- describe(x)[var,'sd']
        obs <- length(x[var][!is.na(x[var])])
        nr1 <- c(n,obs)
        nr2 <- format(round(c(m,sd,alpha),2),nsmall = 2)
        nr3 <- tr.N
        nr4 <- format(round(c(tr.R,tr.P),3),nsmall = 3)
        new_row <- c(nr1,nr2,nr3,nr4)
        ret.df <- rbind(ret.df,new_row,stringsAsFactors = F)
      } else {
        ret.df <- rbind(ret.df,c(n,rep("",length(cols)-1)), stringsAsFactors = F)
      }
    }
  } else{
    print('wrong')
    alpha <- get_alpha(df,items)
    m <- describe(x)[var,'mean']
    sd <- describe(x)[var,'sd']
    obs <- length(x[var][!is.na(x[var])])
    ret.df <- rbind(ret.df,c(as.integer(n),m,sd,alpha))
  }
  names(ret.df) <- cols
  ret.df.t <- t(ret.df)
  write.table(scale_name, file = outfile, col.names = F, row.names = F, append = T)
  write.table(ret.df.t, file = outfile, col.names = F, quote = T, sep = ',', append = T, na = "")
  write.table("", file = outfile, col.names = F, row.names = F, append = T)
}

#get chronbach's alpha
get_alpha <- function(df,items){
  a <- psych::alpha(df[,items], use = "complete.obs")
  raw_alpha <- a$total$raw_alpha
  miic <- a$total$average_r
  max_itc <- max(a$item.stats$r.drop)
  min_itc <- min(a$item.stats$r.drop)
  ret.object <- c(raw_alpha,miic,min_itc,max_itc)
  names(ret.object) <- c("Alpha","MIIC","ITC min","ITC max")
  return(ret.object)
}

#calculate mean with minimum number of responses
mean.n   <- function(df, n) {
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, means, NA)
}

#tests for non-linear effects, prints tTable, plots of non-linear effects and anova results
non_linear <- function(var,time,id,fdata,catdata,f,r, times,ymax){
  mod.c <- model.fitz_mean(var,time,catdata)
  m.mean <- fitz_means(mod.c,var)
  ft <- m.mean[[time]]
  fdata[time] <- fdata[time] - min(ft)
  #ymax <- ceiling(max(fdata[var],na.rm = T)) + 1
  if (times > 2){
    lf <- c("1",time,lapply(2:5, function(x) sprintf("I(%s**%s)",time,as.character(x))))
    write.table(t(c("Variable","Value","Std.Error","DF","t-value","p-value")),file = sprintf("out2/%s.csv",var), col.names = F, row.names = F, sep = ",")
    print(var)
  
    m1 <- lme(fixed = f, random = r, method="REML",data = fdata, control = lCtr, na.action = na.exclude)
    for (x in 1:3){
      #formulas
      mf1 <- paste(var, paste(lf[2:(x+1)], collapse = " + "), sep = " ~ ")
      mr1 <- paste("~",paste(paste(lf[1:x], collapse = " + "), id, sep = " | "), sep = "")
      mr2 <- paste("~",paste(paste(lf[1:(x+1)], collapse = " + "), id, sep = " | "), sep = "")
      #text to formula
      f1 <- formula(mf1)
      r1 <- formula(mr1)
      r2 <- formula(mr2)
      #update
      try(m1 <- update(m1, fixed = f1, random = r1))
      if (x < 3){
        try(m2 <- update(m1, fixed = f1, random = r2))
        #maximum likelihood for anova purposes
        m1_ml <- tryCatch(update(m1, method = "ML"), error = function(e) e)
        m2_ml <- tryCatch(update(m2, method = "ML"), error = function(e) e)
        #if (m1_ml and m2_ml
        if (any(c(class(m1_ml),class(m2_ml)) == "error")){
          anova.p <- "Error"
          write.table("Test failed",file = sprintf("out/%s.csv",var),append = T,  col.names = F, row.names = F)
        } else {
          anova.p <- anova(m1_ml,m2_ml)[2,9]
          write.table(c("",sprintf("%s   |||   %s (Anova.p ML)", mr1,mr2), anova.p, ""), file = sprintf("out/%s.csv",var), append = T, col.names = F, sep = ",", row.names = F)
        }
      } else {write.table("",file = sprintf("out/%s.csv",var), append = T, col.names = F, sep = ",", row.names = F)}
      sm1 <- summary(m1)
      write.table(round(sm1$tTable,5),file = sprintf("out/%s.csv",var), append = T, col.names = F, sep = ",")
      if (x == 1){
        lin <- curve(sm1$tTable[1] + sm1$tTable[2]*(x-min(ft)), from = min(ft), to = max(ft))
      }
      if (x == 2){
        quad <- curve(sm1$tTable[1] + sm1$tTable[2]*(x-min(ft)) + sm1$tTable[3]*(x-min(ft))^2, from = min(ft), to = max(ft))
      }
    }
  }
  png(sprintf('out/%s.png',var), width = 800, height = 800)
  plot(x = m.mean[[time]], y = m.mean[[var]],xlim = c(0,13),ylim = c(0,ymax), col = 'green', ylab = var, xlab = time, pch = 16, cex = 1.5)
  #plot(x = m.mean[[time]], y = m.mean[[var]],xlim = c(min(ft),max(ft)),ylim = c(0,ymax), col = 'green', ylab = var, xlab = time, pch = 16)
  if (times > 2){
    lines(lin, col = "blue", lty=2, lwd = 2)
    lines(quad, col="red", lty=1, lwd = 2)
  }
  legend("topright", c("Mean","Linear","Quadratic"), col = c("green","blue","red"), pch = c(16,NA,NA),lty = c(NA,2,1))
  dev.off()
}

