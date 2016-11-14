require(nlme)
lCtr <- lmeControl(opt = "optim")
data <- Orthodont
fm <- lme(distance ~ age, data = data, random = ~ 1 + age, control = lCtr)
f<-fm
f<- update(fm, correlation = corARMA(p=0,q=4,form = ~age|Subject), weights = varIdent(form = ~1|age))
f2<- update(fm, correlation = corCAR1(form = ~age|Subject), weights = varIdent(form = ~1|age))
f2 <- f
getVarCov(f, type = "conditional")
getVarCov(f2, type = "conditional")

logLik(f)
summary(f)
vc <- getVarCov(f)
vc <- vc[1:2,1:2]
marg <- getVarCov(f, type = "marginal")
l <- getVarCov(f, type = "conditional")
z <- matrix(c(rep(1,4), seq(8,14,2)),ncol = 2)

m <- z%*%vc%*%t(z)
m1 <- m+ l$M01
m2 <- marg$M01
cm1 <- cov2cor(m1)
cm2 <- cov2cor(m2)
ldata <- split(data, age)
real <- cov(sapply(names(ldata), function(x) ldata[[x]]['distance']))
real
m1
real - m1
f
l
