library(tseries)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(scales)
library(lattice)
library(Hmisc)
library(xtable)
source("C:/users/usuario/Desktop/portfolio.r.txt")
source("C:/users/usuario/Desktop/portfolio_noshorts.r.txt")

vbltx.z <- get.hist.quote(instrument="vbltx",start="2009-01-01",end="2013-12-31",quote="AdjClose",provider="yahoo",origin="1970-01-01",compression="m",retclass="zoo")
adbe.z <- get.hist.quote(instrument="adbe",start="2009-01-01",end="2013-12-31",quote="AdjClose",provider="yahoo",origin="1970-01-01",compression="m",retclass="zoo")
orcl.z <- get.hist.quote(instrument="orcl",start="2009-01-01",end="2013-12-31",quote="AdjClose",provider="yahoo",origin="1970-01-01",compression="m",retclass="zoo")
vfinx.z <- get.hist.quote(instrument="vfinx",start="2009-01-01",end="2013-12-31",quote="AdjClose",provider="yahoo",origin="1970-01-01",compression="m",retclass="zoo")
veurx.z <- get.hist.quote(instrument="veurx",start="2009-01-01",end="2013-12-31",quote="AdjClose",provider="yahoo",origin="1970-01-01",compression="m",retclass="zoo")
veiex.z <- get.hist.quote(instrument="veiex",start="2009-01-01",end="2013-12-31",quote="AdjClose",provider="yahoo",origin="1970-01-01",compression="m",retclass="zoo")
index(vbltx.z) <- as.yearmon(index(vbltx.z))
index(adbe.z) <- as.yearmon(index(adbe.z))
index(orcl.z) <- as.yearmon(index(orcl.z))
index(vfinx.z) <- as.yearmon(index(vfinx.z))
index(veurx.z) <- as.yearmon(index(veurx.z))
index(veiex.z) <- as.yearmon(index(veiex.z))
asset.p <- merge(vbltx.z,adbe.z,orcl.z,veurx.z,veiex.z,vfinx.z)  # objeto zoo
asset.r <- diff(log(asset.p))        # objeto zoo
asset.names <- c("vbltx","adbe","orcl","veurx","veiex","vfinx")
colnames(asset.r) <- asset.names
colnames(asset.p) <- c("vbltx","adbe","orcl","veurx","veiex","vfinx")
assetr.mz <- coredata(asset.r)    		# se convierte en matriz
assetr.df <- data.frame(date=index(asset.r),coredata(asset.r),stringsAsFactors=FALSE)
assetr.df$date <- as.Date(assetr.df$date)    # se convierte yearmon - Date
assetr.melt <- melt(assetr.df,id="date",variable.name="activos",value.name="retornos")
rf <- 0.005 # anual
rf.m <- rf/12 # mensual
sigma.mat <- var(asset.r)

#################################################################################
#################################################################################

xyplot(asset.p,type = c("l","g"),main = "Precios Activos",
       lwd=1,col="navy",scales=list(y=list(cex=0.6),x=list(cex=0.8)),
       par.strip.text = list(cex = 1,col="white"),
       strip = strip.custom(factor.levels = paste(colnames(asset.p),"precios")),
       par.settings = list(strip.background = list(col="navy")))
# rendimientos
xyplot(asset.r,type = c("l","g"),main = "Rendimientos Activos",
       par.strip.text = list(cex = 1,col="white"),
       strip=F, strip.left = strip.custom(factor.levels = colnames(asset.r)),
       par.settings = list(strip.background = list(col="navy")),
       panel = function(x,y,...) {
         panel.xyplot(x,y,lwd=1,col="navy",...)
         panel.abline(h=0,lwd=0.7,col="orange",...)},
       scales=list(y=list(cex=0.6),x=list(cex=0.8)))


####################################
####################################
chart.CumReturns(asset.r, lwd=1, legend.loc="topleft", main="Crecimiento de $1",
                 ylab="Valor",xlab="Fecha")

cum.r <- Return.cumulative(asset.r,geometric=TRUE)
cum.t <- xtable(cum.r,caption="Retornos Acumulados",digits=3)
print(cum.t,comment = FALSE,include.rownames =TRUE)
####################################
####################################

hist(assetr.mz[,1],main="rendimientos mensuales continuos",xlab="Vbltx", probability=T, col="navy",border="white")
rug(jitter(assetr.mz[,1]))
boxplot(assetr.mz[,1],outchar=T, main="Boxplot", col="navy")
abline(h=median(assetr.mz[,1]),lwd=3,lty=1,col="white")
plot(density(assetr.mz[,1]),type="l", main="Densidad suavizada",
     xlab="rendimiento mensual", ylab="estimado de densidad", col="navy")
qqnorm(assetr.mz[,1], col="navy")
qqline(assetr.mz[,1])

hist(assetr.mz[,2],main="rendimientos mensuales continuos",
     xlab="Adbe", probability=T, col="navy",border="white")
rug(jitter(assetr.mz[,2]))
boxplot(assetr.mz[,2],outchar=T, main="Boxplot", col="navy")
abline(h=median(assetr.mz[,2]),lwd=3,lty=1,col="white")
plot(density(assetr.mz[,2]),type="l", main="Densidad suavizada",
     xlab="rendimiento mensual", ylab="estimado de densidad", col="navy")
qqnorm(assetr.mz[,2], col="navy")
qqline(assetr.mz[,2])

hist(assetr.mz[,3],main="rendimientos mensuales continuos",
     xlab="orcl", probability=T, col="navy",border="white")
rug(jitter(assetr.mz[,3]))
boxplot(assetr.mz[,3],outchar=T, main="Boxplot", col="navy")
abline(h=median(assetr.mz[,3]),lwd=3,lty=1,col="white")
plot(density(assetr.mz[,3]),type="l", main="Densidad suavizada",
     xlab="rendimiento mensual", ylab="estimado de densidad", col="navy")
qqnorm(assetr.mz[,3], col="navy")
qqline(assetr.mz[,3])

hist(assetr.mz[,4],main="rendimientos mensuales continuos",
     xlab="veurx", probability=T, col="navy",border="white")
rug(jitter(assetr.mz[,4]))
boxplot(assetr.mz[,3],outchar=T, main="Boxplot", col="navy")
abline(h=median(assetr.mz[,4]),lwd=3,lty=1,col="white")
plot(density(assetr.mz[,4]),type="l", main="Densidad suavizada",
     xlab="rendimiento mensual", ylab="estimado de densidad", col="navy")
qqnorm(assetr.mz[,4], col="navy")
qqline(assetr.mz[,4])

hist(assetr.mz[,5],main="rendimientos mensuales continuos",
     xlab="veiex", probability=T, col="navy",border="white")
rug(jitter(assetr.mz[,5]))
boxplot(assetr.mz[,3],outchar=T, main="Boxplot", col="navy")
abline(h=median(assetr.mz[,5]),lwd=3,lty=1,col="white")
plot(density(assetr.mz[,5]),type="l", main="Densidad suavizada",
     xlab="rendimiento mensual", ylab="estimado de densidad", col="navy")
qqnorm(assetr.mz[,5], col="navy")
qqline(assetr.mz[,5])

hist(assetr.mz[,6],main="rendimientos mensuales continuos",
     xlab="vfinx", probability=T, col="navy",border="white")
rug(jitter(assetr.mz[,6]))
boxplot(assetr.mz[,6],outchar=T, main="Boxplot", col="navy")
abline(h=median(assetr.mz[,6]),lwd=3,lty=1,col="white")
plot(density(assetr.mz[,6]),type="l", main="Densidad suavizada",
     xlab="rendimiento mensual", ylab="estimado de densidad", col="navy")
qqnorm(assetr.mz[,6], col="navy")
qqline(assetr.mz[,6])

####################################
####################################
estadis.df <- assetr.melt %.% group_by(activos) %.% summarise( 
  mean=mean(retornos),variance=var(retornos),sd=sd(retornos),
  skewness=skewness(retornos),kurtosis=kurtosis(retornos),
  q1=quantile(retornos,0.01),q5=quantile(retornos,0.05))
estadis.df<- as.data.frame(estadis.df)
estadis.df$mean <- percent(as.numeric(as.character(estadis.df$mean)))
estadis.df$sd <- percent(as.numeric(as.character(estadis.df$sd)))

####################################
####################################

mu.vec = apply(assetr.mz,2,mean)
sd.vec = apply(assetr.mz,2,sd)
plot(sd.vec, mu.vec,  ylim=c(0, 0.03), xlim=c(0, 0.10), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=19, col=1:6, cex=1, cex.lab=1,cex.axis=0.6)     
text(sd.vec[-4], mu.vec[-4], labels=asset.names[-4], pos=4, cex = 1)
text(sd.vec[4], mu.vec[4], labels="veurx", pos=2, cex = 1)

####################################
### Errores Estandar
####################################

muhat.vals <- apply(assetr.mz,2,mean)
sigma2hat.vals <- apply(assetr.mz,2,var)
sigmahat.vals <- apply(assetr.mz,2,sd)
cov.mat = var(asset.r)
cor.mat = cor(asset.r)
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- c("adbe,vbltx","orcl,vbltx",
           "veurx,vbltx","veiex,vbltx","vfinx,vbltx","orcl,adbe",
           "veurx,adbe","veiex,adbe","vfinx,adbe","veurx,orcl",
           "veiex,orcl","vfinx,orcl","veiex,veurx","vfinx,veurx","vfinx,veiex")
nobs <- nrow(assetr.mz)
se.muhat = sigmahat.vals/sqrt(nobs)
se.sigma2hat = sigma2hat.vals/sqrt(nobs/2)
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.rhohat = (1-rhohat.vals^2)/sqrt(nobs)
se.df <- cbind(muhat.vals,se.muhat,sigma2hat.vals,se.sigma2hat,
               sigmahat.vals,se.sigmahat)

####################################
####################################
precision.df <- data.frame(muhat.vals/se.muhat,sigmahat.vals/se.sigmahat)

####################################
####################################
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
mu.width = mu.upper - mu.lower
sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat
sigma.width = sigma.upper - sigma.lower
mu.sigma.ic <- cbind(mu.lower,mu.upper,mu.width,sigma.lower,
                     sigma.upper,sigma.width)

####################################
### bootstraping
####################################

library(boot)
mean.boot = function(x, idx) {     # funcion para el bootstrap de la media
  ans = mean(x[idx])
  ans
}
vbltx.mean.boot = boot(assetr.mz[,1], statistic = mean.boot, R=999)
adbe.mean.boot = boot(assetr.mz[,2], statistic = mean.boot, R=999)
orcl.mean.boot = boot(assetr.mz[,3], statistic = mean.boot, R=999)
veurx.mean.boot = boot(assetr.mz[,4], statistic = mean.boot, R=999)
veiex.mean.boot = boot(assetr.mz[,5], statistic = mean.boot, R=999)
vfinx.mean.boot = boot(assetr.mz[,6], statistic = mean.boot, R=999)
mu.vbltx <- vbltx.mean.boot$t0  # estimado original
mu.boot.vbltx <- mean(vbltx.mean.boot$t) # bootstrap media
se.mu.vbltx <- se.muhat[1] # SE analitico
se.boot.vbltx <- sd(vbltx.mean.boot$t)  # SE.boot
bias.boot.vbltx <- mean(vbltx.mean.boot$t) - vbltx.mean.boot$t0  # bias
mu.adbe <- adbe.mean.boot$t0  # estimado original
mu.boot.adbe <- mean(adbe.mean.boot$t) # bootstrap media
se.mu.adbe <- se.muhat[2] # SE analitico
se.boot.adbe <- sd(adbe.mean.boot$t)  # SE.boot
bias.boot.adbe <- mean(adbe.mean.boot$t) - adbe.mean.boot$t0  # bias
mu.orcl <-  orcl.mean.boot$t0  # estimado original
mu.boot.orcl <- mean(orcl.mean.boot$t) # bootstrap media
se.mu.orcl <- se.muhat[3] # SE analitico
se.boot.orcl <- sd(orcl.mean.boot$t)  # SE.boot
bias.boot.orcl <- mean(orcl.mean.boot$t) - orcl.mean.boot$t0  # bias
mu.veurx <-  veurx.mean.boot$t0  # estimado original
mu.boot.veurx <-  mean(veurx.mean.boot$t) # bootstrap media
se.mu.veurx <- se.muhat[4] # SE analitico
se.boot.veurx <- sd(veurx.mean.boot$t)  # SE.boot
bias.boot.veurx <- mean(veurx.mean.boot$t) - veurx.mean.boot$t0  # bias
mu.veiex <-  veiex.mean.boot$t0  # estimado original
mu.boot.veiex <- mean(veurx.mean.boot$t) # bootstrap media
se.mu.veiex <- se.muhat[5] # SE analitico
se.boot.veiex <- sd(veiex.mean.boot$t)  # SE.boot
bias.boot.veiex <- mean(veiex.mean.boot$t) - veiex.mean.boot$t0  # bias
mu.vfinx <-  vfinx.mean.boot$t0  # estimado original
mu.boot.vfinx <- mean(vfinx.mean.boot$t) # bootstrap media
se.mu.vfinx <- se.muhat[6] # SE analitico
se.boot.vfinx <- sd(vfinx.mean.boot$t)  # SE.boot
bias.boot.vfinx <-mean(vfinx.mean.boot$t) - vfinx.mean.boot$t0  # bias
meanboot.mz <- data.frame(matrix(c(mu.vbltx,mu.boot.vbltx,se.mu.vbltx,
              se.boot.vbltx,bias.boot.vbltx,mu.adbe,mu.boot.adbe,
              se.mu.adbe,se.boot.adbe,bias.boot.adbe,
              mu.orcl,mu.boot.orcl,se.mu.orcl,se.boot.orcl,bias.boot.orcl,
              mu.veurx,mu.boot.veurx,se.mu.veurx,se.boot.veurx,bias.boot.veurx,
              mu.veiex,mu.boot.veiex,se.mu.veiex,se.boot.veiex,bias.boot.veiex,
              mu.vfinx,mu.boot.vfinx,se.mu.vfinx,se.boot.vfinx,
              bias.boot.vfinx),6,5,byrow=T))

####################################
### bootstraping SD
####################################
sd.boot = function(x, idx) {     # funcion para el bootstrap de la sd
  ans = sd(x[idx])
  ans
}
vbltx.sd.boot = boot(assetr.mz[,1], statistic = sd.boot, R=999)
adbe.sd.boot = boot(assetr.mz[,2], statistic = sd.boot, R=999)
orcl.sd.boot = boot(assetr.mz[,3], statistic = sd.boot, R=999)
veurx.sd.boot = boot(assetr.mz[,4], statistic = sd.boot, R=999)
veiex.sd.boot = boot(assetr.mz[,5], statistic = sd.boot, R=999)
vfinx.sd.boot = boot(assetr.mz[,6], statistic = sd.boot, R=999)
sd.vbltx <- vbltx.sd.boot$t0  # estimado original
sd.boot.vbltx <- mean(vbltx.sd.boot$t) # bootstrap sd
se.sigma.vblt <- se.sigmahat[1] # SE analitico
se.bootsd.vbltx <- sd(vbltx.sd.boot$t)  # SE.boot
bias.bootsd.vbltx <- mean(vbltx.sd.boot$t) - vbltx.mean.boot$t0  # bias
sd.adbe <- adbe.sd.boot$t0  # estimado original
sd.boot.adbe <- mean(adbe.sd.boot$t) # bootstrap sd
se.sigma.adbe <- se.sigmahat[2] # SE analitico
se.bootsd.adbe <- sd(adbe.sd.boot$t)  # SE.boot
bias.bootsd.adbe <- mean(adbe.sd.boot$t) - adbe.sd.boot$t0  # bias
sd.orcl <-  orcl.sd.boot$t0  # estimado original
sd.boot.orcl <- mean(orcl.sd.boot$t) # bootstrap sd
se.sigma.orcl <- se.sigmahat[3] # SE analitico
se.bootsd.orcl <- sd(orcl.sd.boot$t)  # SE.boot
bias.bootsd.orcl <- mean(orcl.sd.boot$t) - orcl.sd.boot$t0  # bias
sd.veurx <-  veurx.sd.boot$t0  # estimado original
sd.bootsd.veurx <-  mean(veurx.sd.boot$t) # bootstrap sd
se.sigma.veurx <- se.sigmahat[4] # SE analitico
se.bootsd.veurx <- sd(veurx.sd.boot$t)  # SE.boot
bias.bootsd.veurx <- mean(veurx.sd.boot$t) - veurx.sd.boot$t0  # bias
sd.veiex <-  veiex.sd.boot$t0  # estimado original
sd.boot.veiex <- mean(veurx.sd.boot$t) # bootstrap sd
se.sigma.veiex <- se.sigmahat[5] # SE analitico
se.bootsd.veiex <- sd(veiex.sd.boot$t)  # SE.boot
bias.bootsd.veiex <- mean(veiex.sd.boot$t) - veiex.sd.boot$t0  # bias
sd.vfinx <-  vfinx.sd.boot$t0  # estimado original
sd.boot.vfinx <- mean(vfinx.sd.boot$t) # bootstrap sd
se.sigma.vfinx <- se.sigmahat[6] # SE analitico
se.bootsd.vfinx <- sd(vfinx.sd.boot$t)  # SE.boot
bias.bootsd.vfinx <-mean(vfinx.mean.boot$t) - vfinx.mean.boot$t0  # bias
sigmaboot.mz <- matrix(c(sd.vbltx,sd.boot.vbltx,se.sigma.vblt,se.bootsd.vbltx,
              bias.bootsd.vbltx,sd.adbe,sd.boot.adbe,se.sigma.adbe,
              se.bootsd.adbe,bias.bootsd.adbe,
              sd.orcl,sd.boot.orcl,se.sigma.orcl,se.bootsd.orcl,
              bias.bootsd.orcl,sd.veurx,sd.bootsd.veurx,se.sigma.veurx,
              se.bootsd.veurx,bias.bootsd.veurx,sd.veiex,sd.boot.veiex,
              se.sigma.veiex,se.bootsd.veiex,bias.bootsd.veiex,
              sd.vfinx,sd.boot.vfinx,se.sigma.vfinx,se.bootsd.vfinx,
              bias.bootsd.vfinx),6,5,byrow=T)

####################################
### bootstraping IC
####################################
vbltx.boot.ci <-  boot.ci(vbltx.mean.boot, conf = 0.95, type =c("norm","perc"))
adbe.boot.ci <-  boot.ci(adbe.mean.boot, conf = 0.95, type =c("norm","perc"))
orcl.boot.ci <-  boot.ci(orcl.mean.boot, conf = 0.95, type =c("norm","perc"))
veurx.boot.ci <-  boot.ci(veurx.mean.boot, conf = 0.95, type =c("norm","perc"))
veiex.boot.ci <-  boot.ci(veiex.mean.boot, conf = 0.95, type =c("norm","perc"))
vfinx.boot.ci <-  boot.ci(vfinx.mean.boot, conf = 0.95, type =c("norm","perc"))
mu.boot.vbltx.lower <- vbltx.boot.ci$normal[2]
mu.boot.vbltx.upper <- vbltx.boot.ci$normal[3]
mu.boot.adbe.lower <- adbe.boot.ci$normal[2]
mu.boot.adbe.upper <- adbe.boot.ci$normal[3]
mu.boot.orcl.lower <- orcl.boot.ci$normal[2]
mu.boot.orcl.upper <- orcl.boot.ci$normal[3]
mu.boot.veurx.lower <- veurx.boot.ci$normal[2]
mu.boot.veurx.upper <- veurx.boot.ci$normal[3]
mu.boot.veiex.lower <- veiex.boot.ci$normal[2]
mu.boot.veiex.upper <- veiex.boot.ci$normal[3]
mu.boot.vfinx.lower <- vfinx.boot.ci$normal[2]
mu.boot.vfinx.upper <- vfinx.boot.ci$normal[3]
mu.boot.ci.mz <- matrix(c(mu.boot.vbltx.lower,mu.boot.vbltx.upper,
              mu.boot.adbe.lower,mu.boot.adbe.upper,mu.boot.orcl.lower,
              mu.boot.orcl.upper,mu.boot.veurx.lower,mu.boot.veurx.upper,
              mu.boot.veiex.lower,mu.boot.veiex.upper,mu.boot.vfinx.lower,
              mu.boot.vfinx.upper),6,2,byrow=T)
mu.ci.mz  <- cbind(mu.lower,mu.upper)
mu.boot.ci.df <- cbind(mu.boot.ci.mz,mu.ci.mz)

####################################
### bootstraping IC sd
####################################
vbltx.bootsd.ci <-  boot.ci(vbltx.sd.boot, conf = 0.95, type =c("norm","perc"))
adbe.bootsd.ci <-  boot.ci(adbe.sd.boot, conf = 0.95, type =c("norm","perc"))
orcl.bootsd.ci <-  boot.ci(orcl.sd.boot, conf = 0.95, type =c("norm","perc"))
veurx.bootsd.ci <-  boot.ci(veurx.sd.boot, conf = 0.95, type =c("norm","perc"))
veiex.bootsd.ci <-  boot.ci(veiex.sd.boot, conf = 0.95, type =c("norm","perc"))
vfinx.bootsd.ci <-  boot.ci(vfinx.sd.boot, conf = 0.95, type =c("norm","perc"))
sd.boot.vbltx.lower <- vbltx.bootsd.ci$normal[2]
sd.boot.vbltx.upper <- vbltx.bootsd.ci$normal[3]
sd.boot.adbe.lower <- adbe.bootsd.ci$normal[2]
sd.boot.adbe.upper <- adbe.bootsd.ci$normal[3]
sd.boot.orcl.lower <- orcl.bootsd.ci$normal[2]
sd.boot.orcl.upper <- orcl.bootsd.ci$normal[3]
sd.boot.veurx.lower <- veurx.bootsd.ci$normal[2]
sd.boot.veurx.upper <- veurx.bootsd.ci$normal[3]
sd.boot.veiex.lower <- veiex.bootsd.ci$normal[2]
sd.boot.veiex.upper <- veiex.bootsd.ci$normal[3]
sd.boot.vfinx.lower <- vfinx.bootsd.ci$normal[2]
sd.boot.vfinx.upper <- vfinx.bootsd.ci$normal[3]
sd.boot.ci.mz <- matrix(c(sd.boot.vbltx.lower,sd.boot.vbltx.upper,
              sd.boot.adbe.lower,sd.boot.adbe.upper,sd.boot.orcl.lower,
              sd.boot.orcl.upper,sd.boot.veurx.lower,sd.boot.veurx.upper,
              sd.boot.veiex.lower,sd.boot.veiex.upper,sd.boot.vfinx.lower,
              sd.boot.vfinx.upper),6,2,byrow=T)
sd.ci.mz  <- cbind(sigma.lower,sigma.upper)
sd.boot.ci.df <- cbind(sd.boot.ci.mz,sd.ci.mz)

####################################
### VaR Parametrica/Normal
####################################

Value.at.Risk = function(x,p=0.05,w=100000) {         # funcion VaR
  x = as.matrix(x)
  q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}
# 5% y 1% VaR mensuales estimados basados en  W0 = 100000
VaR5 <- Value.at.Risk(assetr.mz,p=0.05,w=100000)
VaR1 <- Value.at.Risk(assetr.mz,p=0.01,w=100000)
VaR.df <- cbind(VaR1,VaR5)

# 5% y 1% VaR anuales estimados basados en  W0 = 100000 
Value.at.Risk.yr = function(x,p=0.05,w=100000) {           # funcion VaR anual
  x = as.matrix(x)
  q = (apply(x, 2, mean))*12 + sqrt(apply(x, 2, sd)*12)*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}
VaR5.yr <- Value.at.Risk.yr(assetr.mz,p=0.05,w=100000)
VaR1.yr <- Value.at.Risk.yr(assetr.mz,p=0.01,w=100000)
VaR.yr.df <- cbind(VaR1.yr,VaR5.yr)
VaR.dff <- cbind(VaR.df,VaR.yr.df)

####################################
### VaR no Parametrica
####################################
## Mediddas de riesgo no parametricas
## 5% y 1% Cuantiles Empiricos (VaR Historica)
q.hs <- apply(assetr.mz, 2, quantile, probs=c(0.01, 0.05))
rownames(q.hs) <- c("q.01","q.05")
VaR.hs <- 100000*(exp(q.hs) - 1)
VaR.hs.df <- t(rbind(q.hs,VaR.hs))


####################################
### Portafolios con Cortos
####################################

# portafolio gmv con cortos
gmv <- globalMin.portfolio(muhat.vals, sigma.mat)
gmv.p <- as.data.frame(gmv$weights)
gmv.p$activos <- row.names(gmv.p)
gmv.p <- data.frame(activos=gmv.p[,2],pesos=gmv.p[,1])

# Retornos, Desviacion estandar Portafolio con cortos
mu.gmv <- gmv$er
sd.gmv <- gmv$sd
VaRs.gmv05 <- (exp(mu.gmv+sd.gmv*qnorm(0.05))-1)*100000
VaRs.gmv01 <- (exp(mu.gmv+sd.gmv*qnorm(0.01))-1)*100000
sharpe.gmv <- (mu.gmv-rf.m)/sd.gmv
gmvs.df <- rbind(mu.gmv,sd.gmv,VaRs.gmv05,VaRs.gmv01,sharpe.gmv)

# Retornos, Desviacion estandar Portafolio con cortos(anuales)
mu.gmv.y <- mu.gmv*12
sd.gmv.y <- sqrt(sd.gmv*12)
VaRs.gmv05.y <- (exp(mu.gmv.y+sd.gmv.y*qnorm(0.05))-1)*100000
VaRs.gmv01.y <- (exp(mu.gmv.y+sd.gmv.y*qnorm(0.01))-1)*100000
sharpe.gmv.y <- (mu.gmv.y-rf)/sd.gmv.y
gmvs.df.y <- rbind(mu.gmv.y,sd.gmv.y,VaRs.gmv05.y,VaRs.gmv01.y,sharpe.gmv.y)

# tabla gmv con cortos retornos
gmv.df <- data.frame(r.mensuales=gmvs.df[,1] ,r.anuales=gmvs.df.y[,1])
gmv.df$r.mensuales[c(1,2,5)] <- percent(as.numeric(as.character(gmv.df$r.mensuales[c(1,2,5)])))
gmv.df$r.mensuales[c(3,4)] <- dollar(as.numeric(as.character(gmv.df$r.mensuales[c(3,4)])))
gmv.df$r.anuales[c(1,2,5)] <- percent(as.numeric(as.character(gmv.df$r.anuales[c(1,2,5)])))
gmv.df$r.anuales[c(3,4)] <- dollar(as.numeric(as.character(gmv.df$r.anuales[c(3,4)])))

ggplot(gmv.p,aes(x=activos,y=pesos)) +
  geom_bar(stat = "identity", fill = "navy") + 
  geom_text(aes(label = paste(round(pesos,2) * 100, "%"),
                vjust = ifelse(pesos >= 0, -0.5, 1.2))) +
  scale_y_continuous("Pesos del Portafolio", labels = percent,
                     breaks=pretty_breaks(n=8)) +
  theme(axis.title.x = element_blank(),
        title=element_text(vjust=0.9)) +
  ggtitle("Portafolio Global Mínima Varianza con Cortos")

####################################
### Portafolios Eficiente con Cortos
####################################

# portafolio eficiente con el retorno esperado mas alto(Markowitz)
target.return <- muhat.vals[muhat.vals ==max(muhat.vals)]
ef.s <- efficient.portfolio(muhat.vals, sigma.mat, target.return)
ef.sh <- as.data.frame(ef.s$weights)
ef.sh$activos <- row.names(ef.sh)
ef.sh.df <- data.frame(activos=ef.sh[,2],pesos=ef.sh[,1])

# Retornos, Desviacion estandar Portafolio eficiente con cortos
mu.ef <- ef.s$er
sd.ef <- ef.s$sd
e.VaRs.05 <- (exp(mu.ef+sd.ef*qnorm(0.05))-1)*100000
e.VaRs.01 <- (exp(mu.ef+sd.ef*qnorm(0.01))-1)*100000
sharpe.ef <- (mu.ef-rf.m)/sd.ef
ef.df <- rbind(mu.ef,sd.ef,e.VaRs.05 ,e.VaRs.01,sharpe.ef)

# Retornos, Desviacion estandar Portafolio eficiente con cortos (anuales)
mu.ef.y <- mu.ef*12
sd.ef.y <- sqrt(sd.ef*12)
e.VaRs.05.y <- (exp(mu.ef.y+sd.ef.y*qnorm(0.05))-1)*100000
e.VaRs.01.y <- (exp(mu.ef.y+sd.ef.y*qnorm(0.01))-1)*100000
sharpe.ef.y <- (mu.ef.y-rf)/sd.ef.y
ef.df.y <- rbind(mu.ef.y,sd.ef.y,e.VaRs.05.y ,e.VaRs.01.y,sharpe.ef.y)

# tabla ef.p con cortos retornos
efs.df <- data.frame(r.mensuales=ef.df[,1] ,r.anuales=ef.df.y[,1])
efs.df$r.mensuales[c(1,2,5)] <- percent(as.numeric(as.character(efs.df$r.mensuales[c(1,2,5)])))
efs.df$r.mensuales[c(3,4)] <- dollar(as.numeric(as.character(efs.df$r.mensuales[c(3,4)])))
efs.df$r.anuales[c(1,2,5)] <- percent(as.numeric(as.character(efs.df$r.anuales[c(1,2,5)])))
efs.df$r.anuales[c(3,4)] <- dollar(as.numeric(as.character(efs.df$r.anuales[c(3,4)])))

    ggplot(ef.sh.df,aes(x=activos,y=pesos)) +
    geom_bar(stat = "identity", fill = "navy") + 
    geom_text(aes(label = paste(round(pesos,2) * 100, "%"),
                  vjust = ifelse(pesos >= 0, -0.5, 1.2))) +
    scale_y_continuous("Pesos del Portafolio", labels = percent,
                       breaks=pretty_breaks(n=8)) +
    theme(axis.title.x = element_blank(),
          title=element_text(vjust=0.9)) +
    ggtitle("Portafolio Eficiente ")

####################################
### Portafolios Tangente con Cortos
####################################

# PORTAFOLIO DE LA TANGENTE
rf <- 0.005 # anual
rf.m <- rf/12 # mensual
t.port <- tangency.portfolio(muhat.vals, sigma.mat, rf.m)
t.port.s <- as.data.frame(t.port$weights)
t.port.s$activos <- row.names(t.port.s)
t.port.s <- data.frame(activos=t.port.s[,2],pesos=t.port.s[,1])

# Retornos, Desviacion estandar Portafolio tangente con cortos
mu.t <- t.port$er
sig.t <- t.port$sd
t.VaRs.05 <- (exp(mu.t+sig.t*qnorm(0.05))-1)*100000
t.VaRs.01 <- (exp(mu.t+sig.t*qnorm(0.01))-1)*100000
sharpe.t <- (mu.t-rf.m)/sig.t
t.df <- rbind(mu.t,sig.t ,t.VaRs.05 ,t.VaRs.01,sharpe.t)

# Retornos, Desviacion estandar Portafolio tangente con cortos (anuales)
mu.t.y <- mu.t*12
sig.t.y <- sqrt(sig.t*12)
t.VaRs.05.y <- (exp(mu.t.y+sig.t.y*qnorm(0.05))-1)*100000
t.VaRs.01.y <- (exp(mu.t.y+sig.t.y*qnorm(0.01))-1)*100000
sharpe.t.y <- (mu.t.y-rf)/sig.t.y
t.df.y <- rbind(mu.t.y,sig.t.y ,t.VaRs.05.y ,t.VaRs.01.y,sharpe.t.y)

# tabla t.port con cortos retornos
tp.df <- data.frame(r.mensuales=t.df[,1] ,r.anuales=t.df.y[,1])
tp.df$r.mensuales[c(1,2,5)] <- percent(as.numeric(as.character(tp.df$r.mensuales[c(1,2,5)])))
tp.df$r.mensuales[c(3,4)] <- dollar(as.numeric(as.character(tp.df$r.mensuales[c(3,4)])))
tp.df$r.anuales[c(1,2,5)] <- percent(as.numeric(as.character(tp.df$r.anuales[c(1,2,5)])))
tp.df$r.anuales[c(3,4)] <- dollar(as.numeric(as.character(tp.df$r.anuales[c(3,4)])))

    ggplot(t.port.s,aes(x=activos,y=pesos)) +
    geom_bar(stat = "identity", fill = "navy") + 
    geom_text(aes(label = paste(round(pesos,2) * 100, "%"),
                  vjust = ifelse(pesos >= 0, -0.5, 1.2))) +
    scale_y_continuous("Pesos del Portafolio", labels = percent,
                       breaks=pretty_breaks(n=8)) +
    theme(axis.title.x = element_blank(),
          title=element_text(vjust=0.9)) +
    ggtitle("Portafolio de T-Bills y la Tangente con cortos")

####################################
### Frontera Eficiente
####################################

# Frontera eficiente
x.vec <- ef.s$weights
mu.px <- ef.s$er
sig2.px <- (ef.s$sd)^2

m.vec <- gmv$weights
mu.gmin <- gmv$er
sig2.gmin <- gmv$sd^2

a = seq(from=1, to=-1, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, 6)
colnames(z.mat) <- asset.names
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec
for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px +
    2*a[i]*(1-a[i])*sig.mx
}

# grafica portafolio eficiente de activos y activo libre de riesgo
front.efi <- data.frame(mu=mu.z,sigma=sqrt(sig2.z))
asset.df <- data.frame(mu=muhat.vals,sigma=sigmahat.vals)
ef.port <- data.frame(mu=ef.s$er,sigma=ef.s$sd)
t.portf <- data.frame(mu=t.port$er,sigma=t.port$sd)
t.sharpe <- (t.port$er - rf.m)/t.port$sd
rf.df <- data.frame(rf.m,t.sharpe)

    ggplot(data=rbind(front.efi,asset.df,ef.port,t.portf),aes(x=sigma,y=mu)) +
    geom_point(colour="blue") +
    geom_point(data=subset(front.efi, sigma==min(sigma)),colour="red",size=3) +
    geom_point(data=asset.df,aes(x=sigma,y=mu),
               colour="orange",size=3) +
    geom_point(data=ef.port,aes(x=sigma,y=mu),colour="green",size=3) +
    geom_point(data=t.portf,aes(x=sigma,y=mu),colour="green",size=3.2) +
    annotate("text", x=0.021, y=0.0092, label="GMV", color="red")+
    geom_text(data=asset.df,aes(label=row.names(asset.df)),
              hjust=0, vjust=-0.4) +
    geom_text(data=ef.port,aes(label="ef"),
              hjust=1.2, vjust=-0.3) +
    geom_text(data=t.portf,aes(label="tangente"),
              hjust=-0.2, vjust=0.3) +
    annotate("text", x=0.03, y=0.027, label="Portafolios de T-Bills y
             Portafolio Tangente", color="red") +
    annotate("text", x=0.073, y=0.022, label="Frontera Eficiente 
             (Sin T-Bills)", color="red") +
    geom_abline(data=rf.df, aes(slope=t.sharpe,intercept=rf.m),colour="navy") +
    scale_y_continuous(labels = percent,breaks=pretty_breaks(n=10)) +
    scale_x_continuous(labels = percent,breaks=pretty_breaks(n=10))

####################################
### Pesos Portafolios  Eficientes
####################################

ef.w <- data.frame(z.mat,sigma=factor(round(sqrt(sig2.z),4)*100))
ef.melt <- melt(ef.w,id="sigma", measure.vars=c("vbltx","adbe","orcl",
        "veurx","veiex","vfinx"),value.name="pesos",variable.name="activos")
ef.p1 <- subset(ef.melt,pesos >= 0)
ef.p2 <- subset(ef.melt,pesos < 0)

ggplot() + geom_bar(data=ef.p1,aes(x=sigma,y=pesos,
                                   fill=activos),stat = "identity") +
  geom_bar(data=ef.p2 ,aes(x=sigma,y=pesos,
                           fill=activos),stat = "identity") +
  scale_fill_brewer(type = "seq", palette = "BrBG") +
  scale_y_continuous(labels = percent,breaks=pretty_breaks(n=10)) + 
  ggtitle("Pesos Portafolios Eficientes") + 
  xlab("Portafolio Desviación Estandar") +
  theme(title=element_text(vjust=0.9,size=9),axis.title.x=
          element_text(vjust=-0.5),axis.text=element_text(size=7)) 

####################################
### Pesos Portafolios  Eficientes T-bills y Port Tangente
####################################

# portafolios eficientes de t-bills y Portafolio de la Tangente
x.t = seq(0, 2, by=0.1)
mu.pe = rf.m + x.t*(mu.t  - rf.m)
sig.pe = x.t*sig.t 
slope.t = (mu.t - rf.m)/sig.t

# plot weights
t.vec <- t.port$weights
t.mat = x.t %o% t.vec
e.mat = cbind(1-x.t, t.mat)
colnames(e.mat)[1] = "T-Bill"

t.w <- data.frame(e.mat,sigma=factor(round(sig.pe,4)*100))
t.melt <- melt(t.w,id="sigma", measure.vars=c( "T.Bill","vbltx","adbe","orcl",
                                               "veurx","veiex","vfinx"),value.name="pesos",variable.name="activos")
t.p1 <- subset(t.melt,pesos >= 0)
t.p2 <- subset(t.melt,pesos < 0)

ggplot() + geom_bar(data=t.p1,aes(x=sigma,y=pesos,
                                  fill=activos),stat = "identity") +
  geom_bar(data=t.p2 ,aes(x=sigma,y=pesos,
                          fill=activos),stat = "identity") +
  scale_fill_brewer(type = "seq", palette = "BrBG") +
  scale_y_continuous(labels = percent,breaks=pretty_breaks(n=10)) + 
  ggtitle("Pesos Portafolios Eficientes de T-Bills y Portafolio de la Tangente") +
  xlab("Portafolio Desviación Estandar") +
  theme(title=element_text(vjust=0.9,size=9),axis.title.x =
          element_text(vjust=-0.5),axis.text=element_text(size=7)) 

####################################
### Portafolio sin cortos
####################################

# portafolio gmv sin cortos
gmv.ns <- globalMin.portfolio(muhat.vals, sigma.mat,shorts=FALSE)
gmv.nsh <- as.data.frame(gmv.ns$weights)
gmv.nsh$activos <- row.names(gmv.nsh)
gmv.nsh <- data.frame(activos=gmv.nsh[,2],pesos=gmv.nsh[,1])

# Retornos, Desviacion estandar Portafolio sin cortos
mu.gmv.ns <- gmv.ns$er
sd.gmv.ns <- gmv.ns$sd
VaRns.gmv05 <- (exp(mu.gmv.ns+sd.gmv.ns*qnorm(0.05))-1)*100000
VaRns.gmv01 <- (exp(mu.gmv.ns+sd.gmv.ns*qnorm(0.01))-1)*100000
sharpe.gmvns <- (mu.gmv.ns-rf.m)/sd.gmv.ns
gmvns.df <- rbind(mu.gmv.ns,sd.gmv.ns,VaRns.gmv05,VaRns.gmv01,sharpe.gmvns)

# Retornos, Desviacion estandar Portafolio sin cortos
mu.gmv.ns.y <- mu.gmv.ns*12
sd.gmv.ns.y <- sqrt(sd.gmv.ns*12)
VaRns.gmv05.y <- (exp(mu.gmv.ns.y+sd.gmv.ns.y*qnorm(0.05))-1)*100000
VaRns.gmv01.y <- (exp(mu.gmv.ns.y+sd.gmv.ns.y*qnorm(0.01))-1)*100000
sharpe.gmvns.y <- (mu.gmv.ns.y-rf)/sd.gmv.ns.y
gmvns.df.y <- rbind(mu.gmv.ns.y,sd.gmv.ns.y,VaRns.gmv05.y,VaRns.gmv01.y,sharpe.gmvns.y)

# tabla gmv sin cortos retornos
gmvns.df <- data.frame(r.mensuales=gmvns.df[,1] ,r.anuales=gmvns.df.y[,1])
gmvns.df$r.mensuales[c(1,2,5)] <- percent(as.numeric(as.character(gmvns.df$r.mensuales[c(1,2,5)])))
gmvns.df$r.mensuales[c(3,4)] <- dollar(as.numeric(as.character(gmvns.df$r.mensuales[c(3,4)])))
gmvns.df$r.anuales[c(1,2,5)] <- percent(as.numeric(as.character(gmvns.df$r.anuales[c(1,2,5)])))
gmvns.df$r.anuales[c(3,4)] <- dollar(as.numeric(as.character(gmvns.df$r.anuales[c(3,4)])))

    ggplot(gmv.nsh,aes(x=activos,y=pesos)) +
    geom_bar(stat = "identity", fill = "navy") + 
    geom_text(aes(label = paste(round(pesos,2) * 100, "%"),
                  vjust = ifelse(pesos >= 0, -0.5, 1.2))) +
    scale_y_continuous("Pesos del Portafolio", labels = percent,
                       breaks=pretty_breaks(n=8)) +
    theme(axis.title.x = element_blank(),
          title=element_text(vjust=0.9)) +
    ggtitle("Portafolio Global Mínima Varianza sin Cortos")

####################################
### Portafolio Tangente sin cortos
####################################

# PORTAFOLIO DE LA TaNGENTE sin cortos
t.port.n <- tangency.portfolio(muhat.vals, sigma.mat, rf.m,shorts=F)
t.port.ns <- as.data.frame(t.port.n$weights)
t.port.ns$activos <- row.names(t.port.ns)
t.port.ns <- data.frame(activos=t.port.ns[,2],pesos=t.port.ns[,1])

# Retornos, Desviacion estandar Portafolio tangente sin cortos
mu.tns <- t.port.n$er
sig.tns <- t.port.n$sd
t.VaRns.05 <- (exp(mu.tns+sig.tns*qnorm(0.05))-1)*100000
t.VaRns.01 <- (exp(mu.tns+sig.tns*qnorm(0.01))-1)*100000
sharpe.tn <- (mu.tns-rf.m)/sig.tns
t.df.n <- rbind(mu.tns,sig.tns ,t.VaRns.05 ,t.VaRns.01,sharpe.tn)

# Retornos, Desviacion estandar Portafolio tangente sin cortos (anuales)
mu.tns.y <- mu.tns*12
sig.tns.y <- sqrt(sig.tns*12)
t.VaRns.05.y <- (exp(mu.tns.y+sig.tns.y*qnorm(0.05))-1)*100000
t.VaRns.01.y <- (exp(mu.tns.y+sig.tns.y*qnorm(0.01))-1)*100000
sharpe.tn.y <- (mu.tns.y-rf)/sig.tns.y
t.dfns.y <- rbind(mu.tns.y,sig.tns.y ,t.VaRns.05.y ,t.VaRns.01.y,sharpe.tn.y)

# tabla t.port sin cortos retornos
tns.df <- data.frame(r.mensuales=t.df.n[,1] ,r.anuales=t.dfns.y[,1])
tns.df$r.mensuales[c(1,2,5)] <- percent(as.numeric(as.character(tns.df$r.mensuales[c(1,2,5)])))
tns.df$r.mensuales[c(3,4)] <- dollar(as.numeric(as.character(tns.df$r.mensuales[c(3,4)])))
tns.df$r.anuales[c(1,2,5)] <- percent(as.numeric(as.character(tns.df$r.anuales[c(1,2,5)])))
tns.df$r.anuales[c(3,4)] <- dollar(as.numeric(as.character(tns.df$r.anuales[c(3,4)])))

    ggplot(t.port.ns,aes(x=activos,y=pesos)) +
    geom_bar(stat = "identity", fill = "navy") + 
    geom_text(aes(label = paste(round(pesos,2) * 100, "%"),
                  vjust = ifelse(pesos >= 0, -0.5, 1.2))) +
    scale_y_continuous("Pesos del Portafolio", labels = percent,
                       breaks=pretty_breaks(n=8)) +
    theme(axis.title.x = element_blank(),
          title=element_text(vjust=0.9)) +
    ggtitle("Portafolio de la Tangente sin cortos")

####################################
### Portafolio Eficiente retorno objetivo 9%
####################################

# portafolio eficiente2 con el retorno objetivo anual 9% sin cortos
target.return2 <- 0.09/12
ef2.nsh <- efficient.portfolio(muhat.vals, sigma.mat, target.return2,shorts=F)
ef2.ns <- as.data.frame(ef2.nsh$weights)
ef2.ns$activos <- row.names(ef2.ns)
ef2.ns.df <- data.frame(activos=ef2.ns[,2],pesos=ef2.ns[,1])

# Retornos, Desviacion estandar Portafolio eficiente con cortos
mu.ef2 <- ef2.nsh$er
sd.ef2 <- ef2.nsh$sd
e2.VaRns.05 <- (exp(mu.ef2+sd.ef2*qnorm(0.05))-1)*100000
e2.VaRns.01 <- (exp(mu.ef2+sd.ef2*qnorm(0.01))-1)*100000
ef.sharpe <- (mu.ef2 - rf.m)/sd.ef2
ef2ns.df <- rbind(mu.ef2,sd.ef2,e2.VaRns.05 ,e2.VaRns.01,ef.sharpe)

# Retornos, Desviacion estandar Portafolio eficiente con cortos (anuales)
mu.ef2.y <- mu.ef2*12
sd.ef2.y <- sqrt(sd.ef2*12)
e2.VaRns.05.y <- (exp(mu.ef2.y+sd.ef2.y*qnorm(0.05))-1)*100000
e2.VaRns.01.y <- (exp(mu.ef2.y+sd.ef2.y*qnorm(0.01))-1)*100000
ef.sharpe.y <- (mu.ef2.y - rf)/sd.ef2.y
ef2ns.df.y <- rbind(mu.ef2.y,sd.ef2.y,e2.VaRns.05.y ,e2.VaRns.01.y,ef.sharpe.y)

# tabla portafolio eficiente retorno objetivo 9%
p.ef.df <- data.frame(r.mensuales=ef2ns.df[,1] ,r.anuales=ef2ns.df.y[,1])

    ggplot(ef2.ns.df,aes(x=activos,y=pesos)) +
    geom_bar(stat = "identity", fill = "navy") + 
    geom_text(aes(label = paste(round(pesos,2) * 100, "%"),
                  vjust = ifelse(pesos >= 0, -0.5, 1.2))) +
    scale_y_continuous("Pesos del Portafolio", labels = percent,
                       breaks=pretty_breaks(n=8)) +
    theme(axis.title.x = element_blank(),
          title=element_text(vjust=0.9)) +
    ggtitle("Portafolio Eficiente")

####################################
### Portafolio Eficiente retorno objetivo 9% T- Bill
####################################
# ponderaciones T-Bill y activos con Riesgo
xt.vec <- t.port.n$weights
xt <- (target.return2-rf.m)/(mu.tns-rf.m)  # pesos Activos con Riesgo
x.rf  <- 1-xt    # pesos T-Bill
x.rassets <- xt*xt.vec   # pesos por activos individuales
mut <- xt*mu.tns + x.rf*rf.m
sigt <- xt*sig.tns

xtll.vec <- c(T.bill=x.rf,x.rassets)
xt.df <- as.data.frame(xtll.vec)
xt.df$activos <- row.names(xt.df)
xt.dfr <- data.frame(activos=xt.df[,2],pesos=xt.df[,1]) # pesos

# Retornos, Desviacion estandar Portafolio eficiente con cortos (mensuales)
e2tll.VaRns.05 <- (exp(mut+sigt*qnorm(0.05))-1)*100000
e2tll.VaRns.01 <- (exp(mut +sigt*qnorm(0.01))-1)*100000
eftll.sharpe <- (mut - rf.m)/sigt
ef2tllns.df <- rbind(mut,sigt,e2tll.VaRns.05 ,e2tll.VaRns.01,eftll.sharpe)

# Retornos, Desviacion estandar Portafolio eficiente con cortos (anuales)
mut.y <- mut*12
sigt.y <- sqrt(sigt*12)
e2tll.VaRns.05.y <- (exp(mut.y+sigt.y*qnorm(0.05))-1)*100000
e2tll.VaRns.01.y <- (exp(mut.y +sigt.y*qnorm(0.01))-1)*100000
eftll.sharpe.y <- (mut.y - rf)/sigt.y
ef2tllns.df.y <- rbind(mut.y,sigt.y,e2tll.VaRns.05.y ,
                       e2tll.VaRns.01.y,eftll.sharpe.y)

# tabla portafolio eficiente retorno objetivo 9%
peftll.df <- data.frame(r.mensuales=ef2tllns.df[,1] ,r.anuales=ef2tllns.df.y[,1])
peftll.df$r.mensuales[c(1,2,5)] <- percent(as.numeric(as.character(peftll.df$r.mensuales[c(1,2,5)])))
peftll.df$r.mensuales[c(3,4)] <- dollar(as.numeric(as.character(peftll.df$r.mensuales[c(3,4)])))
peftll.df$r.anuales[c(1,2,5)] <- percent(as.numeric(as.character(peftll.df$r.anuales[c(1,2,5)])))
peftll.df$r.anuales[c(3,4)] <- dollar(as.numeric(as.character(peftll.df$r.anuales[c(3,4)])))

    ggplot(xt.dfr,aes(x=activos,y=pesos)) +
    geom_bar(stat = "identity", fill = "navy") + 
    geom_text(aes(label = paste(round(pesos,2) * 100, "%"),
                  vjust = ifelse(pesos >= 0, -0.5, 1.2))) +
    scale_y_continuous("Pesos del Portafolio", labels = percent,
                       breaks=pretty_breaks(n=8)) +
    theme(axis.title.x = element_blank(),
          title=element_text(vjust=0.9)) +
    ggtitle("Portafolio Eficiente con T-Bill")

####################################
### Risk Budgeting SD
####################################


efp.w <- ef2.nsh$weights
mcr.vol = sigma.mat%*%efp.w/sd.ef2
cr.vol = efp.w * mcr.vol
pcr.vol = cr.vol /sd.ef2
cbind(mcr.vol, cr.vol, pcr.vol)  
  
# usando la funcion StdDev() de performance analytics
StdDev(asset.r, portfolio_method="component",
        weights=efp.w)

####################################
### Risk Budgeting VaR
#################################### 
sigma.mat
betas <- (sigma.mat/sd.ef2)*AssetVaR
beta1 <- cov(muhat.vals,mu.ef2)/sd.ef2^2
mVaR <- sd.ef2*AssetVaR*(-1)
cr.VaR = data.frame(efp.w * mVaR)
VaR(asset.r,portfolio_method="component",
     weights=efp.w)

Inv.inicial <- 100000
weight <- efp.w
dollar.All <- weight*Inv.inicial
SD.assets <- sigmahat.vals
AssetVaR <- qnorm(0.05,muhat.vals,sigmahat.vals)
cVaR <- VaR(asset.r,portfolio_method="component",
            weights=efp.w)$contribution
cVaR.dollar <- cVaR*Inv.inicial
pcVaR <- VaR(asset.r,portfolio_method="component",
             weights=efp.w)$pct_contrib_MVaR

NormalVaR.Rpt <- data.frame(Asignacion=dollar(dollar.All),
            Pesos=percent(weight),SD=percent(SD.assets),VaR=percent(AssetVaR*(-1)),
            cVaR=percent(cVaR),dcVaR=dollar(cVaR.dollar),pcVaR=percent(pcVaR))

Portfolio.sum <- data.frame(Asignacion=dollar(sum(dollar.All)),Pesos=percent(sum(weight))
                ,VaR=percent(0),SD=percent(0),
                   cVaR=percent(sum(cVaR)),dcVaR=dollar(sum(cVaR.dollar)),
                pcVaR=percent(sum(pcVaR )))
VaR.Report <- rbind(NormalVaR.Rpt,Portfolio.sum)

####################################
### Risk Budgeting Expected shortfall
####################################

ES(asset.r,portfolio_method="component",
   weights=efp.w)

AssetES <- ES(asset.r, p=0.95, method="historical")
cES <- ES(asset.r,portfolio_method="component",
          weights=efp.w)$contribution
cES.dollar <- cES*Inv.inicial
pcES <- ES(asset.r,portfolio_method="component",
           weights=efp.w)$pct_contrib_MES

ES.Rpt <- data.frame(Asignacion=dollar(dollar.All),
                     Pesos=percent(weight),SD=percent(SD.assets),ES=percent(AssetES*(-1)),
                     cES=percent(cES),dcES=dollar(cES.dollar),pcES=percent(pcES))

Portfolio.sumES <- data.frame(Asignacion=dollar(sum(dollar.All)),
                              Pesos=percent(sum(weight)),ES=0,SD=0,
                              cES=percent(sum(cES)),
                              dcES=dollar(sum(cES.dollar)),pcES=percent(sum(pcES )))

ES.Report <- rbind(ES.Rpt,Portfolio.sumES)
######################################################
# ecuaciones analiticas para hallar la descomposicion de riesgo de VaR
mcr.VaR <- sigma.mat%*%efp.w/var.05 
var.05 <- exp(mu.ef2+sd.ef2*qnorm(0.05))-1
cr.VaR = efp.w * mcr.VaR
#######################################################

#############################################################
normalVaR <- function(mu, sigma, w0, tail.prob = 0.01, invert=FALSE) {
  if ( length(mu) != length(sigma) )
    stop("mu and sigma must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  VaR = w0*(mu + sigma*qnorm(tail.prob))
  if (invert) {
    VaR = -VaR
  }
  return(VaR)
}
normalVaR(mu=c(muhat.vals),sigma=c(sigmahat.vals),
                w0=1, tail.prob=c(0.05))

source("C:/users/usuario/Desktop/portfolio.r.txt")
source("C:/users/usuario/Desktop/portfolio_noshorts.r.txt")