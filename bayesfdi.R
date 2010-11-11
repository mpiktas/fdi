library(MCMCpack)
library(VGAM)
library(mcmc)
library(ggplot2)

###link function

rlink <- function(r) 0.2*r

a <- 1.35
q <- 3.3
scale <- 10/gamma(1 + 1/a)/gamma(q - 1/a)*  gamma(q) 

logvr <- function(x,link=function(r)r,sd,a,scl,q.arg) {
    if(x[2]>0) {
        dnorm(x[1],mean=link(x[2]),sd=sd,log=TRUE)+dsinmad(x[2],a=a,scale=scl,q.arg=q.arg,log=TRUE)
    }
    else -Inf
    
}

logvrls <- function(x,el,sdlog,a,scl,q.arg) {
    if(x[2]>0) {
        dlnorm(x[1],meanlog=el*log(x[2]),sdlog=sdlog,log=TRUE)+dsinmad(x[2],a=a,scale=scl,q.arg=q.arg,log=TRUE)
    }
    else -Inf    
}

mean(rsinmad(2000,a=a,scale=scale,q=q))


smpl <- MCMCmetrop1R(logvr,theta.init=c(2,2),link=function(r)0.2*r,sd=0.01,a=a,scale=scale,q.arg=q)

###This is not working, why?
smpl <- MCMCmetrop1R(logvr,theta.init=c(1,2),link=function(r)r^0.2,sd=0.01,a=a,scale=scale,q.arg=q,optim.lower=0,optim.method="L-BFGS-B",thin=10,mcmc=200000,burnin=140000)

smpl <- MCMCmetrop1R(logvrls,theta.init=c(2,2),el=0.2,sdlog=0.2,a=a,scale=scale,q.arg=q,thin=10)

smpl <- metrop(logvrls,initial=c(10,10),nbatch=1000,blen=1000,scale=0.5,el=0.2,sdlog=0.2,a=a,scl=scale,q.arg=q)

ssdata <- data.frame(v=smpl[,1],r=smpl[,2])

colnames(sdata) <- c("v","r")

lsinmad <- function(x,sample)-sum(dsinmad(sample,a=x[1],scale=x[2],q.arg=x[3],log=TRUE))

llnorm <- function(x,sample)-sum(dlnorm(sample,meanlog=x[1],sdlog=x[2],log=TRUE))

optim(c(2,1,2),lsinmad,method="BFGS",sample=smpl[,2])


chk <- vglm(r~1,sinmad,sdata,trace=TRUE)

tst <- vglm(v~1,sinmad,sdata,trace=TRUE)

