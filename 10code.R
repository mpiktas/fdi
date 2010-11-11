fix.sm.quant <- function(quantiles) {
    n <- length(quantiles)
    c(0,quantiles[-c(1,n)],Inf)
}

lorentz.bi <- function(smpl,quantiles) {
    ##smpl should be matrix with two columns. The first column is value,
    ##the second is income

    colnames(smpl) <- c("Value","Income")
    df <- data.frame(id=cut(smpl[,2],quantiles,right=FALSE),sweep(smpl,2,apply(smpl,2,mean),"/"))

    n <- dim(smpl)[1]
    df <- ddply(df,.(id),function(d)apply(d[,-1],2,sum)/n)
    res <- data.frame(id=df[,1],left=
               quantiles[-length(quantiles)],df[,-1])
    rownames(res) <- NULL
    res    
}
