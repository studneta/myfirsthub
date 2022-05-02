#' mymlnorm
#'
#' @param x the data, in vector form
#' @param mu the mean
#' @param sig the standard deviation
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
mymlnorm=function(x,mu,sig,...){
  nmu=length(mu)
  nsig=length(sig)
  n=length(x)
  zz=c()
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j])
    y=apply(z,2,sum)
    zz=cbind(zz,y)
  }
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),...)
  mlx=round(mean(x),2)
  mly=round(sqrt((n-1)/n)*sd(x),2)
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")
  muest=mu[coord[1]]
  sigest=sig[coord[2]]
  abline(v=muest, h=sigest)
  return(list(x=x,coord=coord,maxl=maxl))
}
