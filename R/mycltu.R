#' Central Limit Theorem Simulation Using Uniform Distribution
#'
#' Generates multiple samples from a uniform distribution and visualizes
#' the distribution of sample means to demonstrate the Central Limit Theorem.
#'
#' @param n Sample size for each iteration.
#' @param iter Number of iterations.
#' @param a Lower limit of the uniform distribution.
#' @param b Upper limit of the uniform distribution.
#' @importFrom graphics curve lines
#' @importFrom stats density dnorm dunif runif
#'
#' @return A vector containing the sample means from each iteration.
#' @export
#'
#' @examples
#' mycltu(n = 10, iter = 10000, a = 0, b = 10)
mycltu=function(n=10,iter=1000,a=0,b=10){
  x <- NULL
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)

  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")

  lines(density(w),col="Blue",lwd=3)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
