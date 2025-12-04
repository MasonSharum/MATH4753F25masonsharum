#' Bootstrap Resampling Function
#'
#' Performs bootstrap resampling for a statistic and plots
#' the bootstrap distribution with confidence interval markings.
#'
#' @param iter Number of bootstrap iterations. Default is 10000.
#' @param x  The data to resample.
#' @param fun Statistic to compute on each sample (e.g., mean, median). Default is "mean".
#' @param alpha Significance level for the confidence interval (default 0.05).
#' @param cx Scaling factor for text size in the plot (default 1.5).
#' @param ... Additional arguments
#'
#' @importFrom stats quantile
#' @importFrom graphics hist segments text abline
#'
#' @return Invisible list containing:
#' \item{ci}{Bootstrap confidence interval.}
#' \item{fun}{Function used for the statistic.}
#' \item{x}{Original data.}
#' \item{xstat}{Vector of bootstrap statistics.}
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(50)
#' myboot2(x = x, fun = mean, iter = 5000, alpha = 0.05)
myboot2 <- function(iter=10000, x, fun="mean", alpha=0.05, cx=1.5, ...) {

  n = length(x)
  y = sample(x, n*iter, replace=TRUE)
  rs.mat = matrix(y, nrow=n, ncol=iter, byrow=TRUE)
  xstat = apply(rs.mat, 2, fun)
  ci = quantile(xstat, c(alpha/2, 1 - alpha/2))
  para = hist(xstat, freq=FALSE, las=1,
              main=paste("Histogram of Bootstrap sample statistics","\n",
                         "alpha=",alpha," iter=",iter,sep=""),...)

  mat = matrix(x, nrow=length(x), ncol=1, byrow=TRUE)

  pte = apply(mat, 2, fun)
  abline(v=pte, lwd=3, col="Black")
  segments(ci[1], 0, ci[2], 0, lwd=4)
  text(ci[1], 0, paste("(", round(ci[1],2), sep=""), col="Red", cex=cx)
  text(ci[2], 0, paste(round(ci[2],2), ")", sep=""), col="Red", cex=cx)
  text(pte, max(para$density)/2, round(pte,2), cex=cx)

  invisible(list(ci=ci, fun=fun, x=x, xstat=xstat))
}
