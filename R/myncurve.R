#' Plot a Normal Curve with a Shaded Probability Area
#'
#' @param mu The mean
#' @param sigma The standard deviation
#' @param a The value to shade to
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @returns A list of the mean, sd, cutoff, and probability
#' @export
#'
#' @examples
#' myncurve(mu = 10, sigma = 2, a = 12)
myncurve <- function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x, mean=mu, sd=sigma),
        from = mu - 4*sigma, to = mu + 4*sigma,
        xlab = "X", ylab = "Density",
        main = paste("Normal Curve: mu =", mu, ", sigma =", sigma))

  xcurve <- seq(mu - 4*sigma, a, length=1000)
  ycurve <- dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu - 4*sigma, xcurve, a), c(0, ycurve, 0), col="Red")

  prob <- pnorm(a, mean=mu, sd=sigma)

  text(a, max(ycurve)/2, paste("P(X <=", a, ") =", round(prob,4)))

  # Return list
  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}
