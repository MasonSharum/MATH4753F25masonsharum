#' mybin
#'
#' @param iter number of iterations
#' @param n sample size
#' @param p probability
#' @importFrom grDevices rainbow
#'
#' @returns a table of relative frequencies and a barplot
#' @export
#'
#' @examples
#' mybin(iter = 100, n = 10, p = 0.7)
mybin = function(iter = 1000, n = 10, p = 0.5) {
  samp.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  success = c()

  for(i in 1:iter) {
    samp.mat[,i] = sample(c(1,0), n, replace = TRUE, prob = c(p, 1 - p))

    success[i] = sum(samp.mat[,i])
  }

  success.tab = table(factor(success, levels = 0:n))

  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(success.tab / (iter), col = rainbow(n+1), main = "Binomial Simulation", sub = lab, xlab = "Number of Successes")
  tabl = success.tab / iter
  return(tabl)
}
