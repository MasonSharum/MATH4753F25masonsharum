#' Calculate Optimal Number of Tickets to Sell for a Flight
#'
#' This function determines how many tickets should be sold for a flight
#' with N seats, given the probability p that a passenger shows up,
#' and the acceptable probability gamma that the flight will be overbooked.
#' It computes the solution using both a discrete binomial distribution
#' and a continuous normal approximation, and plots the corresponding objective functions.
#'
#' @param N The total number of seats available on the flight
#' @param gamma The probability threshold for the flight being overbooked
#' @param p The probability that a passenger shows up
#'
#' @returns A named list containing:
#' \itemize{
#'   \item \code{nd} - Number of tickets (discrete distribution)
#'   \item \code{nc} - Number of tickets (normal approximation)
#'   \item \code{N}, \code{p}, \code{gamma} - Input parameters
#' }
#'
#' The function also produces two plots:
#' \enumerate{
#'   \item Objective function vs \code{n} using the discrete distribution
#'   \item Objective function vs \code{n} using the normal approximation
#' }
#'
#' @export
#' @importFrom stats pbinom pnorm uniroot
#' @importFrom graphics abline
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {
  objective_discrete <- function(n) 1 - gamma - pbinom(N, size = n, prob = p)
  objective_continuous <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    1 - gamma - pnorm(N + 0.5, mean = mu, sd = sigma)
  }

  n_range <- seq(N, N + 50, by = 1)
  obj_discrete_vals <- sapply(n_range, objective_discrete)
  obj_continuous_vals <- sapply(n_range, objective_continuous)

  sign_change_d <- which(diff(sign(obj_discrete_vals)) != 0)[1]
  if (!is.na(sign_change_d)) {
    n1 <- n_range[sign_change_d]
    n2 <- n_range[sign_change_d + 1]
    y1 <- obj_discrete_vals[sign_change_d]
    y2 <- obj_discrete_vals[sign_change_d + 1]
    nd <- n1 - y1 * (n2 - n1) / (y2 - y1)
  }

  nc <- tryCatch({
    uniroot(objective_continuous, lower = N, upper = N + 50)$root
  }, error = function(e) NA)

  par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

  plot(n_range, obj_discrete_vals, type = "o", pch = 16, col = "blue",
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    round(nd, 3), ") gamma=", gamma, " N=", N, " discrete", sep = ""),
       xlab = "n", ylab = "Objective",
       ylim = c(0, 1), xlim = c(N - 2, N + 35))
  abline(h = 0, col = "red", lwd = 2)
  if (!is.na(nd)) abline(v = nd, col = "red", lwd = 2)

  plot(n_range, obj_continuous_vals, type = "l", col = "black", lwd = 1.5,
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    round(nc, 3), ") gamma=", gamma, " N=", N, " continuous", sep = ""),
       xlab = "n", ylab = "Objective",
       ylim = c(0, 1), xlim = c(N - 2, N + 35))
  abline(h = 0, col = "blue", lwd = 2)
  if (!is.na(nc)) abline(v = nc, col = "blue", lwd = 2)

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
