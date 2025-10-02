#' Birthday function
#'
#' @param x The number of people in the group
#'
#' @returns A probability scalar
#' @export
#'
#' @examples
#' birthday(x = 20:25)
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
