#' Birthday function
#'
#' @param x
#'
#' @returns The chance there is a shared birthday among x people
#' @export
#'
#' @examples
#' birthday(20:25)
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
