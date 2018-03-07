#' Lapply then rbind.
#'
#' A wrapper for a do.call('rbind', lapply(X, FUN)).
#'
#' @param X A list.
#' @param FUN A function.
#' @return A matrix or a data.frame, depending on the objects in X.
#' @examples
#' lapply_rbind( list(a = c(1,2), b =c(3,4)), sum)
#' lapply_rbind( as.list(cars), function(x) mean(x))
#' @export

lapply_rbind <- function(X,
                         FUN = function(x){x}){
  # Checks
  if (class(X)!='list') stop('X must be a list.')
  if (class(FUN)!='function') stop('FUN must be a function.')

  # Execute
  list.output <- lapply(X, FUN)
  return( do.call('rbind', list.output) )
}

#eof
