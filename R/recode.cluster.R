#' Recode cluster
#'
#' Recodes cluster variables to be a sequential number
#'
#' This function recodes variables that represent cluster IDs to be sequentially numbered which facilitates
#' cluster analyses.
#'
#' @param Var The variable that represents the cluster
#'
#' @author Mathieu Maheu-Giroux
#'
#' @return The variable \code{ID} which represents cluster IDs, sequentially ordered numbers.

recode.cluster <- function(Var){
  id <- unique(Var)
  n <- length(id)
  clust.id <- rep('NA', length(Var))

  for (i in 1:n){
    id.i <- id[i]
    clust.id[which(Var == id.i)] <- i
  }

  as.numeric(clust.id)
}
