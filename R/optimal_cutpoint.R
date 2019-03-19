#' Estimates the optimal cutpoints.
#'
#' @param data The data.frame containing the variables
#' @param ... To select variables from data (see dplyr::select)
#' @param choice The variable with the selected choice in data
#' @param metric The metric to use for estimating the cut points.
#' @param names The names of the choices. Uses variable names of data by default.
#' @return A list containing two elements. party.cutpoints contains the choice cutpoints. global.cutpoint contains the cutpoint which is globally the best
#' @examples
#' choice <- data.frame(PTV.A = c(0, 10, 5, 4, 7, 1, 2, 0, 0, 10),
#'                      PTV.B = c(5, 0, 1, 10, 6, 7, 10, 9, 5, 0),
#'                      PTV.C = c(10, 5, 10, 5, 2, 8, 0, 5, 5, 0),
#'                      VOTE = c("C", "A", "C", "B", "B", "C", "B","B", "B", "A"))
#' optimal_cutpoint(choice, PTV.A:PTV.C, choice = VOTE)
#' optimal_cutpoint(choice, PTV.A:PTV.C, choice = VOTE, metric = "Kappa")
optimal_cutpoint <- function (data, ..., choice, metric = c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Balanced"), names = NULL){
  parameter_set <- names(match.call())[-1]
  if (sum(parameter_set == "") > 0) {
    selection <- dplyr::select(data, ...)
  }
  else {
    selection <- data
  }
  if (sum(!apply(selection, 2, is.numeric)) > 0) {
    stop("At least one column is not numeric!")
  }
  ptv.matrix <- as.matrix(selection)
  choice <- deparse(substitute(choice))
  choice <- data[[choice]]
  if(ncol(ptv.matrix) != length(levels(as.factor(choice)))){
    stop("Number of choices and selected variables do not match.")
  }
  min <- min(ptv.matrix)
  max <- max(ptv.matrix)
  choice.dummy <- dummies::dummy(choice)
  include <- array(NA, dim = c(nrow(ptv.matrix), ncol(ptv.matrix),
                               max - min + 1))
  for (i in min:max) {
    include[, , i+1] <- ptv.matrix >= i
  }
  storage.mode(include) <- "numeric"
  metric <- match.arg(metric)
  if (is.null(names)) {
    names <- levels(as.factor(choice))
  }
  perform <- matrix(NA, nrow = max - min - 1, ncol = ncol(ptv.matrix))

  # CODE FOR COMPUTING VARIOUS QUANTITIES
  t <- c(12,0,8,0)
  t <- matrix(t, nrow=2, ncol=2, byrow=F)
  row.names(t) <- c("1", "0")
  colnames(t) <- c("1", "0")

  for (i in seq_len(ncol(ptv.matrix))) {
    for (j in seq_len(max - min - 1)) {
      t <- table(include[, i, j], choice.dummy[, i])
      if(sum(dim(t) != c(2, 2))>0){ # not 2x2
        temp <- matrix(0, nrow = 2, ncol = 2)
        colnames(temp) <- rownames(temp) <- 0:1
        r <- which(rownames(t) == rownames(temp))
        c <- which(colnames(t) == colnames(temp))
        temp[r, c] <- t
        t <- temp
      }
      perform[j, i] <- confusionMatrix(t, metric)
    }
  }
  return(perform)
  party.cutpoints <- apply(perform, 2, which.max) - min - 1
  global.cutpoint <- which.max(rowMeans(perform)) - min - 1
  names(party.cutpoints) <- names
  list(party.cutpoints = party.cutpoints, global.cutpoint = global.cutpoint)
}


confusionMatrix <- function(table, type, positive = 1) {
  n <- sum(colSums(table))
  acc <- (1/n) * (table[1,1]+table[2,2])
  row.m <- rowSums(table)
  col.m <- colSums(table)
  chance.freq <- (row.m[1]*col.m[1])/n + (row.m[2]*col.m[2])/n
  kappa <- (acc-chance.freq/n)/(1-chance.freq/n)
  sensitivity <- ifelse(positive == 1, table[1,1]/col.m[1], table[2,2]/col.m[2])
  specificity <- ifelse(positive == 1, table[2,2]/col.m[2], table[1,1]/col.m[1])
  balanced <- .5*(sensitivity+specificity)
  if(type == "Accuracy"){
    return(acc)
  }
  if(type == "Kappa"){
    return(kappa)
  }
  if(type == "Balanced"){
    return(balanced)
  }
  if(type == "Sensitivity"){
    return(sensitivity)
  }
  if(type == "Specificity"){
    return(specificity)
  }
}
