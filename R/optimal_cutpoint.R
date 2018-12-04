#' Estimates the optimal cutpoints.
#'
#' @param data The data.frame containing the variables
#' @param ... to select variables from data (see dplyr::select)
#' @param choice A variablename is character of data wich contains the selected choices
#' @param metric The metric to use for estimating the cut points.
#' @param names The names of the choices. Uses variable names of data by default.
#' @return A list containing two elements. party.cutpoints contains the choice cutpoints. global.cutpoint contains the cutpoint which is globally the best
#' @examples
#' choice <- data.frame(PTV.A = c(0, 10, 5, 4, 7, 1, 2, 0, 0, 10),
#'                      PTV.B = c(5, 0, 1, 10, 6, 7, 10, 9, 5, 0),
#'                      PTV.C = c(10, 5, 10, 5, 2, 8, 0, 5, 5, 0),
#'                      VOTE = c("C", "A", "C", "B", "B", "C", "B","B", "B", "A"))
#' optimal_cutpoint(choice, PTV.A:PTV.C, choice = "VOTE")
#' optimal_cutpoint(choice, PTV.A:PTV.C, choice = "VOTE", metric = "Kappa")
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
  choice <- data[[choice]]
  if(ncol(ptv.matrix) != length(levels(as.factor(choice)))){
    stop("Number of choices and selected variables do not match.")
  }
  min <- min(ptv.matrix)
  max <- max(ptv.matrix)
  choice.dummy <- dummies::dummy(choice)
  include <- array(NA, dim = c(nrow(ptv.matrix), ncol(ptv.matrix),
                               max - min - 1))
  for (i in min:(max - 1)) {
    include[, , i] = ptv.matrix > i
  }
  storage.mode(include) <- "numeric"
  metric <- match.arg(metric)
  if (is.null(names)) {
    names <- levels(as.factor(choice))
  }
  perform <- matrix(NA, nrow = max - min - 1, ncol = ncol(ptv.matrix))
  for (i in seq_len(ncol(ptv.matrix))) {
    for (j in seq_len(max - min - 1)) {
      t = table(include[, i, j], choice.dummy[, i])
      if (metric == "Accuracy")
        perform[j, i] <- caret::confusionMatrix(t, positive = "1")$overall[1]
      else if (metric == "Kappa")
        perform[j, i] <- caret::confusionMatrix(t, positive = "1")$overall[2]
      else if (metric == "Sensitivity")
        perform[j, i] <- caret::confusionMatrix(t, positive = "1")$byClass[1]
      else if (metric == "Specificity")
        perform[j, i] <- caret::confusionMatrix(t, positive = "1")$byClass[2]
      else perform[j, i] <- caret::confusionMatrix(t, positive = "1")$byClass[11]
    }
  }
  party.cutpoints <- apply(perform, 2, which.max) - min - 1
  global.cutpoint <- which.max(rowMeans(perform)) - min - 1
  names(party.cutpoints) <- names
  list(party.cutpoints = party.cutpoints, global.cutpoint = global.cutpoint)
}
