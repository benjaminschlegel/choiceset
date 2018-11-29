#' Create a choiceset from a data.frame.
#'
#' @param data The data.frame containing the variables
#' @param ... OPTIONAL to select variables from data (see dplyr::select)
#' @param type Type of the data: direct for a dummy variable, ptv for ptv data and vote_prop for vote probabilities.
#' @param output choice_list for a list of character vectors containing the choices of the sets; choice_matrix for a dummy matrix containing 1 for the choices includes in the set and 0 otherwiese; numeric_list for a list containing numbered vectors containing the choices of the sets (1 for the first choiche, 2 for the second, ...);  character for a character vector where each elemebt represents a set connecting the choices with -.
#' @param threshold The threshold to use, if left empty 0 is used for vote_prop and threshold.estimate for ptv.
#' @param threshold.estimate Only relevant for type ptv. Default is midpoint where the midpoint between min and max of the data is used. Median uses the median of the data and learning uses the function optimal_cutpoint to estimate a cutpoint for every choice.
#' @param names Names for the choices, default uses the variable names of data.
#' @param choice Only needed for threshold.estimate learning; A variablename as character of the selected choice in data.
#' @param metric Only needed for threshold.estimate learning; The metric to use for the learning.
#' @return The choicesets in the form defined with the parameter output.
#' @examples
#' cnames <- c("50+","CDA","CU","D66","DENK","FvD","GL","PvdA","PvdD","PVV","SGP","SP","VVD")
#' vp <- matrix(c(0,10,20,0,0,0,0,0,0,0,60,0,10,
#'               5,0,0,30,0,0,20,45,0,0,0,0,0,
#'               0,0,0,0,80,0,0,20,0,0,0,0,0,
#'               0,0,0,0,0,20,0,0,10,50,0,0,20,
#'               0,0,0,0,0,0,40,35,0,0,0,25,0),
#'             nrow = 5, ncol = 13, byrow = TRUE)
#' vp <- as.data.frame(vp)
#' choiceset(vp, type = 'ptv', names = cnames)
#' choiceset(vp, type = 'ptv', output = 'character', names = cnames)
#' choiceset(vp, type = 'ptv', output = 'character', threshold = 20, names = cnames)
#' choiceset(vp, type = 'ptv', output = 'choice_matrix', threshold.estimate = 'median', names = cnames)
choiceset <- function(data, ..., type = c('direct', 'ptv', 'vote_prop'),
                      output = c('choice_list','choice_matrix', 'numeric_list', 'character'),
                      threshold = NULL, threshold.estimate = c('midpoint','median','learning'),
                      names = NULL, choice = NULL,
                      metric = c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Balanced")){

  # prepare data
  parameter_set = names(match.call())[-1]
  if(sum(parameter_set=="")>0){
    selection <- dplyr::select(data, ...)
  }else{
    selection <- data
  }
  type <- match.arg(type)
  output <- match.arg(output)
  threshold.estimate <- match.arg(threshold.estimate)

  # check if data is numeric
  if(sum(!apply(selection, 2, is.numeric)) > 0){
    stop("At least one column is not numeric!")
  }

  # generate choice set matrix
  choice_set_matrix <- as.matrix(selection)
  if(type == 'ptv'){
    if(is.null(threshold)){
      if(threshold.estimate == 'learning'){
        if(is.null(choice)){
          stop("Vote must be given to estimate the optimal thresholds.")
        }
        optimal_thresholds <- optimal_cutpoint(selection, choice = choice, metric = match.arg(metric))
        party.cutpoints <- optimal_thresholds$party.cutpoints
        threshold <- rep(party.cutpoints, rep(nrow(choice_set_matrix), ncol(choice_set_matrix)))
        choice_set_matrix <- (choice_set_matrix >= threshold)
      }else{
        threshold <- ifelse(threshold.estimate == "midpoint",
                            mean(range(choice_set_matrix, na.rm = T)),
                            median(choice_set_matrix, na.rm = T))
        choice_set_matrix <- (choice_set_matrix >= threshold)
      }
    }
  }
  if(type == 'vote_prop'){
    if(is.null(threshold)){
      threshold <- 0
    }
    choice_set_matrix <- (choice_set_matrix > threshold)
  }

  # get names
  if(is.null(names)){
    names <- colnames(choice_set_matrix)
  }

  # generate output
  if(output == "choice_matrix"){
    colnames(choice_set_matrix) <- names
    storage.mode(choice_set_matrix) <- "numeric"
    return(choice_set_matrix)
  }
  numeric_choice <- apply(choice_set_matrix, 1, get_positions, matrix = choice_set_matrix)

  if(output == "numeric_list"){
    return(numeric_choice)
  }

  named_list <- lapply(numeric_choice, match_names, names = names)
  if(output == "choice_list"){
    return(named_list)
  }

  if(output == "character"){
    return(sapply(named_list, paste, collapse = "-"))
  }
}

get_positions = function(x, matrix){
  seq_len(ncol(matrix))[x]
}

match_names = function(x, names){
  names[x]
}
