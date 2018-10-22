#' @import Rglpk
extremizeVariable <- function(objective, constraints, maximize) {
  Rglpk_solve_LP(objective, constraints$lhs, constraints$dir, constraints$rhs, max = maximize,
                 types = constraints$variablesType)
}

createObjective <- function(numberOfConstraints, extremizedVariableIndex){
  assert(!is.null(extremizedVariableIndex), 'Variable to extremize is NULL')
  obj <- rep(0, ncol(numberOfConstraints))
  obj[extremizedVariableIndex] <- 1
  obj
}

#this function takes model and values variable that is a vector with
#LP problem solutions
#comprehensiveValue is a vector containing on each index a
#dot product of a marginal value function's coefficient and an alternative's coefficient
generateRanking <- function(utilityValues) {
  ranking <- sort(utilityValues, decreasing = TRUE, index.return = TRUE)
  #values are places in the ranking
  ranking$alternativeToRanking <- rep(0, length(utilityValues))
  names(ranking) <- c("utilityValue", "ranking", "alternativeToRanking")

  for(x in seq_len(length(utilityValues))){
    ranking$alternativeToRanking[ranking$ranking[x]] <- x
  }

  ranking
}

calculateUtilityValues <- function(model, values){
  # v1 %*% v2 => dot product of v1 and v2
  # alternative are in rows, columns represents criteria, values are utility values of the alternative on that criterion
  utilityValues <- sapply(seq_len(length(model$criteriaIndices)), function(y){
    sapply(seq_len(nrow(model$preferencesToModelVariables)), function(alternative)
    {
      #from and to are used to index a criterion's marginal values coefficients on a preferences matrix
      from <- model$criteriaIndices[y]
      #minus (1 - beacuse of the fact we ommit the least value characterisitc point, 1 - last index is included)
      to <- from + model$chPoints[y]-2
      model$preferencesToModelVariables[alternative, from:to] %*% values[from:to]
    })
  })
}

#' @export
toSolution <- function(model, values) {
  #calculate alternatives utility values
  localUtilityValues <- calculateUtilityValues(model, values)
  globalUtilityValues <- apply(localUtilityValues, MARGIN = 1, function(x){ sum(x) })
  #ranks
  ranking <- generateRanking(globalUtilityValues)
  # epsilon
  epsilon <- NULL

  if (!is.null(model$epsilonIndex)) {
    epsilon <- values[model$epsilonIndex]
  } else {
    epsilon <- model$minEpsilon
  }

  return (list(
    ranking = ranking,
    utilityValues = globalUtilityValues,
    localUtilityValues = localUtilityValues,
    solution = values,
    epsilon = epsilon,
    generalVF = model$generalVF
  ))
}

#' @export
getSolutionOrError <- function(solution, allowInconsistency){
  if(is.null(solution))
  {
    stop("Solution object is empty.")
  }
  else if ((solution$status == 0 && solution$optimum >= model$minEpsilon) || allowInconsistency)
  {
    return (toSolution(model, solution$solution))
  }
  else if(solution$status != 0)
  {
    stop("Soultion hasn't been found.")
  }
  else if(solution$status == 0 && solution$optimum < model$minEpsilon)
  {
    stop("Solution has been found but optimum is lower than minEpsilon value.")
  }
  else if(!allowInconsistency)
  {
    stop("Inconsistency is not allowed.")
  }
  else
  {
    stop("Unknown error.")
  }
}
