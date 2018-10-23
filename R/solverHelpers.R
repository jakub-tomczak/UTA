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
  nrAlternatives <- length(utilityValues)
  ranking <- sort(utilityValues, decreasing = TRUE, index.return = TRUE)
  rankingMatrix <- matrix(utilityValues)

  alternativeToRanking <- rep(0, nrAlternatives)
  for(x in seq_len(nrAlternatives)){
    alternativeToRanking[ranking$ix[x]] <- x
  }
  rankingMatrix <- cbind(c(1:nrAlternatives), rankingMatrix, alternativeToRanking)
  colnames(rankingMatrix) <- c("alternativeNo.", "utilityValue", "ranking")
  rankingMatrix
}

#values attained from LP solution
#number of these values corresponds to the number of columns in the model$preferencesToModelVariables
calculateUtilityValues <- function(model, values){
  # v1 %*% v2 => dot product of v1 and v2
  # alternative are in rows, columns represents criteria, values are utility values of the alternative on that criterion
  utilityValues <- sapply(seq_len(length(model$criteriaIndices)), function(y){
    calculateUtilityValuesOnCriterion(model, values, y)
  })
}

#allows multiplying
calculateUtilityValuesOnCriterion <- function(model, value, criterionNumber){
  sapply(seq_len(nrow(model$preferencesToModelVariables)), function(alternative)
  {
    #from and to are used to index a criterion's marginal values coefficients on a preferences matrix
    from <- model$criteriaIndices[criterionNumber]
    #minus (1 - beacuse of the fact we ommit the least value characterisitc point, 1 - last index is included)
    to <- from + model$chPoints[criterionNumber]-2

    if(length(value) == 1){
      #multiplying by number
      sum(model$preferencesToModelVariables[alternative, from:to] * value)
    } else{
      assert(ncol(model$preferencesToModelVariables) == length(value),
             "If value is a vector it must represent the same number of coefficients as in the preferencesToModelVariables.")
      #multiplying by vector
      model$preferencesToModelVariables[alternative, from:to] %*% value[from:to]
    }
  })
}

validateSolution <- function(solution, allowInconsistency){
  if(is.null(solution))
  {
    stop("Solution object is empty.")
  }
  else if ((solution$status == 0 && solution$optimum >= model$minEpsilon) || allowInconsistency)
  {
    return(TRUE)
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
    print("Unknown error.")
    return(FALSE)
  }
}
