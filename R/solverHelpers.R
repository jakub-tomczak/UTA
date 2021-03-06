#' @import Rglpk
extremizeVariable <- function(objective, constraints, maximize) {
  Rglpk_solve_LP(objective, constraints$lhs, constraints$dir, constraints$rhs, max = maximize,
                 types = constraints$variablesType)
}

getMethodResult <- function(model, solution){
  methodResult <- list()
  alternativesNames <- rownames(model$performances)
  criteriaNames <- colnames(model$performances)

  methodResult$localUtilityValues <- calculateUtilityValues(model, solution$solution)
  colnames(methodResult$localUtilityValues) <- criteriaNames
  rownames(methodResult$localUtilityValues) <- alternativesNames

  globalUtilityValues <- utilityValues <- apply(methodResult$localUtilityValues, MARGIN = 1, function(x){ sum(x) })
  methodResult$ranking <- generateRanking(globalUtilityValues)
  rownames(methodResult$ranking) <- alternativesNames

  methodResult$valueFunctionsMarginalValues <- getValueFunctionsMarginalValues(model, solution$solution)
  methodResult
}

createObjective <- function(lhsMatrix, extremizedVariableIndex){
  assert(!is.null(extremizedVariableIndex), 'Variable to extremize is NULL')
  obj <- rep(0, ncol(lhsMatrix))
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
    #minus (1 -> beacuse of the fact that we ommit the least valuable characterisitc point + 1 -> last index is included)
    to <- from + model$chPoints[criterionNumber]-1

    if(length(value) == 1){
      #multiplying by number
      sum(model$preferencesToModelVariables[alternative, from:to] * value)
    } else{
      # assert(ncol(model$preferencesToModelVariables) == length(value),
      #       "If value is a vector it must contain the same number of coefficients as the preferencesToModelVariables.")
      #multiplying by vector
      model$preferencesToModelVariables[alternative, from:to] %*% value[from:to]
    }
  })
}

getValueFunctionsMarginalValues <- function(model, solution){
  sapply(seq_len(length(model$criteriaIndices)), function(j){
    from <- model$criteriaIndices[j]
    to <- from + model$chPoints[j] - 1

    x <- seq(min(model$performances[,j]), max(model$performances[,j]), length.out = model$chPoints[j])
    y <- solution[from:to]

    list(criterionIndex = j,
         characteristicPointsX = x,
         characteristicPointsY = y,
         criterionType = model$criterionPreferenceDirection[j])
  })
}

#' @export
necessaryAndPossiblePreferencesRelationAnalysis <- function(model){
  assert(model$methodName == "roruta",
         "Necessary and possible preference relation analysis is available only in roruta method.")

  nrAlternatives <- nrow(model$preferencesToModelVariables)
  necessaryWeakRelations <- matrix(nrow=nrAlternatives, ncol=nrAlternatives)
  possibleWeakRelations <- matrix(nrow=nrAlternatives, ncol=nrAlternatives)

  objective <- createObjective(model$constraints$lhs, model$epsilonIndex)
  # check whether base model may be solved
  solution <- extremizeVariable(objective = objective, constraints = model$constraints, maximize = TRUE)
  if(!validateSolution(solution, allowInconsistency = TRUE, model$minEpsilon)){
    return(NULL)
  }

  for(i in 1:nrAlternatives)
  {
    for(j in 1:nrAlternatives)
    {
      if(i != j)
      {
        necessaryWeakRelations[i,j] <- checkPreferenceRelationFeasibility(model, i, j, "necessary")
        possibleWeakRelations[i,j] <- checkPreferenceRelationFeasibility(model, i, j, "possible")
      } else {
        necessaryWeakRelations[i, j] <- TRUE
        possibleWeakRelations[i, j] <- TRUE
      }

    }
  }
  rownames(necessaryWeakRelations) <- rownames(model$performances)
  colnames(necessaryWeakRelations) <- rownames(model$performances)
  rownames(possibleWeakRelations) <- rownames(model$performances)
  colnames(possibleWeakRelations) <- rownames(model$performances)
  list(
    necessaryWeakRelations = necessaryWeakRelations,
    possibleWeakRelations = possibleWeakRelations
  )
}

#' @export
extremeRankingAnalysis <- function(model){
  assert(model$methodName == "roruta",
         "Extreme ranking analysis is only available in roruta model.")
  nrAlternatives <- nrow(model$preferencesToModelVariables)
  rankPositions <- matrix(nrow=nrAlternatives, ncol=2)
  colnames(rankPositions) <- c("The worst rank", "The best rank")

  objective <- createObjective(model$constraints$lhs, model$epsilonIndex)
  # check whether base model may be solved
  solution <- extremizeVariable(objective = objective, constraints = model$constraints, maximize = TRUE)
  if(!validateSolution(solution, allowInconsistency = TRUE, model$minEpsilon)){
    return(NULL)
  }

  for(i in 1:nrAlternatives)
  {
    minPosition <- nrAlternatives - analysePositionInRanking(model, i, "min")
    maxPosition <- analysePositionInRanking(model, i, "max") + 1

    rankPositions[i, ] <- c(minPosition, maxPosition)
  }

  rownames(rankPositions) <- rownames(model$performances)
  rankPositions
}

validateSolution <- function(solution, allowInconsistency, minEpsilon){
  if(is.null(solution))
  {
    print("Solution object is empty.")
  }
  else if ((solution$status == 0 && solution$optimum >= minEpsilon) || allowInconsistency)
  {
    return(TRUE)
  }
  else if(solution$status != 0)
  {
    print("Soultion hasn't been found.")
  }
  else if(solution$status == 0 && solution$optimum < minEpsilon)
  {
    print("Solution has been found but optimum is lower than minEpsilon value.")
  }
  else if(!allowInconsistency)
  {
    print("Inconsistency is not allowed.")
  }
  else
  {
    print("Model is not feasible.")
  }
  return(FALSE)
}
