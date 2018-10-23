#UTA-G
#' @export
utag <- function(model, allowInconsistency = FALSE)
{
  nrCriteria <- length(model$criterionPreferenceDirection)
  nrAlternative <- nrow(model$preferencesToModelVariables)
  partialUtilityValues <- matrix(0, nrow = nrAlternative, ncol = nrCriteria)
  methodResult <- list()

  for (j in seq_len(nrCriteria))
  {
    if(model$criterionPreferenceDirection[j] == 'c') {
      extremizedCriterionIndex <- model$criteriaIndices[j]
    } else {
      extremizedCriterionIndex <- model$criteriaIndices[j] + model$chPoints[j] - 2
    }
    objective <- createObjective(model$constraints$lhs, extremizedCriterionIndex)
    solutionMin <- extremizeVariable(objective, model$constraints, maximize=FALSE)
    solutionMax <- extremizeVariable(objective, model$constraints, maximize=TRUE)
    #add appropriate from min and max solution
    partialUtilityValues[, j] <- partialUtilityValues[, j] + calculateUtilityValuesOnCriterion(model, solutionMin$solution, j)
    partialUtilityValues[, j] <- partialUtilityValues[, j] + calculateUtilityValuesOnCriterion(model, solutionMax$solution, j)
  }
  #divide each column by the number of criteria
  partialUtilityValues <- apply(partialUtilityValues, MARGIN = 2, function(x){
    x / nrCriteria
  })

  #calculate global utility values = sum values by rows = sum partial utility values for each alternative
  utilityValues <- apply(partialUtilityValues, MARGIN = 1, function(x){
    sum(x) / 2
  })
  methodResult$ranking <- generateRanking(utilityValues)
  methodResult
}

#UTAMP-1
#' @export
utamp1 <- function(model, allowInconsistency = FALSE) {
  if (is.null(model$epsilonIndex)) {
    stop("Use function buildModel with includeEpsilonAsVariable = TRUE.")
  }

  objective <- createObjective(model$constraints$lhs, model$epsilonIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize=TRUE)
  methodResult <- list()

  if(validateSolution(solution, allowInconsistency)){
    methodResult$localUtilityValues <- calculateUtilityValues(model, solution$solution)
    globalUtilityValues <- utilityValues <- apply(methodResult$localUtilityValues, MARGIN = 1, function(x){ sum(x) })
    methodResult$ranking <- generateRanking(globalUtilityValues)
    #method specific functionality
    if (!is.null(model$epsilonIndex)) {
      methodResult$epsilon <- solution$solution[model$epsilonIndex]
    } else {
      methodResult$epsilon <- model$minEpsilon
    }
  }
  methodResult
}

#UTAMP-2
#' @export
utamp2 <- function(model, allowInconsistency = FALSE) {
  stop("Not implemented yet")

  obj <- rep(0, ncol(constraints$lhs))
  obj[variableIndex] <- 1
  solution <- extremizeVariable(model$constraints, model$epsilonIndex, obj, TRUE)

  return(getSolutionOrError(solution))
}
