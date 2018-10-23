#UTA-G
#' @export
utag <- function(model, allowInconsistency = FALSE)
{
  stop("Not implemented yet")

  nrCriteria <- length(model$criterionPreferenceDirection)
  criteriaAuxiliaryMarginalValues <- matrix(0, nrow=nrCriteria, ncol=2)
  for (j in seq_len(nrCriteria))
  {
    if(model$criterionPreferenceDirection[j] == 'g')
    {
      extremizedCriterionIndex <- model$criteriaIndices[j]
    } else {
      extremizedCriterionIndex <- model$criteriaIndices[j] + model$chPoints[j] - 2
    }
    objective <- createObjective(model$constraints$lhs, extremizedCriterionIndex)
    solutionMinRaw <- extremizeVariable(objective, model$constraints, maximize=FALSE)
    solutionMaxRaw <- extremizeVariable(objective, model$constraints, maximize=TRUE)
    solutionMin = getSolutionOrError(solutionMinRaw, allowInconsistency)
    solutionMax = getSolutionOrError(solutionMaxRaw, allowInconsistency)
    criteriaAuxiliaryMarginalValues[j, ] <- c(solutionMin$solution[extremizedCriterionIndex] , solutionMax$solution[extremizedCriterionIndex])
  }
  return(criteriaAuxiliaryMarginalValues)
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
