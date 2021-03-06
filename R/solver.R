#Solver method used when model$methodName is set
#' @export
solveProblem <- function(model, allowInconsistency = FALSE)
{
  if(is.null(model$methodName)){
    stop("Method name is not set. Set problem$method or model$methodName.")
  }

  availableMethods <- getAvailableMethods()
  if(model$methodName == availableMethods$utag)
  {
    utag(model, allowInconsistency)
  } else if(model$methodName == availableMethods$utamp1)
  {
    utamp1g(model, allowInconsistency)
  } else if(model$methodName == availableMethods$utamp2)
  {
    utamp2g(model, allowInconsistency)
  } else if(model$methodName == availableMethods$roruta){
    roruta(model, allowInconsistency)
  } else {
    stop(paste(availableMethods, " "))
  }
}

#UTA-G
#' @export
utag <- function(model, allowInconsistency = FALSE)
{
  nrCriteria <- length(model$criterionPreferenceDirection)
  nrAlternative <- nrow(model$preferencesToModelVariables)
  partialUtilityValues <- matrix(0, nrow = nrAlternative, ncol = nrCriteria)
  methodResult <- list()
  meanVFMarginalValues <- rep(0, ncol(model$constraints$lhs))

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

    if(!validateSolution(solutionMin, allowInconsistency, model$minEpsilon) || !validateSolution(solutionMax, allowInconsistency, model$minEpsilon))
    {
      return(NULL)
    }

    meanVFMarginalValues <- meanVFMarginalValues + solutionMin$solution + solutionMax$solution
    # add appropriate from min and max solution
    # to all criteria
    partialUtilityValues <- partialUtilityValues + calculateUtilityValues(model, solutionMin$solution) + calculateUtilityValues(model, solutionMax$solution)
  }
  # divide each column by the number of criteria
  partialUtilityValues <- apply(partialUtilityValues, MARGIN = 2, function(x){
    x/(2*nrCriteria)
  })

  VFMarginalValues <- sapply(meanVFMarginalValues, function(x){
    x/(2*nrCriteria)
  })
  # calculate global utility values = sum values by rows = sum partial utility values for each alternative
  utilityValues <- apply(partialUtilityValues, MARGIN = 1, function(x){
    sum(x)
  })
  methodResult$localUtilityValues <- partialUtilityValues
  methodResult$ranking <- generateRanking(utilityValues)
  methodResult$valueFunctionsMarginalValues <- getValueFunctionsMarginalValues(model, VFMarginalValues)

  # assign names
  colnames(methodResult$localUtilityValues) <- colnames(model$performances)
  rownames(methodResult$localUtilityValues) <- rownames(model$performances)

  rownames(methodResult$ranking) <- rownames(model$performances)

  methodResult
}

#UTAMP1-G
#' @export
utamp1g <- function(model, allowInconsistency = FALSE) {
  assert(!is.null(model$kIndex),
         "k must be a variable in the model. Try building model again with a command `buildModel(problem, 'utamp-1')`.")

  objectiveIndex <- c(model$kIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize=TRUE)

  if(validateSolution(solution, allowInconsistency, model$minEpsilon)){
    methodResult <- getMethodResult(model, solution)
    #method specific functionality
    methodResult$k <- solution$solution[model$kIndex]
    return(methodResult)
  }
  NULL
}

#UTAMP2-G
#' @export
utamp2g <- function(model, allowInconsistency = FALSE) {
  assert(!is.null(model$kIndex) && !is.null(model$rhoIndex),
         "k or rho must be a variable in the model. Try building model again with a command `buildModel(problem, 'utamp-2')`.")

  objectiveIndex <- c(model$rhoIndex, model$kIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize = TRUE)

  if(validateSolution(solution, allowInconsistency, model$minEpsilon)){
    methodResult <- getMethodResult(model, solution)
    #method specific functionality
    methodResult$k <- solution$solution[model$kIndex]
    methodResult$rho <- solution$solution[model$rhoIndex]
    return(methodResult)
  }
  NULL
}

# roruta
#' @export
roruta <- function(model, allowInconsistency){
  assert(!is.null(model$epsilonIndex),
         "epsilon must be a variable in the model. Try building model again with a command `buildModel(problem, 'roruta')`.")

  objectiveIndex <- c(model$epsilonIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize = TRUE)

  if(validateSolution(solution, allowInconsistency, model$minEpsilon))
  {
    methodResult <- getMethodResult(model, solution)
    #method specific functionality
    methodResult$epsilon <- solution$solution[model$epsilonIndex]
    return(methodResult)
  }
  NULL
}
