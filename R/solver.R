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
    utamp1(model, allowInconsistency)
  } else if(model$methodName == availableMethods$utamp2)
  {
    utamp2(model, allowInconsistency)
  } else {
    stop(paste("Method", model$method, "is not available. Available methods", unlist(availableMethods)))
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
  methodResult
}

#UTAMP-1
#' @export
utamp1 <- function(model, allowInconsistency = FALSE) {
  assert(!is.null(model$kIndex),
         "k must be a variable in the model. Try building model again with a command `buildModel(problem, 'utamp-1')`.")

  objectiveIndex <- c(model$kIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize=TRUE)
  assert(solution$status == 0, "Model is not feasible")

  methodResult <- list()
  methodResult$solution <- solution
  methodResult$valueFunctions <- list()

  if(validateSolution(solution, allowInconsistency)){
    methodResult$localUtilityValues <- calculateUtilityValues(model, solution$solution)
    globalUtilityValues <- utilityValues <- apply(methodResult$localUtilityValues, MARGIN = 1, function(x){ sum(x) })
    methodResult$ranking <- generateRanking(globalUtilityValues)
    #method specific functionality

    methodResult$epsilon <- solution$solution[objectiveIndex]
    methodResult$valueFunctionsMarginalValues <- getValueFunctionsMarginalValues(model, solution$solution)
  }
  methodResult
}

#UTAMP-2
#' @export
utamp2 <- function(model, allowInconsistency = FALSE) {
  assert(!is.null(model$kIndex) && !is.null(model$rhoIndex),
         "k or rho must be a variable in the model. Try building model again with a command `buildModel(problem, 'utamp-2')`.")

  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize = TRUE)
  methodResult <- list()

  if(validateSolution(solution, allowInconsistency)){
    methodResult$localUtilityValues <- calculateUtilityValues(model, solution$solution)
    globalUtilityValues <- utilityValues <- apply(methodResult$localUtilityValues, MARGIN = 1, function(x){ sum(x) })
    methodResult$ranking <- generateRanking(globalUtilityValues)
    #method specific functionality
    methodResult$k <- solution$solution[model$kIndex]
    methodResult$rho <- solution$solution[model$rhoIndex]
    methodResult$valueFunctionsMarginalValues <- getValueFunctionsMarginalValues(model, solution$solution)
  }
  methodResult
}
