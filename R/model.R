#### BUILDING MODEL

#' @export
buildModel <- function(problem, method, minK = 1e-4, minEpsilon = 1e-4) { # includeEpsilonAsVariable,
  availableMethods <- getAvailableMethods()
  assert(method %in% availableMethods, paste("Method must be one of the following:", unlist(availableMethods)))
  nrAlternatives <- nrow(problem$performance)
  nrCriteria <- ncol(problem$performance)


  #preferences to model variables used in solution
  coefficientsMatrix <- calculateCoefficientsMatrix(problem)

  #when constructing problem we get into consideration only the number of characteristic points from value functions
  #we don't consider rho and k
  #here we add one variable that corresponds to rho value
  numberOfVariables <- problem$numberOfVariables + 2
  rhoIndex <- numberOfVariables - 1
  kIndex <- numberOfVariables

  # constraints
  constraints <- list()
  ## sum to 1
  constraints <- combineConstraints(constraints,
                                    normalizationConstraint(problem, numberOfVariables, nrCriteria))

  ## monotonicity of vf
  constraints <- combineConstraints(constraints,
                                    monotonicityConstraints(problem, numberOfVariables, nrCriteria, rhoIndex))

  #criteria of a continous type
  constraints$variablesTypes <- rep("C", numberOfVariables)

  # remove least valuable characteristic points from coefficientMatrix and criteria indices
  # first characteristic point in case of a gain type criterion
  # last characteristic point in case of a cost type criterion
  leastValuableCharacteristicPoints <- c()
  for(criterion in seq_len(nrCriteria)){
    if(problem$criteria[criterion] == 'g'){
      leastValuableCharacteristicPoints <- c(leastValuableCharacteristicPoints, problem$criteriaIndices[criterion])
    } else {
      leastValuableCharacteristicPoints <- c(leastValuableCharacteristicPoints, problem$criteriaIndices[criterion] + problem$characteristicPoints[criterion] - 1)
    }
  }

  # update criteria indices, before removing the leastValuableCharacteristicPoints
  problem$criteriaIndices <- getCriteriaIndices(problem, substractZeroCoefficients=FALSE)

  ## building model
  model <- list(
    constraints = constraints,
    criteriaIndices = problem$criteriaIndices,
    rhoIndex = rhoIndex,
    kIndex = kIndex,
    chPoints = problem$characteristicPoints,
    preferencesToModelVariables = coefficientsMatrix,
    criterionPreferenceDirection = problem$criteria,
    generalVF = problem$generalVF,
    minEpsilon = minEpsilon,
    methodName = method,
    performances = problem$performanceTable
  )

  # remove least valuable characteristic points
  model <- removeColumnsFromModelConstraints(model, leastValuableCharacteristicPoints)
  problem$criteriaIndices <- getCriteriaIndices(problem, substractZeroCoefficients = TRUE)
  model$criteriaIndices <- problem$criteriaIndices

  # preference information
  #prefInfoIndex <- 1

  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "preference"))

  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "indifference"))

  # method specific actions
  if(method == availableMethods$utag){
    # exchange k variable with a small positive constant
    # get indices of all constraints that use k
    constraintsWithKIndex <- which(model$constraints$lhs[, model$kIndex] %in% 1)
    model <- removeColumnsFromModelConstraints(model, model$kIndex)
    if(length(constraintsWithKIndex) > 0){
      model$constraints$rhs[constraintsWithKIndex] = minK
    }

  } else if(method == availableMethods$roruta){
    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "preference"))
    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "indifference"))
  }

  return(model)
}
