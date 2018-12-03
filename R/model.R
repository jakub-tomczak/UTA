#### BUILDING MODEL

#' @export
buildModel <- function(problem, minEpsilon = 1e-4, method="utamp-1") { # includeEpsilonAsVariable,
  if(! (method %in% c("uta", "utamp-1", "utamp-2")))
  {
    stop("Method must be on of the following: `uta`, `utamp-1`, `utamp-2`")
  }
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
  #add rho column
  coefficientsMatrix <- cbind(coefficientsMatrix, 0)
  #add k column
  coefficientsMatrix <- cbind(coefficientsMatrix, 0)

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
  problem$criteriaIndices <- createCriteriaIndices(problem, substractZeroCoefficients=TRUE)
  coefficientsMatrix <- coefficientsMatrix[,-leastValuableCharacteristicPoints]
  #remove as many variables as columns
  numberOfVariables <- numberOfVariables - length(leastValuableCharacteristicPoints)
  #update epsilion value
  rhoIndex <- numberOfVariables - 1
  kIndex <- numberOfVariables
  #update constraints - remove least valuable variables, update constraints types
  constraints$lhs <- constraints$lhs[, -leastValuableCharacteristicPoints]
  constraints$variablesTypes <- constraints$variablesTypes[-leastValuableCharacteristicPoints]


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
    methodName = problem$methodName
  )

  # preference information
  #prefInfoIndex <- 1

  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "preference"))

  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "indifference"))

  return(model)
}
