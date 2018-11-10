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
  ## sum to 1
  lhs <- rep(0, numberOfVariables)

  for (j in seq_len(nrCriteria)) {
    if (problem$criteria[j] == 'g')
      lhs[problem$criteriaIndices[j] + problem$characteristicPoints[j] - 1] <- 1
    else
      lhs[problem$criteriaIndices[j]] <- 1
  }

  constraints <- list(lhs = lhs, dir = "==", rhs = 1)

  ## monotonicity of vf
  for (j in seq_len(nrCriteria)) {
    for (k in seq_len(problem$characteristicPoints[j] - 1)) {
      lhs <- rep(0, numberOfVariables)
      rhs <- 0

      if (problem$criteria[j] == "g") {
        lhs[problem$criteriaIndices[j] + k - 1] <- 1
        lhs[problem$criteriaIndices[j] + k] <- -1
      } else {
        lhs[problem$criteriaIndices[j] + k - 1] <- -1
        lhs[problem$criteriaIndices[j] + k] <- 1
      }

      if (problem$strictVF) {
        lhs[rhoIndex] <- 1
      }

      constraints <- combineConstraints(constraints,
                                        list(lhs = lhs, dir = "<=", rhs = rhs))
    }
  }

  constraints$variablesTypes <- rep("C", numberOfVariables) #continous criteria

  #remove least valuable characteristic points from coefficientMatri and criteria indices
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

  if (is.matrix(problem$strongPreference)) {
    for (k in seq_len(nrow(problem$strongPreference))) {
      alternative <- problem$strongPreference[k, 1]
      referenceAlternative <- problem$strongPreference[k, 2]
      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, preferenceType = "strong"))

      #model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      #prefInfoIndex <- prefInfoIndex + 1
    }
  }

  if (is.matrix(problem$weakPreference)) {
    for (k in seq_len(nrow(problem$weakPreference))) {
      alternative <- problem$weakPreference[k, 1]
      referenceAlternative <- problem$weakPreference[k, 2]

      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, preferenceType = "weak"))

      #model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      #prefInfoIndex <- prefInfoIndex + 1
    }
  }

  if (is.matrix(problem$indifference)) {
    for (k in seq_len(nrow(problem$indifference))) {
      alternative <- problem$indifference[k, 1]
      referenceAlternative <- problem$indifference[k, 2]

      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, preferenceType = "indifference"))

      #model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      #prefInfoIndex <- prefInfoIndex + 1
    }
  }
  return(model)
}
