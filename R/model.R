#### BUILDING MODEL

#' @export
buildModel <- function(problem, minEpsilon = 1e-4, method="utamp-1") { # includeEpsilonAsVariable,
  if(! (method %in% c("uta", "utamp-1", "utamp-2")))
  {
    stop("Method must be on of the following: `uta`, `utamp-1`, `utamp-2`")
  }
  nrAlternatives <- nrow(problem$performanceTable)
  nrCriteria <- ncol(problem$performanceTable)

  #problem gets into consideration only the number of characteristic points from value functions
  #here we add one variable that corresponds to epsilion value
  numberOfVariables <- problem$numberOfVariables + 1

  #preferences to model variables used in solution
  preferencesToModelVariables <- createPreferencesToModelVariables(problem)

  epsilonIndex <- numberOfVariables

  # constraints
  ## sum to 1
  lhs <- rep(0, numberOfVariables)

  for (j in seq_len(nrCriteria)) {
    if (problem$criteria[j] == 'g')
      lhs[problem$criteriaIndices[j] + problem$characteristicPoints[j] - 2] <- 1
    else
      lhs[problem$criteriaIndices[j]] <- 1
  }

  constraints <- list(lhs = lhs, dir = "==", rhs = 1)

  ## monotonicity of vf
  for (j in seq_len(nrCriteria)) {
    for (k in seq_len(problem$characteristicPoints[j] - 2)) {
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
        lhs[epsilonIndex] <- 1
      }

      constraints <- combineConstraints(constraints,
                                        list(lhs = lhs, dir = "<=", rhs = rhs))
    }

    lhs <- rep(0, numberOfVariables)
    rhs <- 0
    if (problem$criteria[j] == 'g')
      lhs[problem$criteriaIndices[j]] <- -1
    else
      lhs[problem$criteriaIndices[j] + problem$characteristicPoints[j] - 2] <- -1

    if (problem$strictVF) {
      lhs[epsilonIndex] <- 1
    }
    constraints <- combineConstraints(constraints,
                                      list(lhs = lhs, dir = "<=", rhs = rhs))
  }

  constraints$types <- rep("C", numberOfVariables) #continous

  ## building model
  model <- list(
    constraints = constraints,
    criteriaIndices = problem$criteriaIndices,
    epsilonIndex = epsilonIndex,
    chPoints = problem$characteristicPoints,
    preferencesToModelVariables = preferencesToModelVariables,
    criterionPreferenceDirection = problem$criteria,
    prefInfoToConstraints = list(),
    generalVF = problem$generalVF,
    minEpsilon = minEpsilon
  )

  # preference information

  ## assignment examples

  prefInfoIndex <- 1

  if (is.matrix(problem$strongPreference)) {
    for (k in seq_len(nrow(problem$strongPreference))) {
      alternative <- problem$strongPreference[k, 1]
      referenceAlternative <- problem$strongPreference[k, 2]

      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, type = "strongPreference", method))

      model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      prefInfoIndex <- prefInfoIndex + 1
    }
  }

  if (is.matrix(problem$weakPreference)) {
    for (k in seq_len(nrow(problem$weakPreference))) {
      alternative <- problem$weakPreference[k, 1]
      referenceAlternative <- problem$weakPreference[k, 2]

      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, type = "weakPreference", method))

      model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      prefInfoIndex <- prefInfoIndex + 1
    }
  }

  if (is.matrix(problem$indifference)) {
    for (k in seq_len(nrow(problem$indifference))) {
      alternative <- problem$indifference[k, 1]
      referenceAlternative <- problem$indifference[k, 2]

      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, type = "indifference", method))

      model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      prefInfoIndex <- prefInfoIndex + 1
    }
  }

  return (model)
}
