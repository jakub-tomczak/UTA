#### BUILDING MODEL

#' @export
buildModel <- function(problem, minEpsilon = 1e-4, method="utamp-1") { # includeEpsilonAsVariable,
  if(! (method %in% c("uta", "utamp-1", "utamp-2")))
  {
    stop("Method must be on of the following: `uta`, `utamp-1`, `utamp-2`")
  }
  nrAlternatives <- nrow(problem$performanceTable)
  nrCriteria <- ncol(problem$performanceTable)

  firstChPointVariableIndex <- c(1)
  chPoints <- c()

  #calculate characteristic points indices
  for (j in seq_len(nrCriteria)) {
    numberOfCharacteristicPoints <- problem$characteristicPoints[j]

    if (numberOfCharacteristicPoints == 0) { #jeżeli nie wiadomo ile jest punktów charakterystycznych to załóż, że jest tyle ile jest kryteriów de facto
      numberOfCharacteristicPoints <- length(problem$performanceTable[[j]])
    }

    if (j != nrCriteria) { #jeżeli nie jesteśmy w ostatnim przejściu pętli
      #kolejny punkt charakterystyczny to aktualny + liczba punktów charakterystycznych dla tego kryterium -1
      firstChPointVariableIndex[j + 1] <- firstChPointVariableIndex[j] + numberOfCharacteristicPoints - 1
    }

    chPoints[j] <- numberOfCharacteristicPoints
  }

  numberOfVariables <- problem$numberOfVariables + 1

  #preferences to model variables used in solution
  preferencesToModelVariables <- createPreferencesToModelVariables(problem, firstChPointVariableIndex)

  # epsilon index
  numberOfVariables <- numberOfVariables + 1
  epsilonIndex <- numberOfVariables

  # constraints
  ## sum to 1
  lhs <- rep(0, numberOfVariables)

  for (j in seq_len(nrCriteria)) {
    if (problem$criteria[j] == 'g')
      lhs[firstChPointVariableIndex[j] + chPoints[j] - 2] <- 1
    else
      lhs[firstChPointVariableIndex[j]] <- 1
  }

  constraints <- list(lhs = lhs, dir = "==", rhs = 1)

  ## monotonicity of vf
  for (j in seq_len(nrCriteria)) {
    for (k in seq_len(chPoints[j] - 2)) {
      lhs <- rep(0, numberOfVariables)
      rhs <- 0

      if (problem$criteria[j] == "g") {
        lhs[firstChPointVariableIndex[j] + k - 1] <- 1
        lhs[firstChPointVariableIndex[j] + k] <- -1
      } else {
        lhs[firstChPointVariableIndex[j] + k - 1] <- -1
        lhs[firstChPointVariableIndex[j] + k] <- 1
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
      lhs[firstChPointVariableIndex[j]] <- -1
    else
      lhs[firstChPointVariableIndex[j] + chPoints[j] - 2] <- -1

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
    firstChPointVariableIndex = firstChPointVariableIndex,
    epsilonIndex = epsilonIndex,
    chPoints = chPoints,
    preferencesToModelVariables = preferencesToModelVariables,
    criterionPreferenceDirection = problem$criteria,
    prefInfoToConstraints = list(),
    generalVF = problem$characteristicPoints == 0,
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
