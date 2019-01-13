#### BUILDING MODEL

#' @export
buildModel <- function(problem, method, minK = 1e-4, minEpsilon = 1e-4, bigNumber = 1e9) { # includeEpsilonAsVariable,
  availableMethods <- getAvailableMethods()
  assert(method %in% availableMethods, paste(availableMethods, " "))
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
  constraints$variablesTypes <- rep("C", ncol(constraints$lhs))

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
  # model <- removeColumnsFromModelConstraints(model, leastValuableCharacteristicPoints)
  # update criteria indices
  # problem$criteriaIndices <- getCriteriaIndices(problem, substractZeroCoefficients = TRUE)
  # model$criteriaIndices <- problem$criteriaIndices

  # preference information
  #prefInfoIndex <- 1
  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "strong"))

  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "weak"))

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
                                            intensitiesConstraints(problem, model, "strong"))

    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "weak"))
    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "indifference"))
    # remove rho
    model <- removeColumnsFromModelConstraints(model, model$rhoIndex)

    # rename k to epsilon
    model$epsilonIndex <- model$kIndex
    # remove k index
    model$kIndex <- NULL

    # rank requirements
    # add constraints for the desiredRank and desiredUtilityValue
    # desiredUtilityValue matters only when there is at least one row in the desiredRank
    if(!is.null(problem$desiredRank) && nrow(problem$desiredRank) > 0)
    {
      desiredRankConstraints <- createRankRelatedConstraints(problem, model, minEpsilon, bigNumber)

      col.names <- c(rep("-", ncol(model$constraints$lhs)), desiredRankConstraints$variables.labels)
      row.names <- c(rep("-", nrow(model$constraints$lhs)),  desiredRankConstraints$constraints.labels)
      # augment model's lhs to fit desiredRankConstraints
      numberOfColumnsToAddToOldConstraints <- ncol(desiredRankConstraints$lhs) - ncol(model$constraints$lhs)
      matrixToAdd <- matrix(0, nrow = nrow(model$constraints$lhs), ncol = numberOfColumnsToAddToOldConstraints)
      model$constraints$lhs <- cbind(model$constraints$lhs, matrixToAdd)

      # now we can merge desiredRankConstraints with model's
      # LHS
      model$constraints$lhs <- rbind(model$constraints$lhs, desiredRankConstraints$lhs)
      rownames(model$constraints$lhs) <- row.names
      colnames(model$constraints$lhs) <- col.names
      # RHS
      model$constraints$rhs <- c(model$constraints$rhs, desiredRankConstraints$rhs)
      # dir
      model$constraints$dir <- c(model$constraints$dir, desiredRankConstraints$dir)
      model$constraints$variablesTypes <- desiredRankConstraints$variablesTypes
    }
  } else if(method == availableMethods$utamp1){
    model$constraints <- splitVariable(model, model$kIndex)
  } else if(method == availableMethods$utamp2) {
    # split rho into rho_jk
    model$constraints <- splitVariable(model, model$rhoIndex)
    model$constraints <- splitVariable(model, model$kIndex)
  }

  return(model)
}
