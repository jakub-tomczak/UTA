#### BUILDING MODEL

#' @export
buildModel <- function(problem, method, minK = 1e-4, minEpsilon = 1e-4, bigNumber = 1e9) { # includeEpsilonAsVariable,
  includeRho <- TRUE
  includeK <- TRUE

  availableMethods <- getAvailableMethods()
  assert(method %in% availableMethods, paste(availableMethods, " "))
  nrAlternatives <- nrow(problem$performance)
  nrCriteria <- ncol(problem$performance)


  #preferences to model variables used in solution
  coefficientsMatrix <- calculateCoefficientsMatrix(problem)

  #when constructing problem we get into consideration only the number of characteristic points from value functions
  #we don't consider rho and k
  #here we add one variable that corresponds to rho value
  numberOfVariables <- problem$numberOfVariables + ifelse(includeRho, 1, 0) + ifelse(includeK, 1, 0)
  rhoIndex <- NULL
  kIndex <- NULL
  if(includeRho)
    rhoIndex <- numberOfVariables-1
  if(includeK)
    kIndex <- numberOfVariables

  # constraints
  constraints <- list()
  ## sum to 1
  constraints <- combineConstraints(constraints,
                                    normalizationConstraint(problem, numberOfVariables, nrCriteria))

  ## least valuable characteristic points should be equal 0
  constraints <- combineConstraints(constraints,
                                    leastValuableChPointsEqualZero(problem, numberOfVariables, nrCriteria))

  ## monotonicity of vf
  constraints <- combineConstraints(constraints,
                                    monotonicityConstraints(problem, numberOfVariables, nrCriteria, rhoIndex))

  lhs.colnames <- colnames(coefficientsMatrix)
  if(includeRho)
  {
    lhs.colnames <- c(lhs.colnames, "rho")
  }
  if(includeK)
  {
    lhs.colnames <- c(lhs.colnames, "k")
  }

  colnames(constraints$lhs) <- lhs.colnames
  #criteria of a continous type
  constraints$variablesTypes <- rep("C", ncol(constraints$lhs))

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
      result <- createRankRelatedConstraints(problem, model, minEpsilon, bigNumber)
      desiredRankConstraints <- result$constraints
      model$rankConstraintsFirstBinaryVariableIndices <- result$rankConstraintsFirstBinaryVariableIndices

      if(is.null(colnames(model$constraints$lhs))){
        col.names <- c(rep("-", ncol(model$constraints$lhs)), desiredRankConstraints$variables.labels)
      } else {
        col.names <- c(colnames(model$constraints$lhs), desiredRankConstraints$variables.labels)
      }
      model$constraints$constraints.labels <- c(model$constraints$constraints.labels, desiredRankConstraints$constraints.labels)
      # augment model's lhs to fit desiredRankConstraints
      numberOfColumnsToAddToOldConstraints <- ncol(desiredRankConstraints$lhs) - ncol(model$constraints$lhs)
      matrixToAdd <- matrix(0, nrow = nrow(model$constraints$lhs), ncol = numberOfColumnsToAddToOldConstraints)
      model$constraints$lhs <- cbind(model$constraints$lhs, matrixToAdd)

      # now we can merge desiredRankConstraints with model's
      # LHS
      model$constraints$lhs <- rbind(model$constraints$lhs, desiredRankConstraints$lhs)
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

  rownames(model$constraints$lhs) <- model$constraints$constraints.labels

  return(model)
}
