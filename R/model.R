#### BUILDING MODEL

#' @export
buildModel <- function(problem, method, minK = 1e-4, minEpsilon = 1e-4, bigNumber = 1e9) { # includeEpsilonAsVariable,
  availableMethods <- getAvailableMethods()
  assert(method %in% availableMethods, paste(availableMethods, " "))
  nrAlternatives <- nrow(problem$performance)
  nrCriteria <- ncol(problem$performance)

  # used in monotonicity constraint
  includeRho <- FALSE
  # used in strong preference constraint
  includeK <- FALSE
  includeEpsilon <- FALSE

  rhoEpsilon <- 0
  epsilonEpsilon <- 0
  if(method == availableMethods$utamp1){
    includeK <- TRUE
  } else if(method == availableMethods$utamp2){
    includeRho <- TRUE
    includeK <- TRUE
  } else if(method == availableMethods$roruta){
    includeEpsilon <- TRUE
  }
  #preferences to model variables used in solution
  coefficientsMatrix <- calculateCoefficientsMatrix(problem)

  #when constructing problem we get into consideration only the number of characteristic points from value functions
  #we don't consider rho and k
  #here we add one variable that corresponds to rho value
  numberOfVariables <- problem$numberOfVariables + ifelse(includeRho, 1, 0) + ifelse(includeK, 1, 0)
  rhoIndex <- NULL
  kIndex <- NULL
  epsilonIndex <- NULL

  if(includeRho)
    rhoIndex <- numberOfVariables-1
  if(includeK){
    kIndex <- numberOfVariables
  }
  if(includeEpsilon){
    epsilonIndex <- numberOfVariables
  }


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
                                    monotonicityConstraints(problem, numberOfVariables, nrCriteria, rhoIndex, rhoEpsilon))

  lhs.colnames <- colnames(coefficientsMatrix)
  if(includeRho)
  {
    lhs.colnames <- c(lhs.colnames, "rho")
  }
  if(includeK)
  {
    lhs.colnames <- c(lhs.colnames, "k")
  }
  if(includeEpsilon)
  {
    lhs.colnames <- c(lhs.colnames, "eps")
    constraints$lhs <- cbind(constraints$lhs, 0)
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
    epsilonIndex = epsilonIndex,
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
  if(method == availableMethods$roruta){
    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "strong"))

    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "weak"))
    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "indifference"))

    # we can add here column names, merging current colnames with "epsilon" would fail,
    # because after adding a column, R automatically adds an empty label for that new column
    model$epsilonIndex <- ncol(model$constraints$lhs)

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
    model$constraints <- splitVariable(model, model$kIndex, "k")
  } else if(method == availableMethods$utamp2) {
    # split rho into rho_jk
    model$constraints <- splitVariable(model, model$rhoIndex, "rho")
    model$constraints <- splitVariable(model, model$kIndex, "k")
  }

  rownames(model$constraints$lhs) <- model$constraints$constraints.labels

  return(model)
}
