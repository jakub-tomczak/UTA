#### HELPERS
calculateCoefficientsMatrix <- function(problem){
  numberOfColumns <- sum(problem$characteristicPoints)
  numberOfAlternatives <- nrow(problem$performance)
  numberOfCriteria <- ncol(problem$performance)

  characteristicPointsValues <- replicate(numberOfCriteria, c())
  for(j in seq_len(numberOfCriteria))
  {
    minV <- min(problem$performance[,j])
    maxV <- max(problem$performance[,j])
    characteristicPointsValues[[j]] <- seq(minV, maxV, length.out = problem$characteristicPoints[j])
  }

  coefficientsMatrix <- matrix(0, nrow=numberOfAlternatives, ncol=numberOfColumns)
  thresholds <- c("lower", "upper")

  for(criterion in seq_len(numberOfCriteria)){
    characteristicPoints <- characteristicPointsValues[[criterion]]
    interval <- characteristicPoints[2]-characteristicPoints[1]
    for(alternative in seq_len(numberOfAlternatives)){
      value <- problem$performance[alternative, criterion]
      for(threshold in thresholds){
        thresholdResult <- getCharacteristicPointValueAndIndex(value, threshold, characteristicPoints)
        alternativeCoefficient <- calculateCoefficient(value, thresholdResult$value, interval)
        criterionIndex <- problem$criteriaIndices[criterion] + (thresholdResult$index - 1 )

        coefficientsMatrix[alternative,  criterionIndex] <- alternativeCoefficient
      }
    }
  }
  coefficientsMatrix
}

getCharacteristicPointValueAndIndex <- function(value, typeOfBoundToFind, characteristicPoints){
  assert(typeOfBoundToFind %in% c("lower", "upper"), "Type to find should be one of the following: `lower`, `upper`.")
  assert(length(characteristicPoints) > 1, "There must be at least 2 characteristic points!")
  #lower and upper value must be lower than at least one element and greater than at least one element
  foundValue <- NULL

  if(typeOfBoundToFind == "upper" && value == max(characteristicPoints))
  {
    foundValue <- value
  } else if(typeOfBoundToFind == "lower" && value == min(characteristicPoints))
  {
    foundValue <- value
  } else {
    for(x in characteristicPoints){
      if(typeOfBoundToFind == "lower" && value > x) {
        foundValue <- x
      } else if(typeOfBoundToFind == "upper" && value < x){
        foundValue <- x
        break
      }
    }
  }

  if(is.null(foundValue))
    return(NULL)

  index <- match(foundValue, characteristicPoints)
  if(index > 0)
  {
    return(list(index=index, value=foundValue))
  }
  return(NULL)
}

calculateCoefficient <- function(value, thresholdValue, interval){
  distance <- thresholdValue - value
  #hack, avoid rounding numerical error that are lesser than the additionalEpsilon
  #they happen when alternative's value is at characteristic point
  additionalEpsilon <- 1e-9
  assert(abs(distance) <= interval + additionalEpsilon, "Distance between value and threshold cannot be higher then the interval.")
  1 - (abs(distance)/interval)
}

getLowerAndUpperValuesCoefficients <- function(value, minimalValue, intervalLength, direction, characteristicPointIndex){
  assert(direction %in% c("c", "g"), "Direction must be of type `c` or `g`.")

  #get the bounds of this chunk
  lowerValue = minimalValue + intervalLength * characteristicPointIndex
  upperValue = minimalValue + intervalLength * (characteristicPointIndex + 1)

  assert(value >= lowerValue && value <= upperValue, 'Value cannot be neither lower than lowerValue nor higher than upperValue')

  lowerValueCoeff <- if (value == lowerValue) 1.0 else 0.0
  upperValueCoeff <- if (value == upperValue) 1.0 else 0.0

  if(direction == "g") {
    #gain type
    #find the coefficients for the lower and upper bounds
    #U(value) = U(lowerValue) + (value-lowerValue)/(upperValue-lowerValue)*(U(upperValue)-U(lowerValue))=
    # = (value-lowerValue)/(upperValue-lowerValue)
    lowerValueCoeff = (lowerValue - value) / intervalLength + 1.0
    upperValueCoeff = (value - lowerValue) / intervalLength
  } else {
    #cost type
    lowerValueCoeff = (upperValue - value) / intervalLength
    upperValueCoeff = (value - upperValue) / intervalLength + 1.0
  }
  return(list(lowerValueCoeff=lowerValueCoeff, upperValueCoeff=upperValueCoeff))
}

getInterpolationCoefficients <- function(value, minimalValue, intervalLength){
  coeff <- (value - minimalValue)/intervalLength
  return(list(lowerCoefficient = 1-coeff, upperCoefficient = coeff))
}

#substractZeroCoefficients:
# TRUE == remove one column that responds for the characteristic point that has the worst value == 0
# FALSE == don't remve this column
createCriteriaIndices <- function(problem, substractZeroCoefficients){
  criteriaIndices <- c(1)
  zeroCoefficientsToSubstract <- if(substractZeroCoefficients) 1 else 0
  for(i in seq_len(ncol(problem$performance)-1))
  {
    criteriaIndices[i+1] <- criteriaIndices[i] + problem$characteristicPoints[i] - zeroCoefficientsToSubstract
  }
  criteriaIndices
}

buildPairwiseComparisonConstraint <- function(alternativeIndex, referenceAlternativeIndex, model, preferenceType) {
  stopifnot(preferenceType %in% c("weak", "strong", "indifference"))

  lhs <- model$preferencesToModelVariables[referenceAlternativeIndex,] - model$preferencesToModelVariables[alternativeIndex, ]
  dir <- "<="
  rhs <- 0

  if (preferenceType == "strong") {
    if (!is.null(model$epsilonIndex)) {
      lhs[model$epsilonIndex] <- -1
    } else {
      assert(!is.null(model$minEpsilon), "Model has not an epsilon and minEpsilon is not set.")
      rhs <- -model$minEpsilon
    }
  } else if (preferenceType == "indifference") {
    dir <- "=="
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs))
}


addVarialbesToModel <- function(constraints, variables) {
  for (var in variables)
    constraints$lhs <- cbind(constraints$lhs, 0)
  constraints$variablesType <- c(constraints$variablesTypes, variables)
  return (constraints)
}

combineConstraints <- function(...) {
  allConst <- list(...)

  lhs <- c()
  dir <- c()
  rhs <- c()
  variablesTypes <- c()

  for (const in allConst) {
    if (!is.null(const)) {
      lhs <- rbind(lhs, const$lhs)
      dir <- c(dir, const$dir)
      rhs <- c(rhs, const$rhs)
      variablesTypes <- c(variablesTypes, const$variablesTypes)
    }
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs, variablesTypes = variablesTypes))
}

removeConstraints <- function(allConst, constraintsToRemoveIndices) {
  return (list(lhs = allConst$lhs[-c(constraintsToRemoveIndices), ],
               dir = allConst$dir[-c(constraintsToRemoveIndices)],
               rhs = allConst$rhs[-c(constraintsToRemoveIndices)],
               variablesTypes = allConst$variablesTypes))
}


ua <- function(alternative, preferencesToModelVariables) {
  preferencesToModelVariables[alternative,]
}

eliminateEpsilon <- function(model) {
  stopifnot(!is.null(model$epsilonIndex))

  model$constraints$rhs <- model$constraints$rhs - model$constraints$lhs[, model$epsilonIndex] * model$minEpsilon
  model$constraints$lhs <- model$constraints$lhs[, -c(model$epsilonIndex)]
  model$constraints$variablesType <- model$constraints$variablesType[-c(model$epsilonIndex)]
  model$epsilonIndex <- NULL

  return (model)
}
