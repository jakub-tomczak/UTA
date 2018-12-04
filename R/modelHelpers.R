#### HELPERS
normalizationConstraint <- function(problem, numberOfVariables, numberOfCriteria){
  ## sum to 1
  lhs <- rep(0, numberOfVariables)

  for (j in seq_len(numberOfCriteria)) {
    if (problem$criteria[j] == 'g')
      lhs[problem$criteriaIndices[j] + problem$characteristicPoints[j] - 1] <- 1
    else
      lhs[problem$criteriaIndices[j]] <- 1
  }

  list(lhs = lhs, dir = "==", rhs = 1)
}

monotonicityConstraints <- function(problem, numberOfVariables, numberOfCriteria, rhoIndex){
  ## monotonicity of vf
  constraints <- list()
  for (j in seq_len(numberOfCriteria)) {
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
  constraints
}

pairwisePreferenceConstraints <- function(problem, model, typeOfRelation){
  assert(typeOfRelation %in% c("preference", "indifference"),
         paste("typeOfRelation", typeOfRelation, "is not valid. Valid types of pairwise relations are: preference, idifference"))

  constraints <- list()
  relationsMatrix <- NULL
  if(typeOfRelation == "preference" && !is.null(problem$preferenceRelations)){
    relationsMatrix <- problem$preferenceRelations
  } else if(typeOfRelation == "indifference" && !is.null(problem$indifferenceRelations)) {
    relationsMatrix <-problem$indifferenceRelations
  }

  if (is.matrix(relationsMatrix)) {
    for (k in seq_len(nrow(relationsMatrix))) {
      alternative <- relationsMatrix[k, 1]
      referenceAlternative <- relationsMatrix[k, 2]
      constraints <- combineConstraints(constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, relationsType = typeOfRelation))

    }
  }
  constraints
}

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
    interval <- characteristicPoints[2] - characteristicPoints[1]
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
getCriteriaIndices <- function(problem, substractZeroCoefficients){
  criteriaIndices <- c(1)
  zeroCoefficientsToSubstract <- if(substractZeroCoefficients) 1 else 0
  for(i in seq_len(ncol(problem$performance)-1))
  {
    criteriaIndices[i+1] <- criteriaIndices[i] + problem$characteristicPoints[i] - zeroCoefficientsToSubstract
  }
  criteriaIndices
}

buildLHSForPairwiseComparison <- function(alternativeIndex, referenceAlternativeIndex, model){
  # U(referenceAlternative) - U(alternative)
  model$preferencesToModelVariables[referenceAlternativeIndex,] - model$preferencesToModelVariables[alternativeIndex, ]
}

buildPairwiseComparisonConstraint <- function(alternativeIndex, referenceAlternativeIndex, model, relationsType) {
  assert(relationsType %in% c("preference", "indifference"),
         paste("relationsType", relationsType, "is not valid. Valid types of pairwise relations are: preference, idifference"))

  marginalValuesVariables <- c()
  if(length(alternativeIndex) == 1){
    marginalValuesVariables <- buildLHSForPairwiseComparison(alternativeIndex, referenceAlternativeIndex, model)
  } else {
    marginalValuesVariables <- buildLHSForPairwiseComparison(alternativeIndex[0], referenceAlternativeIndex[0], model) -
      buildLHSForPairwiseComparison(alternativeIndex[1], referenceAlternativeIndex[1], model)
  }
  # lhs holds a vector of a length equal to the number of marginal values
  # lhs should be a vector of the length of a number of columns in constraints matrix
  lhs <- rep(0, ncol(model$constraints$lhs))
  lhs[0:length(marginalValuesVariables)] <- marginalValuesVariables

  dir <- "<="
  rhs <- 0

  if (relationsType == "preference") {
    if (!is.null(model$kIndex)) {
      lhs[model$kIndex] <- 1
    } else {
      assert(!is.null(model$minEpsilon), "Model has not an epsilon and minEpsilon is not set.")
      rhs <- -model$minEpsilon
    }
  } else if (relationsType == "indifference") {
    dir <- "=="
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs))
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

ua <- function(alternative, preferencesToModelVariables) {
  preferencesToModelVariables[alternative,]
}

removeColumnsFromModelConstraints <- function(model, columnsIndices){
  assert(ncol(model$constraints$lhs) >= max(columnsIndices),
         paste("Cannot remove a column with index", max(columnsIndices), ", LHS contains only", ncol(model$constraints$lhs)))
  if(length(columnsIndices) < 1){
    return(model)
  }
  sortedColumnsIndices <- sort(columnsIndices)
  lhs <- model$constraints$lhs[, -sortedColumnsIndices]
  variablesTypes <- model$constraints$variablesTypes[-sortedColumnsIndices]

  # remove columns from the model$preferencesToModel if appropriate columns were indicated
  preferencesIndicesToRemove <- sortedColumnsIndices[sortedColumnsIndices < ncol(model$preferencesToModelVariables)]
  preferencesToModelVariables <- model$preferencesToModelVariables
  if(length(preferencesIndicesToRemove) > 0){
    preferencesToModelVariables <- model$preferencesToModelVariables[, -preferencesIndicesToRemove]
  }

  # if the columnIndex is equal to the of the additional variables then remove it
  rhoIndex <- if(model$rhoIndex %in% sortedColumnsIndices) NULL else model$rhoIndex
  kIndex <- if(model$kIndex %in% sortedColumnsIndices) NULL else model$kIndex

  # otherwise move its position in the constraints matrix to the left (decrease its index)
  if(!is.null(rhoIndex)){
    positionToMove <- length(sortedColumnsIndices[sortedColumnsIndices < rhoIndex])
    rhoIndex <- rhoIndex - positionToMove
  }

  if(!is.null(kIndex)){
    positionToMove <- length(sortedColumnsIndices[sortedColumnsIndices < kIndex])
    kIndex <- kIndex - positionToMove
  }

  constraints <- list(lhs = lhs,
                     rhs = model$constraints$rhs,
                     dir = model$constraints$dir,
                     variablesTypes = variablesTypes)

  # return a new model
  list(
    constraints = constraints,
    criteriaIndices = model$criteriaIndices,
    rhoIndex = rhoIndex,
    kIndex = kIndex,
    chPoints = model$chPoints,
    preferencesToModelVariables = preferencesToModelVariables,
    criterionPreferenceDirection = model$criterionPreferenceDirection,
    generalVF = model$generalVF,
    minEpsilon = model$minEpsilon,
    methodName = model$methodName,
    performances = model$performance
  )
}
