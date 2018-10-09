#### HELPERS
createPreferencesToModelVariables <- function(problem, firstChPointVariableIndex)
{
  nrAlternatives <- nrow(problem$performanceTable)
  nrCriteria <- ncol(problem$performanceTable)

  preferencesToModelVariables <- replicate(nrCriteria, replicate(nrAlternatives, list()))
  for (j in seq_len(nrCriteria)) {
    minimalValue <- min(problem$performanceTable[,j]) #minimal value on this criterium
    maximalValue <- max(problem$performanceTable[,j]) #maximal value on this criterium
    direction <- problem$criteria[j]  #
    mostValuableValue <- if(direction == 'c') minimalValue else maximalValue
    leastValuableValue <- if(direction == 'c') maximalValue else minimalValue


    if (problem$characteristicPoints[j] == 0) {
      #set coeff = 1 on all values on the current criterion
      #except for the minimal one
      minimalValueIndex = match(minimalValue, problem$performanceTable[,j])
      for(i in seq_len(nrAlternatives))
      {
        if(i == minimalValueIndex)
        {
          if(direction == "g")
          {
            perfToModelVariables[[i,j]][[1]] = c(firstChPointVariableIndex[j] + index - 2, 1.0)
          } else {
            perfToModelVariables[[i,j]][[1]] = c(firstChPointVariableIndex[j] + index - 1, 1.0)
          }
        }
      }
    } else {
      numberOfCharacteristicPoints <- problem$characteristicPoints[j]
      #interval between characteristic points
      intervalLength <- (maximalValue - minimalValue) / (numberOfCharacteristicPoints - 1);

      for (i in seq_len(nrAlternatives)) {
        #current value
        value <- problem$performanceTable[i, j]
        if(value == mostValuableValue) #epsilion?
        {
          #for a criterion of type 'gain' offset is lowered by 2
          #in order to has index 0 - best value is on the 0th index
          #while for a criterion of type 'cost' best value is on the
          #last index
          offset <- if(direction == "c") 0 else numberOfCharacteristicPoints - 2
          preferencesToModelVariables[[i,j]][[1]] <- c(firstChPointVariableIndex[j] + offset, 1.0)

          #least valuable alternative has its score equal to 0, that is why we ommit this this special case
          #least valuable alternative has an empty list in perfToModelVariable matrix
        } else if(value != leastValuableValue) {
          #get the index of a chunk of intervalLength length in which value stays
          characteristicPointIndex <- floor((value-minimalValue)/intervalLength)

          #get the bounds of this chunk
          lowerValue = minimalValue + intervalLength * characteristicPointIndex
          upperValue = minimalValue + intervalLength * (characteristicPointIndex + 1)

          lowerCoeff <- 0.0
          upperCoeff <- 0.0

          if (value <= lowerValue) {
            lowerCoeff = 1.0
            upperCoeff = 0.0
          } else if (value >= upperValue) {
            lowerCoeff = 0.0
            upperCoeff = 1.0
          } else if(direction == "g") {
            #gain type
            #find the coeff of the current value using linear regression
            #between lower and upper value in the current interval
            lowerCoeff = (lowerValue - value) / (upperValue - lowerValue) + 1.0
            upperCoeff = (value - lowerValue) / (upperValue - lowerValue)
          } else {
            #cost type
            lowerCoeff = (upperValue - value) / (upperValue - lowerValue)
            upperCoeff = (value - upperValue) / (upperValue - lowerValue) + 1.0
          }

          if(direction == "g")
          {
            #gain type
            if(characteristicPointIndex > 0)
            {
              preferencesToModelVariables[[i, j]][[1]] = c(firstChPointVariableIndex[j] + characteristicPointIndex - 1, lowerCoeff)
              preferencesToModelVariables[[i, j]][[2]] = c(firstChPointVariableIndex[j] + characteristicPointIndex, upperCoeff)
            }
            else
            {
              preferencesToModelVariables[[i, j]][[1]] = c(firstChPointVariableIndex[j] + characteristicPointIndex, upperCoeff)
            }
          }
          else
          {
            #cost type
            if(characteristicPointIndex < numberOfCharacteristicPoints - 2)
            {
              preferencesToModelVariables[[i, j]][[1]] = c(firstChPointVariableIndex[j] + characteristicPointIndex, lowerCoeff)
              preferencesToModelVariables[[i, j]][[2]] = c(firstChPointVariableIndex[j] + characteristicPointIndex + 1, upperCoeff)
            }
            else
            {
              preferencesToModelVariables[[i, j]][[1]] = c(firstChPointVariableIndex[j] + characteristicPointIndex, lowerCoeff)
            }
          }
        }
      }
    }
  }
  return(preferencesToModelVariables)
}


buildPairwiseComparisonConstraint <- function(alternative, referenceAlternative, model, type, method) {
  stopifnot(type %in% c("weakPreference", "strongPreference", "indifference"))
  #-1 to remove epsilion index
  variables <- ncol(model$constraints$lhs) - 1
  lhs <- ua(referenceAlternative, ncol(model$constraints$lhs), model$preferencesToModelVariables) - ua(alternative, ncol(model$constraints$lhs), model$preferencesToModelVariables)
  method
  if(method == "uta")
  {
    #for UTA object function
    additionalLHSMatrix <- matrix(rep(0, 2*variables^2), ncol=2*variables)
    lhs <- merge(lhs, additionalLHSMatrix)
  }
  dir <- "<="
  rhs <- 0

  if (type == "strongPreference") {
    if (is.null(model$epsilonIndex)) {
      rhs <- -model$minEpsilon
    } else {
      lhs[model$epsilonIndex] <- 1
    }
  } else if (type == "indifference") {
    dir <- "=="
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs))
}


addVarialbesToModel <- function(constraints, variables) {
  for (var in variables)
    constraints$lhs <- cbind(constraints$lhs, 0)
  constraints$types <- c(constraints$types, variables)
  return (constraints)
}

combineConstraints <- function(...) {
  allConst <- list(...)

  lhs <- c()
  dir <- c()
  rhs <- c()
  types <- c()

  for (const in allConst) {
    if (!is.null(const)) {
      lhs <- rbind(lhs, const$lhs)
      dir <- c(dir, const$dir)
      rhs <- c(rhs, const$rhs)
      types <- c(types, const$types)
    }
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs, types = types))
}

removeConstraints <- function(allConst, constraintsToRemoveIndices) {
  return (list(lhs = allConst$lhs[-c(constraintsToRemoveIndices), ],
               dir = allConst$dir[-c(constraintsToRemoveIndices)],
               rhs = allConst$rhs[-c(constraintsToRemoveIndices)],
               types = allConst$types))
}


ua <- function(alternative, nrVariables, preferencesToModelVariables) {
  res <- rep(0, nrVariables)

  for (j in seq_len(ncol(preferencesToModelVariables))) {
    for (k in seq_len(length(preferencesToModelVariables[[alternative, j]]))) {
      res[preferencesToModelVariables[[alternative, j]][[k]][1]] <- preferencesToModelVariables[[alternative, j]][[k]][2]
    }
  }

  return (res)
}

eliminateEpsilon <- function(model) {
  stopifnot(!is.null(model$epsilonIndex))

  model$constraints$rhs <- model$constraints$rhs - model$constraints$lhs[, model$epsilonIndex] * model$minEpsilon
  model$constraints$lhs <- model$constraints$lhs[, -c(model$epsilonIndex)]
  model$constraints$types <- model$constraints$types[-c(model$epsilonIndex)]
  model$epsilonIndex <- NULL

  return (model)
}
