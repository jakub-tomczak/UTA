context("test-uta_tests")

prepareData <- function(performanceMatrix = NULL, criteriaTypes = c('g', 'g'),
                        characteristicPoints = c(2,2))
{
  strongPreferences <- rbind(c(1,2))
  criteriaTypes <- criteriaTypes
  characteristicPoints <- characteristicPoints
  if(is.null(performanceMatrix))
  {
    performance <- matrix(c(5, 2, 3, 7, 0.5, 0.9, 0.5, 0.4), ncol = length(criteriaTypes))
  } else {
    performance <- performanceMatrix
  }
  expect_is(performance, 'matrix')
  problem <- buildProblem(performance, criteriaTypes, characteristicPoints, strongPreferences)
  expect_is(problem, 'list')
  return(problem)
}

test_that("Should pass data preparation.", {
  data <- prepareData()
  expect_is(data$performance, 'matrix')
  expect_is(data, 'list')
})

test_that("Should calculate lower and upper coefficients.", {
  value <- 5
  minimalValue <- 2
  intervalLength <- 2.5
  characteristicPointIndex <- 1

  direction = 'g'
  coefficients <- getLowerAndUpperValuesCoefficients(value, minimalValue, intervalLength, direction, characteristicPointIndex)
  expect_is(coefficients, "list")
  expect_is(coefficients$lowerValueCoeff, "numeric")
  expect_equal(coefficients$lowerValueCoeff, 0.8)
  expect_equal(coefficients$upperValueCoeff, 0.2)

  characteristicPointIndex <- 1
  direction <- 'c'
  coefficients <- getLowerAndUpperValuesCoefficients(value, minimalValue, intervalLength, direction, characteristicPointIndex)
  expect_equal(coefficients$lowerValueCoeff, 0.8)
  expect_equal(coefficients$upperValueCoeff, 0.2)

  characteristicPointIndex <- 0
  value = 4.3
  coefficients <- getLowerAndUpperValuesCoefficients(value, minimalValue, intervalLength, direction, characteristicPointIndex)
  expect_equal(coefficients$lowerValueCoeff, 0.08)
  expect_equal(coefficients$upperValueCoeff, 0.92)
})

test_that("Should calculate nearest characteristic points' index and value properly.", {
  min <- 2
  max <- 7
  numberOfCharacteristicPoints <- 3
  interval <- (max - min)/(numberOfCharacteristicPoints-1)
  #(2, 4.5, 7)
  characteristicPoints <- seq(min, max, interval)

  #value in the middle
  result <- getCharacteristicPointValueAndIndex(3, "lower", characteristicPoints)
  expect_true(result$index == 1 && result$value == 2)
  result <- getCharacteristicPointValueAndIndex(3, "upper", characteristicPoints)
  expect_true(result$index == 2 && result$value == 4.5)
  result <- getCharacteristicPointValueAndIndex(8, "upper", characteristicPoints)
  expect_true(is.null(result))

  #lower value
  result <- getCharacteristicPointValueAndIndex(2, "lower", characteristicPoints)
  expect_equal(result$value, 2)
  result <- getCharacteristicPointValueAndIndex(4.5, "lower", characteristicPoints)
  expect_equal(result$value, 2)
  result <- getCharacteristicPointValueAndIndex(7, "lower", characteristicPoints)
  expect_equal(result$value, 4.5)

  #upper value
  result <- getCharacteristicPointValueAndIndex(2, "upper", characteristicPoints)
  expect_equal(result$value, 4.5)
  result <- getCharacteristicPointValueAndIndex(4.5, "upper", characteristicPoints)
  expect_equal(result$value, 7)
  result <- getCharacteristicPointValueAndIndex(7, "upper", characteristicPoints)
  expect_equal(result$value, 7)
})

test_that("Should calculate coefficient properly", {
  min <- 2
  max <- 7
  interval <- 2.5
  #(2, 4.5, 7)
  characteristicPoints <- seq(min, max, interval)

  value <- c(2, 3.25, 4.5)
  threshold <- 4.5
  expectedCoefficient <- c(0, .5, 1)
  expect_true(all(calculateCoefficient(value, threshold, interval) == expectedCoefficient))

  value <- c(2, 3.25, 4.5)
  threshold <- 2
  expectedCoefficient <- c(1, .5, 0)
  expect_true(all(calculateCoefficient(value, threshold, interval) == expectedCoefficient))

  value <- 7
  threshold <- 7
  expectedCoefficient <- 1
  expect_equal(calculateCoefficient(value, threshold, interval), 1)
  threshold <- 4.5
  expect_equal(calculateCoefficient(value, threshold, interval), 0)

  value <- .5
  threshold <- .4
  expect_equal(calculateCoefficient(value, threshold, .5), 4/5)
  threshold <- .9
  expect_equal(calculateCoefficient(value, threshold, .5), 1/5)

  value <- 7
  threshold <- 10
  expectedCoefficient <- .7
  expect_error(calculateCoefficient(value, threshold, interval))
})

test_that("Should calculate coefficientsMatrix.",{
  problem <- prepareData(criteriaTypes = c('g', 'g'))

  coefficientsMatrix <- calculateCoefficientsMatrix(problem)
  expect_equal(coefficientsMatrix[1,1], 2/5)
  expect_equal(coefficientsMatrix[1,2], 3/5)
  expect_equal(coefficientsMatrix[1,3], 4/5)
  expect_equal(coefficientsMatrix[1,4], 1/5)

  problem <- prepareData(criteriaTypes = c('c', 'g'))
  expect_equal(coefficientsMatrix[1,1], 2/5)
  expect_equal(coefficientsMatrix[1,2], 3/5)
  expect_equal(coefficientsMatrix[1,3], 4/5)
  expect_equal(coefficientsMatrix[1,4], 1/5)


  problem <- prepareData(criteriaTypes = c('c', 'c'))
  expect_equal(coefficientsMatrix[1,1], 2/5)
  expect_equal(coefficientsMatrix[1,2], 3/5)
  expect_equal(coefficientsMatrix[1,3], 4/5)
  expect_equal(coefficientsMatrix[1,4], 1/5)
})

test_that("Should build pairwise constraints of different types.", {

  #buildPairwiseComparisonConstraint
})

test_that("Should calculate utility values.", {
  problem <- prepareData(criteriaTypes = c('g', 'c'), characteristicPoints = c(4,3))
  #solution from LP
  values <- c(0.2, 0.4, 0.6, 0.4, 0.2, 0.2)
  model <- buildModel(problem)
  utilityValues <- calculateUtilityValues(model, values)

  expect_equal(utilityValues[1], 0.68)
  expect_equal(utilityValues[2], 0.0)
  expect_equal(utilityValues[3], 0.44)
  expect_equal(utilityValues[4], 1.0)
})

test_that("Should create ranking.", {
  problem <- prepareData(criteriaTypes = c('g', 'c'), characteristicPoints = c(4,3))
  #solution from LP
  values <- c(0.2, 0.4, 0.6, 0.4, 0.2, 0.2)
  model <- buildModel(problem)
  utilityValues <- calculateUtilityValues(model, values)
  ranking <- generateRanking(utilityValues)

  expect_equal(ranking$alternative, c(4,1,3,2))
})

test_that("Should calculate characteristic points coefficients", {
  model <- buildModel(prepareData(criteriaTypes = c('g','c'), characteristicPoints = c(4,3)))
  coefficients <- getCharacteristicPointsCoefficients(model)

  expect_is(coefficients, 'list')
  expect_equal(coefficients[[1]], c(1/3, 2/3, 1))
  expect_equal(coefficients[[2]], c(1, .5))

  ##fails when there is only one criterion
  #model <- buildModel(prepareData(criteriaTypes = c('g'), characteristicPoints = c(4)))
  #coefficients <- getCharacteristicPointsCoefficients(model)
  #expect_is(coefficients, 'matrix')
  #expect_equal(coefficients[1], c(1/3, 2/3, 1))
})
