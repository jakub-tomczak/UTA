# TODO
# Check validation messages.
#' @export
buildProblem <- function(performanceTable, criteria, characteristicPoints,
                         strongPreferences = NULL, weakPreferences = NULL, indifferenceRelations = NULL,
                         strongIntensitiesPreferences = NULL, weakIntensitiesPreferences = NULL,
                         indifferenceIntensitiesRelations = NULL,
                         desiredRank = NULL)
{
  problem <- validateModel(performanceTable, criteria, characteristicPoints,
                           strongPreferences, weakPreferences, indifferenceRelations,
                           strongIntensitiesPreferences, weakIntensitiesPreferences,
                           indifferenceIntensitiesRelations,
                           desiredRank)

  nrAlternatives <- nrow(problem$performance)
  #contains TRUE on indices corresponding to the criteria that has no characteristicPoints
  #keep this information in order to calculate the solution properly
  problem$generalVF <- problem$characteristicPoints == 0
  #if there is not stated how many characteristic points we've got on a criterion
  #assume that there are as many as alternatives (each alternative is a characteristic point)
  problem$characteristicPoints <- sapply(problem$characteristicPoints, function(x) {
    if(x == 0)
      nrAlternatives
    else
      x
  })
  #only the sum of characteristic points from criteria except the least valuable point from each criterion
  problem$numberOfVariables <- sum(problem$characteristicPoints)
  problem$criteriaIndices <- getCriteriaIndices(problem, substractZeroCoefficients=FALSE)

  return(problem)
}

validateModel <- function(performanceTable, criteria, characteristicPoints,
                          strongPreferences, weakPreferences, indifferenceRelations,
                          strongIntensitiesPreferences, weakIntensitiesPreferences,
                          indifferenceIntensitiesRelations,
                          desiredRank)
{
  validate(is.matrix(performanceTable), "performanceTable", "performanceTable must be a matrix.")

  numberOfPreferences <- nrow(performanceTable)
  numberOfCriterions <- ncol(performanceTable)
  validate(ncol(criteria) == numberOfCriterions, "numberOfCriterions","Number of criteria given in performanceTable matrix is not equal to the number of criteria names.")
  validate(all(criteria %in% c("c", "g")), "criteria", "Criteria must be of type `c` or `g`.")

  validateRelations(strongPreferences, numberOfPreferences, relationName = "strong")
  validateRelations(weakPreferences, numberOfPreferences, relationName = "weak")
  validateRelations(indifferenceRelations, numberOfPreferences, relationName = "indifference")
  validateRelations(strongIntensitiesPreferences, numberOfPreferences, arity = 4, relationName = "strongIntensities")
  validateRelations(weakIntensitiesPreferences, numberOfPreferences, arity = 4, relationName = "weakIntensities")
  validateRelations(indifferenceIntensitiesRelations, numberOfPreferences, arity = 4, relationName = "indifferenceIntensities")

  validateDesiredRank(desiredRank, performanceTable, "desiredRank")

  return (list(
    performanceTable = performanceTable,
    criteria = criteria,
    characteristicPoints = characteristicPoints,
    strongPreferences = strongPreferences,
    weakPreferences = weakPreferences,
    indifferenceRelations = indifferenceRelations,
    strongIntensitiesPreferences = strongIntensitiesPreferences,
    weakIntensitiesPreferences = weakIntensitiesPreferences,
    indifferenceIntensities = indifferenceIntensitiesRelations,
    strictVF = TRUE,
    desiredRank = desiredRank
  ))
}

validateRelations <- function(relation, numberOfPreferences, arity = 2, relationName = "unknown")
{
  action <- "validateRelations"

  if(!is.null(relation))
  {
    validate(is.matrix(relation), action, paste("Relation", relationName, "must be repesented by a matrix"))
    validate(ncol(relation) == arity, action, paste("Relation", relationName, "has arity:", arity, ". Number of arguments typed:", ncol(relation), "."))

    #check weather minimum and maximum index are in performanceTable indices bounds
    validate(all(relation >= 1), action, "There is no preference with index lower than 1")
    validate(all(relation <= numberOfPreferences), action, paste("There is no preference with index higher than:", numberOfPreferences, ". Typed a preference with index:", max(relation)))
  }
}

validateDesiredRank <- function(desiredRank, performanceTable, action){
  if(is.null(desiredRank)){
    return(matrix(nrow=0, ncol=3))
  }
  error_text <- ifelse(action == "desiredRank",
                       "l = lower place in the ranking, u = upper place in the ranking.",
                        "l = lower value of the utility value, u = upper value of the utility value.")

  validate( is.matrix(desiredRank) && ncol(desiredRank) == 3, action,
          paste(action, " variable should be a matrix with 3 columns (a, l, u), where a = alternative index, ", error_text))

  if(action == "desiredRank"){
    validate(all(desiredRank >= 1), action, "There is no an alternative with index lower than 1")
    numberOfAlternatives <- nrow(performanceTable)
    validate(all(desiredRank <= numberOfAlternatives), action,
             paste("Both alternatives indices and ", action, " must be lower than", numberOfAlternatives))
    validate(all(desiredRank[,2] >= desiredRank[,3]), action,
             paste("All lower places in the desiredRank should be greater or equal to the upper places"))
  } else {
    validate(all(desiredRank[,1] >= 1), action, "There is no an alternative with index lower than 1")
    validate(all(desiredRank[,c(2,3)] >= 0) && all(desiredRank[,c(2,3)] <= 1), action, "Desired utility values must be in range <0, 1>")
    numberOfAlternatives <- nrow(performanceTable)
    validate(all(desiredRank <= numberOfAlternatives), action,
             paste("Both alternatives indices and ", action, " must be lower than", numberOfAlternatives))
    validate(all(desiredRank[,2] <= desiredRank[,3]), action,
             paste("Lower value of the utility value must be lower than the upper value of the utility value."))
  }
}

validate <- function(condition, action, message){
  assert(condition, paste("Error while validating", action, ":", message))
}
