# TODO
# Check validation messages.
#' @export
buildProblem <- function(performanceTable, criteria, characteristicPoints,
                         preferenceRelations = NULL, indifferenceRelations = NULL,
                         preferenceIntensitiesRelations = NULL, indifferenceIntensitiesRelations = NULL,
                         desiredRank = NULL)
{
  problem <- validateModel(performanceTable, criteria, characteristicPoints,
                           preferenceRelations, indifferenceRelations,
                           preferenceIntensitiesRelations, indifferenceIntensitiesRelations,
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

# TODO
# Remove method parameter.
validateModel <- function(performanceTable, criteria, characteristicPoints,
                          preferenceRelations, indifferenceRelations,
                          preferenceIntensitiesRelations, indifferenceIntensitiesRelations,
                          desiredRank,
                          method = NULL)
{
  validate(is.matrix(performanceTable), "performanceTable", "performanceTable must be a matrix.")

  numberOfPreferences <- nrow(performanceTable)
  numberOfCriterions <- ncol(performanceTable)
  validate(ncol(criteria) == numberOfCriterions, "numberOfCriterions","Number of criteria given in performanceTable matrix is not equal to the number of criteria names.")
  validate(all(criteria %in% c("c", "g")), "criteria", "Criteria must be of type `c` or `g`.")

  validateRelations(preferenceRelations, numberOfPreferences, relationName = "preference")
  validateRelations(indifferenceRelations, numberOfPreferences, relationName = "indifference")
  validateRelations(preferenceIntensitiesRelations, numberOfPreferences, arity = 4, relationName = "preferenceIntensities")
  validateRelations(indifferenceIntensitiesRelations, numberOfPreferences, arity = 4, relationName = "indifferenceIntensities")

  validateDesiredRank(desiredRank, performanceTable)
  # assert(is.matrix(indifferenceRelations), "Indifference must be a matrix")

  return (list(
    performanceTable = performanceTable,
    criteria = criteria,
    characteristicPoints = characteristicPoints,
    preferenceRelations = preferenceRelations,
    indifferenceRelations = indifferenceRelations,
    preferenceIntensities = preferenceIntensitiesRelations,
    indifferenceIntensities = indifferenceIntensitiesRelations,
    strictVF = TRUE,
    desiredRank = desiredRank
  ))
}

validateRelations <- function(relation, numberOfPreferences, arity = 2, relationName = "unknown")
{
  action <- "validateRelations"
  if(!is.matrix(relation) || is.null(relation))
  {
    return (matrix(nrow=0, ncol=arity))
  }

  validate(ncol(relation) == arity, action, paste("Relation", relationName, "has arity:", arity, ". Number of arguments typed:", ncol(relation), "."))

  #check weather minimum and maximum index are in performanceTable indices bounds
  validate(all(relation >= 1), action, "There is no preference with index lower than 1")
  validate(all(relation <= numberOfPreferences), action, paste("There is no preference with index higher than:", numberOfPreferences, ". Typed a preference with index:", max(relation)))
}

validateDesiredRank <- function(desiredRank, performanceTable){
  action <- "desiredRank"
  if(is.null(desiredRank)){
    return(matrix(nrow=0, ncol=3))
  }
  validate( is.matrix(desiredRank) && ncol(desiredRank) == 3, action,
          "desiredRank variable should be a matrix with 3 columns (a, l, u), where a = alternative index, l = lower place in the ranking, u = upper place in the ranking.")

  validate(all(desiredRank >= 1), action, "There is no an alternative with index lower than 1")
  numberOfAlternatives <- nrow(performanceTable)
  validate(all(desiredRank <= numberOfAlternatives), action,
           paste("Both alternatives indices and rank desired place must be lower than", numberOfAlternatives))
  validate(all(problem$desiredRank[,2] >= problem$desiredRank[,3]), action,
           "All lower places in the desiredRank should be greater or equal to the upper places")
}

validate <- function(condition, action, message){
  assert(condition, paste("Error while validating", action, ":", message))
}
