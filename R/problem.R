#' @export
buildProblem <- function(performanceTable, criteria, characteristicPoints,
                         preferenceRelations = NULL, indifferenceRelations = NULL)
{
  problem <- validateModel(performanceTable, criteria, characteristicPoints,
                           preferenceRelations, indifferenceRelations)

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
                          preferenceRelations, indifferenceRelations, method = NULL)
{
  assert(is.matrix(performanceTable), "PerformanceTable must be a matrix.")

  numberOfPreferences <- nrow(performanceTable)
  numberOfCriterions <- ncol(performanceTable)
  assert(ncol(criteria) == numberOfCriterions, "Number of criteria given in performanceTable matrix is not equal to the number of criteria names.")
  assert(all(criteria %in% c("c", "g")), "Criteria must be of type `c` or `g`.")

  validateRelations(preferenceRelations, numberOfPreferences)
  validateRelations(indifferenceRelations, numberOfPreferences)

  # assert(is.matrix(indifferenceRelations), "Indifference must be a matrix")

  return (list(
    performanceTable = performanceTable,
    criteria = criteria,
    characteristicPoints = characteristicPoints,
    preferenceRelations = preferenceRelations,
    indifferenceRelations = indifferenceRelations,
    strictVF = TRUE
  ))
}

validateRelations <- function(relation, numberOfPreferences)
{
  if(!is.matrix(relation) || is.null(relation))
  {
    return (matrix(nrow=0, ncol=2))
  }

  assert(ncol(relation) == 2, "Preference relation must be given in pairs.")

  #check weather minimum and maximum index are in performanceTable indices bounds
  assert(min(relation) >= 1, "There is no preference with index lower than 1")
  assert(max(relation) <= numberOfPreferences, paste("There is no preference with index higher than", numberOfPreferences))
}
