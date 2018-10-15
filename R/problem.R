#' @export
buildProblem <- function(performanceTable, criteria, characteristicPoints, strongPreferences = NULL,
                         weakPreferences = NULL , indifference = NULL)
{
  problem <- validateModel(performanceTable, criteria, strongPreferences,
                           weakPreferences, characteristicPoints, indifference)

  nrAlternatives <- nrow(problem$performanceTable)
  #contains TRUE on indices corresponding to the criteria that has no characteristicPoints
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
  problem$numberOfVariables <- sum(characteristicPoints) - length(characteristicPoints)

  return(problem)
}

#' @export
validateModel <- function(performanceTable, criteria, strongPreferences,
                          weakPreferences, characteristicPoints, indifference)
{
  assert(is.matrix(performanceTable), "PerformanceTable must be a matrix.")

  numberOfPreferences <- nrow(performanceTable)
  numberOfCriterions <- ncol(performanceTable)
  assert(ncol(criteria) == numberOfCriterions, "Number of criteria given in performanceTable matrix is not equal to the number of criteria names.")
  assert(all(criteria %in% c("c", "g")), "Criteria must be of type `c` or `g`.")
  #what if a,b,c,d
  #aPb, aPc, bPc, aPd, bPd, cPd
  #stopifnot(ncol(strongPreferences) <= numberOfPreferences) #stops execution if number of strong preference pairs is greater than number of performanceTable

  strongPreferences <- validatePreferenceRelation(strongPreferences, numberOfPreferences)
  weakPreferences <- validatePreferenceRelation(weakPreferences, numberOfPreferences)
  indifference <- validatePreferenceRelation(indifference, numberOfPreferences)

  assert(is.matrix(indifference), "Indifference must be a matrix")

  return (list(
    performanceTable = performanceTable,
    criteria = criteria,
    characteristicPoints = characteristicPoints,
    strongPreferences = strongPreferences,
    weakPreferences = weakPreferences,
    indifference = indifference,
    strictVF = TRUE
  ))
}

validatePreferenceRelation <- function(preferenceRelation, numberOfPreferences)
{
  if(!is.matrix(preferenceRelation) || is.null(preferenceRelation))
  {
    return (matrix(nrow=0, ncol=2))
  }

  assert(ncol(preferenceRelation) == 2, "Preference relation must be given in pairs.")

  #check weather minimum and maximum index are in performanceTable indices bounds
  assert(min(preferenceRelation) >= 1, "There is no preference with index lower than 1")
  assert(max(preferenceRelation) <= numberOfPreferences, paste("There is no preference with index higher than", numberOfPreferences))

  return (preferenceRelation)
}
