#' @export
buildProblem <- function(preferences, criteria, characteristicPoints, strongPreferences = NULL,
                         weakPreferences = NULL , indifference = NULL)
{
  return(validateModel(preferences, criteria, strongPreferences,
                       weakPreferences, characteristicPoints, indifference))
}

#' @export
buildModel_new <- function(model, epsilion = 10e-7)
{
  if(!is.list(model))
  {
    stop( "Model must be of a list type.")
  }
  model$epsilion = epsilion
  lp <- generateLPmodel(model)

}

generateLPmodel <- function(model)
{
  constraintValues <- c()
  for (criterion in seq_len(ncol(model$preferences)))
  {
    for(alternative in seq_len(nrow(model$preferences)))
    {
      a <- c(a, c(criterion, alternative))
      print(paste("cri", criterion, "alt", alternative))
    }
  }
}
#' @export
validateModel <- function(preferences, criteria, strongPreferences,
                          weakPreferences, characteristicPoints, indifference)
{
  assert(is.matrix(preferences), "Preferences must be given as a matrix.")

  numberOfPreferences <- nrow(preferences)
  numberOfCriterions <- ncol(preferences)
  assert(ncol(criteria) == numberOfCriterions, "Number of criteria given in preferences matrix is not equal to the number of criteria names.")
  assert(all(criteria %in% c("c", "g")), "Criteria must be of type `c` or `g`.")
  #what if a,b,c,d
  #aPb, aPc, bPc, aPd, bPd, cPd
  #stopifnot(ncol(strongPreferences) <= numberOfPreferences) #stops execution if number of strong preferences pairs is greater than number of preferences

  strongPreferences <- validatePreferenceRelation(strongPreferences, numberOfPreferences)
  weakPreferences <- validatePreferenceRelation(weakPreferences, numberOfPreferences)
  indifference <- validatePreferenceRelation(indifference, numberOfPreferences)

  assert(is.matrix(indifference), "Indifference must be a matrix")

  return (list(
        preferences = preferences,
        criteria = criteria,
        strongPreferences = strongPreferences,
        weakPreferences = weakPreferences,
        characteristicPoints = characteristicPoints,
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

  #check weather minimum and maximum index are in preferences indices bounds
  assert(min(preferenceRelation) >= 1, "There is no preference with index lower than 1")
  assert(max(preferenceRelation) <= numberOfPreferences, paste("There is no preference with index higher than", numberOfPreferences))

  return (preferenceRelation)
}

assert <- function(expression, message)
{
  if(!all(expression))
  {
    stop(if(is.null(message)) "Error" else message)
  }
}
