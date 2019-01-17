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

  numberOfCriterions <- ncol(performanceTable)
  validate(ncol(criteria) == numberOfCriterions, "numberOfCriterions","Number of criteria given in performanceTable matrix is not equal to the number of criteria names.")
  validate(all(criteria %in% c("c", "g")), "criteria", "Criteria must be of type `c` or `g`.")

  strongPreferences <- validateRelations(strongPreferences, performanceTable, relationName = "strong")
  weakPreferences <- validateRelations(weakPreferences, performanceTable, relationName = "weak")
  indifferenceRelations <- validateRelations(indifferenceRelations, performanceTable, relationName = "indifference")
  strongIntensitiesPreferences <- validateRelations(strongIntensitiesPreferences, performanceTable, arity = 4, relationName = "strongIntensities")
  weakIntensitiesPreferences <- validateRelations(weakIntensitiesPreferences, performanceTable, arity = 4, relationName = "weakIntensities")
  indifferenceIntensitiesRelations <- validateRelations(indifferenceIntensitiesRelations, performanceTable, arity = 4, relationName = "indifferenceIntensities")

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

validateRelations <- function(relation, performances, arity = 2, relationName = "unknown")
{
  numberOfPreferences <- nrow(performances)
  action <- "validateRelations"

  if(!is.null(relation))
  {
    relation <- translateRelationsStringsIntoAlternativesIds(relation, performances)

    validate(is.matrix(relation), action, paste("Relation", relationName, "must be repesented by a matrix"))
    validate(ncol(relation) == arity, action, paste("Relation", relationName, "has arity:", arity, ". Number of arguments typed:", ncol(relation), "."))

    #check weather minimum and maximum index are in performanceTable indices bounds
    validate(all(relation >= 1), action, "There is no preference with index lower than 1")
    validate(all(relation <= numberOfPreferences), action, paste("There is no preference with index higher than:", numberOfPreferences, ". Typed a preference with index:", max(relation)))
  }
  relation
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

translateRelationsStringsIntoAlternativesIds <- function(relation, performances)
{
  action <- "Translating relations strings to ids"
  alternativesNames <- rownames(performances)
  if(is.null(alternativesNames))
  {
    validate(is.numeric(relation),
             action,
             "Relations must be defined as numerical ids of alternatives when alternatives names are not available")
  }

  if(is.numeric(relation))
  {
    return(relation)
  }

  validate(is.character(relation) && !is.null(alternativesNames), action,
           "Alternatives names must be specified when relation arguments are strings.")

  newRelationWithIds <- c()
  for(row in 1:nrow(relation))
  {
    newRow <- c()
    for(col in 1:ncol(relation))
    {
      id <- which(alternativesNames == relation[row, col])
      validate(length(id) > 0, action,
               paste("There is no alternative with a name corresponding to a relation's argument", relation[row, col]))
      newRow <- c(newRow, id)
    }
    newRelationWithIds <- rbind(newRelationWithIds, newRow)
  }
  names <- paste("relation_", 1:nrow(newRelationWithIds), sep="")
  rownames(newRelationWithIds) <- names
  newRelationWithIds
}

validate <- function(condition, action, message){
  assert(condition, paste("Error while validating - ", action, ":", message))
}
