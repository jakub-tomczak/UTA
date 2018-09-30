\name{buildProblem}
\title{
BuildProblem function.
}
\description{
Builds method independent problem. This function validates data and creates a special, unified data structure that is aimed to be used for a specific UTA model.
}
\usage{
buildProblem(preferences, criteria, characteristicPoints,
  strongPreferences = NULL, weakPreferences = NULL, indifference = NULL)
}
\arguments{
  \item{preferences}{
    Matrix with DM preferences. Criteria in columns, alternatives in rows.
  }
  \item{criteria}{
  Types of criterions. Available types of criterions are 'c' - cost or 'g' - gain.
  }
  \item{characteristicPoints}{
    Number of characteristic points per criteria.
  }
  \item{[strong|weak]Preferences}{
    Relations vector indicating strong/weak preferences between alternatives. NULL by default.
  }
  \item{indifference}{
    Indifference relation between alternatives. NULL by default.
  }
}
%%\details{
%%%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Returns a named list with items:
  \item{preferences}{Matrix with DM's preferences.}
  \item{criteria}{Parsed array of characters indicating criteria's type.}
  \item{strongPreferences}{Matrix of strong preferences.}
  \item{weakPreferences}{Matrix of weak preferences.}
  \item{characteristicPoints}{Array of numbers indicating number of characteristic points on a criteria.}
  \item{indifference}{Matrix of indifference relations.}
}
%%\references{
%%%% ~put references to the literature/web site here ~
%%}
%%\author{
%%%%  ~~who you are~~
%%}
%%\note{
%%%%  ~~further notes~~
%%}
%%
%%%% ~Make other sections like Warning with \section{Warning }{....} ~
%%
%%\seealso{
%%%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
preferences <- matrix(c(5, 2, 3, 7, 0.5, 0.9, 0.5, 0.4), ncol = 2)

#1P2 <=> first alternative is preferable to 2nd alternative.
strongPreference <- rbind(c(1,2))

#first criteria has 3 characteristic points, 2nd one has 2 characteristic points.
chPoints <- c(2,2)

#criterions are of a gain type.
chTypes <- c('g', 'g')

#problem variable contains validated data.
problem <- buildProblem(preferences, criteria = chTypes, characteristicPoints = chPoints, strongPreference = strongPreference)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
