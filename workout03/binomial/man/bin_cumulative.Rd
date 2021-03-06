% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binom_funcs.R
\name{bin_cumulative}
\alias{bin_cumulative}
\title{binomial cumulative}
\usage{
bin_cumulative(trials, prob)
}
\arguments{
\item{trials}{the number of trials run}

\item{prob}{the probability of success}
}
\value{
a data frame with successes in the first column, probability in the second column, cumulative in the third column of type bincum and data.frame
}
\description{
Calculates the probability distribution and cumulative probabilites of a binomial distribution given the number of trials and the probability of success
}
\examples{
bin_cumulative(trials = 5, prob = 0.5)
}
