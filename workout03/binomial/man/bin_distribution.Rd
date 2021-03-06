% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binom_funcs.R
\name{bin_distribution}
\alias{bin_distribution}
\title{binomial distribution}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{trials}{the number of trials run}

\item{prob}{the probability of success}
}
\value{
a data frame of number of successes and the corresponding probability of class bindis and data.frame
}
\description{
Calculates the binomial distribution ~Binomial(n, trials) with probability prob for each possible number of successes
}
\examples{
bin_distribution(trials = 5, prob = 0.5)
}
