% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/internalValidation.R
\name{getTTest}
\alias{getTTest}
\title{Independent samples t-Test}
\usage{
getTTest(observed, simulated)
}
\arguments{
\item{observed}{observed marginal sums}

\item{simulated}{simulated marginal sums}
}
\value{
t-test
}
\description{
performs a t-test to compare expected proportions between simulated and
observed marginal totals. This function implements the
\code{\link[stats]{t.test}} function.
}

