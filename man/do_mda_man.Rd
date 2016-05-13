\name{do_mda_man}
\alias{do_mda_man}

\title{Do human Mass Drug Administration (MDA)}

\description{
Intervention function that mimics the possible effects of human mass drug administration.
}

\usage{
do_mda_man(x, coverage, efficacy, min.age = 0, max.age = Inf)
}

\arguments{
  \item{x}{\code{cystiRun} object.}
  \item{coverage}{Presumed coverage of drug administration within eligible population, expressed as a decimal value.}
  \item{efficacy}{Presumed efficacy of tapeworm treatment, expressed as a decimal value.}
  \item{min.age}{Minimum age for MDA, in months. Defaults to \code{0}, i.e., no lower age limit.}
  \item{max.age}{Maximum age for MDA, in months. Defaults to \code{Inf}, i.e., no upper age limit.}
}

\author{\email{brechtdv@gmail.com}}

\seealso{Other interventions: \code{\link{do_mda_pig}}, \code{\link{do_vac_pig}}, \code{\link{do_mda_vac_pig}}}