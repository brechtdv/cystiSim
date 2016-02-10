\name{cystiRun-methods}
\alias{do_mda}

\title{Do human Mass Drug Administration (MDA)}

\description{
Intervention function that mimics the possible effects of human MDA.
}

\usage{
do_mda(x, efficacy, coverage, min.age = 0, max.age = Inf)
}

\arguments{
  \item{x}{\code{cystiSim} object.}
  \item{efficacy}{Presumed efficacy of tapeworm treatment, expressed as a decimal value.}
  \item{coverage}{Presumed coverage of drug administration within eligible population, expressed as a decimal value.}
  \item{min.age}{Minimum age for MDA, in months. Defaults to \code{0}, i.e., no lower age limit.}
  \item{max.age}{Maximum age for MDA, in months. Defaults to \code{Inf}, i.e., no upper age limit.}
}

\author{\email{brechtdv@gmail.com}}
