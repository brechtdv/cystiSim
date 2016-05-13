\name{do_pig_mda}
\alias{do_pig_mda}

\title{Do pig Mass Drug Administration (MDA)}

\description{
Intervention function that mimics the possible effects of pig mass drug administration.
}

\usage{
do_pig_mda(x, coverage, efficacy, immunity = 3, min.age = 1, max.age = Inf)
}

\arguments{
  \item{x}{\code{cystiRun} object.}
  \item{coverage}{Presumed coverage of drug treatment within eligible population, expressed as a decimal value.}
  \item{efficacy}{Presumed efficacy of drug treatment, expressed as a decimal value.}
  \item{immunity}{Presumed duration of immunity following drug treatment of an infected pig. Defaults to 3 months.}
  \item{min.age}{Minimum age for drug treatment, in months. Defaults to 1 month.}
  \item{max.age}{Maximum age for drug treatment, in months. Defaults to \code{Inf}, i.e., no upper age limit.}
}

%\details{}

\author{\email{brechtdv@gmail.com}}

\seealso{Other interventions: \code{\link{do_man_mda}}, \code{\link{do_pig_vac}}, \code{\link{do_pig_mda_vac}}}