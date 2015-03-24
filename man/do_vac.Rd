\name{do_vac}
\alias{do_vac}

\title{Do pig vaccination}

\description{
Intervention function that mimics the possible effects of pig vaccination.
}

\usage{
do_vac(x, efficacy, coverage, interval = 4, immunity = Inf,
       min.age = 0, max.age = Inf)
}

\arguments{
  \item{x}{\code{cystiSim} object.}
  \item{efficacy}{Presumed efficacy of vaccine, expressed as a decimal value.}
  \item{coverage}{Presumed coverage of vaccination within eligible population, expressed as a decimal value.}
  \item{interval}{Maximum interval between two consecutive shots for immunity. See Details.}
  \item{immunity}{Presumed duration of immunity following successful vaccination. See Details.}
  \item{min.age}{Minimum age for vaccination, in months. Defaults to \code{0}, i.e., no lower age limit.}
  \item{max.age}{Maximum age for vaccination, in months. Defaults to \code{Inf}, i.e., no upper age limit.}
}

\details{tb}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{Other interventions: \code{\link{do_mda}}, \code{\link{do_oxf}}}