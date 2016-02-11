\name{do_oxf_vac}
\alias{do_oxf_vac}

\title{Do pig oxfendazole treatment AND vaccination}

\description{
Intervention function that mimics the possible effects of combined pig oxfendazole treatment and vaccination.
}

\usage{
do_oxf_vac(x, coverage, efficacy.oxf, efficacy.vac,
           immunity.oxf = 3, immunity.vac = Inf, interval = 4,
           min.age = 1, max.age = Inf)
}

\arguments{
  \item{x}{\code{cystiRun} object.}
  \item{coverage}{Presumed coverage within eligible population, expressed as a decimal value.}
  \item{efficacy.oxf}{Presumed efficacy of oxfendazole treatment, expressed as a decimal value.}
  \item{efficacy.vac}{Presumed efficacy of vaccine, expressed as a decimal value.}
  \item{immunity.oxf}{Presumed duration of immunity following oxfendazole treatment of an infected pig. Defaults to 3 months.}
  \item{immunity.vac}{Presumed duration of immunity following successful vaccination. Defaults to \code{Inf}.}
  \item{interval}{Maximum interval between two consecutive shots for immunity. Defaults to 4 months.}
  \item{min.age}{Minimum age for intervention, in months. Defaults to 1 month.}
  \item{max.age}{Maximum age for intervention, in months. Defaults to \code{Inf}, i.e., no upper age limit.}
}

%\details{}

\author{\email{brechtdv@gmail.com}}

\seealso{Other interventions: \code{\link{do_mda}}, \code{\link{do_oxf}}, \code{\link{do_vac}}}