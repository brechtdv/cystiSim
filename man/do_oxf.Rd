\name{do_oxf}
\alias{do_oxf}

\title{Do pig oxfendazole treatment}

\description{
Intervention function that mimics the possible effects of pig oxfendazole treatment.
}

\usage{
do_oxf(x, efficacy, coverage, immunity = 3, min.age = 1, max.age = Inf)
}

\arguments{
  \item{x}{\code{cystiSim} object.}
  \item{efficacy}{Presumed efficacy of oxfendazole treatment, expressed as a decimal value.}
  \item{coverage}{Presumed coverage of oxfendazole treatment within eligible population, expressed as a decimal value.}
  \item{immunity}{Presumed duration of immunity following oxfendazole treatment of an infected pig.}
  \item{min.age}{Minimum age for oxfendazole treatment, in months. Defaults to \code{0}, i.e., no lower age limit.}
  \item{max.age}{Maximum age for oxfendazole treatment, in months. Defaults to \code{Inf}, i.e., no upper age limit.}
}

%\details{}

\author{\email{brechtdv@gmail.com}}

\seealso{Other interventions: \code{\link{do_mda}}, \code{\link{do_vac}}}