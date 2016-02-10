\name{cystiSim-package}
\alias{cystiSim}
\alias{cystiSim}

\docType{package}

\title{Agent-based model for \emph{Taenia solium} transmission and control.}

\description{The cystiSim model allows simulating \emph{Taenia solium} taeniosis/cysticercosis transmission in a virtual population of humans and pigs. It also allows evaluating the possible effects of human MDA, pig oxfendazole treatment, and pig vaccination.}

\details{
  \tabular{ll}{
    Package:    \tab  cystiSim\cr
    Type:       \tab  Package\cr
    Version:    \tab  0.0.3\cr
    Date:       \tab  2016-02-10\cr
    Authors:    \tab  Brecht Devleesschauwer, Uffe Christian Braae\cr
    Maintainer: \tab  \email{brechtdv@gmail.com}\cr
    Depends:    \tab  R (>= 3.2.0)\cr
    License:    \tab  GPL (>= 2)\cr
    LazyLoad:   \tab  yes\cr
}

Available functions in the \pkg{cystiSim} package:
  \tabular{ll}{
    \strong{baseline}\cr
    \code{\link{random_baseline_man}}  \tab Generate a random baseline human population.\cr
    \code{\link{random_baseline_pig}}  \tab Generate a random baseline pig population.\cr\cr
    
    \strong{model}\cr
    \code{\link{initiate}}        \tab Initiate a \code{cystiSim} model.\cr
    \code{\link{update}}          \tab Update a \code{cystiSim} model.\cr\cr

    \strong{interventions}\cr
    \code{\link{do_mda}}          \tab Do human mass drug administration.\cr
    \code{\link{do_oxf}}          \tab Do pig oxfendazole treatment.\cr
    \code{\link{do_vac}}          \tab Do pig vaccination.\cr\cr

    \strong{S3 methods}\cr
    \code{\link{print}}  \tab Show current prevalences.\cr
    \code{\link{plot}}   \tab Plot prevalences across time.\cr
  }
}

\keyword{package}
