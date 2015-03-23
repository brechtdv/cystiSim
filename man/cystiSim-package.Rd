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
    Version:    \tab  0.0.1\cr
    Date:       \tab  2015-03-08\cr
    Authors:    \tab  Brecht Devleesschauwer, Uffe Christian Braae\cr
    Maintainer: \tab  \email{Brecht.Devleesschauwer@UGent.be}\cr
    Depends:    \tab  R (>= 3.1.0)\cr
    License:    \tab  GPL (>= 2)\cr
    LazyLoad:   \tab  yes\cr
}

Available functions in the \pkg{cystiSim} package:
  \tabular{ll}{
    \strong{model}\cr
    \code{\link{initiate}}        \tab Info.\cr
    \code{\link{update}}          \tab Info.\cr\cr

    \strong{interventions}\cr
    \code{\link{do_mda}}          \tab Info.\cr
    \code{\link{do_oxf}}          \tab Info.\cr
    \code{\link{do_vac}}          \tab Info.\cr\cr

    \strong{S3 methods}\cr
    \code{\link{print.cystiSim}}  \tab Info.\cr
    \code{\link{plot.cystiSim}}   \tab Info.\cr
  }
}

\keyword{package}
