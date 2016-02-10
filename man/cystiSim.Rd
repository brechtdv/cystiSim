\name{cystiSim}
\alias{cystiSim}
\alias{report}
\alias{print.cystiSim}
\alias{summary.cystiSim}
\alias{plot.cystiSim}
\alias{report.cystiSim}

\title{'cystiSim' object}

\description{
Functions to initiate and explore 'cystiSim' objects. A 'cystiSim' object corresponds to multiple runs of a cystiSim model.}

\usage{
cystiSim(n = 100, mod, main = NULL)

report(x, \dots)

\S3method{print}{cystiSim}(x, \dots)

\S3method{summary}{cystiSim}(object, round = 3, \dots)

\S3method{plot}{cystiSim}(x, y, \dots)

\S3method{report}{cystiSim}(x, name = "cystiSim", \dots)
}

\arguments{
  \item{n}{}
  \item{mod}{}
  \item{main}{}
  \item{x}{Object of class 'cystiSim'.}
  \item{object}{Object of class 'cystiSim'.}
  \item{y}{Currently ignored.}
  \item{round}{Currently ignored.}
  \item{name}{Currently ignored.}
  \item{\dots}{}
}

%\details{}

\value{
\code{cystiSim} returns an object of S3 class 'cystiSim'.
}

\author{\email{brechtdv@gmail.com}}

\seealso{
\code{\link{cystiRun}}
}
