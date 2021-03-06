\name{baseline}
\alias{random_baseline_man}
\alias{random_baseline_pig}

\title{Random baseline populations}

\description{
Generate random baseline human and pig populations.
}

\usage{
random_baseline_man(n, p)
random_baseline_pig(n, p, p.high, size, mu)
}

\arguments{
  \item{n}{Number of individuals to simulate.}
  \item{p}{Proportion of individuals infected with a mature parasite.}
  \item{p.high}{Proportion of infected pigs with high infection intensity.}
  \item{size}{Size of Negative Binomial distribution of age-specific slaughter probability.}
  \item{mu}{Mean of Negative Binomial distribution of age-specific slaughter probability.}
}

\note{
For more details and examples, please visit the \pkg{cystiSim} Wiki pages on \url{https://github.com/brechtdv/cystiSim/wiki}.
}

\author{\email{brechtdv@gmail.com}}

