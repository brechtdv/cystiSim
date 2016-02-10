## CYSTISIM INTERVENTIONS

## MASS DRUG ADMINISTRATION
do_mda <-
function(x, efficacy, coverage, min.age = 0, max.age = Inf) {
  ## identify HT that are eligible for treatment
  id <- (x$man$taenia == 1 | x$man$taenia_immature == 1) &
        x$man$age >= min.age & x$man$age <= max.age

  ## randomize treatment over eligible HT
  is_treated <- rbinom(sum(id), 1, efficacy * coverage) == 1

  ## both mature and immature tapeworms die
  x$man$taenia[id][is_treated] <- 0
  x$man$taenia_immature[id][is_treated] <- 0

  ## 'time_since_infection' gets set to zero
  x$man$time_since_infection[id][is_treated] <- 0

  ## 'time_since_contamination' gets set to one: initiate env contam decay
  x$man$time_since_contamination[id][is_treated] <- 1

  ## return 'cystiSim' object
  return(x)
}


## OXFENDAZOLE TREATMENT PIGS
do_oxf <-
function(x, efficacy, coverage, immunity = 3, min.age = 1, max.age = Inf) {
  ## identify pigs that are eligible for treatment
  id <- x$pig$slaughtered == 0 & x$pig$age >= min.age & x$pig$age <= max.age

  ## randomize treatment over eligible pigs
  is_treated <- rbinom(sum(id), 1, efficacy * coverage) == 1

  ## generate immunity for treated positive pigs (mature & immature)
  is_immune <-
    x$pig$cysti[id][is_treated] == 1 |
    x$pig$cysti_immature[id][is_treated] == 1
  x$pig$immunity[id][is_treated][is_immune] <- immunity

  ## both mature and immature cysts die
  ## 'intensity' and 'time_since_infection' get set to zero
  x$pig$cysti[id][is_treated] <- 0
  x$pig$cysti_immature[id][is_treated] <- 0
  x$pig$intensity[id][is_treated] <- 0
  x$pig$time_since_infection[id][is_treated] <- 0

  ## return 'cystiSim' object
  return(x)
}


## VACCINATION PIGS
do_vac <-
function(x, efficacy, coverage, interval = 4,
         immunity = Inf, min.age = 1, max.age = Inf) {
  ## so far only lifelong immunity implemented !!!
  if (is.finite(immunity)) {
    stop("Currently only lifelong immunity is implemented.")
  }

  ## identify pigs that are eligible for vaccination
  id <- x$pig$slaughtered == 0 & x$pig$age >= min.age & x$pig$age <= max.age

  ## randomize vaccination over eligible pigs
  is_vaccinated <- rbinom(sum(id), 1, efficacy * coverage) == 1

  ## generate immunity for pigs that were vaccinated < XXX months before
  is_immune <- !is.na(x$pig$time_since_vaccination[id][is_vaccinated]) &
               x$pig$time_since_vaccination[id][is_vaccinated] > 0
  x$pig$immunity[id][is_vaccinated][is_immune] <- immunity

  ## reset counter for pigs that were vaccinated > XXX months before
  is_reset <- !is.na(x$pig$time_since_vaccination[id][is_vaccinated]) &
               x$pig$time_since_vaccination[id][is_vaccinated] <= 0
  x$pig$time_since_vaccination[id][is_vaccinated][is_reset] <- interval + 1

  ## initiate 'time_since_vaccination' if first vaccination
  is_first <- is.na(x$pig$time_since_vaccination[id][is_vaccinated])
  x$pig$time_since_vaccination[id][is_vaccinated][is_first] <- interval + 1

  ## return 'cystiSim' object
  return(x)
}
