### CYSTISIM: AGENT BASED TAENIA SOLIUM TRANSMISSION MODEL

## -------------------------------------------------------------------------#
## MAIN SIMULATION FUNCTION ------------------------------------------------#

sim <-
function(n = 100, mod, main = NULL) {
  ## predefine list of simulations
  out <- vector("list", n)

  ## perform simulations
  for (i in seq(n)) {
    print(i)
    out[[i]] <- eval(substitute(mod))$out[, c(1, 5, 6)]
  }

  ## create 'cystiSims' object
  sim <-
    list(out = out,
         mod = substitute(mod),
         main = main,
         par = list(ph2m = ph2m,
                    pl2m = pl2m,
                    m2p = m2p,
                    e2p = e2p,
                    eff_mda = eff_mda, cov_mda = cov_mda,
                    eff_oxf = eff_oxf, cov_oxf = cov_oxf,
                    eff_vac = eff_vac, cov_vac = cov_vac))

  ## add S3 class 'cystiSims'
  class(sim) <- "cystiSims"

  ## return 'cystiSims' object
  return(sim)
}


## -------------------------------------------------------------------------#
## cystiSims METHODS -------------------------------------------------------#

print.cystiSims <-
function(sim) {
  mod <- strsplit(as.character(sim$mod)[2], "%>%")[[1]]
  mod <- gsub("\n", "", mod)
  mod <- sub("^ +", "", mod)
  mod <- sub(" +$", "", mod)
  for (i in seq_along(mod)) {
    if (i > 1) cat ("  ")
    cat(mod[i])
    if (i < length(mod)) cat(" %>%\n")
  }
  cat("\n")
}

summary.cystiSims <-
function(sim, round = 3) {
  sim_mx <- sapply(sim$out, as.matrix)
  m <- nrow(sim_mx) / ncol(sim$out[[1]])

  out <-
  rbind(
    c(mean(sim_mx[m, ] == 0), quantile(sim_mx[m, ], 0.95)),      # PC
    c(mean(sim_mx[3*m, ] == 0), quantile(sim_mx[3*m, ], 0.95)))  # HT

  rownames(out) <- c("PC", "HT")
  colnames(out) <- c("Pr(elim)", "95%")

  return(round(out, round))
}

plot.cystiSims <-
function(sim) {
  lab <- factor(c("PC", "PR", "HT"), c("PC", "PR", "HT"))
  grp <- factor(c("pig", "pig", "human"), c("pig", "human"))
  col <- c(1, 3, 2)

  sim_mx <- sapply(sim$out, as.matrix)  # ncol=it; nrow=par*months

  df <-
  data.frame(mean = rowMeans(sim_mx),
             lwr  = apply(sim_mx, 1, quantile, 0.025),
             upr  = apply(sim_mx, 1, quantile, 0.975),
             m = rep(seq(nrow(sim_mx) / ncol(sim$out[[1]])),
                     ncol(sim$out[[1]])),
             par = rep(lab, each = nrow(sim_mx) / ncol(sim$out[[1]])),
             grp = rep(grp, each = nrow(sim_mx) / ncol(sim$out[[1]])))

  stats <- summary(sim)

  x_txt <- nrow(sim_mx) / ncol(sim$out[[1]])
  y_txt <- tapply(apply(sim_mx, 1, max), df$grp, max)

  df_txt <-
    data.frame(lab = paste0(ncol(sim_mx), " simulations",
                            "\nPr(elim)=", stats[, 1]),
               grp = levels(df$grp),
               x = x_txt,
               y = y_txt)

  ggplot(df, aes(x = m, y = mean)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = par), alpha = .25) +
    geom_line(aes(y = mean, col = par), size = 1) +
    geom_line(aes(y = lwr, col = par)) +
    geom_line(aes(y = upr, col = par)) +
    scale_fill_manual(values = col) +
    scale_colour_manual(values = col) +
    scale_x_continuous("month") +
    scale_y_continuous("prevalence") +
    facet_grid(grp~., scale = "free") +
    theme(legend.position = "none") +
    theme_bw() +
    ggtitle(sim$main) +
    geom_text(data = df_txt,
              aes(label = lab, x = x, y = y),
              size = 4, hjust = 1, vjust = 1)
}

report.cystiSims <-
function(sim, name = "cystiSim") {
  ## temporary 'mod' file
  ## write model
  tmp <- tempfile(fileext = ".Rnw")
  sink(tmp)
  cat("<<eval=FALSE>>=\n")  
  print(sim)
  cat("@\n")
  sink()

  ## knit PDF
  knitr::knit2pdf("cystiSim.Rnw")

  ## rename PDF
  file.rename("cystiSim.pdf",
              paste0(name, "_", today(), ".pdf"))

  ## remove helper files
  unlink(tmp)
  file.remove(paste0("cystiSim.",
                     c("tex", "log", "aux")))
  unlink("figure", recursive = TRUE)

  ## generate PNG
  png(paste0(name, "_", today(), ".png"),
      width = 10, height = 5, units = "in", res = 300)
  print(plot(sim))
  graphics.off()
}


## -------------------------------------------------------------------------#
## HELPER FUNCTIONS --------------------------------------------------------#

today <-
function() {
  return(format(Sys.time(), "%Y%m%d"))
}