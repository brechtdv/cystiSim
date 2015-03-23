### CYSTISIM METHODS

print.cystiSim <-
function(x, from = 200, to = NA) {
  if (is.na(to)) to <- nrow(x$out)
  p <- colMeans(x$out[seq(from, to), ])
  cat("PC: ", round(p[1], 3),
      "| Pr(H):", round(p[2], 3), "\n")
  cat("PCI:", round(p[4], 3), "\n")
  cat("HT: ", round(p[6], 4),
      "| HT(inf):", round(p[7], 4),
      ", HT(chl):", round(p[8], 4),
      ", HT(adl):", round(p[9], 4), "\n")
  cat("E:  ", round(p[10], 4), "\n")
}

plot.cystiSim <-
function(x, show = c("cysti", "taenia"), from = 1, to = NA) {
  show <- match.arg(show)
  if (is.na(to)) to <- nrow(x$out)

  label <- c("PC", "P(H)", "P(L)", "PCI", "PR",
             "HT", "HT(inf)", "HT(ch)", "HT(ad)", "EN")

  id <-
    switch(show,
           cysti  = c(1, 4:5),
           taenia = 6:10)

  par(mar = c(4, 4, 1, 1))
  matplot(seq(from, to), x$out[seq(from, to), id],
          type = "l", lty = 1)
  legend("topright", cex = .5,
         label[id], col = seq_along(id), lty = 1)
}
