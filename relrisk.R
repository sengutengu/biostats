relrisk <- function(contingencyTable) {
  success <- dimnames(contingencyTable)[[1]][1]
  group1 <- dimnames(contingencyTable)[[2]][1]
  group2 <- dimnames(contingencyTable)[[2]][2]
  p1 <- contingencyTable[1]/sum(contingencyTable[1:2])
  p2 <- contingencyTable[3]/sum(contingencyTable[3:4])
  RR <- p1/p2
  SElnRR <- sqrt((1/contingencyTable[1])+(1/contingencyTable[3])-(1/sum(contingencyTable[1:2]))-(1/sum(contingencyTable[3:4])))
  lnlow <- log(RR)-(1.96*SElnRR)
  lnhigh <- log(RR)+(1.96*SElnRR)
  convertlow <- exp(lnlow)
  converthigh <- exp(lnhigh)
  cat("\nRelative Risk\n")
  cat(c("\nRisk of",success,"in",group1,"group:",round(p1,6)))
  cat(c("\nRisk of",success,"in",group2,"group:",round(p2,6)))
  cat(c("\n\nRelative risk:",round(RR,6)))
  cat(c("\n\n95% CI:",round(convertlow,6),"-",round(converthigh,6),"\n\n"))
}