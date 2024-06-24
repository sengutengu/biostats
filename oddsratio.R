oddsratio <- function(contingencyTable) {
  success <- dimnames(contingencyTable)[[1]][1]
  group1 <- dimnames(contingencyTable)[[2]][1]
  group2 <- dimnames(contingencyTable)[[2]][2]
  odds1 <- (contingencyTable[1]/sum(contingencyTable[1:2]))/(1-contingencyTable[1]/sum(contingencyTable[1:2]))
  odds2 <- (contingencyTable[3]/sum(contingencyTable[3:4]))/(1-contingencyTable[3]/sum(contingencyTable[3:4]))
  OR <- odds1/odds2
  SElnOR <- sqrt((1/contingencyTable[1])+(1/contingencyTable[2])+(1/contingencyTable[3])+(1/contingencyTable[4]))
  lnlow <- log(OR)-(1.96*SElnOR)
  lnhigh <- log(OR)+(1.96*SElnOR)
  convertlow <- exp(lnlow)
  converthigh <- exp(lnhigh)
  cat("\nOdds Ratio\n")
  cat(c("\nOdds of",success,"in",group1,"group:",round(odds1,6)))
  cat(c("\nOdds of",success,"in",group2,"group:",round(odds2,6)))
  cat(c("\n\nOdds Ratio:",round(OR,6)))
  cat(c("\n\n95% CI:",round(convertlow,6),"-",round(converthigh,6),"\n\n"))
}