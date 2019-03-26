  library(benchmarkMetrics)
  source("../R/benchmarking.r")
  ## C. I. Bliss (1952) The Statistics of Bioassay. Academic Press.
  ## In McNeil, D. R. (1977) Interactive Data Analysis. New York: Wiley.
  ## see ?ToothGrowth

  ## Load data
  Obs   = ToothGrowth[,'len' ]

  ## Construct Model
  supp  = ToothGrowth[,'supp']
  dose  = ToothGrowth[,'dose']
  toothFairy = sample(c(TRUE,FALSE), length(Obs),replace=TRUE)
  Mod   = predict(lm(Obs ~  supp + dose + toothFairy))/2 + 10

  ## Compare model errors using metrics
  nme_kelley   = NME (Obs, Mod)
  nme_forrest  = calcNME(Mod, Obs) 
  nmse_kelley  = NMSE(Obs, Mod) 
  nmse_forrest = calcNMSE(Mod, Obs)
  
   ## View metric info
  cat('\nNME score from "benchmarkMetrics" package:\n')
  print(nme_kelley)
  cat("long version: \n")
  print(score(nme_kelley))
  cat('\nNME score from "DGVMTools" package:\n')
  cat(nme_forrest)
  cat("\n\n")
  
  cat('\nNMSE score from "benchmarkMetrics" package:\n')
  print(nmse_kelley)
  cat("long version: \n")
  print(score(nmse_kelley))
  cat('\nNMSE score from "DGVMTools" package:\n')
  print(nmse_forrest)
  summary(nme)

  ## Plot metric info
  plot(nme)
  mtext("NME")

