# Make random genotypes of unrelated individuals based on population data
makeRandGt <- function(np, afList){
  mapply(sample, sapply(sapply(afList, names), as.numeric), 2 * np, prob = afList, replace = TRUE)
}
