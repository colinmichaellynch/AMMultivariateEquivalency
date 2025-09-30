multivariateEquivalency = function(referenceLimits, data, b, alpha=.01) {
 
  n = nrow(data)
  p = length(referenceLimits)
  alphaPrime = 1 - (1 - alpha)^(1 / p)
  chiSquareU = qchisq(p = alphaPrime, df = b - 1, lower.tail = FALSE)
  cramersVUpper = sqrt(chiSquareU / n) / sqrt(b - 1)
  names = names(referenceLimits)
  cramersV = c()
  for(i in 1:p){
    name = names[i]
    bins = as.numeric(referenceLimits[[name]])
    observations = data[,i]
    binnedData = cut(observations, breaks = bins, include.lowest = TRUE, right = FALSE)
    observed = table(binnedData)
    expected = rep(n / b, b)
    chiSquare = sum(((observed - expected)^2) / expected)
    cramersV[i] = sqrt(chiSquare / n) / sqrt(b - 1)
  }
  
  if (any(cramersV > cramersVUpper)) {
    equivalent = "Not Equivalent"
  } else {
    equivalent = "Equivalent"
  }
  
  dataOutput = data.frame(cramersV, cramersVUpper, df = b-1, equivalent)
  return(dataOutput)
   
}