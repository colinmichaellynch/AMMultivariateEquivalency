binLimits = function(data, b) {
  # Define the percentile breakpoints
  percentileVec = seq(0, 1, length.out = b + 1)
  
  # Compute bin edges for each column
  bin_limits_list = lapply(data, function(col) {
    bins = quantile(col, percentileVec, na.rm = TRUE)
    bins[1] = -Inf
    bins[length(bins)] = Inf
    return(bins)
  })
  
  return(bin_limits_list)
}
