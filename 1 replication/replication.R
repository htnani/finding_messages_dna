#' Counts the number of times pattern appears in text
#' @param text
#' @param pattern
#' @return the number of times pattern shows in text
patternCount <- function(text, pattern) {
  count <- 0
  pattern_length <- nchar(pattern)
  for (i in 1:(nchar(text)-nchar(pattern))) {
    if (substr(text,i,(i+pattern_length-1)) == pattern) {
      count <- count + 1
    }
  }
  
  return(count)
}

#' 
#' @param text
#' @param k the length of the substring pattern to look for
#' @return 
frequentWords <- function(text, k) {
  # Iterate through text counting for k-length substring patterns
  count <- sapply(1:(nchar(text)-k), function(i) {
    pattern <- substr(text, i, i+(k-1))
    patternCount(text, pattern)
  })
  # Get most frequent pattern positions
  max_count <- max(count)
  max_count_i <- which(count==max_count)
  # Substring using those positions
  frequent_patterns <- sapply(max_count_i, function(i) {
    substr(text, i, i+(k-1))
  })
  # remove duplicates
  frequent_patterns <- unique(frequent_patterns)
  
  return(frequent_patterns)
}

