
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


input <- read.delim("dataset_2_7 (1).txt", header = F, stringsAsFactors = F)
text <- input[1,1]
pattern <- input[2,1]

patternCount(text, pattern)
