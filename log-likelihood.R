# function to get the log likelihood of each of a 
# speaker's words vs. the rest of the dataframe
# written by Kaylin Walker
# https://github.com/walkerkq/textmining_southpark/blob/master/scripts/southpark_plot_loglikelihood.R
# note: df assumes that word variable is last column and NA values are 0
LL <- function(df, threshold, sig.level) {
  
  LL.df <- NULL
  
  df <- df[rowSums(df[ , -length(df[1,])]) > threshold, ] 
  
  people <- colnames(df[ , -length(df[1,])])
  
  for(speaker in people) {
    
    # subset the data frame to hold one speaker's data
    temp.p <- subset(df, select=speaker)
    temp.count <- subset(df, select=word)
    temp.w <- cbind(temp.p, temp.count)
    
    # loop through their words
    for(k in seq_along(temp.w[,1])) {
      
      word <- temp.w$word[k]
      
      speaker.sums <- data.frame(speaker = names(colSums(df[ , -length(df[1,])])), total=colSums(df[ , -length(df[1,])]), row.names=NULL)
      word.sums <- data.frame(word = df$word , total=rowSums(df[ , -length(df[1,])]), row.names=NULL)
      all.words.total <- sum(speaker.sums$total)
      
      word.total <- word.sums[word.sums$word==word, 2]
      
      speaker.total <- speaker.sums[speaker.sums$speaker==speaker, 2]
      if(speaker.total == 0) speaker.total <- 0.0001
      other.total <- all.words.total - speaker.total
      
      speaker.word <- df[df$word==word, ]
      speaker.word <- data.frame(speaker=names(speaker.word), count=t(speaker.word), row.names=NULL)
      speaker.word <- as.numeric(as.character(speaker.word[speaker.word$speaker==speaker, 2]))
      other.word <- word.total - speaker.word
      
      if(speaker.word == 0) speaker.word <- 0.0001
      E1 <- (speaker.total*word.total)/all.words.total
      E2 <- (other.total*word.total)/all.words.total
      LL <- 2*(speaker.word*log(speaker.word/E1) + other.word*log(other.word/E2))
      
      if(abs(LL) > sig.level) {
        if(E1 > speaker.word) LL <- -1*LL
        speaker.word <- round(speaker.word)
        speaker.total <- round(speaker.total)
        row <- data.frame(speaker, word, word.total, speaker.total, speaker.word, E1, E2, LL)
        LL.df <- rbind(LL.df, row)
      }
      
    }
  }
  LL.df <- LL.df[order(LL.df$speaker, -LL.df$LL), ]
  return(LL.df)
}