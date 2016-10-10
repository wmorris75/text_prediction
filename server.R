library(NLP)
library(tm)
library(tau)
library(shiny)

read_files <- function(){
  blog <- readLines('../en_US/en_US.blogs.txt')
  news <- readLines('../en_US/en_US.news.txt')
  twitter <- readLines('../en_US/en_US.twitter.txt')
  
  blogsample<-sample(blog, size=0.1*length(blog))
  newsample<-sample(news, size=0.05*length(news))
  twittersample<-sample(twitter, size=0.05*length(twitter))
  
  twitter_txt<-c(blogsample, newsample, twittersample)
  return (twitter_txt)
}

#Returns sorted n grams after words have been processed and cleaned
get_n_grams<-function(content.words, n){
  # the NLP function "ngrams" returns a list of n pairs of words.
  content.bigrams = vapply(ngrams(content.words, n), paste, "", collapse = " ")
  
  # we count them using xtabs,
  # and put the result into a data frame.
  content.bigram.counts = as.data.frame(xtabs(~content.bigrams))
  
  filename <- paste(as.character(n), "_grams.txt", sep='')
  content.bigram.counts<-content.bigram.counts[order(content.bigram.counts$Freq, decreasing = TRUE),]
  write.table(content.bigram.counts, filename)
  return (content.bigram.counts)
}

add_rows<- function(n_rows, df){
  row_diff = 4 - n_rows
  return (head(df, n=row_diff))
}

list_top_finds<-function(df){
  return(head(df, n=8))
}

predict_text<-function(text, get_3_grams_count, get_2_grams_count, get_4_grams_count){
  finds = 8
  max_count_grams = get_4_grams_count[which(get_4_grams_count$Freq == max(get_4_grams_count$Freq)),]
  if (length(max_count_grams$Freq) < 5){
    #No 3 grams present. Use 2 grams instead
    #max_count_grams = get_3_grams_count[which (get_3_grams_count$Freq == max(get_3_grams_count$Freq)),]
    max_count_grams = list_top_finds(get_3_grams_count)
  }
  
  max_count_grams = get_3_grams_count[which(get_3_grams_count$Freq == max(get_3_grams_count$Freq)),]
  if (length(max_count_grams$Freq) < 5){
    #No 3 grams present. Use 2 grams instead
    max_count_grams = list_top_finds(get_2_grams_count)
  }
  
  #return a list of the top 4 possibilities
  if(length(max_count_grams$content.bigram) > 0){
    predicted_word = strsplit(max_count_grams$content.bigram, " ", fixed = TRUE)[[1]]
    #return (predicted_word[length(predicted_word)])
    if (nrow(max_count_grams) < finds){
      number_of_rows= nrow(max_count_grams) 
      missing_rows= add_rows(number_of_rows, grams_2_table)
      new_df<- rbind(max_count_grams, missing_rows)
      print('Add missing rows')
      return(new_df)
    }
    print('No missing rows')
    return (max_count_grams)
  }
  else{
    print('Found nothing')
    # if (nrow(max_count_grams) < finds){
    #   number_of_rows= nrow(max_count_grams) 
    #   missing_rows= add_rows(number_of_rows, grams_2_table)
    #   new_df<- rbind(max_count_grams, missing_rows)
    #   print('Add missing rows')
    #   return(new_df)
    # }
    
    top_finds_list = grams_2_table[sample(nrow(grams_2_table)),]
    top_finds<-list_top_finds(top_finds_list)
    return (top_finds)
  }
  print ("Outside")
  # return(words[length(words)])
}

text_analyze<-function(text){
  content = gsub("[^[:alnum:] ]", "", text)
  content = gsub("[ ]+", " ", content)
  words = strsplit(content, " ", fixed = T)[[1]]
  
  #Check by n-grams
  words_4 = paste(words[(length(words)-2)], words[(length(words)-1)], words[length(words)], sep=" ")
  grams_4_predict = paste("^", words_4, " ", sep='')
  get_4_grams_count <- grams_4_table[grep(grams_4_predict, grams_4_table$content.bigrams),]
  
  words_3 = paste(words[(length(words)-1)], words[length(words)], sep=" ")
  grams_3_predict = paste("^", words_3, " ", sep='')
  get_3_grams_count <- grams_3_table[grep(grams_3_predict, grams_3_table$content.bigrams),]
  
  grams_2_predict = paste('^', words[length(words)], " ", sep='')
  get_2_grams_count <- grams_2_table[grep(grams_2_predict, grams_2_table$content.bigrams),]
  
  word_predicted_df<-predict_text(text, get_3_grams_count, get_2_grams_count, get_4_grams_count)
  return (word_predicted_df)
}

load_files<-function(){
  #text is the input text from the user
  grams_2_table <- read.table('2_grams.txt', stringsAsFactors = FALSE)
  grams_3_table <- read.table('3_grams.txt', stringsAsFactors = FALSE)
  grams_4_table <- read.table('4_grams.txt', stringsAsFactors = FALSE)
}
#########################################
#Dynamic Input
#text = "world is coming"
###############################################
grams_2_table <- read.table('2_grams.txt', stringsAsFactors = FALSE)
grams_3_table <- read.table('3_grams.txt', stringsAsFactors = FALSE)
grams_4_table <- read.table('4_grams.txt', stringsAsFactors = FALSE)

shinyServer(
  function(input, output, session){

    output$out_text<-renderUI({
      out<-text_analyze(input$text)
      pred_word = lapply(out$content.bigrams, function(content){words = strsplit(content, " ", fixed = T)[[1]][-1]})
      input_n_char = length(out$content.bigrams)

      html = ""
      for(i in pred_word){
        html<- paste(html, "<button ", "id=",i," onClick=\"get_id(this.id);\">", i, "</button>", sep = "" )
      }
      textjs = "<script src=\"text.js\"></script>"
      html_out = paste( html, textjs, sep = "<br/>")

      HTML(paste ("", html_out))
    })
  }
)
