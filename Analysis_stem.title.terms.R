# Script to clean and stem words in titles
  
# Import data, functions, & load packages
library(tidyverse)
library(gridExtra)
#library(quanteda)
library(stringr)
library(tm)
library(Matrix)
library(rlang) #needed for tidytext
library(vctrs) #needed for tidytext
library(tidytext)
library(stopwords)
library(igraph)
library(wordcloud)
#library(ggwordcloud)


# Functions
#extract right n chars of string
str_right <- function(string, n) {
  substr(string, nchar(string) - (n-1), nchar(string))
}

#convert dataframe to matri=x==x
matrix.please <- function(x) {
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}

# Import data and exclude any year prior to 1922
d <- read_csv("Data_raw.csv") #head(d)
d.post1922 <- subset(d, year>=1922) # check #min(d.post1922$year)


# Process data for text analysis
# see tm vignette: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

## Remove punctuation etc from titles
names(d.post1922)
titles <- d.post1922
head(titles)

#make all title words lowercase
titles$title.lower <- tolower(titles$title)

#remove any punctuation
titles$titleWOpunctu <- tm::removePunctuation(titles$title.lower)
head(titles)

# remove any unicode
titles$titleWOpunctuORunicode <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", titles$titleWOpunctu)
titles$titleWOpunctuORunicode <- str_replace_all(string = titles$titleWOpunctuORunicode, pattern = "[;#??????Y]", replacement = "")
titles$titleWOpunctuORunicode <- str_replace_all(string = titles$titleWOpunctuORunicode, pattern = "[^[:alnum:][:blank:]?&/\\-]", replacement = "")
titles$titleWOpunctuORunicode <- str_replace_all(string = titles$titleWOpunctuORunicode, pattern = "[<>]", replacement = "")
titles$titleWOpunctuORunicode <- str_replace_all(string = titles$titleWOpunctuORunicode, pattern = "[ãâ]", replacement = "")


## Make database of all words per document
## NOTE: this database is NOT STEMMED these are the raw words per document

#split titles and save each word on own row
p <- strsplit(titles$titleWOpunctuORunicode, split = " ")
titlewordsXpapers <- data.frame(Paper.KEY = rep(titles$rowIDs, sapply(p, length)), title.words=unlist(p))
head(titlewordsXpapers, 25)
titlewordsXpapers$title.words <- as.character(titlewordsXpapers$title.words)
glimpse(titlewordsXpapers)

#keep only rows with terms (exclude na)
titlewordsXpapers <- titlewordsXpapers[complete.cases(titlewordsXpapers),]
head(titlewordsXpapers, 20)

#get rid of any numbers
titlewordsXpapers$no.digits <- stringr::str_replace_all(titlewordsXpapers$title.words, "[:digit:]", "")

#http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html#intro-to-word-stemming-and-stem-completion
stemmed.term <- tm::stemDocument(titlewordsXpapers$no.digits)
length(titlewordsXpapers$no.digits)
length(stemmed.term)

titlewordsXpapers.stemmed <- cbind.data.frame(titlewordsXpapers, stemmed.term)
head(titlewordsXpapers.stemmed, 25)

#get stopwords
stopwords.df <- as.data.frame(tidytext::get_stopwords(language = "en", source = "snowball"))
glimpse(stopwords.df)

stopwords.vector <- as.vector(stopwords.df$word)
stopwords.df <- subset(stopwords.df, select=c(word))
stopwords.df$stop.word
#merge stopwords with data

head(titlewordsXpapers.stemmed)

titlewordsXpapers.stemmed.stopped<- titlewordsXpapers.stemmed

#merge stemmed word df with stopwords
titlewordsXpapers.stemmed.stopped$stopword <- match(titlewordsXpapers.stemmed$no.digits,stopwords.vector)
head(titlewordsXpapers.stemmed.stopped,100)

#get rid of any cases where stopwords are NA (which are indications they are not stopwords)
titlewordsXpapers.stemmed.stopped.exclude <- subset(titlewordsXpapers.stemmed.stopped, is.na(stopword))
head(titlewordsXpapers.stemmed.stopped.exclude)

# delete stopword column
titlewordsXpapers.stemmed.stopped.exclude <- subset(titlewordsXpapers.stemmed.stopped.exclude, 
                                                    select=c(Paper.KEY, title.words, no.digits, stemmed.term))


# get rid of any new NAs
titlewordsXpapers.stemmed.stopped.exclude <- titlewordsXpapers.stemmed.stopped.exclude[complete.cases(titlewordsXpapers.stemmed.stopped.exclude),] #check #unique(is.na(d.stem$stemmed.term))

# merge metadata back to stemmed/stopped data
titlewordsXpapers.stemmed.stopped.exclude.meta <- merge(subset(titles, select=c(rowIDs, year)), 
                                                        subset(titlewordsXpapers.stemmed.stopped.exclude, 
                                                               select=c(Paper.KEY, title.words, no.digits, stemmed.term)),
                                                        by.x="rowIDs", by.y="Paper.KEY", 
                                                        all.x=TRUE)

titlewordsXpapers.stemmed.stopped.exclude.meta[complete.cases(titlewordsXpapers.stemmed.stopped.exclude.meta),] 
head(titlewordsXpapers.stemmed.stopped.exclude.meta)

# add decade
titlewordsXpapers.stemmed.stopped.exclude.meta$decade <- as.numeric(paste0(substr(titlewordsXpapers.stemmed.stopped.exclude.meta$year, 1, 3), 0))
titlewordsXpapers.stemmed.stopped.exclude.meta <- subset(titlewordsXpapers.stemmed.stopped.exclude.meta, select=c(year, decade, rowIDs, title.words, no.digits, stemmed.term))

# merge behaviour and behavior by hand
titlewordsXpapers.stemmed.stopped.exclude.meta$stemmed.term[titlewordsXpapers.stemmed.stopped.exclude.meta$stemmed.term=="behaviour"] <- "behavior"

# clean up column names
colnames(titlewordsXpapers.stemmed.stopped.exclude.meta) <- c("year", "decade", "docID", "orig.title.word", "orig.title.word.nodigits", "stemmed.term")

#SAVE DATA
#write.csv(titlewordsXpapers.stemmed.stopped.exclude.meta, file="Data_stemmed.title.terms.csv")
