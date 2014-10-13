#Introduction - A predictive model for the english language was built using R packages tm, RWeka, and custom code. The model was designed specifically to predict the next word typed by a user in a text application. Accordingly, traning set data was chosen so as to approximate actual text messages. Twitter with its 140 character limit most closely matches the style and length of a text message. Thus, the training set consisted mainly of twitter messages with a small proportion of blogs to round out the training set and widen the array of topics and styles present in the training data.

##Data Preprpocessing - Load the text mining package, set the working directory
library(tm)
setwd("C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US")

#Create filehandles and read in data.
tw = "en_US.twitter.txt"
bl = "en_US.blogs.txt"
#Read Lines
tl <- readLines(tw)



t <- tl[1:5000] #sample
#tcopy <- t
bl <- readLines(bl)
b <- bl[1:50]
bt <- c(t, b)


Compile the cleanup function which will provide a variety of cleanup functionality including:
* convert to lower case
* Indicate the first word in a text string (distribution differs for the first word as opposed to subsequent words).
* Replace punctuation (the application can predict punctuation as well as words).
* Correct common misspellings
* Filter common profanity
* Indicate the beginning of a string and punctuation so that at least in principle, it is possible to predict the first word in a text or its punctuation. 


cleanup <- function(lines) { ##Cleanup - Remove non ASCII / tolower / <start> / punctuation: <period>, <question>, <exclamation>
	t <- tolower(lines)
	t <- gsub("\\.\\.\\.", " <ellipse> ", t)
	t <- gsub("\\.", " <period>", t)
	t <- gsub("\\?", " <question>", t)
	t <- gsub("\\!", " <exclamation>", t)
	t <- sub("^", "<start> ", t)
	t <- gsub(" teh ", " the ", t)
	t <- gsub(" siad ", " said ", t)
	t <- gsub("fuck ", "", t) #profanity filter, we wouldn't want the application to suggest a nughty word!
	t <- gsub("boobs ", "", t)
	return(t)	
}
t <- cleanup(bt)


#Make a corpus 

tv <- VectorSource(t)
tc <- Corpus(tv)


##Tokeinzation and Further Processing
Separate the text into unigrams, bigrams, and trigrams.

####

ngram3 <- rapply(cleandata, function(x) as.matrix(get.ngrams(x)))
ngram3 <- rapply(ngram3_list, function(x) as.matrix(get.ngrams(x)))


##Tokenize with RWeka
library(RWeka)
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
UniBiTrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
tdm_t2 <- TermDocumentMatrix(tc, control = list(tokenize = BigramTokenizer)); tdm_t2_10 <- Terms(tdm_t2)[7000:7010]
tdm_t3 <- TermDocumentMatrix(tc, control = list(tokenize = TrigramTokenizer))
tdm_t1 <- TermDocumentMatrix(tc, control = list(tokenize = UnigramTokenizer))
tdm_t123 <- TermDocumentMatrix(tc, control = list(tokenize = UniBiTrigramTokenizer))

save(tdm_t123, file="t123.rda") # store R objects for use with the model / application
save(tdm_t2, file="t2.rda")

##Remove Uncommon N-grams
N-grams will be processed as a whole (unigrams, bigrams, trigrams) and then split for faster post-processing.

tdm_tsmall <- tdm_t123[10000:20000,]
v <- apply(tdm_tsmall, 1, sum) #identify singletons
subset <- v > 1 
tg <- tdm_tsmall[subset,] #tgrams appearing more than once - this step removes garbage
grand_total <- sum(tg)
rowsums <- apply(tg, 1, sum) #row_sum / grand_total probability
agram <- rowsums
inspect(tg[1:10, 1:20])
terms <- Terms(tg)


## Process N-grams in Preparation for Assembling the Markov Chain Transition Matrix
Separate the unigrams, bigrams, and trigrams. Each will have to be processed separately by the nminus1words and lastwords function. These functions take n (the number of words in the n-gram) as an input, a design choice that allows faster overall processing albeit at the expense of more upfront preprocessing.

 
# Get the Grams
unigrams <- tg[agram == 1,]
bisgrams <- tg[agram == 2,] #Greek "bis" vs. Latin "bi" allows all n-grams to have a three letter prefix
trigrams <- tg[agram == 3,] #Tris is four letters - back to Latin!

# Get the Terms
uniterms <- terms[agram == 1]
bisterms <- terms[agram == 2]
triterms <- terms[agram == 3]

# Get the rowsums as a list, use rowf to extract numeric vector
unisumsl <- rowsums[agram == 1]
bissumsl <- rowsums[agram == 2]
trisumsl <- rowsums[agram == 3]

##Separate the bigrams and trigrams into two parts:
1) All but the last word (input)
2) The last word (output)

#Get all but the all but the last word in each ngram.
nminus1words <- function(ngrams, n) { # ngrams=ngrams, n="n" in ngram, 
	termvector <- Terms(ngrams)
	wordvector <- strsplit(termvector, split=" ")
	beginning  <- character(length(termvector))
	for (i in 1:length(wordvector)) {
		beginning[i] <- paste(wordvector[[i]][1:(n-1)], sep="", collapse=" ")
	}
	return(beginning)
}

##Get the last word in each ngram.
lastword <- function(ngrams, n) { # ngrams=ngrams, n="n" in ngram, 
	termvector <- Terms(ngrams)
	wordvector <- strsplit(termvector, split=" ")
	last       <- character(length(termvector))
	for (i in 1:length(wordvector)) {
		last[i] <- wordvector[[i]][n]
	}
	return(last)
}

bisbegin  <- nminus1words(bisgrams,  2)
tribegin <- nminus1words(trigrams, 3)
bislast   <- lastword(bisgrams,  2)
trilast  <- lastword(trigrams, 3)

##The rowsums indicate the "popularity" of the individual ngrams.
rowf <- function(terms, rowsums) {
	rowsum <- numeric(length(terms))
	for (i in 1:length(terms)) {
	rowsum[i] <- rowsums[[i]]
	}
	return(rowsum)
}

bissums <- rowf(bisterms, bissumsl)
trisums <- rowf(triterms, trisumsl)


##Assemble the Markov Chain Transition Matrix
A square transition matrix is the goal of the next two code chunks. Because it is possible that some words are present at the beginning of an n-gram but not the end or vice versa, the beginning and end word lists must be concatenated to produce a single list which will be used for both rows as well as columns. Concatenate the unique set of word(s) occurring in the input, and output positions. Rowsums must equal 1. Since not every word in the final position is also followed by a word in the corpus, this would introduce zeros into the matrix. To avoid this, words which only occur in the final position of an ngram will lead to a prediction of <end>. 

bisboth <- unique(c(bisbegin, bislast, "<end>"))
triboth <- unique(c(tribegin, trilast, "<end>"))

ufirst  <- unique(bisbegin)
usecond <- unique(bislast)
first   <- bisbegin
second  <- bislast
uboth <- unique(c(ufirst, usecond, "<end>"))

The final prediction is based on the "Markov" property - that is to say "memorylessness". Essentially this means that all that matters to the prediction is the last n-1 words. This ignores long-range interactions (influences of words more than n-words away). Nonetheless, relatively simple and yet very effective models can be produced using the markov property as the semantics of most sentences are not heavily-dependent on long-range interactions.  
The final model takes the form of a Markov chain. A square transition matrix indicating the likelihood of moving from the word or words in the row to the word in the column. Thus, the row sum corresponds to the likelihood of moving from the input word (row) to all possible next words.  

squarematf <- function(uboth, begin, last, rowsums) {
	m <- matrix(0, nrow=length(uboth), ncol=length(uboth))
	ufirst  <- unique(begin)
	usecond <- unique(last)
	for (i in 1:length(ufirst)) { ## Square array is needed to work with markovchain
		rowword = ufirst[i]
		ipositions = grep(paste("^", rowword, "$", sep=""), begin)
		denominator = sum(rowsums[ipositions])
		row = grep(paste("^", rowword, "$", sep=""), uboth)
		for (j in 1:length(ipositions)) {
			searchword <- last[ipositions[j]]
			column <- grep(paste("^", searchword, "$", sep=""), uboth)
			m[row, column] <- rowsums[ipositions[j]] / denominator
		}
	}
	for (i in (length(ufirst) + 1):length(uboth)) {
		m[i, length(uboth)] <- 1
	}
	return(m)	
}

mbi <- squarematf(bisboth, bisbegin, bislast, bissums)

mrowsums <- apply(mbi, 1, sum)


Transform the square transition matrix into a markov chain object using the markovchain package. This package also provides a handy prediction functionality which will search the appropriate transition matrix row to find the most likely next word.

## Markov Chain ##
mvector <- c(mbi)
nrows = dim(mbi)[1]
mc <- new("markovchain", transitionMatrix=matrix(c(mbi), nrow=nrows, byrow=FALSE, dimnames=list(uboth, uboth))) 


The `predict3()` custom prediction function allows more control over the returned value than the `predict()` function from the markovchain package. The `predict3() function can return 

## Predictions ##
predict(mc, newdata="for")
predict3 <- function(matrix, uboth, begin, last,  rowsums, input="<start>") { #rowsums for the ngrams not the matrix!	
	if (is.null(input)) { input = "<start>" }
	ufirst  <- unique(begin)
	usecond <- unique(last)
	row = grep(paste("^", input, "$", sep=""), uboth)
	ipositions = grep(paste("^", input, "$", sep=""), begin)
	frequencies = rowsums[ipositions]
	words = last[ipositions]
	nindex1 = which.max(frequencies); option1 = words[nindex1]; frequencies = frequencies[-nindex1]; words = words[-nindex1]
	nindex2 = which.max(frequencies); option2 = words[nindex2]; frequencies = frequencies[-nindex2]; words = words[-nindex2]
	nindex3 = which.max(frequencies); option3 = words[nindex3]
	return(c(option1, option2, option3))
}



##Full Description of how the Data are Used in Building the Prediction Algorithm
###"Roses are Red"
#Consider the following highly-simplified example using three sentences as inputs and bigrams for the purposes of building the Markov Chain Transition Matrix.

#Prepare a simple data set.

library(tm)
library(RWeka)
rh <- "roses.txt"
d <- readLines(rh)
d <- cleanup(d)
d
dv <- VectorSource(d)
dc <- Corpus(dv)


#Rowsums will be used to calculate the denominator for the transition matrix entries. Each rowname in the table is unique. However, after splitting the last word from each bigram, there will be multiple rows with the same input; for example, the rows "roses beutiful" and "roses are" will both have "roses" as the input. The denominator for the row in the final transition matrix (which will have only one row for "roses") will be the sum of the rowsums for both rows. The numerator for the row will be the rowsum for the row with the matching last term (predicted term).

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bisgramse <- TermDocumentMatrix(dc, control = list(tokenize = BigramTokenizer)) # uncommon bigrams are not removed due to the "toy-sized" dataset
save(bisgramse, file="bisgramse.rda") # store R objects for use with the model / application / or demonstrations
#Fancy code to append the rowsums to the term document 
tdm <- inspect(bisgramse)
rows=dim(tdm)[1]
cols=dim(tdm)[2]+ 1
tdmmat <- matrix(0, nrow=rows, ncol=cols)
for(i in 1:rows) {
  for(j in 1:(cols-1)) {
    tdmmat[i, j] <- tdm[i,j]
  }
}

 for (i in 1:rows) {
  tdmmat[i,4] <- sum(tdm[i, 1:3])
}
tdmnames  <- Terms(bisgramse)
tdmmat <- matrix(tdmmat,nrow=10, ncol=4, dimnames=list(c(tdmnames), c("1","2","3","rowsums")))
tdmmat

#Prepare the transition matrix. The "squarematf" function creates the transition matrix from several inputs which are aggregated immediately above the function call. In addition to calculating the probability for the next word when there is a next word observed, the "squarematf" function will also set the transition matrix entry for input word(s) with no observed following word to 1 in the "<end>" column. Thus, the rowsums are always equal to one even when the input word has only been observed at the end of a text string. Finally, load the resulting matrix into a "markovchain" object.

bistermse <- Terms(bisgramse)
bissumse <- apply(bisgramse, 1, sum)
bissumse <- rowf(bistermse, bissumse)
bisbegine  <- nminus1words(bisgramse,  2)
bislaste   <- lastword(bisgramse,  2)
bisbothe <- unique(c(bisbegine, bislaste, "<end>"))
ufirste  <- unique(bisbegine)
useconde <- unique(bislaste)
firste   <- bisbegine
seconde  <- bislaste
ubothe <- unique(c(ufirste, useconde, "<end>"))
mbe <- squarematf(ubothe, bisbegine, bislaste, bissumse)
mvectore <- c(mbe)
nrowse = dim(mbe)[1]
ubothe <- gsub("<period>", ".", ubothe) # Substitute back the appropriate punctuation for aesthetic purposes
ubothe <- gsub("<question>", "?", ubothe)
ubothe <- gsub("<exclamation>", ".", ubothe)
bisbegine <- gsub("<period>", ".", bisbegine)
bisbegine <- gsub("<question>", "?", bisbegine)
bisbegine <- gsub("<exclamation>", ".", bisbegine)
bislaste <- gsub("<period>", ".", bislaste)
bislaste <- gsub("<question>", "?", bislaste)
bislaste <- gsub("<exclamation>", ".", bislaste)
seconde <- gsub("<period>", ".", seconde)
seconde <- gsub("<question>", "?", seconde)
seconde <- gsub("<exclamation>", ".", seconde)

mce <- new("markovchain", transitionMatrix=matrix(c(mbe), nrow=nrowse, byrow=FALSE, dimnames=list(ubothe, ubothe))) 
#save(mce, file="rosemce.rda")
#save(mbe, file="rosembe.rda")
#save(ubothe, file="ubothe.rda")
#save(firste, file="firste.rda")
#save(seconde, file="seconde.rda")
#save(bissumse, file="bissumse.rda")

#Apply the model. Note that even without data, a prediction can be rendered; the most common start word is the prediction returned. By setting n.ahead to 4, the entire most likely sentence can be predicted. In this very restricted case, the sentence is logical, grammatical, and present in the corpus.

#predict(mce, newdata="<start>", n.ahead=4)


#The `predict3()` function has the very practical feature of producing up to three alternate predictions whcih are prioritized from left to right on the basis of greatest to least probability. In this very simple example using a very restricted corpus, there was not enough data to produce a third prediction.

predict3(mbe, ubothe, firste, seconde, bissumse)
predict3(mbe, ubothe, firste, seconde, bissumse, "roses")
predict3(mbe, ubothe, firste, seconde, bissumse, "are")
predict3(mbe, ubothe, firste, seconde, bissumse, "red")


##Future Directions - Accounting for Long-Range Interactions
#"All problems in computer science can be solved by another layer of indirection..."  
#-David Wheeler, British Computer Scientist  
#Although n-gram models lack an explicit way to account for long range interactions, an interesting avenue for further development would be to add a layer of logic that would detect the presence of a long-range modifier and modify the output of the model either directly through the application of a model that takes into account long range interactions or by a case statement which would select the n-gram model trained on the most appropriate training set. In a domain as complex as human language, the opportunities for layering and ensembling are legion.
