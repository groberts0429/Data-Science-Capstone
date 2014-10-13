---
title: "Coursera Capstone - SwiftKey"
author: "George Gordon Roberts III"
output: word_document
---

#Introduction
A predictive model for the English language was built using R packages tm, RWeka, and custom code. The model specifically predicts the next word typed by a user in a text application. Twitter with its 140 character limit most closely matches the style and length of a text message. Thus, the majority of the training set was made up of Twitter messages. A small proportion of blogs were added to round out the training set and widen the array of topics and styles present in the training data.  
  
After introducing the functions and data used in the production model, an in-depth study of a simple corpus is presented to further expound on the construction of the model and its predictions.

##Data Preprpocessing
Load the text mining package, set the working directory, load packages
```{r, message=FALSE, warning=FALSE}
library(tm) #Warnings are uninformative, referring only to version issues
library(markovchain) #load tm (text mining), set working directory
```

Read in data.
```{r create filehandles, read data}
tl = readLines("C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
t <- tl[1:5000] #sample
bl <- readLines("C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
b <- bl[1:50] #sample
bt <- c(t, b) #samples are used in this document to speed compilation. 
```

Compile the cleanup function which will convert to lower case, indicate the first word in a text string, replace punctuation, correct common misspellings, filter common profanity.

```{r cleanup function}
cleanup <- function(lines) { 
  t <- tolower(lines)
  #Substitution preserves elements which would otherwise be lost during tokenization
	t <- gsub("\\.\\.\\.", " <ellipse> ", t) 
	t <- gsub("\\.", " <period>", t)
	t <- gsub("\\?", " <question>", t)
	t <- gsub("\\!", " <exclamation>", t)
	t <- sub("^", "<start> ", t) #Distribution of first word differs from . 
	t <- gsub(" teh ", " the ", t) #fix common spelling errors
	t <- gsub(" siad ", " said ", t) #fix common spelling errors
	t <- gsub("fuck ", "", t) #Profanity filter
	t <- gsub("boobs ", "", t) 
	return(t)	
}
t <- cleanup(bt)
```

Make a corpus 
```{r}
tv <- VectorSource(t)
tc <- Corpus(tv)
```

##Tokeinzation and Further Processing
Separate the text into unigrams, bigrams, and trigrams.
```{r}
##Objects were created in RGUI and then direcetly loaded to avoid RWeka/RStudio compatability issues.
#library(RWeka) #Tokenize with RWeka
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#UniBiTrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
#tdm_t123 <- TermDocumentMatrix(tc, control = list(tokenize = UniBiTrigramTokenizer))
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/t2.rda")
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/t123.rda")
```
##Remove Uncommon N-grams
N-grams will be processed as a whole (unigrams, bigrams, trigrams) and then split for faster post-processing.
```{r}
tdm_tsmall <- tdm_t123[10000:20000,]
v <- apply(tdm_tsmall, 1, sum) #identify singletons
subset <- v > 1 
tg <- tdm_tsmall[subset,] #tgrams appearing more than once - remove hapax legomenon
grand_total <- sum(tg)
rowsums <- apply(tg, 1, sum) #row_sum / grand_total probability
agram <- rowsums
terms <- Terms(tg)
```

## Process N-grams in Preparation for Assembling the Markov Chain Transition Matrix
Separate the unigrams, bigrams, and trigrams which will be processed separately.
```{r}
unigrams <- tg[agram == 1,]; bisgrams <- tg[agram == 2,]; trigrams <- tg[agram == 3,] 
uniterms <- terms[agram == 1]; bisterms <- terms[agram == 2]; triterms <- terms[agram == 3]
unisumsl <- rowsums[agram == 1]; bissumsl <- rowsums[agram == 2]; trisumsl <- rowsums[agram == 3]
```
##Separate the bigrams and trigrams into two parts:
1) All but the last word (input) - nminus1words
2) The last word (output) - lastword
```{r}
nminus1words <- function(ngrams, n) { # ngrams=ngrams, n="n" in ngram, 
	termvector <- Terms(ngrams)
	wordvector <- strsplit(termvector, split=" ")
	beginning  <- character(length(termvector))
	for (i in 1:length(wordvector)) {
		beginning[i] <- paste(wordvector[[i]][1:(n-1)], sep="", collapse=" ")
	}
	return(beginning)
}

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
```
##The rowsums indicate the "popularity" of the individual ngrams.
```{r}
rowf <- function(terms, rowsums) {
	rowsum <- numeric(length(terms))
	for (i in 1:length(terms)) {
	rowsum[i] <- rowsums[[i]]
	}
	return(rowsum)
}

bissums <- rowf(bisterms, bissumsl)
trisums <- rowf(triterms, trisumsl)
```

##Assemble the Markov Chain Transition Matrix
The next two code chunks produce a square transition matrix to be loaded into the markovchain package.
```{r}
bisboth <- unique(c(bisbegin, bislast, "<end>"))
triboth <- unique(c(tribegin, trilast, "<end>"))
ufirst  <- unique(bisbegin)
usecond <- unique(bislast)
first   <- bisbegin
second  <- bislast
uboth <- unique(c(ufirst, usecond, "<end>"))
```
The final prediction is based on the "Markov" property - that is to say "memorylessness". Essentially this means that all that matters to the prediction is the last n-1 words. This ignores long-range interactions (influences of words more than n-words away). Nonetheless, relatively simple and yet very effective models can be produced using the Markov property as the semantics of most sentences are not heavily-dependent on long-range interactions.  
The final model takes the form of a Markov chain. A square transition matrix indicating the likelihood of moving from the word or words in the row to the word in the column. Thus, the row sum corresponds to the likelihood of moving from the input word (row) to all possible next words.  
```{r}
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
```

Transform the square transition matrix into a Markov chain objectt. 
```{r, eval=FALSE}
## Markov Chain ##
mvector <- c(mbi)
nrows = dim(mbi)[1]
mc <- new("markovchain", transitionMatrix=matrix(c(mbi), nrow=nrows, byrow=FALSE, dimnames=list(uboth, uboth))) # Demo is with a smaller dataset, skip evaluation
```

The `predict3()` function will return the top three next-word candidates.
```{r}
## Predictions ##
predict3 <- function(matrix, uboth, begin, last,  rowsums, input="<start>") { 
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
	return(c(option1, option2, option3)) #option1 is the most likely...
}
```


##Full Description of how the Data are Used in Building the Prediction Algorithm
###"Roses are Red"
The following highly-simplified example illustrates the prediction process.
```{r}
rh <- "C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/roses.txt"
d <- readLines(rh) #Prepare a simple data set
d <- cleanup(d)
d
dv <- VectorSource(d)
dc <- Corpus(dv)
```

Rowsums will be used to calculate the denominator for the transition matrix entries. Each rowname in the table is unique. However, after splitting the last word from each bigram, there will be multiple rows with the same input; for example, the rows "roses beautiful" and "roses are" will both have "roses" as the input. The denominator for the row in the final transition matrix (which will have only one row for "roses") will be the sum of the rowsums for both rows. The numerator for the row will be the rowsum for the row with the matching last term (predicted term).
```{r, results="hide"}
#bisgramse <- TermDocumentMatrix(dc, control = list(tokenize = BigramTokenizer)) 
#uncommon bigrams are not removed due to the "toy-sized" dataset
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/bisgramse.rda")
tdm <- inspect(bisgramse) # a better display is coming in the next chunk...
```

##Display TDM with Rowsums
```{r}
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
tdmnames <- Terms(bisgramse)
tdmmat <- matrix(tdmmat,nrow=10, ncol=4, dimnames=list(c(tdmnames), c("1","2","3","rowsums")))
tdmmat
```

Prepare the transition matrix. The "squarematf" function creates the transition matrix from several inputs which are aggregated immediately above the function call. In addition to calculating the probability for the next word when there is a next word observed, the "squarematf" function will also set the transition matrix entry for input word(s) with no observed following word to 1 in the "<end>" column (leading to an end of text prediction). Thus, the rowsums are always equal to one even when the input word has only been observed at the end of a text string. Finally, load the resulting matrix into a "markovchain" object.
```{r}
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
ubothe <- gsub("<period>", ".", ubothe)
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
```
Apply the model. Note that even without input data, a prediction can be rendered; the most common start word will be the prediction returned. By setting n.ahead to 4, the entire most likely sentence can be predicted. In this very restricted case, the sentence is logical, grammatical, and present in the corpus.
```{r}
predict(mce, newdata="<start>", n.ahead=4)
```

The `predict3()` function produces up to three alternate predictions which are prioritized from left to right on the basis of greatest to least probability. In this very simple example using a very restricted corpus, there was not enough data to produce a third prediction.
```{r}
predict3(mbe, ubothe, firste, seconde, bissumse)
predict3(mbe, ubothe, firste, seconde, bissumse, "roses")
predict3(mbe, ubothe, firste, seconde, bissumse, "are")
predict3(mbe, ubothe, firste, seconde, bissumse, "red")
```

##Future Directions - Accounting for Long-Range Interactions
"All problems in computer science can be solved by another layer of indirection..."  
-David Wheeler, British Computer Scientist  
Although n-gram models lack an explicit way to account for long range interactions, an interesting avenue for further development would be to add a layer of logic that would detect the presence of a long-range modifier and modify the output of the model either directly through the application of a model that takes into account long range interactions or by a case statement which would select the n-gram model trained on the most appropriate training set. In a domain as complex as human language, the opportunities for layering and ensembling are legion.
