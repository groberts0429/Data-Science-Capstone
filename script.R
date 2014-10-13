#### To expedite the loading process and to make the application more robust, the matrix is not calculated here but rather simply read in from a stored R object ####
#### For details on the production of the matrix, please see the report ####

##### Load the Prediction Matrix ####
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/rosembe.rda")
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/ubothe.rda")
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/firste.rda")
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/seconde.rda")
load(file="C:/Users/Owner/Documents/R/capstone/Coursera-SwiftKey/final/en_US/bissumse.rda")

#### Prediction Function ####
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

