
# Basic Sentiment Analysis Model 

library(tm) ; library(RWeka) ; 

# Read the file containing Postive and Negative terms
positive_terms = read.csv("Positive terms.csv")
positive_terms = as.character(positive_terms$Positive)

negative_terms = read.csv("Negative terms.csv")
negative_terms = as.character(negative_terms$Negative)

# load the document on which sentiment analysis is to be conducted
corpus = Corpus(DirSource(paste(getwd(),"/TextMining",sep="")))

# clean the text using the multipurpose tm_map function
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
writeLines(as.character(corpus[[1]]))

# write the tokenizer function 
ngram_tokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))

# create the term-document matrix.
tdm = TermDocumentMatrix(corpus,control=list(tokenize = ngram_tokenizer))
terms = tdm$dimnames$Terms
print(terms)
 
# To check if any of the positive/negative words in our dictionary are present
# in the text document. The output is "TRUE" if words match, else "FALSE".
p = positive_terms %in% terms 
n = negative_terms %in% terms
print(p); print(n) ;

# extract the positive/negative words that are present in the text
p_extract = terms[match(positive_terms, terms)]
p_extract = p_extract[!is.na(p_extract)]

n_extract = terms[match(negative_terms, terms)] 
n_extract = n_extract[!is.na(n_extract)]

print(p_extract) ; print(n_extract)

# this code calculates the positivity and the negativity score
p_score = count(p_extract)
n_score = count(n_extract)
print(p_score) ; print(n_score) ;

# this code computes the Sentiment score
sentiment = (p_score - n_score)
print(sentiment)






