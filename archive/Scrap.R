#----------------------------------------------------------
#   Old quanteda code from StackOverFlow
#----------------------------------------------------------

tokenizedTexts <- lapply(docs, function(x) tokenize(x))

#   Remove profanity here  ??????

dtms <- lapply(tokenizedTexts, function(x) dfm_quanteda(x))

topfeatures(dtms[[1]])




#cnt <- read_lines('files/Backup/combined_4.csv', n_max = -1)


#----------------------------------------------------------
#   Attempt creating TermDocMatrix without tm/corpus step
#       stri_trans_tolower was removed but maybe it would be faster than tolower() above?
#----------------------------------------------------------
out <- stri_extract_all_words(docs[[1]], locale = "english", simplify = F)

names(out) <- paste0("doc", 1:length(out))

lev <- sort(unique(unlist(out)))
dat <- do.call(cbind, lapply(out, function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
}, lev = lev))
rownames(dat) <- sort(lev)




dat <- dat[!rownames(dat) %in% tm::stopwords("english"), ] 

dat2 <- slam::as.simple_triplet_matrix(dat)

tdm <- tm::as.TermDocumentMatrix(dat2, weighting=weightTf)

## or...
dtm <- tm::as.DocumentTermMatrix(dat2, weighting=weightTf)



#-----------------------------------------------------
#   Create corpa and prepare for n-gram processing
#       All lower case
#       Strip white space
#-----------------------------------------------------
corps <- lapply(docs, function(x) VCorpus(VectorSource(x)))

summary(corps[[1]][[1]])

meta(corps[[1]])

corps <- lapply(corps, function(x) tm_map(x, stripWhitespace))



#---------------------------------------------------
#   Create DocumentTermMatrix and inspect items
#---------------------------------------------------
dtms <- lapply(corps, DocumentTermMatrix)




#   Display frequent terms using tm.findFreqTerms
lapply(dtms, function(x) findFreqTerms(x, lowfreq = n/20))


removeSparseTerms(dtms[[1]], .98)[[2]]