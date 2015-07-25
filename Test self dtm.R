
#library(readr)
#library(ngram)
#library(RWeka)
#library(tm)
#library(plyr)
#library(ggplot2)
#library(lattice)
#library(SnowballC)
#library(stringi)
#library(slam)
#library(quanteda)

#   set to -1 to import all records
n <- 20000

filenames <- c("news", "blogs", "twitter")
files <- c("files/news_final.txt",
           "files/blogs_final.txt",
           "files/twitter_final.txt")


#---------------------------------------------------------
#   Import sample of each dataset
#   "Keep in mind that the first n rows of the data set
#       may not be representative."
#   Prepocessing that has been completed:
#       Change slashes to spaces (athletes/competitors)
#       Change punctuation to spaces (Hmmm...I think)
#       Remove everything but alphanumeric and spaces
#   After import, make all text lower case
#---------------------------------------------------------
docs_small <- lapply(files, function(x) read_lines(x, n_max = n))
docs_small <- lapply(docs_small, function(x) tolower(x))






#----------------------------------------------------------
#   Test running against all data
#----------------------------------------------------------
corps <- corpus(docs_small[[1]], notes="test", enc="UTF-8")

tok <- tokenize(corps, ngrams=2)

dfms <- dfm(tok, toLower = F, removeNumbers = T, removePunct = F, removeSeparators = T, ignoredFeatures = c(stopwords("english")), )

topfeatures(dfms)






#----------------------------------------------------------
#   Attempt creating TermDocMatrix without tm/corpus step
#       stri_trans_tolower was removed but maybe it would be faster than tolower() above?
#   http://stackoverflow.com/questions/29463464/r-slowly-working-lapply-with-factor
#----------------------------------------------------------
out <- stri_extract_all_words(docs[[1]], locale = "english", simplify = F)

names(out) <- paste0("doc", 1:length(out))

lev.terms <- sort(unique(unlist(out)))

v1 = lapply(out, function(x, lev) 
    {
        ind = which(lev %in% x)
        cnt = as.integer(factor(x, levels = lev[ind], ordered = TRUE))
        sort(ind[cnt])
    },
    lev = lev.terms
)

v2 = lapply(seq_along(v1), function(i, x, n)
    {
        rep(i, length(x[[i]]))
    },
    x = v1,
    n = names(v1)
)

stm = data.frame(i = unlist(v1), j = unlist(v2)) %>%
    group_by(i, j) %>%
    tally() %>%
    ungroup()

tmp = simple_triplet_matrix(
    i = stm$i,
    j = stm$j,
    v = stm$n,
    nrow = length(lev.terms),
    ncol = length(lev.docs),
    dimnames = list(Terms = lev.terms, Docs = lev.docs)
)





#   http://stackoverflow.com/questions/25330753/more-efficient-means-of-creating-a-corpus-and-dtm

dat <- do.call(cbind, lapply(out, function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
}, lev = lev))
rownames(dat) <- sort(lev)

#   Remove profanity here
#   !!!!!!!!!!!!!!

head(do.call(cbind, lapply(out[126564:126565], function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
}, lev = lev)), n = 20)


lapply(out[126564:126565], function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
}, lev = lev)




dat <- dat[!rownames(dat) %in% tm::stopwords("english"), ] 

dat2 <- slam::as.simple_triplet_matrix(dat)

tdm <- tm::as.TermDocumentMatrix(dat2, weighting=weightTf)

## or...
#dtm <- tm::as.DocumentTermMatrix(dat2, weighting=weightTf)




#   data.table / Matrix code from Ken Benoit
install.packages('data.table')
install.packages('Matrix')
install.packages('quanteda')
require(data.table)
require(Matrix)
require(quanteda)

dfm_quanteda <- function(x) {
    docIndex <- 1:length(x)
    if (is.null(names(x))) 
        names(docIndex) <- factor(paste("text", 1:length(x), sep="")) else
            names(docIndex) <- names(x)
        
        alltokens <- data.table(docIndex = rep(docIndex, sapply(x, length)),
                                features = unlist(x, use.names = FALSE))
        alltokens <- alltokens[features != ""]  # if there are any "blank" features
        alltokens[, "n":=1L]
        alltokens <- alltokens[, by=list(docIndex,features), sum(n)]
        
        uniqueFeatures <- unique(alltokens$features)
        uniqueFeatures <- sort(uniqueFeatures)
        
        featureTable <- data.table(featureIndex = 1:length(uniqueFeatures),
                                   features = uniqueFeatures)
        setkey(alltokens, features)
        setkey(featureTable, features)
        
        alltokens <- alltokens[featureTable, allow.cartesian = TRUE]
        alltokens[is.na(docIndex), c("docIndex", "V1") := list(1, 0)]
        
        sparseMatrix(i = alltokens$docIndex, 
                     j = alltokens$featureIndex, 
                     x = alltokens$V1, 
                     dimnames=list(docs=names(docIndex), features=uniqueFeatures))
}



tokenizedTexts <- tokenize(docs_small[[1]])
dtm <- dfm_quanteda(tokenizedTexts)

topfeatures(dtm)
##  user  system elapsed 
## 0.060   0.005   0.064 

