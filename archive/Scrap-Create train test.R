dir <- paste("files/", samplenum, "_", seed, "_", reccount, "/", sep="")

testname <- paste(dir, filenames[filenum], "_test.csv", sep="")
trainname <- paste(dir, filenames[filenum], "_train.csv", sep="")

if (file.exists(testname) | file.exists(trainname)) {
    break;
}

if (!dir.exists(dir)) {
    dir.create(dir, showWarnings = FALSE)
}

#-----------------------------------------------------------------
#   Import entire dataset in order to generate a random sample
#   After import, make all text lower case
#-----------------------------------------------------------------
doc <- tolower(read_lines(files[filenum]))

#   Sample reccount docs from the full file
if (reccount == -1) {
    partial <- 1:length(doc)
} else
{
    partial <- sample(1:length(doc),
                      size = reccount,
                      replace = F)
}

#   Determine training set indicator
train_ind <- sample(partial,
                    size = as.integer(floor(length(partial)*(samplenum/100))),
                    replace = F)

#   Identify the test set indexes
test_indexes <- partial[-which(partial %in% train_ind)]

#   Pass only the first reccount records to the preprocess function
test_doc <- preprocess_test_sentences(doc[test_indexes])

doc[test_indexes]

#------------------------------------------------------------
#   Write test set out to disk
#   Name directory based on sample and seed.
#       This will allow for easy regeneration of train/test
#   Create directory if it doesn't exist
#------------------------------------------------------------
write_csv(as.data.frame(test_doc), path=testname)
write_csv(as.data.frame(doc[train_ind]), path=trainname)











docs <- read_lines(trainingfiles[1])
docs <- gsub("\"", "", docs)

corp <- corpus(docs, enc="UTF-8")
str(corp)
removeFeatures(docs[2], stopwords=c('defended'), verbose=F)
#   Clean up memory space
rm(docs)
gc()

#----------------------------------------------------------
#   Create n-gram DocumentFrequencyMatrix
#   DFM can accept a corpus directly but only uses unigrams in that case
#----------------------------------------------------------
for (n in 1:4) {
    tokens <- tokenize(corp, ngrams = 1, removeNumbers = T, removeSeparators = T)
    
    dfm <- dfm(corp, ignoredFeatures = words_to_remove, verbose=F)
    
    freq <- as.data.table(docfreq(dfm), keep.rownames=T)
    
    write.csv(freq, file=paste(dir, filenames[d],"_", n, ".csv", sep=""), quote = F)
}













