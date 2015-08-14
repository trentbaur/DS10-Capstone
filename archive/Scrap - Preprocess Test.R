
set.seed(1)

doc <- tolower(read_lines(files[1]))
reccount = -1
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









max(length(doc[test_indexes]))


object.size(stub)


#   Break the sentence into words but only keep the last x words. (default is 20)
#   Nothing (yet) is gained from long entries from the test corpus.
#   Small values for wordcount will cause issues if there are numerous stopwords in a row
#       but anything above 10 should be safe to use
sentence <- lapply(strsplit(tolower(doc[test_indexes]), " "), function(x) x[max(1, length(x)-20):length(x)])

#   Determine where the last non-stop/profanity word is. This is the word we will try to predict
wordnum <- sapply(sentence, function(x) max(which(!x %in% words_to_remove)))

shortsentences <- which(wordnum==-Inf)


sentence <- sentence[-shortsentences]
wordnum <- wordnum[-shortsentences]

#   Retrieve the actual word based on wordnum
answer <- sapply(seq_along(sentence), function(i) sentence[[i]][wordnum[i]])

#   Build the formatted stub (up to the answer word) that we're going to predict against
#stub <- sapply(seq_along(sentence[1:1111]), function(i) paste(sentence[[i]][1:(wordnum[i]-1)], collapse = "_"), simplify = "array")
stub <- vapply(seq_along(sentence), function(i) paste(sentence[[i]][1:(wordnum[i]-1)], collapse = "_"), "")
#stub <- lapply(seq_along(sentence)[1:1110], function(i) paste(sentence[[i]][1:(wordnum[i]-1)], collapse = "_"))



#sentence[length(sentence)]

#length(sentence[[1110]])
#   Return the preprocessed test data
data.table(stub, wordnum, answer)

sentence[1111]
wordnum[1111]==-Inf

which(wordnum==-Inf)

length(sentence)
length(wordnum)


#------------------------------------------------------------
#   Write test set out to disk
#   Name directory based on sample and seed.
#       This will allow for easy regeneration of train/test
#   Create directory if it doesn't exist
#------------------------------------------------------------
dir <- paste("files/", samplenum, "_", seed, "/", sep="")

testname <- paste(dir, filenames[filenum], "_test.csv", sep="")
trainname <- paste(dir, filenames[filenum], "_train.csv", sep="")

if (!dir.exists(dir)) {
    dir.create(dir, showWarnings = FALSE)
}

if (file.exists(testname)) {
    file.remove(testname)
}

if (file.exists(trainname)) {
    file.remove(trainname)
}

write_csv(as.data.frame(test_doc), path=testname)
write_csv(as.data.frame(doc[train_ind]), path=trainname)