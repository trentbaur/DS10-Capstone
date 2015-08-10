source(file="capbase.R")

#-------------------------------
#   Function Declarations
#-------------------------------
preprocess_sentences <- function(data, wordcount=20) {

    #   Break the sentence into words but only keep the last x words. (default is 20)
    #   Nothing (yet) is gained from long entries from the test corpus.
    #   Small values for wordcount will cause issues if there are numerous stopwords in a row
    #       but anything above 10 should be safe to use
    sentence <- lapply(strsplit(tolower(data), " "), function(x) x[max(1, length(x)-wordcount):length(x)])
    
    #   Determine where the last non-stop/profanity word is. This is the word we will try to predict
    wordnum <- sapply(sentence, function(x) max(which(!x %in% words_to_remove)))

    #   Short sentences will result in empty wordnums
    #   Remove them from both sentences and wordnum
    shortsentences <- which(wordnum==-Inf)
    if (length(shortsentences)>0) {
        sentence <- sentence[-shortsentences]
        wordnum <- wordnum[-shortsentences]
    }

    #   Retrieve the actual word based on wordnum
    answer <- vapply(seq_along(sentence), function(i) sentence[[i]][wordnum[i]], "")
    
    #   Build the formatted phrase (up to the answer word) that we're going to predict against
    phrase <- vapply(seq_along(sentence), function(i) paste(sentence[[i]][1:(wordnum[i]-1)], collapse = "_"), "")
    
    #   Clean up phrase of any repeated underscores
    phrase <- gsub("(_)\\1+", "\\1", phrase)
    
    #   Return the preprocessed test data
    data.table(phrase, wordnum, answer)
}


split_train_test <- function(dir, filenum, reccount, samplenum = 100, seed=1) {
    set.seed(seed)

    testname <- paste(dir, filenames[filenum], "_test.csv", sep="")
    trainname <- paste(dir, filenames[filenum], "_train.csv", sep="")
    trainraw <- paste(dir, filenames[filenum], "_trainraw.csv", sep="")
    
    if (file.exists(testname) | file.exists(trainname)) {
        stop("Files already exist")
    }
    
    if (!dir.exists(dir)) {
        dir.create(dir, showWarnings = FALSE)
    }
    
    #-----------------------------------------------------------------
    #   Import entire dataset in order to generate a random sample
    #   After import, make all text lower case
    #-----------------------------------------------------------------
    doc <- tolower(read_lines(files[filenum]))
    #   Remove consecutive spaces, replace with single space
    doc <- gsub("( )\\1+", "\\1", doc)
    
    #   Add start/end of document tokens
    #   These needs to be applied to entire file because they will be processed in ngrams
    doc <- vapply(doc, function(x) { paste('#d#', trim(x), '##d#', collapse = ' ')}, '', USE.NAMES = F)
    #doc <- paste0('<d>', doc, '</d>', collapse = ' ')

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
    test_doc <- doc[test_indexes]
    train_doc <- doc[train_ind]
    
    
    #------------------------------------------------------------
    #   Write test set out to disk
    #   Name directory based on sample and seed.
    #       This will allow for easy regeneration of train/test
    #   Create directory if it doesn't exist
    #------------------------------------------------------------
    #   Have to leave in short wordnums -> Have to program around it
    write_csv(as.data.frame(doc[train_ind]), path=trainraw)

    write_csv(as.data.frame(train_doc), path=trainname)
    write_csv(as.data.frame(test_doc), path=testname)
}


build_ngrams <- function(dir, reccount, samplenum, seed) {
    trainingfiles <- vapply(filenames, function(x) paste(dir, x, "_trainraw.csv", sep=''), "")

    #   Make sure the directory and files exist
    #   Otherwise, abort the function
    if (dir.exists(dir) == F || lapply(trainingfiles, function(x) file.exists(x)) == F) {
        stop("Directory or training files do not exist")
    }

        
    #----------------------------------------------------------
    #   Create training corpus via quanteda package
    #----------------------------------------------------------
    for(d in 1:3) {
        docs <- read_lines(trainingfiles[d])
        #   Eliminate quotes surrounding each doc
        docs <- gsub("\"", "", docs)
        
        corp <- corpus(docs, enc="UTF-8")
        
        #   Clean up memory space
        rm(docs)
        gc()
        
        #--------------------------------------------------------------------------
        #   Create n-gram DocumentFrequencyMatrix
        #   DFM can accept a corpus directly but only uses unigrams in that case
        #   Leave in numbers: These will later be turned into placeholders
        #   Don't remove separators as it tends to mess up the <d> tokens
        #--------------------------------------------------------------------------
        for (n in 1:4) {
            tokens <- tokenize(corp, ngrams = n, removeNumbers = F, removeSeparators = T, removePunct = F)
            
            dfm <- dfm(tokens, ignoredFeatures = words_to_remove, verbose=F)
            
            freq <- as.data.table(docfreq(dfm), keep.rownames=T)
            
            write.csv(freq, file=paste0(dir, filenames[d],"_", n, ".csv"), quote = F, row.names = F)
        }
        
        rm(tokens)
        rm(dfm)
        rm(freq)
        rm(corp)
        gc()
    }
}


combine_files <- function(dir, reccount, n, lowfreq = 1) {
    freqs <- list()
    
    for (i in 1:3) {
        freqs[[i]] <- fread(input = paste(dir,filenames[i],"_", n, ".csv", sep=""),
                            sep = ",",
                            nrows = -1,
                            header = T,
                            stringsAsFactors = F,
                            verbose = F)[,, with=F] # Remove first column which is just the rowname from the write.csv step
        
    }
    
    merged <- Reduce(function(x,y) {merge(x,y,by="V1",all=T)}, freqs)
    
    setnames(merged, old= c('V1', 'V2.x', 'V2.y', 'V2'), new = c('token', 'news_cnt', 'blog_cnt', 'twit_cnt'))
    
    merged[is.na(merged$news_cnt),]$news_cnt <- c(0)
    merged[is.na(merged$blog_cnt),]$blog_cnt <- c(0)
    merged[is.na(merged$twit_cnt),]$twit_cnt <- c(0)
    merged$total <- merged$news_cnt + merged$blog_cnt + merged$twit_cnt
    
    merged
}


#----------------------------------------------------------------------------------------------
#   Preprocessing via batch script
#   1)  Read in entire raw file
#   2)  Delete apostrophes and compress to avoid weird split words (TBD). "You're" becomes youre
#   3)  Replace all remaining punctuation with spaces
#   4)  Keep only alphanumeric and spaces
#           NO CODE NEEDED HERE
#----------------------------------------------------------------------------------------------




#---------------------------------------------------------------------
#           N-GRAM GENERATION PROCESS OVERVIEW
#
#   Each of these steps/loops can be run independently as long as
#   the files from the prior step have been performed
#
#   1)  Read in x records from en_US.xxxx.txt files and split into train/test
#   2)  Generate ngram frequency list
#   3)  LEFT JOIN all three datasets together and write combined results out to csv
#           Only output records with sufficient counts
#---------------------------------------------------------------------
for(i in 1:3) {
    split_train_test(dir=dir,
                     filenum = i,
                    reccount = reccount,
                    samplenum = samplenum,
                    seed = seed)
}

build_ngrams(dir=dir,
             reccount = reccount,
             samplenum = samplenum,
             seed = seed)

for (n in 1:4) {
    #n=1
    combined <- combine_files(dir, n=n)

    #   Remove singletons and lower frequency ngrams
    #   Vary threshold for output by n since freq decreases as n increases
    #   This threshold number can/should be made more complicated
    lowfreq <- 1 #(6-n)
    combined_clean <- combined[total > lowfreq]
    
    #   Remove grams shorter than n (Not sure why these happen)
    combined_clean <- combined_clean[str_count(token, '_')==(n-1)]

    
    #--------------------------------------------------------
    #   Extract stub and last word from filtered dataset
    #--------------------------------------------------------
    combined_clean$stub <- vapply(combined_clean[,token], function(x) paste0(str_split(x, '_')[[1]][c(1:(n-1))], collapse = "_"), '')

    #   Avoid writing out lastword to unigram file, waste of space and processing
    #   ACTUALLY, we need the column count to match. Fill in stub.
    if(n == 1) {
        combined_clean$lastword <- combined_clean$stub
    } else {
        combined_clean$lastword <- vapply(combined_clean[,token], function(x) str_split(x, '_')[[1]][n], '')
    }

    write.csv(combined_clean[,c('stub', 'lastword', 'news_cnt', 'blog_cnt', 'twit_cnt', 'total'), with=F], file=paste(dir, "combined_", n, ".csv", sep=""), quote = F, row.names = F)
    
    rm(combined)
    rm(combined_clean)
    gc()
}






doc <- tolower(read_lines(files[1]))
doc[3]

paste('<d>', doc[1:10], '</d>', collapse = ' ')
vapply(doc[1:10], function(x) { paste('<d>', x, '</d>', collapse = ' ')}, '', USE.NAMES = F)






