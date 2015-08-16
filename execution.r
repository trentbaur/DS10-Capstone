source(file="capbase.r")
source(file="models.r")


#-------------------
#   Load Data
#-------------------
masterdir <- 'files/mastergrams/'
mincount <- 5

if(!exists('grams')) {  
    grams <- load_grams(masterdir, mincount <- mincount)
}   #   rm(grams)


filetype <- 'train'

if(!exists('textdata')) {  
    textdata <- load_text_data (filename = filenames[1], filetype = filetype, nrows = -1)
}


#-------------------------------------------------
#   Execute/evaluate models against test data
#-------------------------------------------------
modelresults <- data.frame()

for (n in 1:5) {
    if(exists('grams')) {
        rm(grams)
    }
    grams <- load_grams(masterdir, mincount = n)

    for (i in 1:3) {
        textdata <- load_text_data (filename = filenames[i], filetype = filetype, nrows = -1)
        
        recs <- nrow(textdata)
        
        modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '2gram', evaluate_model(textdata[1:recs], model_gram, ' ', 2, 0, n)))
        modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '3gram', evaluate_model(textdata[1:recs], model_gram, ' ', 3, 0, n)))
        modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '4gram', evaluate_model(textdata[1:recs], model_gram, ' ', 4, 0, n)))
        modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = 'backoff_simple', evaluate_model(textdata[1:recs], model_backoff_simple, ' ', 0, 0, n)))
        modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = 'backoff_stupid', evaluate_model(textdata[1:recs], model_backoff_stupid, ' ', 0, 0, n)))
    }
}

write.csv(modelresults, paste0(dir, 'modelresults_multiple_nonums.csv'))

modelresults

----------------------------------------------------------------------------------------------


ngram_counts <- data.frame()

for (m in 1:5) {
    if(exists('grams')) {
        rm(grams)
    }
    grams <- load_grams(masterdir, mincount = m)
    
    for (n in 1:4) {
        recs <- nrow(grams[[n]])
        size <- object.size(grams[[n]])
        
        ngram_counts <- rbind(ngram_counts, cbind(n, recs, size, m))
    }
}

write.csv(ngram_counts, paste0(dir, 'ngram_counts.csv'))

ngram_counts



predict_word('this')

recs <- nrow(grams[[1]])
size <- object.size(grams[[1]])

class(cbind(recs, size))
