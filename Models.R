source(file="capbase.r")

#-----------------------------
#   Data load functions
#-----------------------------
load_grams <- function(master=0) {
    lapply(1:4, function(x) {
        dt <- fread(input = paste(ifelse(master, masterdir, dir), 'combined_', x, '.csv', sep=''),
          sep = ",",
          nrows = -1,
          header = T,
          stringsAsFactors = F,
          verbose = F) 
        
        setkey(dt, stub, total, news_cnt, blog_cnt)
        
    })
}

load_text_data <- function(filename = 'news', filetype = 'train', nrows = -1) {
    docs <- fread(input = paste(dir, filename, '_', filetype, '.csv', sep=''),
            nrows = nrows,
            header = T,
            stringsAsFactors = F,
            verbose = F)
    
    setnames(x = docs, old = paste0(filetype, '_doc'), new = 'document')
    
    docs
}


#-----------------------------
#   Model Utility functions
#-----------------------------
get_tomatch <- function(words, n) {
    #   Match last word only for n=1,2
    #   Otherwise match last n-1 words
    #       n=3, match last 2 words
    #       n=4, match last 3 words
    firstword <- max(1, length(words) - ifelse(n<3, 0, n-2))
    
    paste(words[firstword:length(words)], collapse = '_')
}

get_logprob <- function(matches, lambda = 1) {
    if (nrow(matches) > 0)
    { 
        prediction <- matches[1,]$lastword
        prob <- log((matches[1,]$total / sum(matches$total)) * lambda, base = 2)
    } else {
        prediction <- ''
        prob <- log(.00001, base = 2)
    }
    
    data.table(pred=prediction, logprob=prob, keep.rownames = F)
}

calculate_perplexity <- function(logprob, m) {
    2 ^ (-1 * (logprob / m))
}

#------------------------
#       Models
#------------------------
model_gram <- function (words, param1, param2) {
    n <- param1
    
    preceding <- get_tomatch(words, n)
    
    #   Retrieve ngram matches and calculate the probability for the best choice
    matches <- grams[[n]][stub==preceding,][order(-total, -news_cnt, -blog_cnt)]

    logprobs <- get_logprob(matches)
    logprobs$preceding <- preceding
    logprobs
}

model_backoff_simple <- function(words, param1, param2) {
    #   Get last three words and try to match against 4grams
    tomatch4 <- get_tomatch(words, 4)
    matches4 <- grams[[4]][stub==tomatch4, ][order(-total, -news_cnt, -blog_cnt)]
    if (nrow(matches4) > 0) {
        logprobs <- get_logprob(matches4)
        logprobs$preceding <- tomatch4
        logprobs
    } else {
        
        #   If no match, try 3grams
        tomatch3 <- get_tomatch(words, 3)
        matches3 <- grams[[3]][stub==tomatch3, ][order(-total, -news_cnt, -blog_cnt)]
        if (nrow(matches3) > 0) {
            logprobs <- get_logprob(matches3, lambda = .4)
            logprobs$preceding <- tomatch3
            logprobs
        } else {
            
            #   If no match, try 2grams
            tomatch2 <- get_tomatch(words, 2)
            matches2 <- grams[[2]][stub==tomatch2, ][order(-total, -news_cnt, -blog_cnt)]
            logprobs <- get_logprob(matches2, lambda = (.4*.4))
            logprobs$preceding <- tomatch2
            logprobs
        }
    }
}


#------------------------
#   Execution functions
#------------------------
process_document <- function(text, model, separator=' ', param1 = 0, param2 = 0) {
    #   Use stringi function directly, will be a small bit faster
    words <- stri_split_fixed(text, separator)[[1]]
    
    #   Fix the SOD/EOD tokens. Even though we're going to delete the SOD rows,
    #   we still want to predict off of it. (And potentially match the first word.)
     words[grep(pattern = '##d#', x = words)] <- '##d#'
     words[grep(pattern = '"#d#', x = words)] <- '#d#'
    
    
    #   Loop through each word and send it and the previous words to supplied model
    results <- lapply(1:length(words), function(x) {
        output <- model(words[max(1,(x-3)):x], param1, param2)
        output$to_predict <- words[x+1]
        output
    })
    
    #   Return results as data frame to simplify usage
    solution <- data.frame()
    rbind(solution, do.call(rbind, results))
}

run_model <- function(docs, model, separator=' ', param1 = 0, param2 = 0) {
    results <- process_document(docs, model, ' ', param1, param2)
    
    #   Shift data so the predicted word is next to the actual word
    accuracy <- data.table(results[-1, .(preceding, to_predict)], results[-nrow(results),pred], results[-nrow(results),logprob])
    setnames(accuracy, old = c('V2', 'V3'), new = c('predicted', 'logprob'))
    
    #   For purposes of perplexity, remove any rows with preceding ending in SOD. (#d#)
    #   There is no prediction possible when preceding = EOD so it is just 
    #       an unnecessary penalty to perplexity score.
    #   EOD, however, can have a prediction based on legitimate preceding words.
    #   Leave these logprobs in for the time being.
    accuracy <- accuracy[!grepl(pattern = '(##d#)$', accuracy$preceding), ]
    
    accuracy$Correct <- accuracy[,to_predict] == accuracy[,predicted]
    
    accuracy
}

evaluate_model <- function(docs, model, separator=' ', param1 = 0, param2 = 0) {

    #   SET TIMER
    ptm <- proc.time()
    
    accuracy <- run_model(docs, model, separator, param1, param2)
    
    doc_count <- nrow(docs)
    word_count <- nrow(accuracy)
    accuracy_rate <- sum(accuracy[,to_predict] == accuracy[,predicted]) / nrow(accuracy)
    perplexity <- calculate_perplexity(sum(accuracy$logprob), nrow(accuracy))
    
    #   STOP TIMER
    runtime <- (proc.time() - ptm)[c('elapsed')]
    
    data.frame(accuracy_rate, perplexity, doc_count, word_count, runtime)
    
}


#-----------------------------------------
#   Execute models
#-----------------------------------------
#   grams <- load_grams(master=1)
#   traindata <- load_text_data (filename = filenames[1], filetype = filetype, nrows = -1)

filetype <- 'train'

modelresults <- data.frame()

for (i in 1:3) {
    traindata <- load_text_data (filename = filenames[i], filetype = filetype, nrows = -1)

    recs <- nrow(traindata)
    
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '2gram', evaluate_model(traindata[1:recs], model_gram, ' ', 2)))
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '3gram', evaluate_model(traindata[1:recs], model_gram, ' ', 3)))
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '4gram', evaluate_model(traindata[1:recs], model_gram, ' ', 4)))
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = 'backoff_simple', evaluate_model(traindata[1:recs], model_backoff_simple)))
}

modelresults


#----------------------------------------------------------------------------------------------


evaluate_model(traindata, model_gram, ' ', 2)
evaluate_model(traindata[2:4], model_gram, ' ', 4)

run_model(traindata[2:4], model_gram, ' ', 2)

run_model(traindata[2:4], model_gram, ' ', 3)

run_model(traindata[2:4], model_gram, ' ', 4)

results <- run_model(traindata[1:20], model_gram, ' ', 2)



run_model(traindata[2], model_gram, ' ', 2)



debug(run_model)
debug(evaluate_model)
debug(process_document)
debug(model_gram)
debug(model_backoff_simple)

undebug(run_model)
undebug(evaluate_model)
undebug(process_document)
undebug(model_gram)
undebug(model_backoff_simple)







