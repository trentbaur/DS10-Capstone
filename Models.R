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
    2 ^ ((-1/m) * logprob)
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
    
    #   Loop through each word and send it and the previous words to supplied model
    results <- lapply(1:length(words), function(x) {
        model(words[max(1,(x-3)):x], param1, param2)
    })
    
    #   Return results as data frame to simplify usage
    solution <- data.frame()
    rbind(solution, do.call(rbind, results))
}

run_model <- function(docs, model, separator=' ', param1 = 0, param2 = 0) {
    results <- docs[, process_document(docs, model, ' ', param1, param2), by = 1:nrow(docs)]
    
    accuracy <- data.table(results[-1,preceding], results[-nrow(results),pred], results[-nrow(results),logprob])
    setnames(accuracy, old = c('V1', 'V2', 'V3'), new = c('expected', 'predicted', 'logprob'))
    accuracy$correct <- accuracy[,expected] == accuracy[,predicted]
    
    accuracy
}

evaluate_model <- function(docs, model, separator=' ', param1 = 0, param2 = 0) {

ptm <- proc.time()
    
    accuracy <- run_model(docs, model, separator, param1, param2)
    
    doc_count <- nrow(docs)
    word_count <- nrow(accuracy)
    accuracy_rate <- sum(accuracy[,expected] == accuracy[,predicted]) / nrow(accuracy)
    perplexity <- calculate_perplexity(sum(accuracy$logprob), nrow(accuracy))
    
runtime <- (proc.time() - ptm)[c('elapsed')]
    
    data.frame(accuracy_rate, perplexity, doc_count, word_count, runtime)
    
}


#-----------------------------------------
#   Execute models
#-----------------------------------------
modelresults <- data.frame()

filetype <- 'train'

for (i in 1:3) {
    traindata <- load_text_data (filename = filenames[i], filetype = filetype, nrows = -1)

    recs <- nrow(traindata)
    
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '2gram', evaluate_model(traindata[1:recs], model_gram, ' ', 2)))
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '3gram', evaluate_model(traindata[1:recs], model_gram, ' ', 3)))
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = '4gram', evaluate_model(traindata[1:recs], model_gram, ' ', 4)))
    modelresults <- rbind(modelresults, cbind(filetype, filename = filenames[i], model = 'backoff_simple', evaluate_model(traindata[1:recs], model_backoff_simple)))
}

modelresults


traindata <- load_text_data (filename = filenames[1], filetype = filetype, nrows = -1)

evaluate_model(traindata[1:20], model_gram, ' ', 2)


ptm <- proc.time()
run_model(traindata[1:20], model_gram, ' ', 2)
runtime <- proc.time() - ptm



results <- run_model(traindata[1:20], model_gram, ' ', 2)


undebug(evaluate_model)
undebug(process_document)
t<-evaluate_model(testdata, model_gram, ' ', 3)

t<-evaluate_model(testdata, model_gram, ' ', 2)
evaluate_model(testdata[1:2], model_gram, ' ', 3)
evaluate_model(testdata[1:2], model_gram, ' ', 4)
evaluate_model(testdata[1:2], model_backoff_simple)


t
accuracy <- data.table(t[-1,]$preceding, t[-length(t),pred], t[-length(t),logprob])
setnames(accuracy, old = c('V1', 'V2', 'V3'), new = c('expected', 'predicted', 'logprob'))
accuracy$correct <- sum(accuracy[,expected] == accuracy[,predicted])/nrow(accuracy)
accuracy

t[!pred=='',]

     t
     undebug(get_tomatch)
     undebug(model_gram)
     
class(testdata)
str(testdata)

length(testdata)
test <- process_document('Initialize data before calling various model functions', model_gram, ' ', 2)
sum(test$logprob)^()

grams <- load_grams(master=0)

testdata <- load_text_data (filename = 'news', filetype = 'test', nrows = -1)
str(testdata)
evaluate_model(testdata, model_gram, ' ', 2)

str_split(testdata[1], ' ')
stri_split_fixed(testdata[1], ' ')[[1]]

sapply(testdata, function(x) { process_document(x, model_gram, ' ', 2, 0) })




#------------------------
#   Process Driver
#------------------------
run_models <- function(filename = 'news', filetype = 'train', nrows = -1) {
    td <- fread(input = paste(dir, filename, '_', filetype, '.csv', sep=''),
                       nrows = nrows,
                       header = T,
                       stringsAsFactors = F,
                       verbose = F)

    #   Pass traindata into various models and store answer
    td$prediction4 <- model_predict_unique(td, 4)
    td$prediction3 <- model_predict_unique(td, 3)
    td$prediction2 <- model_predict_unique(td, 2)
    td$backoff <- model_backoff(td)

    write.table(lapply(td, unlist), file = paste(dir, filename, '_', filetype, '_results.csv', sep=''), quote = F, append = F, sep = ',')
    
    td
}





