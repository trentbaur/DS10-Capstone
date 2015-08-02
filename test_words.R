
quiz <- c(  "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
            "You're the reason why I smile everyday. Can you follow me please? It would mean the",
            "Hey sunshine, can you follow me and make me the",
            "Very early observations on the Bills game: Offense still struggling but the",
            "Go on a romantic date at the",
            "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
            "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
            "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
            "'Be grateful for the good times and keep the faith during the",
            "If this isn't the cutest thing you've ever seen, then you must be")


preprocess_test_sentences(quiz, wordcount = 12)
preprocess_test_sentences(quiz)


quiz






quiz[1]

unlist(strsplit(tolower(quiz[1]), " ")) 

max(which(!unlist(strsplit(tolower(quiz[1]), " ")) %in% stopwords()))
                        
unlist(strsplit(quiz[1], " "))[1:max(which(!unlist(strsplit(tolower(quiz[1]), " ")) %in% stopwords()))]



x<-2

quiz[x]

sentence <- unlist(strsplit(tolower(quiz[x]), " ")) 

sentence[1:max(which(sentence %in% stopwords()))]

paste(sentence[1:max(which(sentence %in% stopwords()))], collapse = "_")




 sapply(sentence, function(x) max(length(x)))

lapply(strsplit(tolower(quiz), " "), function(x) x[max(1, length(x)-10):length(x)])
 
 
 
 
sentence <- strsplit(tolower(quiz), " ")

wordnum <- sapply(sentence, function(x) max(which(!x %in% stopwords())))

answer <- sapply(seq_along(sentence), function(i) sentence[[i]][wordnum[i]])
stub <- sapply(seq_along(sentence), function(i) paste(sentence[[i]][1:(wordnum[i]-1)], collapse = "_"))
    
    
data.table(stub, wordnum, answer)


stub <- lapply(sentence, function(x) paste(sentence[1:max(which(sentence %in% stopwords()))], collapse = "_"))


str(quiz)
str(word)

x<-data.frame()
cbind(x, quiz, word)


paste(sentence[1:max(which(sentence %in% stopwords()))], collapse = "_")




str(sentence)












doc <- tolower(read_lines("files/en_US.news.txt"))

partial <- sample(1:length(doc),
                    size = reccount,
                    replace = F)

#   Determine training/test set
train_ind <- sample(partial,
                    size = as.integer(floor(length(partial)*(samplenum/100))),
                    replace = F)

partial[-which(partial %in% train_ind)]

length(train_ind)
testsample <- train_ind[1:reccount]
length(doc[-testsample])

#   Pass only the first reccount records to the preprocess function
test_doc <- preprocess_test_sentences(doc[-train_ind[1:reccount]])
