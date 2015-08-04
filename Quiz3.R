quiz <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
                        "You're the reason why I smile everyday. Can you follow me please? It would mean the",
                        "Hey sunshine, can you follow me and make me the",
                        "Very early observations on the Bills game: Offense still struggling but the",
                        "Go on a romantic date at the",
                        "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
                        "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
                        "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
                        "'Be grateful for the good times and keep the faith during the",
                        "If this isn't the cutest thing you've ever seen, then you must be")

quiz2 <- as.data.table(matrix(c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
           "die",
           "sleep",
           "give",
           "eat",
           
           "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
           "horticultural",
           "spiritual",
           "financial",
           "marital",
           
           "I'd give anything to see arctic monkeys this",
           "month",
           "weekend",
           "morning",
           "decade",
           
           "Talking to your mom has the same effect as a hug and helps reduce your",
           "hunger",
           "stress",
           "happiness",
           "sleepiness",
           
           "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
           "look",
           "minute",
           "picture",
           "walk",
           
           "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
           "matter",
           "case",
           "incident",
           "account",
           
           "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
           "hand",
           "finger",
           "arm",
           "toe",
           
           "Every inch of you is perfect from the bottom to the",
           "top",
           "center",
           "middle",
           "side",
           
           "I’m thankful my childhood was filled with imagination and bruises from playing",
           "weekly",
           "inside",
           "daily",
           "outside",
           
           "I like how the same people are in almost all of Adam Sandler's",
           "pictures",
           "stories",
           "movies",
           "novels"), ncol = 5, byrow = T))

setnames(quiz2,
         c('V1', 'V2', 'V3', 'V4', 'V5'),
         c('phrase', 'answer1', 'answer2', 'answer3', 'answer4') )





qnum <- 10

qanswers <- quiz2[qnum, c(2:5), with=F]
  
#z<-gsub("'", '', quiz2[3])

quizmatch  <- lapply(2:4, function(x) {
    get_ngram_match(quiz2[qnum,]$phrase, n=x, m=100, ' ')
}
)

quizmatch

for (i in 2:4) {
    if (length(quizmatch[[i-1]])==7) {
        print(quizmatch[[i-1]][quizmatch[[i-1]]$lastword %in% qanswers,])
    }
}

grams[[1]][token %in% qanswers, ]








str(grams)
setkey(grams[[1]], "token", "total")
    
attributes(grams[[4]])
q<-create_stub(quiz2[4]$phrase, 4, seperator=' ')
grams
    