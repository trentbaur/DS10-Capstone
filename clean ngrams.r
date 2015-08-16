library(readr)
library(data.table)

masterdir <- 'files/mastergrams/'

#-----------------------------
#   Data load functions
#-----------------------------
load_grams <- function(gram_dir, mincount=0) {
    lapply(1:4, function(x) {
        dt <- fread(input = paste0(gram_dir, 'combined_', x, '.csv'),
                    sep = ",",
                    nrows = -1,
                    header = T,
                    stringsAsFactors = F,
                    verbose = F) 
        
        if (mincount > 0) {
            dt <- dt[total > mincount]
        }
        
        setkey(dt, stub, total, news_cnt, blog_cnt)
        
    })
}


grams <- load_grams(masterdir, mincount=0)
n <- 4
all_numbers <- '(^\\d+$)'
numbers_stub_start <- '(^\\d+_)'
numbers_stub_in <- '(_\\d+)'
profanity_regex <- '(shit|fuck|damn|piss)'

rows <- list()

for(n in 1:4) {
    #   Remove ngrams that have all number tokens
    #   Need different logic when stub is only one word
    if (n > 2) {
        grams_clean <- grams[[n]][!grepl(x = grams[[n]][,stub], pattern = numbers_stub_start),]
        grams_clean <- grams_clean[!grepl(x = grams_clean[,stub], pattern = numbers_stub_in),]
    } else {
        grams_clean <- grams[[n]][!grepl(x = grams[[n]][,stub], pattern = all_numbers),]
    }

    #   Remove any ngrams that have all numbers as the lastword (i.e. -Suggested word)
    grams_clean <- grams_clean[!grepl(x = grams_clean[,lastword], pattern = all_numbers),]

    #   Remove ngrams that include profanity
    grams_clean <- grams_clean[!grepl(x = grams_clean[,lastword], pattern = profanity_regex),]
    
    rows[[n]] <- nrow(grams_clean)/nrow(grams[[n]])    
    
    write.csv(grams_clean, paste0(masterdir, 'combinedclean_', n, '.csv'), quote = F, row.names = F)
}










#----------------------
#       Scratch
#----------------------
head(grams_clean,80)


nrow(grams[[n]])

sum(grepl(x = grams_clean[,stub], pattern = '(^\\d+th_)'))


View(grams[[2]][grepl(x = grams[[2]][,stub], pattern = profanity_regex),])


#-------------------------------
grams_clean[grepl(x = grams_clean[,lastword], pattern = '(^\\d+$)'),][1201:1300]
z[grepl(x = z[,lastword], pattern = '(^\\d+$)'),]



#   Look at lastword
z<- grams[[2]][!grepl(x = grams[[2]][,stub], pattern = '(^\\d+$)'),]
z[grepl(x = z[,lastword], pattern = '(^\\d+$)'),]
#-------------------------------



View(grams_clean[grepl(x = grams_clean[,stub], pattern = '(\\d+am_)'),])

View(grams_clean[grepl(x = grams_clean[,stub], pattern = '(zzz)'),])


review <- grams[[n]][grepl(x = grams[[n]][,stub], pattern = '((^\\d+_)|(_\\d+($|_)))'),]
write.csv(review, file=paste0(masterdir, 'review.csv'), quote = F, row.names = F)



nrow(grams[[n]][!grepl(x = grams[[n]][,stub], pattern = '((^\\d+_)|(_\\d+($|_)))'),])


nrow(grams[[n]][grepl(x = grams[[n]][,stub], pattern = '_\\d+'),]) / nrow(grams[[n]])




grep(x = grams[[n]][,stub], pattern = '(^\\d+_)')

