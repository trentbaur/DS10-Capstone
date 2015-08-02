

unigrams <- fread(input = "files/combined_1_clean.csv",
      sep = ",",
      nrows = -1,
      header = T,
      stringsAsFactors = T,
      verbose = F)


unigrams[is.na(unigrams$news_cnt),]$news_cnt = 0
unigrams[is.na(unigrams$blog_cnt),]$blog_cnt = 0
unigrams[is.na(unigrams$twit_cnt),]$twit_cnt = 0
unigrams$total <- unigrams$news_cnt + unigrams$blog_cnt + unigrams$twit_cnt

unigrams[grep("\\dam|\\dpm", unigrams$token, ignore.case = T),][1:80,]

numbergrams <- unigrams[grep("\\d", unigrams$token),]

good_numbergrams <- c('1st', '2nd', '3rd', '4th', '5th', '6th', '7th', '8th', '9th', '10th',
                      '1920s',
                      '1930s',
                      '1940s',
                      '1950s',
                      '1960s',
                      '1970s',
                      '1980s',
                      '1990s',
                      '80s', '90s')

numbergrams <- numbergrams[!(numbergrams$token %in% good_numbergrams),]

numbergrams[order(-total)][81:160]
