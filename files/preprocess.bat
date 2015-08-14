tr -d "\'" < en_US.news.txt > news_1.txt
tr '[:punct:]' ' ' < news_1.txt > news_2.txt
tr -cd '[:alnum:][:space:]' < news_2.txt > news_3.txt
tr '\b(january)|(february)|(april)|(june)|(july)|(august)|(september)|(october)|(november)|(december)\b/i' '##month##' < news_3.txt > news_4.txt
tr '\b(1st)|(2nd)|(3rd)|(4th)|(5th)|(6th)|(7th)|(8th)|(9th)|(10th)\b/i' '##ord##' < news_4.txt > news_5.txt
tr '\b(1900s)|(1910s)|(1920s)|(1930s)|(1940s)|(1950s)|(1960s)|(1970s)|(1980s)|(1990s)|(2000s)|(2010s)\b/i' '##decade##' < news_5.txt > news_6.txt


tr -d "\'" < en_US.blogs.txt > blogs_1.txt
tr '[:punct:]' ' ' < blogs_1.txt > blogs_2.txt
tr -cd '[:alnum:][:space:]' < blogs_2.txt > blogs_final.txt



tr -d "\'" < en_US.twitter.txt > twitter_1.txt
tr '[:punct:]' ' ' < twitter_1.txt > twitter_2.txt
tr -cd '[:alnum:][:space:]' < twitter_2.txt > twitter_final.txt
