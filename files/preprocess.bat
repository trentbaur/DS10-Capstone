tr -d "\'" < en_US.news.txt > news_1.txt
tr '[:punct:]' ' ' < news_1.txt > news_2.txt
tr -cd '[:alnum:][:space:]' < news_2.txt > news_final.txt



tr -d "\'" < en_US.blogs.txt > blogs_1.txt
tr '[:punct:]' ' ' < blogs_1.txt > blogs_2.txt
tr -cd '[:alnum:][:space:]' < blogs_2.txt > blogs_final.txt



tr -d "\'" < en_US.twitter.txt > twitter_1.txt
tr '[:punct:]' ' ' < twitter_1.txt > twitter_2.txt
tr -cd '[:alnum:][:space:]' < twitter_2.txt > twitter_final.txt
