#   Question 1 - Size of blogs.txt
200

#   Question 2 - How many lines in twitter file?

twitter <- read_lines("files/en_US.twitter.txt", n_max = -1)

str(twitter)


#   Question 3 - Max line length in any file
news <- read_lines("files/en_US.news.txt", n_max = -1)
blogs <- read_lines("files/en_US.blogs.txt", n_max = -1)


max(nchar(news))
max(nchar(blogs))
max(nchar(twitter))


#   Question 4 - Twitter: love / hate
sum(grepl(twitter, pattern = 'love', ignore.case = FALSE)) / sum(grepl(twitter, pattern = 'hate', ignore.case = FALSE))


#   Question 5 - biostats tweet
twitter[grepl(twitter, pattern = 'biostats', ignore.case = FALSE)]


#   Question 6 - Exact match
twitter[grepl(twitter, pattern = 'A computer once beat me at chess, but it was no match for me at kickboxing', ignore.case = FALSE)]


rm(news)
rm(blogs)
rm(twitter)
