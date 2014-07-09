cat('Start!\n')

x = read.csv('./Diving2000.csv', as.is=TRUE) # read Diving data

dim(x)  # To see the dimension and number of data

plot(JScore ~ Difficulty, data=x)

plot(jitter(JScore) ~ jitter(Difficulty), data=x, 
     col = factor(x$Round)) # adding some noises by jitter

abline(lm(jitter(JScore) ~ jitter(Difficulty), data=x,
          subset=x$Difficulty >= 2.3), lwd=3)  # linear model traverse the Dif >= 2.3

abline(lm(jitter(JScore) ~ jitter(Difficulty), data =x),
       col = "green", lwd=1.5)  # linear model traversing the whole dataset

# To see the relationship between Judge's Country and Country
x$match <- x$Country == x$JCountry
table(x$match)

# Choose a specific Judge to continue
thisJudge <- "WANG Facheng"
mean(x$JScore)
mean(x$JScore[x$Judge==thisJudge & x$match])
mean(x$JScore[x$Judge==thisJudge & !x$match])

# Non-Chinese judges for Chinese divers
mean(x$JScore[x$JCountry != "CHN" &
              x$Country == "CHN"])

# lowest score of Chinese divers
#x_chn <- x[x$Country == "CHN", 1:5]
x_chn_min <- min()

# Which judge give this low score?
#x$Judge[x$JScore == x_chn_min & x$Country == "CHN"]
x[x$JScore == x_chn_min & x$Country == "CHN", c("Judge", "JCountry")]
#which(x$JScore == 3.5)  # Giving the index

# Would have same effect but faster, traversing only one-time
x$Judge[which.min(x$JScore[x$Country =="CHN"])]


# Given a specific judge's name, output his or her country
thisJudge <- "WANG Facheng"
thisCountry <- x$JCountry[x$Judge == thisJudge]
cat(thisJudge, "is from:", unique(thisCountry), "\n")
