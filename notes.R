###Notes and scratchwork for scatteRgories###


#readline()
#> menu(c("Yes", "No"), title="Do you want this?")
#Do you want this? 
#  
#  1: Yes
#2: No
#
#Selection:
  
##To Do##

#1. score(): function to score both by scrabble score and by commonality of word
###scrabble score - (commenality)*scaling (maybe difficulty?)

#2. generate(): pick word from common list
###given a letter, and skill level

#3.load and check commonalities list
###get range, decided on adequate number of breaks

freq = read.csv("freqs.csv", as.is = T)
View(freq)

range(freq[,2])
plot(1:500, freq[,2])
##very skewed

#taking the log
freq[,3] = log(freq[,2])

#converting all to uppercase
freq[,1] = toupper(freq[,1])

##better
plot(1:500, freq[,3])

hist(freq[,3])
##12 levels, seems reasonable

#####scrabble score####
scrab = read.csv("scrab.csv")

#'score a word played in scrabble and calculate a (DEE)score for scatergories
#'
#'

t = "test"
t <- readline("word?")


h = hist(freq[,3])
View(h)
(h$counts[order(h$counts)])

n = matrix(nrow = 12, ncol = 2)
n[,1] = 1:12
n[,2] = h$counts[order(h$counts)]
View(n)
l = NULL
for (i in 1:12){
  t.l = rep(n[i,1], n[i,2])
  l = append(t.l, l)
}
View(l)

freq[,4] = l
###there had to be a better way to do this###

table(h$counts,)
a = numeric(4)
names(a) = c("this","is","a","test")
View(a)
substitute(t)
scrab_score <- function(w, full = TRUE){
  w <- toupper(w)
  adj <- NULL
  
  score <- numeric(3)
  t.score <-NULL
  names(score) <- c("scrabble.score", "scrabble.freq", "scattR.score")
  
  ##for each letter
  for (i in 1:nchar(w)){
    t.score[1] <- scrab[scrab[,1]==substr(w,i,i),3] #scrabble score
    t.score[2] <- scrab[scrab[,1]==substr(w,i,i),2] #scrabble freq
    
    score[1] <- sum(score[1], t.score[1])
    score[2] <- sum(score[2], t.score[2])
  }
  
  ##once we calculate scrabble scores
  
  if(w %in% freq[,1]){ 
    adj <- freq[freq[,1]==w,4]
  } else {
    adj <- 0
  }
  score[3] <- score[1] - adj
  
return(score)  
}

scrab_score(t)

