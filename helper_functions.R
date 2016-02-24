library(shiny)
library(shinyapps)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(caret)
library(twitteR)
library(plyr)
library(stringr)
library(shinythemes)
library(randomForest)
library(highcharter)
library(rCharts)


profanity = readLines("profanity.txt")
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <-"xxxxx"
consumerSecret <-"xxxx"
accesstoken <- "xxxx"
accesssecret <- "xxxxx"

df<-data.frame()

setup_twitter_oauth(consumerKey, consumerSecret,accesstoken,accesssecret)
options(httr_oauth_cache=T)
1




# Sentiment Score
score.sentiment = function(tweets, positive, negative, .progress='none')
{
  
  scores = laply(tweets,
                 function(tweet, positive, negative)
                 {
                   # Cleaning the tweets of punctuations, ctrl chars and numbers
                   tweet = gsub("[[:punct:]]", "", tweet)
                   tweet = gsub("[[:cntrl:]]", "", tweet)
                   tweet = gsub('\\d+', '', tweet)
                   
                   # Catching error when converting tweets to lowercase
                   lower2 = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   tweet = sapply(tweet, lower2)
                   
                   # tokenize tweets
                   word_list = str_split(tweet, "\\s+")
                   words = unlist(word_list)
                   
                   positivematch = match(words, positive)
                   negativematch = match(words, negative)
                   
                   positivematch = !is.na(positivematch)
                   negativematch = !is.na(negativematch)
                   
                   # final score
                   score = sum(positivematch) - sum(negativematch)
                   return(score)
                   #print (score)
                 } , positive, negative, .progress=.progress )
  
  
}


#same as sentiment score but scores profanity
score.profanity = function(tweets, profanity, .progress='none')
{
  
  scores = laply(tweets,
                 function(tweet, profanity)
                 {
                   tweet = gsub("[[:punct:]]", "", tweet)
                   tweet = gsub("[[:cntrl:]]", "", tweet)
                   tweet = gsub('\\d+', '', tweet)
                   
                   lower2 = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   tweet = sapply(tweet, lower2)
                   
                   word_list = str_split(tweet, "\\s+")
                   words = unlist(word_list)
                   
                   profanity.matches = match(words, profanity)
                   
                   profanity.matches = !is.na(profanity.matches)
                   
                   
                   profanityscore = sum(profanity.matches)
                   return(profanityscore)
                 } , profanity, .progress=.progress )
  
}


process.tweet<-function(text){
  tweet = gsub("[[:punct:]]", "", text)
  tweet = gsub("[[:cntrl:]]", "", text)
  tweet = gsub('\\d+', '', text)
  
  lower2 = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  tweet = sapply(text, lower2)
  word_list = str_split(text, "\\s+")
  words = unlist(word_list)
  return (words)
  
}



###########word frequency functions##########################

finding.they<-function(text){
  numberofthey<-grep('(t|T)hey',text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofthey)/lengthoftweet) }

finding.gerund<-function(text){
  numberofing<-grep("ing$",text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofing)/lengthoftweet) }

finding.url<-function(text){
  urlpattern<-"http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  numberofurl<-grep(urlpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofurl)/lengthoftweet) }
 
 #unique/whole
lexicaldiversity<-function(text){
  lexicaldiversity<-length(unique(text))/length(text)
  return (lexicaldiversity)
}


finding.past<-function(text){
  pastpattern="ed$"
  numberofed<-grep(pastpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofed)/lengthoftweet) }



finding.i<-function(text){
  ipattern="^(i|I)$|^(i|I)'m"
  numberofi<-grep(ipattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofi)/lengthoftweet) }



finding.hashtag<-function(text){
  hashpattern="^#."
  numberofhash<-grep(hashpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofhash)/lengthoftweet) }

finding.mention<-function(text){
  atpattern="^@."
  numberofat<-grep(atpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofat)/lengthoftweet) }


finding.lol<-function(text){
  lolpattern="^lol"
  numberoflol<-grep(lolpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberoflol)/lengthoftweet) }

finding.rflol<-function(text){
  rflolpattern="^rflol$"
  numberofrflol<-grep(rflolpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofrflol)/lengthoftweet) }

finding.omg<-function(text)
{
  omgpattern="^omg"
  numberofomg<-grep(omgpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofomg)/lengthoftweet) }


finding.RT<-function(text)
{
  rtpattern="^RT"
  numberofRT<-grep(rtpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofRT)/lengthoftweet) }

finding.happyemoticons<-function(text)
{
  happyemoticonpattern=":)|:-)|=)|=-)|:D|:-D|=D|=-D"
  numberofemoticon<-grep(happyemoticonpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofemoticon)/lengthoftweet) }


finding.sademoticons<-function(text)
{
  sademoticonpattern=":\\(|:-\\(|=\\(|=-\\(|:c|:-c|=c|=-c"
  numberofemoticon<-grep(sademoticonpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofemoticon)/lengthoftweet) }

finding.sademoticonsor<-function(text)
{
  sademoticonpattern=":(|:-(|=(|=-(|:c|:-c|=c|=-c"
  numberofemoticon<-grep(sademoticonpattern,text)
  #tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofemoticon)/lengthoftweet) }





process.tweet2<-function(text){
  # remove punctuation
  #tweet = gsub("[[:punct:]]", "", text)
  # remove control characters
  tweet = gsub("[[:cntrl:]]", "", text)
  # remove digits
  tweet = gsub('\\d+', '', text)
  
  # define error handling function when trying tolower
  lower2 = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # use lower2 with sapply 
  tweet = sapply(text, lower2)
  
  # split tweet into words with str_split (stringr package)
  word_list = str_split(text, "\\s+")
  words = unlist(word_list)
  return (words)
  
}




finding.medication<-function(text)
{
  medicationpattern="^medication"
  numberofmedication<-grep(medicationpattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofmedication)/lengthoftweet) }

finding.effect<-function(text)
{
  effectpattern="^effect"
  numberofeffect<-grep(effectpattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofeffect)/lengthoftweet) }

finding.depression<-function(text)
{
  depressionpattern="^depression"
  numberofdepression<-grep(depressionpattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofdepression)/lengthoftweet) }

finding.side<-function(text)
{
  sidepattern="^side"
  numberofside<-grep(sidepattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofside)/lengthoftweet) }


finding.week<-function(text)
{
  weekpattern="^week"
  numberofweek<-grep(weekpattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberofweek)/lengthoftweet) }

finding.therapy<-function(text)
{
  pattern="therapy"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.suffer<-function(text)
{
  pattern="suffer"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.disorder<-function(text)
{
  pattern="disorder"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.doctor<-function(text)
{
  pattern="doctor"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.antidepressant<-function(text)
{
  pattern="antidepress(a|e)nt"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.experience<-function(text)
{
  pattern="experience"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }



finding.major<-function(text)
{
  pattern="major"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.mental<-function(text)
{
  pattern="mental"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.psychiatrist<-function(text)
{
  pattern="psychiatrist"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.tell<-function(text)
{
  pattern="tell"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.people<-function(text)
{
  pattern="people"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.know<-function(text)
{
  pattern="know"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.happy<-function(text)
{
  pattern="happy"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.talk<-function(text)
{
  pattern="talk"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.feel<-function(text)
{
  pattern="feel"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.want<-function(text)
{
  pattern="want"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.suppose<-function(text)
{
  pattern="suppose"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.read<-function(text)
{
  pattern="read"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.hurt<-function(text)
{
  pattern="hurt"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.wrong<-function(text)
{
  pattern="wrong"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.emotional<-function(text)
{
  pattern="emotional"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.mind<-function(text)
{
  pattern="mind"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.sad<-function(text)
{
  pattern="sad"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.make<-function(text)
{
  pattern="ma(k|d)e"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }




finding.hate<-function(text)
{
  pattern="hate"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }



finding.you<-function(text)
{
  pattern="^you$"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }

finding.your<-function(text)
{
  pattern="^your$"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.them<-function(text)
{
  pattern="^them$"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }


finding.me<-function(text)
{
  pattern="^me$"
  numberof<-grep(pattern,text)
  tweetwords<-str_match_all( text, "\\S+" )
  lengthoftweet<-length(text)
  return (length(numberof)/lengthoftweet) }





word.count<-function(text)
{
  
  lengthoftweet<-length(text)
  return (lengthoftweet) }

#highcharter function:Doesn't migrate well to Shiny
profanitycharter<-function(){
    hc<-highchart()
    hc <- hc_title(hc, text = "Profanity Score")
    hc<-hc_add_series_scatter(hc,seq(1:100), seq(1:100),lineWidth=2) 
    hc<-hc_add_theme(hc,hc_theme_gridlight()) 
    return (hc)
  
}

########MAIN FUNCTION######################

happyorsad<-function(x){
  out <- tryCatch({
								#This is the "try" part. It will try to run the happyorsad function
								user <- getUser(x)
									
								tweets <- userTimeline(user,110 )
								  
								 fol <- user$getFollowersCount() 
								 friends <- user$getFriendsCount() 
								  
								 follower.friend_mean<-fol/friends
								  
								  
								 tempdf<-data.frame()
								 l<-length(tweets)
								 texts <- vector(mode = "character", length = l)
								  
								 # Create an empty vector to store the tweet texts
								texts <- vector(mode = "character", length = l)
								 retweets<-vector(mode="numeric", length = l)
								 favorites<-vector(mode="numeric", length= l)
								 time<-vector(length=l)
								  
								# Extract the tweet text from each tweet status
								  for (i in 1:l) texts[i] <- tweets[[i]]$getText()
								  for (i in 1:l) texts[i] <- tweets[[i]]$getText()
								  for (i in 1:l) retweets[i] <- tweets[[i]]$getRetweetCount()
								  for (i in 1:l) favorites[i] <- tweets[[i]]$getFavoriteCount()
								  for (i in 1:l) time[i] <- as.Date(tweets[[i]]$getCreated(),format='%m/%d/%Y')
								  
								  timepassed2<-vector(mode="numeric", length=l)
								  t0<-time[1]
								  timepassed2<-t0-time
								  
								  #feature extraction from tweets file name texts a vector 
								  #initialize data frame
								  
								  I.freq<-vector(mode="numeric",length=l)
								  they.freq<-vector(mode="numeric",length=l)
								  gerund.freq<-vector(mode="numeric",length=l)
								  ed.freq<-vector(mode="numeric",length=l)
								  hashtag.freq<-vector(mode="numeric",length=l)
								  lol.freq<-vector(mode="numeric",length=l)
								  omg.freq<-vector(mode="numeric",length=l)
								  rflol.freq<-vector(mode="numeric",length=l)
								  url.freq<-vector(mode="numeric",length=l)
								  lex<-vector(mode="numeric",length=l)
								  scores<-vector(mode="numeric",length=l)
								  mention.freq<-vector(mode="numeric",length=l)
								  RT.freq<-vector(mode="numeric",length=l)
								  happyemot.freq<-vector(mode="numeric",length=l)
								  sademot.freq<-vector(mode="numeric",length=l)
								  antidepressant.freq<-vector(mode="numeric",length=l)
								  depression.freq<-vector(mode="numeric",length=l)
								  disorder.freq<-vector(mode="numeric",length=l)
								  doctor.freq<-vector(mode="numeric",length=l)
								  effect.freq<-vector(mode="numeric",length=l)
								  emotional.freq<-vector(mode="numeric",length=l)
								  experience.freq<-vector(mode="numeric",length=l)
								  feel.freq<-vector(mode="numeric",length=l)
								  hate.freq<-vector(mode="numeric",length=l)
								  hurt.freq<-vector(mode="numeric",length=l)
								  know.freq<-vector(mode="numeric",length=l)
								  major.freq<-vector(mode="numeric",length=l)
								  make.freq<-vector(mode="numeric",length=l)
								  medication.freq<-vector(mode="numeric",length=l)
								  mental.freq<-vector(mode="numeric",length=l)
								  you.freq<-vector(mode="numeric",length=l)
								  your.freq<-vector(mode="numeric",length=l)
								  them.freq<-vector(mode="numeric",length=l)
								  me.freq<-vector(mode="numeric",length=l)
								  wordcount<-vector(mode="numeric",length=l)
								  profanityscores<-vector(mode="numeric",length=l)
								  
								  
								  for (i in 1:l) I.freq[i] <- finding.i(process.tweet(texts[i]))
								  for (i in 1:l) they.freq[i] <- finding.they(process.tweet(texts[i]))
								  for (i in 1:l) gerund.freq[i] <- finding.gerund(process.tweet(texts[i]))
								  for (i in 1:l) ed.freq[i] <- finding.past(process.tweet(texts[i]))
								  for (i in 1:l) hashtag.freq[i] <- finding.hashtag(process.tweet(texts[i]))
								  for (i in 1:l) lol.freq[i] <- finding.lol(process.tweet(texts[i]))
								  for (i in 1:l) omg.freq[i] <- finding.omg(process.tweet(texts[i]))
								  for (i in 1:l) rflol.freq[i] <- finding.rflol(process.tweet(texts[i]))
								  for (i in 1:l) url.freq[i] <- finding.url(process.tweet(texts[i]))
								  for (i in 1:l) lex[i] <- lexicaldiversity(process.tweet(texts[i]))
								  for (i in 1:l) scores[i] <- score.sentiment(texts[i], pos, neg, .progress='none')
								  for (i in 1:l) mention.freq[i] <- finding.mention(process.tweet(texts[i]))
								  for (i in 1:l) RT.freq[i] <- finding.RT(process.tweet(texts[i]))
								  for (i in 1:l) happyemot.freq[i] <- finding.happyemoticons(process.tweet2(texts[i]))
								  for (i in 1:l) sademot.freq[i] <- finding.sademoticons(process.tweet2(texts[i]))
								  for (i in 1:l) antidepressant.freq[i] <- finding.antidepressant(process.tweet(texts[i]))
								  for (i in 1:l) depression.freq[i] <- finding.depression(process.tweet(texts[i]))
								  for (i in 1:l) disorder.freq[i] <- finding.disorder(process.tweet(texts[i]))
								  for (i in 1:l) doctor.freq[i] <- finding.doctor(process.tweet(texts[i]))
								  for (i in 1:l) effect.freq[i] <- finding.effect(process.tweet(texts[i]))
								  for (i in 1:l) emotional.freq[i] <- finding.emotional(process.tweet(texts[i]))
								  for (i in 1:l) experience.freq[i] <- finding.experience(process.tweet(texts[i]))
								  for (i in 1:l) feel.freq[i] <- finding.feel(process.tweet(texts[i]))
								  for (i in 1:l) hate.freq[i] <- finding.hate(process.tweet(texts[i]))
								  for (i in 1:l) hurt.freq[i] <- finding.hurt(process.tweet(texts[i]))
								  for (i in 1:l) know.freq[i] <- finding.know(process.tweet(texts[i]))
								  for (i in 1:l) major.freq[i] <- finding.major(process.tweet(texts[i]))
								  for (i in 1:l) make.freq[i] <- finding.make(process.tweet(texts[i]))
								  for (i in 1:l) medication.freq[i] <- finding.medication(process.tweet(texts[i]))
								  for (i in 1:l) mental.freq[i] <- finding.mental(process.tweet(texts[i]))
								  for (i in 1:l) you.freq[i] <- finding.you(process.tweet(texts[i]))
								  for (i in 1:l) your.freq[i] <- finding.your(process.tweet(texts[i]))
								  for (i in 1:l) them.freq[i] <- finding.them(process.tweet(texts[i]))
								  for (i in 1:l) me.freq[i] <- finding.me(process.tweet(texts[i]))
								  for (i in 1:l) wordcount[i] <- word.count(process.tweet(texts[i]))
								  for (i in 1:l) profanityscores[i] <- score.profanity(texts[i], profanity, .progress='none')
								  
								  names<-c("favorite.count","retweencount","sentimentscore","I.freq","they.freq","gerund.freq","ed.freq","hashtag.freq","lol.freq","omg.freq","rflol.freq","url.freq","lexicaldiversity","mention.freq",
										   "RT.freq", "happyemot.freq","antidepressant.freq", "depression.freq", "disorder.freq", "doctor.freq", "effect.freq", "emotional.freq",
										   "experience.freq","feel.freq","hate.freq", "hurt.freq", "know.freq", "major.freq","make.freq","medication.freq", "mental.freq",
										   "you.freq","your.freq","them.freq","me.freq","word.count","timepassed2","profanity")          
								  
								  
								  df<-data.frame(favorites,retweets,scores,I.freq,they.freq,gerund.freq,ed.freq,hashtag.freq,lol.freq,omg.freq,rflol.freq,url.freq,lex,mental.freq,RT.freq,happyemot.freq,antidepressant.freq,
												 depression.freq,disorder.freq,doctor.freq,effect.freq,emotional.freq,experience.freq,feel.freq,hate.freq,hurt.freq,know.freq,
												 major.freq,make.freq,medication.freq,mental.freq,your.freq,your.freq,them.freq,me.freq,wordcount,timepassed2,profanityscores)
								  
								  colnames(df)<-names
								  
								  #aggragage
								  
								  dftest<-df %>% summarise_each(funs(mean,var))
								  ifelse(friends==0,friends<-1,friends<-friends)
								  dftest$follower.friend_mean<-as.numeric(fol/friends)
								  dftest$class_var<-0
								  
								  #loading the model
								  
								  load("rf_fit.rda")
								  
								  model_predictions2 <- predict(rf_fit, dftest, type = "prob" )
								  return(model_predictions2[1,1])
								}, warning = function(war){
									#This is the warning part. It will not return anything if there is a warning.
								}, error = function(err){
									#This is the error part. If try fails, this part will catch the two errors and return the appropriate flag.
									if(err == "Forbidden (HTTP 403)."){return("err1")}
									else{return("err2")}
								}, finally = {
								})
	return(out)
}

plotdatamaker<-function(x){
  
  #This is the "try" part. It will try to run the happyorsad function
  user <- getUser(x)
  
  tweets <- userTimeline(user,110 )
  tempdf<-data.frame()
  l<-length(tweets)
  #return (l)
  texts <- vector(mode = "character", length = l)
  
  # Create an empty vector to store the tweet texts
  texts <- vector(mode = "character", length = l)
  
  
  # Extract the tweet text from each tweet status
  for (i in 1:l) texts[i] <- tweets[[i]]$getText()
  
  
  #feature extraction from tweets file name texts a vector 
  #initialize data frame
  
  
  lex<-vector(mode="numeric",length=l)
  scores<-vector(mode="numeric",length=l)
  profanityscores<-vector(mode="numeric",length=l)
  
  
  for (i in 1:l) lex[i] <- lexicaldiversity(process.tweet(texts[i]))
  for (i in 1:l) scores[i] <- score.sentiment(texts[i], pos, neg, .progress='none')
  for (i in 1:l) profanityscores[i] <- score.profanity(texts[i], profanity, .progress='none')
  #return(lex)
  #return(scores)
  #return(profanityscores)
  axis1<-seq(1:l)
  axis2<-profanityscores
  axis3<-lex
  axis4<-scores
  plotdata<-data.frame(axis1,axis2,axis3,axis4)
  colnames(plotdata)<-c("tweets","profanity scores","lexical diversity","polarity scores")
  h1<-ggplot(data=plotdata, aes(x=plotdata$tweets, y=plotdata$`profanity scores`)) +
    geom_line(size=1, color="red") +
    geom_point(size=1, color="red",fill="red") +
    expand_limits(y=0.8)+
    ggtitle("Pottymouth Score")+
    theme_bw() + 
    theme (axis.text = element_text(size = 20), panel.background = element_rect(fill = "transparent"),axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20), plot.title=element_text(size=20))+
    labs(
      x = "Tweets (from recent to past)",
      y = "Score")
  
  return(h1)
  #return(h2)
  #return(h3)
  }

plotdatamaker2<-function(x){
  
  #This is the "try" part. It will try to run the happyorsad function
  user <- getUser(x)
  
  tweets <- userTimeline(user,110 )
  tempdf<-data.frame()
  l<-length(tweets)
  #return (l)
  texts <- vector(mode = "character", length = l)
  
  # Create an empty vector to store the tweet texts
  texts <- vector(mode = "character", length = l)
  
  
  # Extract the tweet text from each tweet status
  for (i in 1:l) texts[i] <- tweets[[i]]$getText()
  
  
  #feature extraction from tweets file name texts a vector 
  #initialize data frame
  
  
  lex<-vector(mode="numeric",length=l)
  scores<-vector(mode="numeric",length=l)
  profanityscores<-vector(mode="numeric",length=l)
  
  
  for (i in 1:l) lex[i] <- lexicaldiversity(process.tweet(texts[i]))
  for (i in 1:l) scores[i] <- score.sentiment(texts[i], pos, neg, .progress='none')
  for (i in 1:l) profanityscores[i] <- score.profanity(texts[i], profanity, .progress='none')
  #return(lex)
  #return(scores)
  #return(profanityscores)
  axis1<-seq(1:l)
  axis2<-profanityscores
  axis3<-lex
  axis4<-scores
  plotdata<-data.frame(axis1,axis2,axis3,axis4)
 
  colnames(plotdata)<-c("tweets","profanity scores","lexical diversity","polarity scores")
  
  h2<-ggplot(data=plotdata, aes(x=plotdata$tweets, y=plotdata$`polarity scores`)) +
    geom_line(size=1, color="red") +
    geom_point(size=1, color="red",fill="red") +
    expand_limits(y=0.8)+
    ggtitle("Opinion Score")+
    theme_bw() + 
    theme (axis.text = element_text(size = 20), panel.background = element_rect(fill = "transparent"),axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20), plot.title=element_text(size=20))+
    labs(
      x = "Tweets (from recent to past)",
      y = "Score")
 # h1 <- hPlot(x = "tweets", y = "profanity scores", data = plotdata, title="Pottymouth Score", 
              #type = c("line"))
  #h2 <- hPlot(x = "tweets", y = "polarity scores", data = plotdata, title="Attitude Score", 
              #type = c("line"))
  #h3 <- hPlot(x = "tweets", y = "lexical diversity", data = plotdata, title="Wordsmith Score", 
              #type = c("line"))
  
  #return(h1)
  return(h2)
  #return(h3)
}
plotdatamaker3<-function(x){
  
  #This is the "try" part. It will try to run the happyorsad function
  user <- getUser(x)
  
  tweets <- userTimeline(user,110 )
  tempdf<-data.frame()
  l<-length(tweets)
  #return (l)
  texts <- vector(mode = "character", length = l)
  
  # Create an empty vector to store the tweet texts
  texts <- vector(mode = "character", length = l)
  
  
  # Extract the tweet text from each tweet status
  for (i in 1:l) texts[i] <- tweets[[i]]$getText()
  
  
  #feature extraction from tweets file name texts a vector 
  #initialize data frame
  
  
  lex<-vector(mode="numeric",length=l)
  scores<-vector(mode="numeric",length=l)
  profanityscores<-vector(mode="numeric",length=l)
  
  
  for (i in 1:l) lex[i] <- lexicaldiversity(process.tweet(texts[i]))
  for (i in 1:l) scores[i] <- score.sentiment(texts[i], pos, neg, .progress='none')
  for (i in 1:l) profanityscores[i] <- score.profanity(texts[i], profanity, .progress='none')
  #return(lex)
  #return(scores)
  #return(profanityscores)
  axis1<-seq(1:l)
  axis2<-profanityscores
  axis3<-lex
  axis4<-scores
  plotdata<-data.frame(axis1,axis2,axis3,axis4)
  colnames(plotdata)<-c("tweets","profanity scores","lexical diversity","polarity scores")
  
  h3<-ggplot(data=plotdata, aes(x=plotdata$tweets, y=plotdata$`lexical diversity`)) +
    geom_line(size=1, color="red") +
    geom_point(size=1, color="red",fill="red") +
    expand_limits(y=0.8)+
    ggtitle("Wordsmith Score")+
    theme_bw() + 
    theme (axis.text = element_text(size = 20), panel.background = element_rect(fill = "transparent"),axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20), plot.title=element_text(size=20))+
    labs(
    x = "Tweets (from recent to past)",
    y = "Score")
  

  #return(h1)
  #return(h2)
  return(h3)
}

