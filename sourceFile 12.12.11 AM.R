#Install Required Packages
needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc", "slam", "plyr", "doMC", "stringr", "rpart", "randomForest")
#install.packages(needed, dependencies=TRUE) #This only needs to be done once
list = lapply(needed, require, character.only = TRUE) #This needs to be done every time you restart R

#Import the data
#The data will only load if the text file containing the dataset is in the current working directory 
#and named 'dataset.txt'
#Use the setwd() command to set the working directory
setwd("~/R/Wells Fargo Competition")

#Create a Data Frame from the text file
df = read.table('dataset.txt',sep="|",header=T)

#Define Functions for text cleanup
#This function removes the non-ASCII characters from the text so that it can be analyzed 
removeOffendingCharacters = function(df)
{
  df.texts.clean = as.data.frame(iconv(df$FullText, "latin1", "ASCII", sub=""))
  colnames(df.texts.clean) = 'FullText'
  df$FullText = df.texts.clean$FullText
  return(df)
}

#This function performs several operations to clean up the text, including:
#Removing extra whitespace
#Removing any punctuation with the exception of '_'
#Removing numbers
#Converting all text to lowercase
#Removing common and meaningless words (stopwords)
cleanText = function(df)
{
  require(tm)
  docs = Corpus(VectorSource(df$FullText))
  docs = tm_map(docs, stripWhitespace)
  print("   Removing stopwords...")
  docs = tm_map(docs, removeWords, stopwords("english"))  
  print("   Removing numbers...")
  docs = tm_map(docs, removeNumbers)   
  print("   Converting to lowercase...")
  docs = tm_map(docs, tolower) 
  docs = tm_map(docs, PlainTextDocument)
  removeSomePunct = function(x) gsub("[^[:alnum:][:blank:]_]", "", x)
  print("   Removing punctuation...")
  docs = tm_map(docs, content_transformer(removeSomePunct))
  docs = tm_map(docs, stripWhitespace)  
  docs = tm_map(docs, PlainTextDocument)
  
  df.texts.cleaner = data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F)
  colnames(df.texts.cleaner) = 'FullText'
  df$FullTextClean = df.texts.cleaner$FullText
  return(df)
}


#This function "stemms" the document, meaning that it removes word endings like "ing", "ed", and "es"
#For example, it will change the words "analyze", "analyzing", and "analyzed" to "analyz" 
#This makes it so that they are all treated as the same word
stemText = function(df)
{
  require(tm)
  require(SnowballC)
  docs = Corpus(VectorSource(df$FullTextClean))
  docs = tm_map(docs, stemDocument)
  docs = tm_map(docs, PlainTextDocument)
  df.texts.cleaner = data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F)
  colnames(df.texts.cleaner) = 'FullText'
  df$FullTextStemmed = df.texts.cleaner$FullText
  return(df)
}


#This function takes stemmed words and completes them into dictionary words.
#It is an extremely slow function, so we do not call it in this analysis.
#However, you may choose to call it for your own analysis.
#If you do this, we reccomend that you do so on a computer with multiple threads so that the function can be done in parallel.
stemCompleteText = function(df)
{
  require(plyr)
  require(doMC)
  require(tm)
  doMC::registerDoMC(cores=3) #Change the number of threads here to match your machine.
  docs = Corpus(VectorSource(df$FullTextStemmed))
  dictionary = Corpus(VectorSource(df$FullText))
  
  stemCompletion2 <- function(y, dictionary) 
  {
    y = unlist(strsplit(as.character(y), " "))
    y = y[y != ""]
    y = stemCompletion(y, dictionary=dictionary)
    y = paste(y, sep="", collapse=" ")
    stripWhitespace(y)
  }
  
  completedText = llply(docs, stemCompletion2, dictionary=dictionary, .parallel = TRUE)
  df$FullTextCompleted = unlist(completedText)
  return(df)
}


#This function analyzes the dataset to identify which banks are mentioned in each post.
#It creates 5 new variables (for banks A-E)
#The field has a 0 if the bank is not mentioned, and a 1 if the bank is mentioned.
#It also creates a field that counts the number of banks mentioned in the post.
#Some posts mention 0 banks, most mention 1, and a few mention multiple
bankMentions = function(df)
{
  df$BankA = 0
  df$BankB = 0
  df$BankC = 0
  df$BankD = 0
  df$BankE = 0
  df$NumBanks = 0
  for( i in 1:nrow(df))
  {
    if (grepl("BankA",df$FullText[i]))  df$BankA[i] = 1
    if (grepl("BankB",df$FullText[i]))  df$BankB[i] = 1
    if (grepl("BankC",df$FullText[i]))  df$BankC[i] = 1
    if (grepl("BankD",df$FullText[i]))  df$BankD[i] = 1
    if (grepl("banke",df$FullText[i]))  df$BankE[i] = 1
    df$NumBanks[i] = df$BankA[i] + df$BankB[i] + df$BankC[i] + df$BankD[i] + df$BankE[i]
  }
  
  return(df)
}

#This function counts the number of positive words and negative words in a post
#Positive and Negative words are imported from text files named positive-words.txt and negative-words.txt
#The difference is the sentiment score
#The difference divided by the number of words in the FullTextClean column is the sentiment density
#This normalizes sentiment scores between very short posts (tweets) and very long posts (on facebook)
sentimentAnalysis = function(df)
{
  pos <- scan('positive-words.txt',what='character',comment.char=';')
  neg <- scan('negative-words.txt',what='character',comment.char=';')
  require(plyr)
  require(stringr)
  
  scores = laply(df$FullTextClean, function(sentence, pos.words, neg.words) {
    sentence = as.character(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    words.length = length(words)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    if(words.length == 0) score.density = 0
    else score.density = score/words.length
    score.list = c(score, score.density)
    
    return(score.list)
  }, pos, neg, .progress = 'text')
  
  scores.df = data.frame(SentimentScore=scores[,1], SentimentDensity=scores[,2])
  df$SentimentScore = scores[,1]
  df$SentimentDensity = scores[,2]
  df$very.pos = as.numeric(df$SentimentScore >= 2)
  df$very.neg = as.numeric(df$SentimentDensity <= -2)
  return(df)
}


#This function combines all the previous functions to create a large data frame with all the fields
#If you want to use stemCompletion, make stemComplete = TRUE in the function call
createDataFrame = function(df, stemComplete = FALSE)
{
  print("Removing offending characters...")
  df = removeOffendingCharacters(df)
  print("Cleaning text...")
  df = cleanText(df)
  print("Stemming text...")
  df = stemText(df)
  if(stemComplete == TRUE)
  {
    print("StemCompleting text...") 
    df = stemCompleteText(df)
  }
  print("Analyzing bank mentions...")
  df = bankMentions(df)
  print("Computing sentiment scores...")
  df = sentimentAnalysis(df)
  print("Done.")
  return(df)
}

#Begin Analysis
#Add fields to the data frame so that it can be analyzed
df = createDataFrame(df)

#Count the number of times each bank is mentioned
bankAMentions = sum(df$BankA)
bankBMentions = sum(df$BankB)
bankCMentions = sum(df$BankC)
bankDMentions = sum(df$BankD)
bankEMentions = sum(df$BankE)

#Plot the number of mentions of each bank in the dataset
Bank = c("BankA", "BankB", "BankC", "BankD", "BankE")
Mentions = c(bankAMentions, bankBMentions, bankCMentions, bankDMentions, bankEMentions)
bankMentions = data.frame(Bank, Mentions)
p = ggplot(bankMentions, aes(Bank, Mentions))
p = p + geom_bar(stat="identity")   
p = p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

#Create a Document Term Matrix to analyze the texts
docs = Corpus(VectorSource(df$FullTextClean))
dtm = DocumentTermMatrix(docs)   
dtm.sparse = dtms <- removeSparseTerms(dtm, 0.999)
dtm.sparse.simple = as.simple_triplet_matrix(dtm.sparse) 

#Compute the word frequencies
freq <- sort(col_sums(dtm.sparse.simple), decreasing=TRUE)   
#Remove the 9 most frequently occuring words because they are outliers
freq = freq[-c(1:9)]
wf <- data.frame(word=names(freq), freq=freq)  

#Make a bar graph of the words appearing more than 6000 times
library(ggplot2)   
p = ggplot(subset(wf, freq>6000), aes(word, freq))    
p = p + geom_bar(stat="identity")   
p = p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

#Create a word cloud of the 60 most frequently occuring words
library(wordcloud)   
set.seed(145)   
wordcloud(names(freq), freq, max.words = 60, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#Find words associated with individual banks
assocs = findAssocs(dtm.sparse, c("banka", "bankb", "bankc", "bankd", "banke"), corlimit=0.03)
assocsa = data.frame(BankA = assocs$banka[1:15])
assocsb = data.frame(BankB = assocs$bankb[1:15])
assocsc = data.frame(BankC = assocs$bankc[1:15])
assocsd = data.frame(BankD = assocs$bankd[1:15])
assocse = data.frame(BankE = assocs$banke[1:15])


#Plot a cluster dendogram
dtms3 <- removeSparseTerms(dtm, 0.975)
library(cluster)   
d <- dist(t(dtms3), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit   
plot(fit, hang=-1) 
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=15)
rect.hclust(fit, k=15, border="red") 

#Categorize the data
#Import training data
train = read.csv("train.csv")
train = createDataFrame(train)
docs.train = Corpus(VectorSource(train$FullTextClean))
dtm.train = DocumentTermMatrix(docs.train) 
dtm.sparse.train = removeSparseTerms(dtm.train, 0.99)
terms = as.data.frame(as.matrix(dtm.sparse.train))
terms$MediaType = train$MediaType
terms$very.pos = train$very.pos
terms$very.neg = train$very.neg
terms$Topic = train$Topic

#Split dataset for cross validation
terms.train = droplevels(terms[c(1:399),])
terms.test = droplevels(terms[c(400:nrow(terms)),])
classifier = randomForest(Topic ~ ., data = terms.train)
prediction = predict(classifier, terms.test)
terms.test$Prediction = prediction
terms.test$Accuracy = as.numeric(terms.test$Topic == terms.test$Prediction)
accuracy = sum(terms.test$Accuracy)/nrow(terms.test)
#Accuracy comes out to about 77% accuracy, higher than CART or naive bayes

#Create test dataset
test.terms = as.data.frame(as.matrix(dtm.sparse))
test.terms$MediaType = df$MediaType
test.terms$very.pos = df$very.pos
test.terms$very.neg = df$very.neg

#Build CART Model
classifier = randomForest(Topic ~ ., data = terms)

#Predict topic for the dataset
prediction = predict(classifier, test.terms)
df$Topic = prediction

#Number of each topic
pr = length(which(df$Topic == "PR"))
cs = length(which(df$Topic == "CS"))
ns = length(which(df$Topic == "NS"))
s = length(which(df$Topic == "S"))


#Plot Topic vs Sentiment Density
# colors
cols = c("#DF0101", "#7CAE00", "#0080FF", "#CC2EFA")
names(cols) = c("CS", "NS", "S", "PR")

# boxplot
library(ggplot2)
ggplot(df, aes(x=Topic, y=SentimentDensity, group=Topic)) +
  geom_boxplot(aes(fill=Topic)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",position=position_jitter(width=0.2), alpha=0.3) +
  labs(title = "Sentiment Density Scores by Topic") + 
  xlab('Topic') + ylab('Sentiment Density')

#barplot
meanscore = tapply(df$SentimentDensity, df$Topic, mean)
df.plot = data.frame(Topic=names(meanscore), meanscore=meanscore)
df.plot$Topics <- reorder(df.plot$Topic, df.plot$meanscore)

ggplot(df.plot, aes(x = factor(Topics), y = meanscore, fill=Topics)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=cols[order(df.plot$meanscore)]) +
  labs(title = "Average Sentiment Density Score") + 
  xlab('Topic') + ylab('Average Density Score')


#Topic Sentiment Score by Bank
bankAindices = which(df$BankA ==1)
df.BankA = df[bankAindices,]
meanscore.BankA = tapply(df.BankA$SentimentDensity, df.BankA$Topic, mean)
df.plot.A = data.frame(Bank = c("BankA", "BankA", "BankA", "BankA"), Topic=names(meanscore.BankA), meanscore=meanscore.BankA)

bankBindices = which(df$BankB ==1)
df.BankB = df[bankBindices,]
meanscore.BankB = tapply(df.BankB$SentimentDensity, df.BankB$Topic, mean)
df.plot.B = data.frame(Bank = c("BankB", "BankB", "BankB","BankB"), Topic=names(meanscore.BankB), meanscore=meanscore.BankB)


bankCindices = which(df$BankC ==1)
df.BankC = df[bankCindices,]
meanscore.BankC = tapply(df.BankC$SentimentDensity, df.BankC$Topic, mean)
df.plot.C = data.frame(Bank = c("BankC", "BankC", "BankC","BankC"), Topic=names(meanscore.BankC), meanscore=meanscore.BankC)


bankDindices = which(df$BankD ==1)
df.BankD = df[bankDindices,]
meanscore.BankD = tapply(df.BankD$SentimentDensity, df.BankD$Topic, mean)
df.plot.D = data.frame(Bank = c("BankD", "BankD", "BankD", "BankD"), Topic=names(meanscore.BankD), meanscore=meanscore.BankD)

#Grouped bar plot of scores by topic and bank
df.plot = rbind(df.plot.A, df.plot.B, df.plot.C, df.plot.D)
ggplot(df.plot, aes(factor(Topic), meanscore, fill = Bank)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + 
  labs(title = "Average Sentiment Density Score by Topic and Bank") +
  ylab('Average Density Score')
