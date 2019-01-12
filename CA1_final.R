# CA1 - JL
# Goal = Find and test page recommendations using association and sequence finding

getwd()
setwd("E:/OneDrive/NUS/2018 Semester 2/EB5202 Web Analytics/CA1")

# install.packages("dplyr")
# library(dplyr)

### Sequencing Mining ###
install.packages("arulesSequences")
library(arulesSequences)

vrootnames = read.table("anonymous-vrootnames-msweb.csv" , sep = "," , header = T)
alldata = read.table(file = "anonymous-msweb.data", skip = 7, sep = ","
                  , check.names = T, fill = T,
                  col.names=c("line_type", "vroot", "ignore", "title","url"))

### Data Pre-Processing for Sequencing Mining ###

# remove "A" line type and left only c and V
filteredData = alldata[alldata[,'line_type'] != "A",]
# Extract required columns
filteredData = filteredData[c("line_type", "vroot")]

sequenceData = data.frame(userid = character(), eventSeq = numeric(), size = numeric(), vroot = character(), stringsAsFactors=FALSE)
#sequenceData = data.frame( userid = character(), stringsAsFactors=FALSE)

lastKnownUser = ""
lastKnownSeq = 0

### Loop below will take a long time, around 30 mins, depends on your computing power ###
for (i in 1:nrow(filteredData)){
  linetype = as.character(filteredData[i,"line_type"])
  attr_id = as.character(filteredData[i,"vroot"])
  
  print (i)
  if (linetype == "C"){
    lastKnownSeq=lastKnownSeq+1
    lastKnownUser = attr_id
    #set to 0 when new user id is detected
    lastKnownEventSeq = 0  
    sequenceData = rbind(sequenceData, data.frame(userid=lastKnownUser, eventseq = 0 , size = 0,  vroot = "0" ))
    
  }
  else{
    if (linetype == "V"){
      lastKnownEventSeq = lastKnownEventSeq + 1
      sequenceData = rbind(sequenceData, data.frame(userid=lastKnownUser, eventseq = lastKnownEventSeq , size = 1, vroot = attr_id))
    }
    
    #Comment away below to get full list, else stop at 50 to check data
    #if (i == 50){
     # break
    #}
    
  }
}

#remove rows with 0 - Because no need attributo for 'C' values
sequenceDataClean = sequenceData[sequenceData[,'vroot'] != "0",]

#look up the vroot
sequenceDataWithVroot = merge(sequenceDataClean,vrootnames, all.x = T)

# create new col with title and URL
sequenceDataWithVroot$pageInfo = (paste(sequenceDataWithVroot$title, sequenceDataWithVroot$url, sep = "|"))

# Get required columns
sequenceDataWithVroot = sequenceDataWithVroot[,c(2,3,4,7)]

# Sort for arules
sequenceDataWithVroot = sequenceDataWithVroot[with(sequenceDataWithVroot,order(userid,eventseq)),]

# Write to file for read_basket function
write.table(sequenceDataWithVroot, "sequenceDataWithVroot.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)

### Pre processing Ends , use the .txt file for mining tasks ###

### R studio bug - unable to write to tmpdir, use terminal to run codes below onwards ###
### Set working directory again ###
setwd("E:/OneDrive/NUS/2018 Semester 2/EB5202 Web Analytics/CA1")

# Create transactional data type
transactionalData = read_baskets(con = "sequenceDataWithVroot.txt", sep = "," , info = c("sequenceID","eventID","SIZE"))
head(transactionalData) # view first few rows of the data
summary(transactionalData)
#as(head(transactionalData), "data.frame") # view first few rows of the data

#takes about 3 mins to run below
as(transactionalData, "data.frame")

# get support bearing - insufficient memory to run - do not run this
seqs0 = cspade(transactionalData, parameter = list(support = 0), control = list(verbose = TRUE, summary = TRUE, memsize = 2048))
as(seqs0,"data.frame")  # view the sequences

# Mine frequent sequences
seqs = cspade(transactionalData, parameter = list(support = 0.1), control = list(verbose = TRUE, summary = TRUE, tidLists = TRUE))
summary(seqs)
as(seqs,"data.frame")  # view the sequences

# Mine frequent sequences with Breadth First Search - no difference. 
seqsBFS = cspade(transactionalData, parameter = list(support = 0.1), control = list(verbose = TRUE, summary = TRUE, tidLists = TRUE, bfstype = TRUE))
summary(seqsBFS)
as(seqsBFS,"data.frame")  # view the sequences


rules = ruleInduction(seqs, confidence = 0.1,control = list(verbose = TRUE))
as(rules,"data.frame")  # view the rules

### Association Mining ###
install.packages("arules")
install.packages("caret")
library("arules")
library("caret")

#######################################################################
# the supporting functions
#######################################################################

#remove duplicate items from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
}

# execute ruleset using item as rule antecedent (handles single item antecedents only)
makepreds <- function(item, rulesDF) {
  antecedent = paste("{",item,"} =>",sep="") 
  firingrules = rulesDF[grep(antecedent, rulesDF$rules,fixed=TRUE),1]
  gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}

# count how many predictions are in the basket of items already seen by that user 
# Caution : refers to "baskets" as a global
checkpreds <- function(preds, baskID) {
  plist = preds[[1]]
  blist = baskets[baskets$basketID == baskID,"items"][[1]]
  cnt = 0 
  for (p in plist) {
    if (p %in% blist) cnt = cnt+1
  }
  cnt
}

# count all predictions made
countpreds <- function(predlist) {
  len = length(predlist)
  if (len > 0 && (predlist[[1]] == "")) 0 # avoid counting an empty list
  else len
}

# split the data into train(80%) and test(20%) set 
# ensure the process is reproducible
set.seed(1234)

# Read the full dataset
fullDataSet = read.delim(file="sequenceDataWithVroot.txt", header = F, sep = ",")
#fullDataSet = read.table(file="sequenceDataWithVroot.txt", sep="," , header = F)
colnames(fullDataSet) = c("userid","eventid","size","vroot")
fullDataSet = fullDataSet[,c("userid","vroot")]

trainIndex = createDataPartition(fullDataSet$userid, p = .8,list = FALSE,times = 1)
head(trainIndex)

basketTrain = fullDataSet[ trainIndex,]
#basketTrainclean = basketTrain[,c("userid","vroot")]
write.table(basketTrain, "basketTrain.txt", sep=",", row.names = FALSE, col.names = TRUE, quote = FALSE)

basketTest = fullDataSet[-trainIndex,]
write.table(basketTest, file = "basketTest.csv",row.names=FALSE, na="",col.names=T, sep="," , quote = FALSE)

#build rules
trainegs = read.transactions(file="basketTrain.txt",rm.duplicates=TRUE, format="single", sep=",", cols=c("userid","vroot"))
rules <- apriori(trainegs, parameter = list(supp=0.04, conf=0.1, minlen=2))
summary(rules)
inspect(rules)

# a useful plot of training data
itemFrequencyPlot(trainegs,topN=15,type="absolute")

#read the test data
testegs = read.csv(file="basketTest.csv");
colnames(testegs) <- c("basketID","items")

#execute rules against test data
rulesDF = as(rules,"data.frame")
testegs$preds = apply(testegs,1,function(X) makepreds(X["items"], rulesDF))

# extract unique predictions for each test user
userpreds = as.data.frame(aggregate(preds ~ basketID, data = testegs, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(items ~ basketID, data = testegs, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["items"]))

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["basketID"])))

# count total number of unique predictions made
totalpreds = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

precision = correctpreds*100/totalpreds

cat("precision=", precision, "correct prediction=",correctpreds,"total prediction=",totalpreds)



###########################################################################
# rule visualisation
# also see (for example) http://www.rdatamining.com/examples/association-rules
###########################################################################
install.packages("arulesViz")
library("arulesViz")
plot(rules)
plot(rules, method="graph")
plot(rules, method="graph",nodeCol=grey.colors(10),edgeCol=grey(.7),alpha=1)
plot(rules, method="matrix")
plot(rules, method="paracoord", control=list(reorder=TRUE))
