#Innoplex AV

library(tidyverse)
library(DataExplorer)
library(xgboost)
library(party)
library(randomForest)
library(neuralnet)
library(data.table)
library(stringr)

train <- read_csv("C:/Users/Patrick Jane/Google Drive/Kaggle/Innoplex AV/train_innoplexus/train.csv")
test <- read_csv("C:/Users/Patrick Jane/Google Drive/Kaggle/Innoplex AV/test_innoplexus/test.csv")
sample <- read_csv("C:/Users/Patrick Jane/Google Drive/Kaggle/Innoplex AV/sample_submission_innoplexus/sample_submission.csv")

dim(train)
dim(test)
dim(sample)

train%>%
  filter(!tag == "O")

create_report(train)
head(train)

train%>%
  count(flag)

dplyr::filter(train, !grepl('O', tag))

test$tag <- NA
test$flag <- 0
total = rbind(train,test)

train <- train%>%
  mutate(flag = if_else(tag == "O" , 0, if_else(tag == "B-indications", 1, 2)))

train%>%
  distinct(Word)

test%>%
  distinct(Word)

test%>%
  filter(Word %in% train$Word[train$flag == 1])

train%>%
  filter(Word == ",")%>%
  count(flag)

train <- train%>%
  filter(!flag == 0)%>%
  mutate(flag_rle = rleid(flag))

train_diseases_swise <- train%>%
  filter(!flag == 0)%>%
  mutate(flag_rle = rleid(flag))%>%
  select(Sent_ID, Word)%>%
  group_by(Sent_ID)%>%
  nest()%>%
  mutate(words = map(data, unlist),
         words = map_chr(words, paste, collapse = " "))

train%>%
  filter(!flag == 0)%>%
  mutate(flag_rle = rleid(flag))%>%
  print(n=40)

test_words_swise <- test%>%
  select(Sent_ID, Word)%>%
  group_by(Sent_ID)%>%
  nest()%>%
  mutate(words = map(data, unlist),
         words = map_chr(words, paste, collapse = " "))



test_words_swise$Sent_ID[filter(test_words_swise, grep(diseases$disease[1], words))]


test_words_swise$Sent_ID[grep(diseases$disease[1], test_words_swise$words)]





#Getting all Sentence IDs with diseases

listvector <- NA

for (i in 2) {
  ttt = test_words_swise$Sent_ID[grep(diseases$disease[i], test_words_swise$words)]
  
  
}

listvector

listdf <- as.data.frame(listvector)
dim(listvector)
diseases

ttt = test_words_swise$Sent_ID[grep(diseases$disease[2], test_words_swise$words)]
test_words_swise$Sent_ID[grep(diseases$disease[2], test_words_swise$words)]

diseases

paste0(ttt, ttt)
ttt

test_words_swise%>%
  filter(Sent_ID == 261686)

diseases$disease[1]
grep(tt, test_words_swise$words)
str_locate(tt, as.vector(test_words_swise$words))



colnames(train_diseases_swise)[3] <- "disease"
train_diseases_swise

test_words_swise%>%
  filter(grepl(paste(x, collapse = "|")), words)

#test_words_swise[grepl((paste(x, collapse = "|")), test_words_swise$words), ]

#sapply(x, function(x) grepl(x, test_words_swise))

test%>%
  filter(Sent_ID == 211324)

filter(test_words_swise, grepl(train_diseases_swise$disease, words))



nest(train_diseases_swise$disease)
class(train_diseases_swise$disease)
map_chr(, paste, collapse = "|")

x <- train_diseases_swise$disease

#To concatenate all the disease names

ssa <- c("kill", "killals")
class(x)
paste(ssa, collapse = "|")

test_words_swise%>%
  filter(grepl(tt, words))

tt <- paste(x, collapse = "|")    

dim(tt)  
diseases <- train_diseases_swise%>%
  distinct(disease)
  
class(diseases)
diseases <- as.character(diseases)
train%>%
  count(tag)
test_words_swise
paste(diseases), collapse = "|")

apply(diseases, 1, paste, collapse="")

paste(letters, squigs, blargs, sep)

matches <- c("cat","dog")
class(tt)
tt
matches

paste(matches, collapse = "|")

tt <- diseases$disease[1:30]

############33

test_words_swise%>%
  filter(grepl("chronic hepatitis", words))

test_words_swise%>%
  filter(grepl(paste(tt,  collapse = "|"), words))


test_words_swise%>%
  filter(grepl("strategies|MICROCEPHALIA VERA|reactive hyperemia|acute", words))

test_words_swise%>%
  filter(grepl(dd, words))




ss <- c("strategies|MICROCEPHALIA VERA|reactive hyperemia|acute")
dd <- paste(tt,  collapse = "|")
class(dd)
tt

diseases$disease[18664]
ff <- c(dd)


dis_new <- str_replace_all(diseases$disease, "[^[:alnum:]]", " ")
tt <- str_replace_all(tt, "[^[:alnum:]]", " ")
