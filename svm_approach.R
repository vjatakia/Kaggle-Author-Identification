library(e1071)
library(tm)
library(data.table)
library(caret)

#Getting the data
train = fread("train.csv")
train$id = NULL


#############  PRE-PROCESSING THE DATA  ###############

#Cleaning of Train Data
corpus = Corpus(VectorSource(train$text))# Get a vector of all the sentences as elements 
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))

#Create a DTM from the  corpus 
dtm = DocumentTermMatrix(corpus)

#Remove the sparse(least frequent) terms
dtm_sm = removeSparseTerms( dtm,0.99) #value 0.99 can be increased to get more words 
dtm_mat = as.matrix(dtm_sm)

#Create the the training dataset using the DTM and target label
train_dtm = cbind( train$author, dtm_mat)
train_dtm = as.data.frame(train_dtm)
train_dtm$V1 = as.factor(train_dtm$V1)

#Splitting the data into train and test
fin_train_dtm = train_dtm[1:12000, ]
fin_test_dtm = train_dtm[12001:nrow(train_dtm), ]

View(fin_train_dtm)
View(fin_test_dtm)


########## TRAINING THE SVM MODEL  ############# 


#Training the model
model_svm = svm(V1 ~ .,data = fin_train_dtm,probability = TRUE)

#Making prediction using the model
pred_svm = predict(model_svm, newdata = fin_test_dtm, probability = TRUE) 

#Error Calculation
confusionMatrix(fin_test_dtm$V1, pred_svm)
