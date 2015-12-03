setwd("C:/Users/Kile/Desktop/Hackathon")

library(jiebaR)

# load the train data
train_data = read.csv("train.csv",stringsAsFactors = FALSE)

# cut the words, add the pos and extract the nouns, which we think they would 
# be more meaningful
cutter = worker(stop_word = "stop_words.utf8", type = "tag")
train_cut_data = lapply(1:nrow(train_data), function(i) cutter[train_data[i,3]])
for(i in 1:nrow(train_data)){
    train_cut_data[[i]] = train_cut_data[[i]][grep("^n",names(train_cut_data[[i]]))]    
}
train_data$cut = train_cut_data 

cut_word_list = list()
cut_word_table = list()

# put the cut words into a list
for(i in 1:nrow(train_data)){
    cut_word_list[[train_data[i,2]]] = c(cut_word_list[train_data[i,2]], train_data[i,4])        
}

# unlist the list elements
for(i in 1:length(cut_word_list)){
    cut_word_list[[i]] = unlist(cut_word_list[[i]]) 
}

# put the each list element into sorted table
for(i in 1:length(cut_word_list)){
    cut_word_table[[i]] = sort(table(cut_word_list[[i]]), decreasing = T)
}

# extract the words length >= 2 and with top 50 frequencies for each category
score_word_list = list()
for(i in 1:length(cut_word_table)){
    score_word_list[[i]] =  names(cut_word_table[[i]][nchar(names(cut_word_table[[i]])) >= 2][1:500])                       
}

# put the cut words in each news into table
news_word_table = list()
for(i in 1:nrow(train_data)){
    news_word_table[[i]] = table(train_data$cut[i])    
}

### new version 
#score_word_list = unlist(score_word_list)
### new version

# establish the function which can find the intersection of news and category top 
# words, and calculate the scores for each news and each category

### old version
givescore = function(score_word_list, news_word_table){
    score = c()
    for(i in 1:length(score_word_list)){
        if(length(intersect(score_word_list[[i]], names(news_word_table)))>0){
            score[i] = sum(news_word_table[intersect(score_word_list[[i]], names(news_word_table))])       
        }else{
            score[i] = 0
        }
    }
    return(score)
}
word_score = lapply(1:nrow(train_data), function(i) givescore(score_word_list, news_word_table[[i]]))

givescore = function(score_word_list, news_word_table){
    score = c()
    for(i in 1:length(score_word_list)){
        if(length(intersect(score_word_list[[i]], names(news_word_table)))>0){
            score[i] = length(intersect(score_word_list[[i]], names(news_word_table)))       
        }else{
            score[i] = 0
        }
    }
    return(score)
}
word_score = lapply(1:nrow(train_data), function(i) givescore(score_word_list, news_word_table[[i]]))
### old version

### new version
givescore = function(score_word_list, news_word_table){
    score = c()
    for(i in 1:length(score_word_list)){
        if(length(intersect(score_word_list[i], names(news_word_table)))>0){
            score[i] = news_word_table[intersect(score_word_list[i], names(news_word_table))]
        }else{
            score[i] = 0
        }
    }
    return(score)
}
word_score = lapply(1:nrow(train_data), function(i) givescore(score_word_list, news_word_table[[i]]))
### new version

# 
word_score_data_frame = as.data.frame(do.call("rbind", word_score), stringsAsFactors= FALSE)
names(word_score_data_frame) = paste("Scores_", seq(1:10), sep="")
word_score_data_frame$category = train_data$category
word_score_data_frame$category = as.factor(word_score_data_frame$category)
str(word_score_data_frame$category)

### test train data

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}
library(caTools)
split = sample.split(word_score_data_frame$category, SplitRatio = 0.75)
train_data_ml = word_score_data_frame[1:10]
train_data_ml = as.data.frame(lapply(word_score_data_frame[1:10], normalize))
train_data_ml_trainning = train_data_ml[split == T,]
train_data_ml_testing = train_data_ml[split == F,]
train_data_ml_train_labels = word_score_data_frame[split == T,11]
train_data_ml_test_labels = word_score_data_frame[split == F,11]

library(class)
train_data_ml_pred <- knn(train = train_data_ml_trainning, test = train_data_ml_testing,
                      cl = train_data_ml_train_labels, k = 100)
t = table(train_data_ml_test_labels, train_data_ml_pred)
sum = 0
for(i in 1:nrow(t)){
    for(j in 1:ncol(t)){
        if(i == j){
            sum = sum + t[i,j]
        } 
    }
}
sum

### using Trees
library(rpart)
library(rpart.plot)
train_data_cart = word_score_data_frame[split == T,]
test_data_cart = word_score_data_frame[split == F,]

cart_word_model = rpart(category ~ ., data = train_data_cart, method = "class", minbucket=1)
prp(cart_word_model)
PredictCART = predict(cart_word_model, newdata = test_data_cart, type = "class")
t = table(test_data_cart$category, PredictCART)

sum_table = function(table){
    sum = 0
    for(i in 1:nrow(t)){
        for(j in 1:ncol(t)){
            if(i == j){
                sum = sum + t[i,j]
            } 
        }
    }
    sum
}    
sum_table(t)


new_feature_1 = read.csv("freq_mat.csv",stringsAsFactors = F)
new_feature_2 = read.csv("test_freq_mat.csv",stringsAsFactors = F)
new_feature_1 = new_feature_1[,2:11]
new_feature_2 = new_feature_2[,2:11]
names(new_feature_1) = paste("Freq_Scores_", seq(1:10), sep="")
names(new_feature_2) = paste("Freq_Scores_", seq(1:10), sep="")
new_feature_1$category = train_data$category
split = sample.split(new_feature_1$category, SplitRatio = 0.75)
train_data_cart = new_feature_1[split == T,]
test_data_cart = new_feature_1[split == F,]
cart_word_model = rpart(category ~ ., data = train_data_cart, method = "class", minbucket=20)
prp(cart_word_model)
PredictCART = predict(cart_word_model, newdata = test_data_cart, type = "class")
t = table(test_data_cart$category, PredictCART)

sum_table = function(table){
    sum = 0
    for(i in 1:nrow(t)){
        for(j in 1:ncol(t)){
            if(i == j){
                sum = sum + t[i,j]
            } 
        }
    }
    sum
}    
sum_table(t)



###
###
### test test data 
test_data = read.csv("test.csv",stringsAsFactors = FALSE)

cutter = worker(stop_word = "stop_words.utf8", type = "tag")
test_cut_data = lapply(1:nrow(test_data), function(i) cutter[test_data[i,2]])
for(i in 1:nrow(test_data)){
    test_cut_data[[i]] = test_cut_data[[i]][grep("^n",names(test_cut_data[[i]]))]    
}

test_data$cut = test_cut_data 

test_news_word_table = list()
for(i in 1:nrow(test_data)){
    test_news_word_table[[i]] = table(test_data$cut[i])    
}


givescore = function(score_word_list, news_word_table){
    score = c()
    for(i in 1:length(score_word_list)){
        if(length(intersect(score_word_list[[i]], names(news_word_table)))>0){
            score[i] = sum(news_word_table[intersect(score_word_list[[i]], names(news_word_table))])       
        }else{
            score[i] = 0
        }
    }
    return(score)
}

test_word_score = lapply(1:nrow(test_data), function(i) givescore(score_word_list, test_news_word_table[[i]]))

test_word_score_data_frame = as.data.frame(do.call("rbind", test_word_score), stringsAsFactors= FALSE)
names(test_word_score_data_frame) = paste("Scores_", seq(1:10), sep="")

test_data_ml = as.data.frame(lapply(test_word_score_data_frame[1:10], normalize))
test_data_ml_pred <- knn(train = train_data_ml_trainning, test = test_data_ml,
                          cl = train_data_ml_train_labels, k = 100)
###
###
### the prediction is in test_data_ml_pred
write.csv(test_data_ml_pred, "test_outcome.csv")

###
###
### add a new feature
new_feature_1 = read.csv("freq_mat.csv",stringsAsFactors = F)
new_feature_2 = read.csv("test_freq_mat.csv",stringsAsFactors = F)
new_feature_1 = new_feature_1[,2:11]
new_feature_2 = new_feature_2[,2:11]
names(new_feature_1) = paste("Freq_Scores_", seq(1:10), sep="")
names(new_feature_2) = paste("Freq_Scores_", seq(1:10), sep="")
new_feature_3 = read.csv("more_test_freq_mat.csv", stringsAsFactors = F)
new_feature_3 = new_feature_3[,2:372]

word_score_data_frame = as.data.frame(do.call("rbind", word_score), stringsAsFactors= FALSE)
names(word_score_data_frame) = paste("Scores_", seq(1:10), sep="")
word_score_data_frame = cbind(word_score_data_frame, new_feature_1)
word_score_data_frame$category = train_data$category
word_score_data_frame$category = as.factor(word_score_data_frame$category)

split = sample.split(word_score_data_frame$category, SplitRatio = 0.75)
train_data_ml = as.data.frame(lapply(word_score_data_frame[1:20], normalize))
train_data_ml_trainning = train_data_ml[split == T,]
train_data_ml_testing = train_data_ml[split == F,]
train_data_ml_train_labels = word_score_data_frame[split == T,21]
train_data_ml_test_labels = word_score_data_frame[split == F,21]

library(class)
train_data_ml_pred <- knn(train = train_data_ml_trainning, test = train_data_ml_testing,
                          cl = train_data_ml_train_labels, k = 100)
table(train_data_ml_test_labels, train_data_ml_pred)

### test for more 
word_score_data_frame = as.data.frame(do.call("rbind", word_score), stringsAsFactors= FALSE)
names(word_score_data_frame) = paste("Scores_", seq(1:10), sep="")
word_score_data_frame = cbind(word_score_data_frame, new_feature_3)
word_score_data_frame$category = train_data$category
word_score_data_frame$category = as.factor(word_score_data_frame$category)

split = sample.split(word_score_data_frame$category, SplitRatio = 0.75)
#train_data_ml = word_score_data_frame[,1:871]
train_data_ml = as.data.frame(lapply(word_score_data_frame[1:871], normalize))
train_data_ml[is.na(train_data_ml)] = 0
train_data_ml_trainning = train_data_ml[split == T,]
train_data_ml_testing = train_data_ml[split == F,]
train_data_ml_train_labels = word_score_data_frame[split == T,872]
train_data_ml_test_labels = word_score_data_frame[split == F,872]

library(class)
train_data_ml_pred <- knn(train = train_data_ml_trainning, test = train_data_ml_testing,
                          cl = train_data_ml_train_labels, k = 100)
table(train_data_ml_test_labels, train_data_ml_pred)
###



###
test_word_score_data_frame = as.data.frame(do.call("rbind", test_word_score), stringsAsFactors= FALSE)
names(test_word_score_data_frame) = paste("Scores_", seq(1:10), sep="")
test_word_score_data_frame = cbind(test_word_score_data_frame, new_feature_2)
test_data_ml = as.data.frame(lapply(test_word_score_data_frame[1:20], normalize))
test_data_ml_pred <- knn(train = train_data_ml_trainning, test = test_data_ml,
                         cl = train_data_ml_train_labels, k = 100)

write.csv(test_data_ml_pred, "test_outcome.csv")
