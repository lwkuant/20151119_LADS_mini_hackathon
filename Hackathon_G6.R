### train data process
setwd("C:/Users/Kile/Desktop/Hackathon")

library(jiebaR)

# load the train data
train_data = read.csv("train.csv",stringsAsFactors = FALSE)


# cut words
cutter = worker(stop_word = "stop_words.utf8", type = "tag")
train_cut_data = lapply(1:nrow(train_data), function(i) cutter[train_data[i,3]])
for(i in 1:nrow(train_data)){
    train_cut_data[[i]] = train_cut_data[[i]][grep("^n",names(train_cut_data[[i]]))]    
}

# set up the train table
train_table = as.data.frame(train_data$category)
colnames(train_table)[1] = "Category"

# add variable: length of article
Article_length = lapply(1:nrow(train_data), function(i) nchar(train_data[i,3]))
Article_length_count = Article_length
train_table = as.data.frame(cbind(train_table, unlist(Article_length)), stringsAsFactors = F)
colnames(train_table)[2] = "Article_length"
for(i in 1:nrow(train_table)){
    if(train_table[i,2]<=200 | train_table[i,2]>=1000){
        train_table[i,2] = 1        
    }else{
        train_table[i,2] = 0
    }
}

# cut words with punctuations
cutter = worker(symbol = T)
Punctuation_rate = sapply(1:nrow(train_data), function(i) length(grep("[[:punct:]]",cutter[train_data[i,3]],value = T)))
Punctuation_rate = Punctuation_rate/unlist(Article_length_count)
train_table = as.data.frame(cbind(train_table, Punctuation_rate), stringsAsFactors = F)

# put the cut words into a list containing 10 categories
cut_word_list = list()
for(i in 1:nrow(train_data)){
    cut_word_list[[train_data[i,2]]] = c(cut_word_list[train_data[i,2]], train_cut_data[[i]])        
}
# put the cut words in each news into table
news_word_table = list()
for(i in 1:nrow(train_data)){
    news_word_table[[i]] = table(train_cut_data[[i]])    
}
# unlist the list elements
for(i in 1:length(cut_word_list)){
    cut_word_list[[i]] = unlist(cut_word_list[[i]]) 
}
# put the each list element into sorted table
cut_word_table = list()
for(i in 1:length(cut_word_list)){
    cut_word_table[[i]] = sort(table(cut_word_list[[i]]), decreasing = T)
}
# extract the words with length at least 2 and top 500 frequency
score_word_list = list()
for(i in 1:length(cut_word_table)){
    score_word_list[[i]] =  names(cut_word_table[[i]][nchar(names(cut_word_table[[i]])) >= 2][1:500])                       
}

# calculate the words scored
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

# add a variable: the proportion of PE news related words
PE_proportion = sapply(1:nrow(train_data), function(i) word_score[[i]][7]/sum(news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, PE_proportion), stringsAsFactors = F)

# add a variable: the proportion of HELTH news related words
HELTH_proportion = sapply(1:nrow(train_data), function(i) word_score[[i]][9]/sum(news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, HELTH_proportion), stringsAsFactors = F)

# add a variable: the proportion of LIFE news related words
#LIFE_proportion = sapply(1:nrow(train_data), function(i) word_score[[i]][3]/sum(news_word_table[[i]]))
#train_table = as.data.frame(cbind(train_table, LIFE_proportion), stringsAsFactors = F)

# add a variable: the proportion of SOCIAL news related words
SOCIAL_proportion = sapply(1:nrow(train_data), function(i) word_score[[i]][4]/sum(news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, SOCIAL_proportion), stringsAsFactors = F)

# add a variable: the proportion of HOT news related words
HOT_proportion = sapply(1:nrow(train_data), function(i) word_score[[i]][1]/sum(news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, HOT_proportion), stringsAsFactors = F)

# add a variable: the proportion of FOCUS news related words
FOCUS_proportion = sapply(1:nrow(train_data), function(i) word_score[[i]][2]/sum(news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, FOCUS_proportion), stringsAsFactors = F)

# add a variable: the level of international
international_list = c("����", "�^��", "�p�X��", "�ڬw", "�����", "�D�w", "�æ���", "�۴�", "��þ",
                       "�w��", "��h", "�ì�", "�q�j�Q", "�ڤڰ�", "�ڰ򴵩Z", "���Ӧ��", "�饻", "�s�[�Y",
                       "�n��", "���ԧB", "�H��C", "���I��", "���", "���ۤ��", "�g�ը�", "�L��", "�ڦ�", 
                       "�ڰǴ��Z", "�@��", "�[���j", "�_��", "��Z��", "�����", "�Xù��", "�իXù��", "��ԧJ",
                       "���", "�k��", "���ڧ�", "�����F", "�L��", "�j��", "����", "����", "���|", "���", "���Q",
                       "�ز��y", "�ة�", "�H������", "�ڷ�", "�q�l", "�s�[��", "���", "����")
calculate_international = function(international_list, news_word_table){
    count = 0
        if(length(intersect(names(news_word_table), international_list)) > 0){
            count = sum(news_word_table[intersect(names(news_word_table), international_list)])
        }else{
            count = 0   
        }
    count/sum(news_word_table)
}
INT_Level = sapply(1:length(news_word_table), function(i) calculate_international(international_list, news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, INT_Level), stringsAsFactors = F)

# add a variable: the level of Cross-Strait Relations(CSR)
CSR_list = c("����", "�j��", "�_��", "���@", "�H����", "�W��", "�s�F", "�e�_", "�Ѭz", "�ѰO", "�`�ѰO", "�D�u",
             "�|�t", "����", "�x��", "�⩤", "��Ĭ", "����", "���|", "�F��", "�e�n", "�C�{", "����",
             "���y", "���l", "����|", "����|", "�⩤", "���n", "�n��", "����", "��n", "���n", "�sæ", "�s�{",
             "�s��")
calculate_CSR = function(CSR_list, news_word_table){
    count = 0
    if(length(intersect(names(news_word_table), CSR_list)) > 0){
        count = sum(news_word_table[intersect(names(news_word_table), CSR_list)])
    }else{
        count = 0   
    }
    count/sum(news_word_table)
}
CSR_Level = sapply(1:length(news_word_table), function(i) calculate_CSR(CSR_list, news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, CSR_Level), stringsAsFactors = F)

# add a variable: the level of Education(EDU)
EDU_list = c("�ǥ�", "�j��", "�ǰ|", "�z�ǰ|", "��ǰ|", "���", "��ߤj��", "�p��", "����", "����",
             "�p��", "��p", "�Юv", "�Ѯv", "�б�", "�ۥ�", "�s��", "�Ҹ�", "�j�ǥ�", "�Ш|", "�Ш|��",
             "��¾", "Ū��", "����", "�j��", "�Ǵ�", "����", "�ǤO", "�Ǿ�", "���", "�Ǧ~", "�Ǧ~��",
             "�J��", "�ꤤ", "�P��" ,"�ƾ�", "�a��", "����", "�H��", "�ն�", "�פ�", "���i")
calculate_EDU = function(EDU_list, news_word_table){
    count = 0
    if(length(intersect(names(news_word_table), EDU_list)) > 0){
        count = sum(news_word_table[intersect(names(news_word_table), EDU_list)])
    }else{
        count = 0   
    }
    count/sum(news_word_table)
}
EDU_Level = sapply(1:length(news_word_table), function(i) calculate_EDU(EDU_list, news_word_table[[i]]))
train_table = as.data.frame(cbind(train_table, EDU_Level), stringsAsFactors = F)

# assign the NA as 0
train_table[is.na(train_table)] = 0

### train data tree test

# transform list as factor
str(train_table)
train_table$Category = factor(unlist(train_table$Category))

# group the train data
library(caTools)
split = sample.split(train_table$Category, SplitRatio = 0.75)
train_trainning_news = train_table[split == T,]
train_testing_news = train_table[split == F,]

# test the tree with train data
library(rpart)
library(rpart.plot)
news_train_model = rpart(Category ~ ., data = train_table, method = "class", minbucket=10)
prp(news_train_model)
PredictCART = predict(news_train_model, newdata = train_testing_news, type = "class")
tab = table(train_testing_news$Category, PredictCART)
tab
sum_table = function(table){
    sum = 0
    for(i in 1:nrow(table)){
        for(j in 1:ncol(table)){
            if(i == j){
                sum = sum + table[i,j]
            } 
        }
    }
    sum
}    
sum_table(tab)/sum(tab)

### train data knn test
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}
train_table_knn = as.data.frame(lapply(train_table[2:ncol(train_table)], normalize))
train_table_knn = train_table[2:ncol(train_table)]
library(caTools)
train_trainning_news = train_table_knn[split == T,]
train_testing_news = train_table_knn[split == F,]
train_training_labels = train_table[split == T,1]
train_testing_labels = train_table[split == F,1]

library(class)
news_train_model = knn(train = train_trainning_news, test = train_testing_news,
                       cl = train_training_labels, k = 50)
tab = table(train_testing_labels, news_train_model)
sum_table = function(table){
    sum = 0
    for(i in 1:nrow(table)){
        for(j in 1:ncol(table)){
            if(i == j){
                sum = sum + table[i,j]
            } 
        }
    }
    sum
}    
sum_table(tab)



### trees example
a = c(1,2,3,2,1,3,2)
b = c(0.1,0.3,5,0.05,0.7,10,0.1)
c = c(0,1,1,1,0,1,1)
dt = data.frame(a,b,c)


d = c(0.001, 0.2,6)
e = c(0,1,1)
f = c(1,2,3)
ddt = data.frame(f,d,e)
names(ddt) = c("a","b","c")

cart_model = rpart(a ~., data = dt,method = "class", minbucket=1 )
prp(cart_model)
pred = predict(cart_model, newdata = ddt, type = "class")
table(ddt$a, pred)