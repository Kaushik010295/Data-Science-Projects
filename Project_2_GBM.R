setwd("/Users/apple/Desktop/Projects/Project-2")
getwd()

store_train = read.csv("store_train.csv")
store_test = read.csv("store_test.csv")

store_train$data = 'train'
store_test$data = 'test'
store_test$store = NA

store_data = rbind(store_train, store_test)

glimpse(store_data)
library(dplyr)

store_data = store_data %>%
mutate(total_sales = (sales0 + sales1 + sales2 + sales3 + sales4)) %>%
select(-(sales1))

store_data = store_data[,(!(names(store_data) %in% c("sales1", "sales2", "sales3", "sales4")))]


glimpse(store_data)


#############################################################################

variabl_info = function(d)
{
int_col <- vector()
char_col <- vector()
int_na_col<- vector()
char_na_col<- vector()
for(col in colnames(d))
{
  if(is.integer(d[,col])==TRUE)
  {
    int_col <- c(int_col, col)
    if(sum(is.na(d[,col]))>0)
    {
      print("Into inner if")
      print(col)
      int_na_col <- c(int_na_col,col)
    }
  }else if(is.character(d[,col])==TRUE)
  {
    char_col <- c(char_col, col)
    if(sum(is.na(d[,col]))>0)
    {
      char_na_col <- c(int_na_col,col)
    }
  } 
}
my_list <- list("int_col" = int_col, "int_na_col" = int_na_col, "char_col" = char_col, "char_na_col"=char_na_col)

return(my_list)
}


#######################################################################################

var_inf = variabl_info(store_data)
int_col = var_inf$int_col
char_col = var_inf$char_col
int_na_col = var_inf$int_na_col
char_na_col = var_inf$char_na_col




##################################################################

store_data[(store_data$population %in% NA),"population"] = 1183.385;

store_data[(store_data$country %in% NA),"country"] = 99;


#####################################################################

CreateDummies=function(data,var,freq_cutoff=0){
maxvalue = max(table(data[,var]))
cutoff = round(maxvalue/10)
t=table(data[,var])
t=t[t>freq_cutoff]
t=sort(t)
categories=names(t)[-1]

for( cat in categories){
  name=paste(var,cat,sep="_")
  name=gsub(" ","",name)
  name=gsub("-","_",name)
  name=gsub("\\?","Q",name)
  
  data[,name]=as.numeric(data[,var]==cat)
}

data[,var]=NULL
return(data)
}

##################################################################


store_data = CreateDummies(store_data,"store_Type",300)

store_data = store_data[,(!(names(store_data) %in% c("State")))]

store_data = store_data[,(!(names(store_data) %in% c("storecode")))]

store_data = store_data[,(!(names(store_data) %in% c("Areaname","countytownname")))]

store_data = CreateDummies(store_data,"state_alpha",60)

store_data = CreateDummies(store_data,"countyname",5)

store_clean_train = store_data %>%
filter(data =='train') %>%
select(-data)

store_clean_test = store_data %>%
filter(data =='test') %>%
select(-data)

library(gbm)

library(cvTools)

param = list(interaction.depth = c(1:7), n.trees = c(50,100,250,500),
           shrinkage=c(0.1,0.01,0.001),
           n.minobsinnode = c(1,2,5,10))


subset_paras = function(full_list_para, n = 10){
all_comb = expand.grid(full_list_para)
s = sample(1:nrow(all_comb),n)
subset_para = all_comb[s,]
return(subset_para)
}
mycost_auc = function(y,yhat)
{
roccurve = pROC::roc(y,yhat)
score = pROC::auc(roccurve)
return(score)
}

num_trails = 10
my_params = subset_paras(param, num_trails)


myauc = 0


for(i in 1:num_trails){
print(paste0('starting iteration:', i))
params = my_params[i,]

k = cvTuning(gbm, store~., data = store_clean_train,
             tuning = params,
             args = list(distribution = "bernoulli"),
                         folds = cvFolds(nrow(store_clean_train), K = 10, type = "random"),
             cost=mycost_auc,
    
                         seed = 2,
                         predictArgs = list(type = "response",n.trees = params$n.tree)
                         )

score.this = k$cv[,2]

if(score.this>myauc){
  print(params)
  
  myauc = score.this

  print(myerror)
  best_params = params
}
print("DONE")
}
myauc
ci.gbm.final = gbm(store~., data = store_clean_train,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsinnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "gaussian")

test.pred = predict(ci.gbm.final, newdata = store_clean_test, type = 'response',
                  n.trees = best_params$n.trees)

write.csv(test.pred,"kaushik_jagini_P2",row.names = F)

