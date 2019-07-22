library(ggplot2)

h_train_tree = read.csv("housing_train.csv", stringsAsFactors = F)
h_test_tree = read.csv("housing_test.csv", stringsAsFactors = F)


h_train_tree$data = "train"
h_test_tree$data = "test"
h_test_tree$Price = NA

h_data_tree = rbind(h_train_tree,h_test_tree)

h_data_tree$Postcode = as.character(h_data_tree$Postcode)

###################################################################################
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

###################################################################################

#######################################################################################

var_inf = variabl_info(h_data_tree)
int_col = var_inf$int_col
char_col = var_inf$char_col
int_na_col = var_inf$int_na_col
char_na_col = var_inf$char_na_col
##################################

#######################################################################################

replace_na = function(x){
  for(col in int_na_col)
  {
    h_data_tree[is.na(h_data_tree[,col]),col] = median(h_data_tree[,col],na.rm=T)
  }
  return(h_data_tree)
}

#######################################################################################

h_clean_tree = replace_na(int_na_col)


y_tree= c("Address","Postcode","data")

char_col_dummy = char_col[ - which(char_col %in% y_tree)]

h_clean_tree = CreateDummies(h_clean_tree,"Suburb",34)
h_clean_tree = CreateDummies(h_clean_tree,"Type",591)
h_clean_tree = CreateDummies(h_clean_tree,"Method",610)
h_clean_tree = CreateDummies(h_clean_tree,"SellerG",120)
h_clean_tree = CreateDummies(h_clean_tree,"CouncilArea",198)


h_clean_tree = h_clean_tree[,!((names(h_clean_tree) %in% c("Address","Postcode","data")))]


set.seed(6)
s=sample(1:nrow(h_clean_tree),0.7*nrow(h_clean_tree))
h_clean_train1_tree=h_clean_tree[s,]
h_clean_train2_tree=h_clean_tree[-s,]  

glimpse(h_clean_train1_tree)
library(tree)

ld.tree = tree(Price~., data = h_clean_train1_tree)

ld.tree

plot(ld.tree)
text(ld.tree)


val.Price = predict(ld.tree,newdata = h_clean_train2_tree)


rmse_val = ((val.Price)-(h_clean_train2_tree$Price))^2 %>% mean() %>% sqrt() 