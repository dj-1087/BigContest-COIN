library(data.table)
library(dplyr)
library(randomForest)
library(MASS)

# 데이터 읽기
mk_df <- function(PATH) {
  RESULT_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_공통.csv"
  df <- read.csv(PATH); str(df$WLS)
  tryCatch(expr = df$WLS, 
           error = {
             result <- read.csv(RESULT_PATH); str(result)
             result <- result[,c("G_ID","T_ID","VS_T_ID","WLS")]
             df <- left_join(df,result,by=c("G_ID"="G_ID","T_ID"="T_ID","VS_T_ID"="VS_T_ID"))           
           }
  )
  if(typeof(df$WLS.y)=="NULL") {
    
    df = df[!(df$WLS=="D"),]
    rownames(df) <- NULL
    
    #factor
    df$WLS <- factor(x = df$WLS,levels = c("W","L"),labels = c("W","L"));
    df$G_ID <- as.character(df$G_ID)
    df$GDAY_DS <- as.integer(gsub("-", "", df$GDAY_DS))
  }else {
    df = df[!(df$WLS.y=="D"),]
    rownames(df) <- NULL
    
    df$WLS <- factor(x = df$WLS.y,levels = c("W","L"),labels = c("W","L"));
    df$G_ID <- as.character(df$G_ID)
    df$GDAY_DS <- as.integer(gsub("-", "", df$GDAY_DS))
  }
  #select num type
  df.num <- copy(df)
  
  for (i in 1:ncol(df.num)) {
    if(typeof(df.num[1,i])=="character") {
      df.num <- df[,-i]
    }
  }
  return(df.num)
}

getImportance <- function(df, path) {
  MSE<-rep(NA,6)
  set.seed(1)
  rf.df<-randomForest(WLS~.,data=df, mtry=6, importance=TRUE, do.trace=500)
  print(rf.df$confusion)
  importance(rf.df)
  varImpPlot(rf.df,main = path)
  rel.importance<-rf.df$importance[,3]/max(rf.df$importance[,3])*100
  print(rel.importance.dec<-sort(rel.importance, decreasing=T))
  barplot(rel.importance.dec,horiz=T,names=names(rel.importance.dec),main = path)
  return(rel.importance.dec)
}

#팀타자
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_팀타자.csv"
df <- mk_df(DF_PATH); 
str(df)
table(is.na(df$WLS))
result <- getImportance(df,DF_PATH)
str(result)
head(result)

#최근 팀타자
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_최근_팀타자.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))
result <- getImportance(df,DF_PATH)
str(result)
head(result)
#상대 팀타자
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_상대_팀타자.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))
result <- getImportance(df,DF_PATH)
str(result)
head(result)
#개인타자
###########
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_개인타자.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))
result <- getImportance(df, DF_PATH)
str(result)
head(result)
#팀투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_팀투수.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))
result <- getImportance(df, DF_PATH)
str(result)
head(result)
#최근 팀투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_최근_팀투수.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))
result <- getImportance(df, DF_PATH)
str(result)
head(result)
#상대 팀투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_상대_팀투수.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))
result <- getImportance(df, DF_PATH)
str(result)
head(result)
#개인투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_개인투수.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))  
result <- getImportance(df, DF_PATH)
str(result)
head(result)
#최종 공통
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_공통.csv"
df <- mk_df(DF_PATH); str(df)
table(is.na(df$WLS))
result <- getImportance(df, DF_PATH)
str(result)
head(result)
