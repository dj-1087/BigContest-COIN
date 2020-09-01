library(data.table)
library(dplyr)
library(randomForest)
library(MASS)

# 데이터 읽기
get_df <- function(PATH) {
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
  df = subset(df, select = -c(WLS.y,WLS.x))
  df.num <- copy(df)
  
  for (i in 1:ncol(df.num)) {
    if(typeof(df.num[1,i])=="character") {
      df.num <- df[,-i]
    }
  }
  return(df.num)
}

get_raw_df <- function(PATH) {
  df <- read.csv(PATH);
  rownames(df) <- NULL
  
  df$일자 <- as.integer(gsub("-", "", df$일자))
  print("done")
  #select num type
  df.num <- copy(df)
  
  for (i in 1:ncol(df.num)) {
    if(typeof(df.num[1,i])=="character") {
      df.num <- df[,-i]
    }
  }
  df.num$WLS = df.num$결과
  df.num = subset(df.num, select = -결과)
  print("done2")
  return(df.num)
}


getBestModel <- function(df) {
  set.seed(1)
  errors <- c()
  for (i in 1:ncol(df)) {
    rf.m <- randomForest(WLS~., data=df, mtry=i, importance=TRUE)
    print(i/ncol(df)*100)
    error_rate = (rf.m$confusion[5]+rf.m$confusion[6])/2
    errors = c(errors,error_rate)
  }
  best_idx = which.min(errors)
  best.m <- randomForest(WLS~., data=df, mtry=best_idx, importance=TRUE)
  return(best.m)
}

getImportantVariableByGini <- function(rf.m, path) {
  set.seed(1)
  rel.importance<-rf.m$importance[,4]/max(rf.m$importance[,4])*100
  print(rel.importance.dec<-sort(rel.importance, decreasing=T))
  barplot(rel.importance.dec,horiz=T,names=names(rel.importance.dec),main = path)
  return(rel.importance.dec)
}

#팀타자
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_팀타자.csv"
df <- get_df(DF_PATH); str(df)
rf.m <- getBestModel(df)
rf.m
vars.gini <- getImportantVariableByGini(rf.m, DF_PATH)
vars.gini
head(vars.gini,24)
#팀투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_팀투수.csv"
df <- get_df(DF_PATH); str(df)
rf.m <- getBestModel(df)
rf.m
vars.gini <- getImportantVariableByGini(rf.m, DF_PATH)
vars.gini
head(vars.gini,24)
#최근 팀투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_최근_팀투수.csv"
df <- get_df(DF_PATH); str(df)
rf.m <- getBestModel(df)
rf.m
vars.gini <- getImportantVariableByGini(rf.m, DF_PATH)
vars.gini
head(vars.gini,24)
#팀투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_팀투수.csv"
df <- get_df(DF_PATH); str(df)
rf.m <- getBestModel(df)
rf.m
vars.gini <- getImportantVariableByGini(rf.m, DF_PATH)
vars.gini
head(vars.gini,24)
#팀투수
DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_팀투수.csv"
df <- get_df(DF_PATH); str(df)
rf.m <- getBestModel(df)
rf.m
vars.gini <- getImportantVariableByGini(rf.m, DF_PATH)
vars.gini
head(vars.gini,24)
