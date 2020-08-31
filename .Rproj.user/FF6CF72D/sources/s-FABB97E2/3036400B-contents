library(leaps)

mk_df <- function(PATH) {
  
  df <- read.csv(PATH)
  
  
  #factor
  df$G_ID <- as.character(df$G_ID)
  df$GDAY_DS <- as.integer(gsub("-", "", df$GDAY_DS))
  
  #select num type
  df.num <- df
  
  for (i in 1:ncol(df.num)) {
    if(typeof(df.num[1,i])=="character") {
      df.num <- df[,-i]
    }
  }
  return(df.num)
}

hitter_test <- function (hitter, nvmax = 8){
  num = ncol(hitter)
  regfit.full = regsubsets(RUN~.,hitter, really.big = T, nvmax = nvmax)
  reg.summary = summary(regfit.full)
  
  adjr2 = which.max(reg.summary$adjr2)
  cp = which.min(reg.summary$cp)
  bic = which.min(reg.summary$bic)
  
  par(mfrow=c(2,2))
  plot(reg.summary$adjr2,type="l",xlab="No. of variables", ylab="Adjusted R2")
  points(adjr2,reg.summary$adjr2[adjr2],col="red",cex=2,pch=20)
  plot(reg.summary$cp,ylab="Cp",type="l")
  points(cp,reg.summary$cp[cp],col="red",cex=2,pch=20)
  plot(reg.summary$bic, ylab="BIC",type="l")
  points(bic,reg.summary$bic[bic],col="red",cex=2,pch=20)
  return(regfit.full)
}

pitcher_test <- function (pitcher, nvmax = 8){
  num = ncol(pitcher)
  regfit.full = regsubsets(R~.,pitcher, really.big = T, nvmax = nvmax)
  reg.summary = summary(regfit.full)
  
  adjr2 = which.max(reg.summary$adjr2)
  cp = which.min(reg.summary$cp)
  bic = which.min(reg.summary$bic)
  
  par(mfrow=c(2,2))
  plot(reg.summary$adjr2,type="l",xlab="No. of variables", ylab="Adjusted R2")
  points(adjr2,reg.summary$adjr2[adjr2],col="red",cex=2,pch=20)
  plot(reg.summary$cp,ylab="Cp",type="l")
  points(cp,reg.summary$cp[cp],col="red",cex=2,pch=20)
  plot(reg.summary$bic, ylab="BIC",type="l")
  points(bic,reg.summary$bic[bic],col="red",cex=2,pch=20)
  return(regfit.full)
}

view_result <- function(test_model, nvmax = 8) {
  print(summary(result))
  print(vcov(result, nvmax))
  print(coef(result, nvmax))
}



DF_PATH <- "../../1차_최종_데이터(08_29)/1차_최종_팀투수.csv"
df <- mk_df(DF_PATH); str(df)

result = pitcher_test(df)

view_result(result)

