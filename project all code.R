#Project Code
library(e1071)
library(ISLR)
library(leaps)
library(VIF)
library(HH)
library(clusterGeneration)
library(MASS)
library(fmsb)
library(ggplot2)
options(stringsAsFactors = FALSE)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)
library(stringr)
library(data.table)
library(randomForest)
library(glmnet)
library(pls)
library(class)
library(gam)
library(akima)
library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(ROCR)
dim(news)
news<-read.csv('/Users/anmoe/Desktop/OnlineNewsPopularity.csv',header = T)
news$url<-NULL
news$url=rep("popular",dim(news)[1])

quantile(news$shares,0.3)
news$url[news$shares<1000]="unpopular"
news$url=factor(news$url)
#One of the features, url, is URL of the article, which is not useful. So I delete this feature.
summary(news$shares)
dim(news)
news=na.omit(news)
dim(news)
#no missing data
#scale
news$shares=news$shares/1000
quantile(news$shares,0.9)
news %>%
  filter(news$shares<6.2) ->news
quantile(news$shares,0.1)
news %>%
  filter(news$shares>0.688) ->news




#data cleaning
news %>%
  filter(n_tokens_content!=0) ->news
news %>%
  filter(news$weekday_is_monday+news$weekday_is_tuesday+news$weekday_is_wednesday+
           news$weekday_is_thursday+news$weekday_is_friday+news$weekday_is_saturday+news$weekday_is_sunday==1) ->news
news %>%
  filter(news$weekday_is_saturday+news$weekday_is_sunday==news$is_weekend) ->news
news %>%
  filter(news$shares!=0) ->news
dim(news)

news$shares<-NULL
vif_func<-function(in_frame,thresh=10,trace=T,...){
  library(fmsb)
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    in_dat<-in_frame
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      vif_vals<-NULL
      var_names <- names(in_dat)
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      if(vif_max<thresh) break
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    return(names(in_dat))
  }
}


vif_func(news)
#removed:  n_non_stop_words 2086.212 
news$n_non_stop_words<-NULL
#removed:  weekday_is_monday Inf
news$weekday_is_monday<-NULL
#removed:  weekday_is_saturday Inf
news$weekday_is_saturday<-NULL
#removed:  LDA_04 981618118
news$LDA_04<-NULL
#removed:  n_unique_tokens 13681.29 
news$n_unique_tokens<-NULL
#removed:  rate_negative_words 18.21274
news$rate_negative_words<-NULL
#removed:  self_reference_avg_sharess 19.14889 
news$self_reference_avg_sharess<-NULL
#removed:  kw_max_min 11.30715
news$kw_max_min<-NULL
head(news)
dim(news)

news$url<-NULL
#Forward Stepwise Selection
regfit.fwd=regsubsets(shares~.,data=news,nvmax=51,method="forward")
summary(regfit.fwd)
reg.summary=summary(regfit.fwd)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(32,reg.summary$adjr2[32], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(30,reg.summary$cp[30],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(10,reg.summary$bic[10],col="red",cex=2,pch=20)
par(mfrow=c(1,1))
plot(regfit.fwd,scale="r2")
plot(regfit.fwd,scale="adjr2")
plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="bic")
coef(regfit.fwd,40)
coef(regfit.fwd,28)
#1timedelta 2 n_tokens_title 3 n_tokens_content
#4 num_hrefs 5 num_self_hrefs 6 average_token_length 7 num_keywords 
#8 data_channel_is_lifestyle 9 data_channel_is_entertainment 10 kw_min_min 11 kw_min_max
#12 kw_min_avg 13 kw_max_avg 14 kw_avg_avg 15 self_reference_min_shares
#16 self_reference_max_shares 17 weekday_is_tuesday 18 weekday_is_thursday 19 weekday_is_sunday
#20 is_weekend 21 LDA_02 22 LDA_03 23  global_subjectivity 
#24 global_rate_positive_words 25 min_positive_polarity 26 avg_negative_polarity 27 abs_title_subjectivity
#28 abs_title_sentiment_polarity
coef(regfit.fwd,10)
#BIC: choose 10 variables: 1 timedelta 2 n_tokens_title 3 num_hrefs 
#4 data_channel_is_entertainment 5 kw_min_avg 6 kw_max_avg 7 kw_avg_acg 
#8 self_reference_min_shares 9 global_subjectivity 10 avg_negative_polarity




#Backward Stepwise Selection
regfit.bwd=regsubsets(shares~.,data=news,nvmax=51,method="backward")
summary(regfit.bwd)
reg.summary=summary(regfit.bwd)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(35,reg.summary$adjr2[35], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(27,reg.summary$cp[27],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(10,reg.summary$bic[10],col="red",cex=2,pch=20)
par(mfrow=c(1,1))
plot(regfit.bwd,scale="r2")
plot(regfit.bwd,scale="adjr2")
plot(regfit.bwd,scale="Cp")
plot(regfit.bwd,scale="bic")
coef(regfit.bwd,35)
coef(regfit.bwd,27)
#1 timedelta 2 n_tokens_title 3 n_tokens_content
#4 num_hrefs 5 num_self_hrefs 6 average_token_length 7 data_channel_is_lifestyle
#8 data_channel_is_entertainment 9 data_channel_is_bus 10 data_channel_is_tech 11 kw_min_max
#12 kw_min_avg 13 kw_max_avg 14 kw_avg_avg 15 self_reference_min_shares
#16 self_reference_max_shares 17 weekday_is_tuesday 18 weekday_is_wednesday 19 weekday_is_thursday
#20 weekday_is_friday 21 LDA_02 22 global_subjectivity 23 global_rate_positive_words
#24 min_positive_polarity 25 avg_negative_polarity 26 abs_title_subjectivity 27 abs_title_sentiment_polarity
coef(regfit.bwd,10)
#BIC: choose 11 variables: 1 timedelta 2 n_tokens_title 3 num_hrefs 4  data_channel_is_entertainment 
#5 kw_min_avg 6 kw_max_avg 7 kw_avg_avg 8 self_reference_min_shares 9 global_subjectivity 10 avg_negative_polarity


news$LDA_03
#forward 28 knn classification
attach(news)
train =( timedelta <365)
train_28.X=cbind(timedelta, n_tokens_title, n_tokens_content,
                 num_hrefs, num_self_hrefs, average_token_length, num_keywords,
                 data_channel_is_lifestyle, data_channel_is_entertainment, kw_min_min, kw_min_max,
                 kw_min_avg, kw_max_avg, kw_avg_avg, self_reference_min_shares,
                 self_reference_max_shares, weekday_is_tuesday, weekday_is_thursday, weekday_is_sunday,
                 is_weekend, LDA_02, LDA_03,  global_subjectivity,
                 global_rate_positive_words, min_positive_polarity, avg_negative_polarity, abs_title_subjectivity,
                 abs_title_sentiment_polarity)[train ,]
test_28.X=cbind(timedelta, n_tokens_title, n_tokens_content,
                num_hrefs, num_self_hrefs, average_token_length, num_keywords,
                data_channel_is_lifestyle, data_channel_is_entertainment, kw_min_min, kw_min_max,
                kw_min_avg, kw_max_avg, kw_avg_avg, self_reference_min_shares,
                self_reference_max_shares, weekday_is_tuesday, weekday_is_thursday, weekday_is_sunday,
                is_weekend, LDA_02, LDA_03,  global_subjectivity,
                global_rate_positive_words, min_positive_polarity, avg_negative_polarity, abs_title_subjectivity,
                abs_title_sentiment_polarity)[!train ,]
train_28.y=url[train]
test_28.y=url[!train]
news$url=rep("popular",dim(news)[1])
quantile(news$shares,0.3)
news$url[news$shares<1000]="unpopular"
news$url=factor(news$url)
for(i in 10:100){
  news.knn<-knn(train_28.X, test_28.X, train_28.y, k=i)
  print(i)
  print(mean(news.knn==test_28.y))
}
news.knn<-knn(train_28.X, test_28.X, train_28.y, k=50)
summary(news.knn)
table(news.knn,test_28.y)
mean(news.knn==test_28.y)
#0.7015071
train_28.y=shares[train]
test_28.y=shares[!train]
news.knn<-knn(train_28.X, test_28.X, train_28.y, k=50)
class(news.knn)
mean((as.numeric(news.knn)-test_28.y)^2)
#203400980
plot(test_28.y, news.knn, xlab="y", ylab=expression(hat(y)))





#backward & forward 10 knn classification
attach(news)
train =( timedelta <365)
train_10.X=cbind(timedelta, n_tokens_title, num_hrefs, data_channel_is_entertainment
                 , kw_min_avg, kw_max_avg, kw_avg_avg, self_reference_min_shares, global_subjectivity,
                 avg_negative_polarity)[train ,]
test_10.X=cbind(timedelta, n_tokens_title, num_hrefs, data_channel_is_entertainment
                , kw_min_avg, kw_max_avg, kw_avg_avg, self_reference_min_shares, global_subjectivity,
                avg_negative_polarity)[!train ,]
train_10.y=url[train]
test_10.y=url[!train]
news$url=rep("popular",dim(news)[1])
quantile(news$shares,0.3)
news$url[news$shares<1000]="unpopular"
news$url=factor(news$url)
for(i in 1:100){
  news.knn<-knn(train_10.X, test_10.X, train_10.y, k=i)
  print(i)
  print(mean(news.knn==test_10.y))
}
news.knn<-knn(train_10.X, test_10.X, train_10.y, k=30)
summary(news.knn)
table(news.knn,test_10.y)
mean(news.knn==test_10.y)
#0.7021034
train_10.y=shares[train]
test_10.y=shares[!train]
news.knn<-knn(train_10.X, test_10.X, train_10.y, k=30)
class(news.knn)
mean((as.numeric(news.knn)-test_10.y)^2)
#203394969
plot(test_10.y, news.knn, xlab="y", ylab=expression(hat(y)))



#backward 27 knn classification
attach(news)
train =( timedelta <365)
train_27.X=cbind(timedelta, n_tokens_title, n_tokens_content,
                 num_hrefs, num_self_hrefs, average_token_length, data_channel_is_lifestyle,
                 data_channel_is_entertainment, data_channel_is_bus, data_channel_is_tech, kw_min_max,
                 kw_min_avg, kw_max_avg, kw_avg_avg, self_reference_min_shares,
                 self_reference_max_shares, weekday_is_tuesday, weekday_is_wednesday, weekday_is_thursday,
                 weekday_is_friday, LDA_02, global_subjectivity, global_rate_positive_words,
                 min_positive_polarity, avg_negative_polarity, abs_title_subjectivity, 
                 abs_title_sentiment_polarity)[train ,]
test_27.X=cbind(timedelta, n_tokens_title, n_tokens_content,
                num_hrefs, num_self_hrefs, average_token_length, data_channel_is_lifestyle,
                data_channel_is_entertainment, data_channel_is_bus, data_channel_is_tech, kw_min_max,
                kw_min_avg, kw_max_avg, kw_avg_avg, self_reference_min_shares,
                self_reference_max_shares, weekday_is_tuesday, weekday_is_wednesday, weekday_is_thursday,
                weekday_is_friday, LDA_02, global_subjectivity, global_rate_positive_words,
                min_positive_polarity, avg_negative_polarity, abs_title_subjectivity, 
                abs_title_sentiment_polarity)[!train ,]
news$url=rep("popular",dim(news)[1])
quantile(news$shares,0.3)
news$url[news$shares<1000]="unpopular"
news$url=factor(news$url)
train_27.y=url[train]
test_27.y=url[!train]
news.knn<-knn(train_27.X, test_27.X, train_27.y, k=30)
mean(news.knn==test_27.y)
summary(news.knn)
table(news.knn,test_27.y)
mean(news.knn==test_27.y)
#0.6996097
#prediction
train_27.y=shares[train]
test_27.y=shares[!train]
news.knn<-knn(train_27.X, test_27.X, train_27.y, k=30)
class(news.knn)
mean((as.numeric(news.knn)-test_27.y)^2)
#MSE=203397787
plot(test_27.y, news.knn, xlab="y", ylab=expression(hat(y)))

#backward & forward selection
#1 timedelta 2 n_tokens_title 3 num_hrefs 
#4 data_channel_is_entertainment 5 kw_min_avg 6 kw_max_avg 7 kw_avg_acg 
#8 self_reference_min_shares 9 global_subjectivity 10 avg_negative_polarity



#smoothing spline: 1 timedelta
attach(news)
lims_time=range(timedelta)
time.grid=seq(from=lims_time[1],to=lims_time[2])

plot(timedelta,shares,xlim=lims_time,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit2=smooth.spline(timedelta,shares,cv=TRUE)
#13.59733
fit2$df
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","13.6029 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#smoothing spline: 2 n_tokens_title
attach(news)
lims_title=range(n_tokens_title)
title.grid=seq(from=lims_title[1],to=lims_title[2])

plot(n_tokens_title,shares,xlim=lims_title,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(n_tokens_title,shares,df=16)
fit2=smooth.spline(n_tokens_title,shares,cv=TRUE)
fit2$df
#2.000001
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","2 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#smoothing spline: 3 num_hrefs
lims_num=range(num_hrefs)
num.grid=seq(from=lims_num[1],to=lims_num[2])

plot(num_hrefs,shares,xlim=lims_num,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(num_hrefs,log(shares),df=16)
fit2=smooth.spline(num_hrefs,log(shares),cv=TRUE)
fit2$df
#20.05601
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","10.63935 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)



#smoothing spline: 5 kw_min_avg 
lims_num=range(kw_min_avg)
num.grid=seq(from=lims_num[1],to=lims_num[2])

plot(kw_min_avg,shares,xlim=lims_num,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(kw_min_avg,log(shares),df=16)
fit2=smooth.spline(kw_min_avg,log(shares),cv=TRUE)
fit2$df
#27.09881
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","25.44033 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)




#smoothing spline: 6 kw_max_avg 
lims_num=range(kw_max_avg)
num.grid=seq(from=lims_num[1],to=lims_num[2])

plot(kw_max_avg,shares,xlim=lims_num,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(kw_min_avg,log(shares),df=16)
fit2=smooth.spline(kw_max_avg,shares,cv=TRUE)
fit2$df
#6.905792
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","25.44033 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)




#smoothing spline: 7 kw_avg_avg 
lims_num=range(kw_avg_avg)
num.grid=seq(from=lims_num[1],to=lims_num[2])

plot(kw_avg_avg,shares,xlim=lims_num,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(kw_min_avg,log(shares),df=16)
fit2=smooth.spline(kw_avg_avg,shares,cv=TRUE)
fit2$df
#6.078141
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","25.44033 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)



#smoothing spline: 8 self_reference_min_shares 
lims_num=range(self_reference_min_shares)
num.grid=seq(from=lims_num[1],to=lims_num[2])

plot(self_reference_min_shares,shares,xlim=lims_num,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(kw_min_avg,log(shares),df=16)
fit2=smooth.spline(self_reference_min_shares,shares,cv=TRUE)
fit2$df
#13.37476
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","25.44033 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)



#smoothing spline: 9 global_subjectivity
lims_num=range(global_subjectivity)
num.grid=seq(from=lims_num[1],to=lims_num[2])

plot(global_subjectivity,shares,xlim=lims_num,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(kw_min_avg,log(shares),df=16)
fit2=smooth.spline(global_subjectivity,shares,cv=TRUE)
fit2$df
#4.243846
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","25.44033 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


#smoothing spline: 10 avg_negative_polarity
lims_num=range(avg_negative_polarity)
num.grid=seq(from=lims_num[1],to=lims_num[2])

plot(avg_negative_polarity,shares,xlim=lims_num,cex=.5,col="darkgrey")
title("Smoothing Spline")
#fit=smooth.spline(kw_min_avg,log(shares),df=16)
fit2=smooth.spline(avg_negative_polarity,shares,cv=TRUE)
fit2$df
#2.63109
#lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
#legend("topright",legend=c("16 DF","25.44033 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
set.seed(1)
train=sample(1:nrow(news), nrow(news)/2)
test=(-train)

gam=lm(shares~s(timedelta,14)+s(n_tokens_title,2)+s(num_hrefs,20)+data_channel_is_entertainment+
         s(kw_min_avg,27)+s(kw_max_avg,7)+s(kw_avg_avg,6)+s(self_reference_min_shares,13)+s(global_subjectivity,4)+
         s(avg_negative_polarity,3),data=news[train,])
par(mfrow=c(1,1))
plot.gam(gam, se=TRUE, col="red")
summary(gam)
y.test=news$shares[test]
preds=predict(gam,newdata=news[test,])
mean((preds-y.test)^2)
#156557987
#bad


#random forest
set.seed(1)
train=sample(1:nrow(news),2000)
bag.news=randomForest(shares~.,data=news,subset=train,mtry=17,ntree=34)
yhat.bag = predict(bag.news,newdata=news[-train,])
news.test=news[-train,"shares"]
par(mfrow=c(1,1))
plot(yhat.bag, news.test)
abline(0,1)
mean((yhat.bag-news.test)^2)
#143869700
set.seed(1)
rf.news=randomForest(shares~.,data=news,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.news,newdata=news[-train,])
mean((yhat.rf-news.test)^2)
#126437588
importance(rf.news)
varImpPlot(rf.news)


# Ridge Regression
x=model.matrix(shares~.,news)[,-1]
y=news$shares
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
#251356589
mean((mean(y[train])-y.test)^2)
#158937836
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
#158937815
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)
#251514958
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
#bad plot
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
#best lam MSE=206494352
var(y.test)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
#bad

# The Lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
#201407596
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
#bad

# Principal Components Regression PCA
set.seed(2)
pcr.fit=pcr(shares~., data=news,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
set.seed(1)
pcr.fit2=pcr(shares~., data=news,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit2,val.type="MSEP")
pcr.pred=predict(pcr.fit2,x[test,],ncomp=20)
#1:21 choose20
mean((pcr.pred-y.test)^2)
#158072532
pcr.fit=pcr(y~x,scale=TRUE,ncomp=4)
#1:5 choose 4
summary(pcr.fit)

# Partial Least Squares
set.seed(1)
pls.fit=plsr(shares~., data=news,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=12)
mean((pls.pred-y.test)^2)
#255595567
pls.fit=plsr(shares~., data=news,scale=TRUE,ncomp=50)
summary(pls.fit)
#bad

attach(news)
# Logistic Regression
train=(timedelta<365)
news_test=news[!train,]
dim(news_test)
url_test=url[!train]
glm.fit=glm(url~timedelta+n_tokens_title+num_hrefs+data_channel_is_entertainment
            +kw_min_avg+kw_max_avg+kw_avg_avg+self_reference_min_shares+global_subjectivity+
              avg_negative_polarity,data=news,family=binomial,subset=train)
glm.probs=predict(glm.fit,news_test,type="response")

glm.pred=rep("popular",dim(news_test)[1])
glm.pred[glm.probs>.5]="unpopular"
table(glm.pred,url_test)
mean(glm.pred==url_test)
#0.7076331
glm.fit=glm(url~timedelta + n_tokens_title+n_tokens_content+
              num_hrefs+ num_self_hrefs+ average_token_length+ data_channel_is_lifestyle+
              data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_tech+ kw_min_max+
              kw_min_avg+ kw_max_avg+ kw_avg_avg+self_reference_min_shares+
              self_reference_max_shares+weekday_is_tuesday+ weekday_is_wednesday+weekday_is_thursday+
              weekday_is_friday+ LDA_02+global_subjectivity+ global_rate_positive_words+
              min_positive_polarity+ avg_negative_polarity+ abs_title_subjectivity+ 
              abs_title_sentiment_polarity,data=news,family=binomial,subset=train)

glm.probs=predict(glm.fit,news_test,type="response")
glm.pred=rep("popular",dim(news_test)[1])
glm.pred[glm.probs>.5]="unpopular"
table(glm.pred,url_test)
mean(glm.pred==url_test)
summary(glm.fit)
#0.7087715
#predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")



# Linear Discriminant Analysis
lda.fit=lda(url~timedelta+n_tokens_title+num_hrefs+data_channel_is_entertainment
            +kw_min_avg+kw_max_avg+kw_avg_avg+self_reference_min_shares+global_subjectivity+
              avg_negative_polarity,data=news,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, news_test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,url_test)
mean(lda.class==url_test)
#0.7072536
lda.fit=lda(url~timedelta + n_tokens_title+n_tokens_content+
              num_hrefs+ num_self_hrefs+ average_token_length+ data_channel_is_lifestyle+
              data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_tech+ kw_min_max+
              kw_min_avg+ kw_max_avg+ kw_avg_avg+self_reference_min_shares+
              self_reference_max_shares+weekday_is_tuesday+ weekday_is_wednesday+weekday_is_thursday+
              weekday_is_friday+ LDA_02+global_subjectivity+ global_rate_positive_words+
              min_positive_polarity+ avg_negative_polarity+ abs_title_subjectivity+ 
              abs_title_sentiment_polarity,data=news,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, news_test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,url_test)
mean(lda.class==url_test)
#0.7082294
lda.fit=lda(url~.-shares,data=news,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, news_test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,url_test)
mean(lda.class==url_test)
#0.6709314
#all predictor，decrease accuracy rate


# Quadratic Discriminant Analysis worse
qda.fit=qda(url~timedelta+n_tokens_title+num_hrefs+data_channel_is_entertainment
            +kw_min_avg+kw_max_avg+kw_avg_avg+self_reference_min_shares+global_subjectivity+
              avg_negative_polarity,data=news,subset=train)
qda.fit
qda.class=predict(qda.fit,news_test)$class
table(qda.class,url_test)
mean(qda.class==url_test)
#0.6837255
qda.fit=qda(url~timedelta + n_tokens_title+n_tokens_content+
              num_hrefs+ num_self_hrefs+ average_token_length+ data_channel_is_lifestyle+
              data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_tech+ kw_min_max+
              kw_min_avg+ kw_max_avg+ kw_avg_avg+self_reference_min_shares+
              self_reference_max_shares+weekday_is_tuesday+ weekday_is_wednesday+weekday_is_thursday+
              weekday_is_friday+ LDA_02+global_subjectivity+ global_rate_positive_words+
              min_positive_polarity+ avg_negative_polarity+ abs_title_subjectivity+ 
              abs_title_sentiment_polarity,data=news,subset=train)
qda.fit
qda.class=predict(qda.fit,news_test)$class
table(qda.class,url_test)
mean(qda.class==url_test)
#0.6414941
qda.fit=qda(url~.-shares,data=news,subset=train)
qda.fit
qda.class=predict(qda.fit,news_test)$class
table(qda.class,url_test)
mean(qda.class==url_test)
#0.3663667



#random forest classification
set.seed(1)
train=sample(1:nrow(news),2000)
bag.news=randomForest(url~.-shares,data=news,subset=train,mtry=17,ntree=34)
yhat.bag = predict(bag.news,newdata=news[-train,])
news.test=news[-train,"url"]
mean(yhat.bag==news.test)
#0.7176316
set.seed(1)
rf.news=randomForest(url~.-shares,data=news,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.news,newdata=news[-train,])
mean(yhat.bag==news.test)
#0.7176316
varImpPlot(rf.news)
important=importance(rf.news)
important
newlist=important[,4]
x<-max(important[,4])/100
list=newlist/x
list
list=list[order(list)]
list
list=rev(list)
newlist1=c(list[[1]],list[[2]],list[[3]],list[[4]],list[[5]],list[[6]],list[[7]],
           list[[8]],list[[9]],list[[10]],list[[11]],list[[12]],list[[13]],list[[14]],list[[15]])

dataset=data.frame(newlist1,names(list)[1:15])
dataset
names(dataset)=c("meandecreaseGini","variablenames")
barplot(rev(dataset$meandecreaseGini),horiz=T,xlim=c(-50,100),axes=F,col="red") 
text(seq(from=0.6,length.out=125,by=1.2),x=-10,label=rev(dataset$variablenames))
length(dataset$variablenames)
axis(3,c(0,20,40,60,80,100),c("0","20","40","60","80","100"))


set.seed(1)
train=sample(1:nrow(news),2000)
bag.news=randomForest(url~timedelta+n_tokens_title+num_hrefs+data_channel_is_entertainment
                      +kw_min_avg+kw_max_avg+kw_avg_avg+self_reference_min_shares+global_subjectivity+
                        avg_negative_polarity,data=news,subset=train,mtry=10,ntree=34)
yhat.bag = predict(bag.news,newdata=news[-train,])
news.test=news[-train,"url"]
mean(yhat.bag==news.test)
#0.7097332
set.seed(1)
rf.news=randomForest(url~timedelta+n_tokens_title+num_hrefs+data_channel_is_entertainment
                     +kw_min_avg+kw_max_avg+kw_avg_avg+self_reference_min_shares+global_subjectivity+
                       avg_negative_polarity,data=news,subset=train,mtry=3,importance=TRUE)
yhat.rf = predict(rf.news,newdata=news[-train,])
mean(yhat.bag==news.test)
#0.7097332


set.seed(1)
train=sample(1:nrow(news),2000)
bag.news=randomForest(url~timedelta + n_tokens_title+n_tokens_content+
                        num_hrefs+ num_self_hrefs+ average_token_length+ data_channel_is_lifestyle+
                        data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_tech+ kw_min_max+
                        kw_min_avg+ kw_max_avg+ kw_avg_avg+self_reference_min_shares+
                        self_reference_max_shares+weekday_is_tuesday+ weekday_is_wednesday+weekday_is_thursday+
                        weekday_is_friday+ LDA_02+global_subjectivity+ global_rate_positive_words+
                        min_positive_polarity+ avg_negative_polarity+ abs_title_subjectivity+ 
                        abs_title_sentiment_polarity,data=news,subset=train,mtry=27,ntree=34)
yhat.bag = predict(bag.news,newdata=news[-train,])
news.test=news[-train,"url"]
mean(yhat.bag==news.test)
#0.7112415
set.seed(1)
rf.news=randomForest(url~timedelta + n_tokens_title+n_tokens_content+
                       num_hrefs+ num_self_hrefs+ average_token_length+ data_channel_is_lifestyle+
                       data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_tech+ kw_min_max+
                       kw_min_avg+ kw_max_avg+ kw_avg_avg+self_reference_min_shares+
                       self_reference_max_shares+weekday_is_tuesday+ weekday_is_wednesday+weekday_is_thursday+
                       weekday_is_friday+ LDA_02+global_subjectivity+ global_rate_positive_words+
                       min_positive_polarity+ avg_negative_polarity+ abs_title_subjectivity+ 
                       abs_title_sentiment_polarity,data=news,subset=train,mtry=9,importance=TRUE)
yhat.rf = predict(rf.news,newdata=news[-train,])
mean(yhat.bag==news.test)
#0.7112415




#SVM 
#radical
dim(news)
train=sample(dim(news)[1],3000)
tune.out=tune(svm, url~timedelta + n_tokens_title+n_tokens_content+
                num_hrefs+ num_self_hrefs+ average_token_length+ data_channel_is_lifestyle+
                data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_tech+ kw_min_max+
                kw_min_avg+ kw_max_avg+ kw_avg_avg+self_reference_min_shares+
                self_reference_max_shares+weekday_is_tuesday+ weekday_is_wednesday+weekday_is_thursday+
                weekday_is_friday+ LDA_02+global_subjectivity+ global_rate_positive_words+
                min_positive_polarity+ avg_negative_polarity+ abs_title_subjectivity+ 
                abs_title_sentiment_polarity, data=news[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
#summary(tune.out)
true=news[-train,"url"]
pred=predict(tune.out$best.model,newdata=news[-train,])
table(true, pred)
mean(true==pred)
#0.7146603502
tune.out=tune(svm, url~.-shares, data=news[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
true=news[-train,"url"]
pred=predict(tune.out$best.model,newdata=news[-train,])
table(true, pred)
mean(true==pred)
#0.7146603502


#linear 
set.seed(1)
tune.out=tune(svm,url~.-shares,data=news[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
true=news[-train,"url"]
pred=predict(tune.out$best.model,newdata=news[-train,])
table(true, pred)
mean(true==pred)
#0.7146039534
tune.out1=tune(svm,url~timedelta + n_tokens_title+n_tokens_content+
                 num_hrefs+ num_self_hrefs+ average_token_length+ data_channel_is_lifestyle+
                 data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_tech+ kw_min_max+
                 kw_min_avg+ kw_max_avg+ kw_avg_avg+self_reference_min_shares+
                 self_reference_max_shares+weekday_is_tuesday+ weekday_is_wednesday+weekday_is_thursday+
                 weekday_is_friday+ LDA_02+global_subjectivity+ global_rate_positive_words+
                 min_positive_polarity+ avg_negative_polarity+ abs_title_subjectivity+ 
                 abs_title_sentiment_polarity,data=news[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)
true=news[-train,"url"]
pred=predict(tune.out1$best.model,newdata=news[-train,])
table(true, pred)
mean(true==pred)


#ROC curve
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
svmfit.opt=svm(url~.-shares, data=news[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,news[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,news[train,"url"],main="Training Data")
svmfit.flex=svm(url~.-shares, data=news[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,news[train,],decision.values=T))$decision.values
rocplot(fitted,news[train,"url"],add=T,col="red")
fitted=attributes(predict(svmfit.opt,news[-train,],decision.values=T))$decision.values
rocplot(fitted,news[-train,"url"],main="Test Data")
fitted=attributes(predict(svmfit.flex,news[-train,],decision.values=T))$decision.values
rocplot(fitted,news[-train,"url"],add=T,col="red")

svmfit.opt=svm(url~.-shares, data=news[train,], kernel="linear", cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,news[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,news[train,"url"],main="Training Data")
svmfit.flex=svm(url~.-shares, data=news[train,], kernel="linear", cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,news[train,],decision.values=T))$decision.values
rocplot(fitted,news[train,"url"],add=T,col="red")
fitted=attributes(predict(svmfit.opt,news[-train,],decision.values=T))$decision.values
rocplot(fitted,news[-train,"url"],main="Test Data")
fitted=attributes(predict(svmfit.flex,news[-train,],decision.values=T))$decision.values
rocplot(fitted,news[-train,"url"],add=T,col="red")

