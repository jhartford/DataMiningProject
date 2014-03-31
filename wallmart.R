library(randomForest)
library(quantreg)
#Functions....

# calculate the weighted average error...
wmae <- function(isHoliday,predicted,actual){
  error <- abs(actual-predicted)
  weights = 4*as.numeric(isHoliday)+1
  w.sum = sum(weights)
  return((1/w.sum)*sum(weights*error))
}

# get lagged value and set first value to median
lag.med <- function(data, k = 1){
  return(c(rep(median(data),k),data[1:(length(x)-k)]))
}

# get 'future lagged' booleand and set last value to median
lag.fut <- function(data, k = 1){
  return(c(data[(1+k):length(data-k)],rep(FALSE,k)))
}

prepData <- function(data){
  data$Store <- as.factor(data$Store)
  data$Dept <- as.factor(data$Dept)
  data <- merge(data,stores)
  #data$month <- as.numeric(substr(data,start=6,stop=7))
  return(data)
}

outputModel <- function(model, data,cpi = FALSE){
  if (cpi){
    out <- predict(model,newdata = data)*data$CPI
  }
  if (!cpi){
    out <- predict(model,newdata = data)
  }
  return(data.frame(Id = sampleSubmission$Id, Weekly_Sales = out))
}


#modify feature set to estimate data
get.Week <- function(date){
  startdate <- as.Date(paste(format(date,"%Y"),"-01-01",sep="")) - 1
  weeks <- (date-startdate)/7
  weeks <- ceiling(weeks)
  weeks[weeks>52] <- 52
  return(weeks)
}

est.markDown <- function(data, var, method = "lm"){
  formu <- as.formula(paste(var,"~week + factor(Store)"))
  if (method == "lm"){
    mod <- lm(formu,data = data[!is.na(features1[,c(var)]),])}
  else if (method =="rq"){
    mod <- rq(formu,data = data[!is.na(features1[,c(var)]),])
  }
  else{
    return()
  }
  pred <- predict(mod,newdata=data[is.na(features1[,c(var)]),])
  out <- data[,c(var)]
  out[is.na(out)] <- pred
  return(out)
}

normalizeCPI <- function(data){
  for (store in levels(factor(data$Store))){
    mindate <- min(data[data$Store==store,c("rdate")])
    maxdate <- max(data[data$Store==store & !is.na(data$CPI),c("rdate")])
    base <- data[data$Store==store & data$rdate==mindate,c("CPI")]
    final <- data[data$Store==store & data$rdate==maxdate,c("CPI")]
    inflation <- (final/base-1)^(1/as.numeric(mindate - maxdate))
    empty <- data[data$Store==store & data$rdate>maxdate,c("CPI")]
    growth <- cumprod(rep(inflation,length(empty)))*final
    data[data$Store==store,c("CPI")] <- data[data$Store==store,c("CPI")]/base
    data[data$Store==store & data$rdate>maxdate,c("CPI")] <- growth
  }
  return(data$CPI)
}

realValues <- function(data,variables){
  for (val in variables){
    data[,c(val)] <-data[,c(val)]/data$CPI
  }
  return(data)
}

sampleSubmission <- read.csv("sampleSubmission.csv")
stores <- read.csv("stores.csv")
test <- read.csv("test.csv")
train <- read.csv("train.csv")

features <- read.csv("features.csv")
features$rdate <- as.Date(features$Date)
features$month <- months(features$rdate)
features$year <- substr(features$Date,start=1,stop=4)
features$week <- get.Week(features$rdate)
features$week <- factor(features$week)
features1 <- merge(features,stores)
features1$CPI <- normalizeCPI(features1)
features1 <- realValues(features1,c("Fuel_Price","MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5"))
#features1$md1 <- est.markDown(features1,"MarkDown1",method="rq")
features1$md1 <- est.markDown(features1,"MarkDown1")
features1$md2 <- est.markDown(features1,"MarkDown2")
features1$md3 <- est.markDown(features1,"MarkDown3")
features1$md4 <- est.markDown(features1,"MarkDown4")
features1$md5 <- est.markDown(features1,"MarkDown5")

trainfull <- prepData(train)
testfull <- prepData(test)

trainfull <- merge(trainfull,features1)
trainfull$Weekly_Sales <- trainfull$Weekly_Sales/trainfull$CPI
testfull <- merge(testfull,features1)

# Create subsample for testing purposes
set.seed(2)
nobs <- nrow(train)
size.train <- floor(nobs*0.7)
size.test <- nobs - size.train
idx.train <- sort(sample(c(1:nobs),size.train))
idx.test <- c(1:nobs)[-idx.train]
sub.train <- trainfull[idx.train,]
sub.test <- trainfull[idx.test,]

mod.glm <- glm(normalised_sales~IsHoliday+Type,data=sub.train)
mod.lm <- lm(normalised_sales~IsHoliday+Store+Dept+Type+Unemployment+CPI+Fuel_Price+Temperature,data=sub.train)
mod.lm1 <- lm(Weekly_Sales~Size+IsHoliday+Store+Dept+Fuel_Price+Temperature,data=trainfull)
mod.lm <- lm(Weekly_Sales~Size+IsHoliday+Store+Dept+Fuel_Price+Temperature+md1+md3+md4,data=trainfull)
mod.lagged <- lm(normalised_sales~IsHoliday+Type,data=sub.train)
mod.wlm <- lm(normalised_sales~Weekly_Sales+IsHoliday+Type,data=sub.train,weights=(4*IsHoliday+1))
mod.rf <- randomForest(Weekly_Sales~Size+IsHoliday+Fuel_Price+Temperature+md1+md3+md4,data=trainfull,ntree=10,do.trace=TRUE)

mod.rf <- randomForest(y = sub.train$Weekly_Sales, 
                       x = sub.train[,c("Size","IsHoliday","Fuel_Price","Temperature","md1","md3","md4")],
                       ytest=sub.train$Weekly_Sales,
                       xtest=sub.test[,c("Size","IsHoliday","Fuel_Price","Temperature","md1","md3","md4")],
                       ntree=100,do.trace=TRUE)


wmae(sub.test$IsHoliday,predict(mod.lm,newdata = sub.test),sub.test$Weekly_Sales)
#wmae(sub.test$IsHoliday,predict(mod.rf,newdata = sub.test),sub.test$Weekly_Sales)

lmout1 <- outputModel(mod.lm1,testfull)
lmout <- outputModel(mod.lm,testfull,cpi=TRUE)
rfout <- outputModel(mod.rf,testfull)
write.csv(outputModel(mod.rf,testfull), file="rf.csv", row.names=FALSE)
write.csv(outputModel(mod.lm,testfull), file="lm.csv", row.names=FALSE)
