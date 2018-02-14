library(dplyr)
library(stringr)
library(caret)
library(readr)
library(RANN)
library(data.table)
library(tidyr)
library(randomForest)
library(Boruta)
library(uwIntroStats)

options(scipen=999)

RTtotal <- read_csv("RTtotal.csv")
mdb <- read.csv("movies.csv")

r <- RTtotal %>%
  filter(., is.na(box_office))


m <- merge(r, mdb, by = "titles")
m$total <- m$box_office.y * 1000000
m$box_office.y <- NULL

m <- m %>%
  select(., titles, total)

l <- merge(RTtotal, m, by = "titles", all.x = TRUE)

RTtotal2 <- unique(setDT(l), by = c("titles", "month")) 


RTtotal2$box_office <- ifelse((is.na(RTtotal2$box_office)) &
                                (!is.na(RTtotal2$total)),
                              RTtotal2$total , RTtotal2$box_office)


RTtotal2$total <- NULL
RTtotal2[RTtotal2$box_office == ""] <- NA
namest <- c("actor1", "actor2", "actor3")
rttotal2 <- RTtotal2 %>%
  select(., -one_of(namest)) %>%
  mutate(., genre = word(RTtotal2$genre, 1),
         mp_rating = word(RTtotal2$mp_rating, 1)) %>%
  filter(., !is.na(box_office))

rttotal2$genre <- gsub(",", "",rttotal2$genre)
rttotal2$mp_rating <- as.factor(rttotal2$mp_rating)
rttotal2$genre <- as.factor(rttotal2$genre)

rttotal2$box_office <- as.numeric(rttotal2$box_office)
rttotal2 <- rttotal2 %>%
  mutate(., interval = ifelse(rttotal2$box_office <= 302188, 1,
                              ifelse((rttotal2$box_office > 302188) & (rttotal2$box_office <= 2341226), 2,
                                     ifelse((rttotal2$box_office > 2341226) & (rttotal2$box_office <= 42785549), 3, 
                                            ifelse((rttotal2$box_office > 42785549) & (rttotal2$box_office <= 61349516), 4, 
                                                   ifelse((rttotal2$box_office>  61349516) & (rttotal2$box_office <= 936658640), 5, 6))))) )

rt_train <- rttotal2
rt_train$year <- NULL
#rt_train$box_office <- NULL
#impute na and address multicoliniearity 
preproc <- preProcess(rt_train, method = c("knnImpute","center",
                                           "scale"))
rt_proc <- predict(preproc, rt_train)
rt_proc$box_office <- rt_train$box_office
sum(is.na(rt_proc))

titles <- rt_proc$titles
rt_proc$titles <- NULL
#rt_train$interval <- as.factor(rt_train$interval)

dmy <- dummyVars(" ~ .", data = rt_proc,fullRank = T)
rt_transform <- data.frame(predict(dmy, newdata = rt_proc))

index <- createDataPartition(rt_transform$interval, p =.75, list = FALSE)
train_m <- rt_transform[index, ]
rt_test <- rt_transform[-index, ]
str(rt_train)           

y_train <- train_m$box_office
y_test <-rt_test$box_office


train_m$box_office <- NULL
rt_test$box_office <- NULL

#selected feature attributes
boruta.train <- Boruta(interval~., train_m, doTrace =1)

#graph to see most important var to interval
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)

boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
plot(boruta.train, xlab = "", xaxt = "n")
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


#get most important attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)
boruta.rt_df <- attStats(final.boruta)
boruta.rt_df
boruta.rt_df <- setDT(boruta.rt_df, keep.rownames = TRUE)[]

predictors <- boruta.rt_df %>%
  filter(., decision =="Confirmed") %>%
  select(., rn)
predictors <- unlist(predictors)

control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=6)

#look at residuals
#p-value is very small so reject H0 that predictors have no effect so 
#we can use rotten tomatoes to predict box_office ranges
train_m$interval <- NULL
model_lm <- train(train_m[,predictors],
                  y_train, method='lm',
                  trControl = control, tuneLength = 10)
model_lm #.568
# 
plot(model_lm)
 plot(model_lm)
z <- varImp(object=model_lm)
z <- setDT(z, keep.rownames =  TRUE)
z$model <- NULL
z$calledFrom <- NULL
row.names(z)
plot(varImp(object=model_lm),main="Linear Model Variable Importance")

predictions<-predict.train(object=model_lm,rt_test[,predictors],type="raw")
table(predictions)

#get coeff
interc <- coef(model_lm$finalModel)
slope <- coef(model_lm$finalModel)
ggplot(data = rt_train, aes(y = box_office)) +
  geom_point() +
  geom_abline(slope = slope, intercept = interc, color = 'red')



rfv <- summary(RTtotal2)
getModelInfo("lm", regex = TRUE)[[1]]$param
