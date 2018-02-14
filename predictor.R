library(stringr)
library(caret)
library(readr)
library(data.table)
library(randomForest)
library(Boruta)
library(plotly)
library(usdm)
library(dplyr)

options(scipen=999)
setwd("C:/Users/mjoy/RTScrape")
#read in datasets 
RTtotal <- read_csv("RTtotal.csv")
mdb <- read.csv("movies.csv")

#merge datasets to fill in NAs for box_office profits
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

#get dataframe with values of box office that are missing 
rttotal2 <- RTtotal2 %>%
  select(., -one_of(namest)) %>%
  mutate(., genre = word(RTtotal2$genre, 1),
         mp_rating = word(RTtotal2$mp_rating, 1)) %>%
  filter(., !is.na(box_office))


rttotal2$genre <- gsub(",", "",rttotal2$genre)
rttotal2$mp_rating <- as.factor(rttotal2$mp_rating)
rttotal2$genre <- as.factor(rttotal2$genre)

#Exploratory Analysis
# get only numeric columns to look at correlation
rttotal <- sapply(rttotal2, is.numeric)
rttotal <- rttotal2[ ,rttotal]
rttotal$interval <- NULL
vifcor(rttotal) #check for colinearity
mod <- lm(box_office ~ ., rttotal2)
plot(mod)


# gen <- rttotal2 %>%
#   group_by(., genre) %>%
#   summarise(., Total = length(genre))
# plot_ly(rttotal,x = ~year ,y = ~box_office, 
#         type = "heatmap")
rt_train <- rttotal2
rt_train$year <- NULL

#impute na 
preproc <- preProcess(rt_train, method = c("knnImpute","center",
                                           "scale"))
rt_proc <- predict(preproc, rt_train)
rt_proc$box_office <- rt_train$box_office
sum(is.na(rt_proc))

titles <- rt_proc$titles
rt_proc$titles <- NULL

dmy <- dummyVars(" ~ .", data = rt_proc,fullRank = T)
rt_transform <- data.frame(predict(dmy, newdata = rt_proc))

index <- createDataPartition(rt_transform$box_office, p =.75, list = FALSE)
train_m <- rt_transform[index, ]
rt_test <- rt_transform[-index, ]
str(rt_train)           

y_train <- train_m$box_office
y_test <-rt_test$box_office


rt_test$box_office <- NULL

#selected feature attributes
boruta.train <- Boruta(box_office~., train_m, doTrace =1)

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

z <- varImp(object=model_lm)
z <- setDT(z, keep.rownames =  TRUE)
z$model <- NULL
z$calledFrom <- NULL
plot(varImp(object=model_lm),main="Linear Model Variable Importance")
z <- varImp(object=model_lm)
imp <- z[["importance"]]
imp <- as.data.frame(imp)
row <- as.vector(rownames(imp))
imp$feat <- row
imp$Overall <- (as.vector(imp$Overall))
imp$Overall <- sort(imp$Overall)
imp$feat <- sort(imp$feat)
m <- list(
  l = 200,
  #r = 50,
  #b = 100,
  pad = 4
)

plot_ly(imp) %>%
  add_trace(x = ~Overall, y = ~feat, type = 'bar',
            marker = list(color = 'rgb(58,200,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
  layout(autosize = F, width = 800, height = 400, margin = m,
         yaxis = list(
                      title = "Attribute"),
         title = "Linear Model Important Attributtes")


 predictions<-predict.train(object=model_lm,rt_test[,predictors],type="raw")
table(predictions)

#get coeff
interc <- coef(model_lm$finalModel)
slope <- coef(model_lm$finalModel)
ggplot(data = rt_train, aes(y = box_office)) +
  geom_point() +
  geom_abline(slope = slope, intercept = interc, color = 'red')
   


