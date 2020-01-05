#set working directory
rm(list=ls())
setwd("C:/Users/chandini c/Desktop")
getwd()

#read csv file
day=read.csv("day.csv")

#structure of the data
str(day)
#Data overview
head(day)

#import libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information","Metrics",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies',"GGally","usdm","dplyr","plyr","data.table")
lapply(x, require, character.only = TRUE)
rm(x)

##Feature Engineering##
day$act_season = factor(x = day$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
day$act_yr = factor(x = day$yr, levels = c(0,1), labels = c("2011","2012"))
day$act_holiday = factor(x = day$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
day$act_weathersit = factor(x = day$weathersit, levels = c(1,2,3,4), 
                               labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

day$weathersit = as.factor(day$weathersit)
day$season = as.factor(day$season)
day$dteday = as.character(day$dteday)
day$mnth = as.factor(day$mnth)
day$weekday = as.factor(as.character(day$weekday))
day$workingday = as.factor(as.character(day$workingday))
day$yr = as.factor(day$yr)
day$holiday = as.factor(day$holiday)

#remove column 'instant' as it is record index and does not help us to predict any value
#remove 'casual' and 'registered' as its sum is equal to target variable 'cnt 
day=subset(day,select = -c(instant,casual,registered))
dim(day)
#structure of the data
str(day)

##Exploratory Data Analysis

# Analyze variables  by visualize

# function to create univariate distribution of numeric  variables
univariate_numeric = function(num_x) {
  
  
  ggplot(day)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}

# analyze the distribution of  target variable 'cnt'
univariate_numeric(day$cnt)

# analyse the distrubution of  independence variable 'temp'
univariate_numeric(day$temp)

# analyse the distrubution of  independence variable 'atemp'
univariate_numeric(day$atemp)

# analyse the distrubution of  independence variable 'hum'
univariate_numeric(day$hum)

# analyse the distrubution of  independence variable 'windspeed'
univariate_numeric(day$windspeed)

#Check the distribution of categorical Data using bar graph
bar1 = ggplot(data = day, aes(x = act_season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = day, aes(x = act_weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = day, aes(x = act_holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = day, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)


##Missing Values Analysis
#checking for missing value
missing_val = data.frame(apply(day,2,function(x){sum(is.na(x))}))
missing_val

##Outlier Analysis
#Check for outliers in data using boxplot
cnames = colnames(day[,c("temp","atemp","windspeed","hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)
#'hum' and 'windspeed' has outliers, which has to be removed
#Removal of outliers in numerical variables:
for(i in cnames){
  val=day[,i][day[,i]%in%boxplot.stats(day[,i])$out]
  day=day[which(!day[,i]%in%val),]
}

#Feature selection
#Check for collinearity using corelation analysis
corrgram(day, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
# correlation matrix  stating  'temp' and 'atemp' having strong relationship
# and there is no  relationship between 'hum' and 'cnt'


#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = day, aes(x =temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = day, aes(x =hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = day, aes(x =atemp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = day, aes(x =windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Remove the unwanted variables

day=subset(day, select = -c(dteday,hum,atemp,act_season,act_yr,act_holiday,act_weathersit))

rmExcept("day")


##MODELING##
train_index = sample(1:nrow(day), 0.8 * nrow(day))
train = day[train_index,]
test = day[-train_index,]

###########Decision tree regression  #################
#mae=640.7
#rmse=856.9
#mape=17.36%
#Accuracy=82.64%

fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-10])

regr.eval(trues = test[,10], preds = predictions_DT, stats = c("mae","mse","rmse","mape"))
#############Random Forest Model##########################
#mae=463.4
#rmse=629.9
#mape=13.54%
#accuracy=86.46%
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = predict(RF_model, test[,-10])

regr.eval(trues = test[,10], preds = predictions_RF, stats = c("mae","mse","rmse","mape"))
################Linear Regression#################
#mae=562.76
#rmse=722.17
#mape=17.668%
#accuracy=82.3%

#converting multilevel categorical variable into binary dummy variable
cnames= c("season","mnth","weekday","weathersit")
data_lr=day[,cnames]
cnt=data.frame(day$cnt)
names(cnt)[1]="cnt"
data_lr=fastDummies::dummy_cols(data_lr)
data_lr= subset(data_lr,select = -c(season,mnth,weekday,weathersit))
d3 = cbind(data_lr,day)
d3= subset(d3,select = -c(season,mnth,weekday,weathersit,cnt))
data_lr=cbind(d3,cnt)


#dividind data into test and train
train_index = sample(1:nrow(data_lr), 0.8 * nrow(data_lr))
train_lr = data_lr[train_index,]
test_lr = data_lr[-train_index,]

#Linear regression model making
lm_model = lm(cnt ~., data = train_lr)
summary(lm_model)
predictions_LR = predict(lm_model,test_lr[,-32])

regr.eval(trues = test_lr[,32], preds = predictions_LR, stats = c("mae","mse","rmse","mape"))

output=data.frame(test, pred_cnt = predictions_RF)

write.csv(output, file = 'RF output R .csv', row.names = FALSE, quote=FALSE)
