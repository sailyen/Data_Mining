getwd()
setwd("/Users/theresa_xie/Desktop/Practicum/Data")

#######################################################
#Data Preparation

###Clean Weather data
Weather<-read.csv("LND Weather 0917.csv", header=TRUE)
nrow(Weather)
Weather<-subset(Weather, NAME=="HEATHROW, UK")
#(LDNWeather<-sqldf("SELECT * FROM Weather WHERE Name = 'HEATHROW, UK'"))

#Dupllicated date test
nrow(Weather)
length(unique(Weather$DATE))

#Some dates have entries from both stations
Weather$DATE[duplicated(Weather$DATE)]
sqldf("SELECT * FROM Weather WHERE Date= '2011-07-21'")

#UKM00003772 records everyday
nrow(sqldf("SELECT * FROM Weather WHERE Station= 'UKM00003772'"))
length(unique(Weather$DATE))==nrow(sqldf("SELECT * FROM Weather WHERE Station= 'UKM00003772'"))

#Generate clean weather data from 01/01/2009-12/31/2017
Weather<-subset(Weather, STATION=="UKM00003772")
#[LDNWeatherC<-sqldf("SELECT * FROM LDNWeather WHERE Station= 'UKM00003772' ORDER BY Date DESC")]
nrow(Weather)
summary(Weather)

#Length check
Weather$DATE
Weather$DATE<-format(as.Date(Weather$DATE), "%m/%d/%Y")
#3285 data points (2 missing)
length(Weather$DATE)

#Reloaded incident_data with date formatted
Incident_data2<-read.csv("LFB_Incident_data_0917.csv", header=TRUE)
Incident_data2$DateOfCall
#3287 data points
length(unique(Incident_data2$DateOfCall))

#Two dates are missing from the weather (11/11/2016, 01/01/2017)
sqldf("SELECT DISTINCT DateOfCall FROM Incident_data2 WHERE DateOfCall NOT IN (SELECT Date FROM Weather)")

#Drop irrelevant columns
Weather <- Weather[ -c(5, 8:10, 12:16) ]
summary(Weather)
#Impute missing value for TAVG
#Calculate the average TAVG of 11/11 of all years to generate 11/11/2016, same with 01/01/2017
TAVG1111<-sqldf("SELECT tavg FROM Weather WHERE DATE LIKE '11/11/%'")
mean(as.numeric(TAVG1111$TAVG))

Weather[nrow(Weather) + 1,] = c("UKM00003772", "HEATHROW, UK", 51.478, -0.461, "11/11/2016", NA, 9.6)

TAVG0101<-sqldf("SELECT tavg FROM Weather WHERE DATE LIKE '01/01/%'")
mean(as.numeric(TAVG0101$TAVG))

Weather[nrow(Weather) + 1,] = c("UKM00003772", "HEATHROW, UK", 51.478, -0.461, "01/01/2017", NA, 6.0625)
nrow(Weather)

Weather$PRCP<-as.numeric(Weather$PRCP)
Weather$TAVG<-as.numeric(Weather$TAVG)
summary(Weather)
sum(is.na(Weather$PRCP))

#Create seperate cols for year and month/date
Weather$DATE_Fmtd <- strptime(as.character(TEST$DATE), format="%m/%d/%Y")
Weather$Year<-as.numeric(format(Weather$DATE_Fmtd, format = "%Y"))
Weather$MD<-format(Weather$DATE_Fmtd, format = "%m/%d")

summary(Weather)

#Impute missing PRCP as the average of the same month/day of different years
#install.packages("data.table")

library(data.table)
setDT(Weather)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
Weather[,PRCP := impute.mean(PRCP), by = MD]
summary(Weather)

write.csv(Weather, "Weather_Clean.csv" )

###Generate dataset for modeling
Incident <- read.csv("LFB_Incident.csv")
View(Incident)
#extract actual fire incidents
Fire <- subset(Incident, IncidentGroup == "Fire")
#extract necessary columns
incident <- subset(Fire, select = c("DateOfCall", "IncidentGroup", "IncGeo_BoroughCode"))
View(incident)

#by looking at the unique values in borough code column, there are a strange value "E00000000"
unique(incident$IncGeo_BoroughCode)
strangerows <- subset(incident, IncGeo_BoroughCode =="E00000000")
View(strangerows)

#to delect these rows from the dataset
incident_clean <- subset(incident, IncGeo_BoroughCode != "E00000000")
View(incident_clean)

#read demographics dataset
demographics <- read.csv("LDN_Demographics.csv")
View(demographics)

#read weather dataset
weather <- read.csv("Weather_Clean.csv")
#formatting the date for further join
weather$DATE.Fmtd <- as.Date(weather$DATE, format="%m/%d/%y")
weather$DATE_Fmtd <- format(weather$DATE.Fmtd, "%m/%d/%Y")
View(weather)

#join the dataframes by using SQL queries
library("sqldf")
inci_dt_brgh <- sqldf("SELECT DateOfCall as Date, IncGeo_BoroughCode as Borough_Code, count(*) as Fire_Number
                      FROM incident_clean GROUP BY DateOfCall, IncGeo_BoroughCode ORDER BY DateOfCall")
View(inci_dt_brgh)

#not all boroughs had fires each day, and thus we should fill up the records with zero number of fire.
#to achieve that, create a new dataframe with all boroughs&dates combinations 
#and then left join original "inci_dt_brgh" dataset
sd <- as.Date("01/01/2009", format="%m/%d/%Y")
ed <- as.Date("12/31/2017", format="%m/%d/%Y")

daily.df <- data.frame(Date = seq(sd, ed, "days"))
days <- do.call("rbind", replicate(33, daily.df, simplify = FALSE))
library(dplyr)
days.df <- arrange(days, Date)
View(days.df)

borough <- data.frame(Borough_Code = paste("E0",9000001:9000033,sep =""))
boroughs.df <- do.call("rbind", replicate(108471/33, borough, simplify = FALSE))
View(boroughs.df)

date_borough <- data.frame(Date = format(days.df$Date, "%m/%d/%Y"), Borough_Code = boroughs.df$Borough_Code)
View(date_borough)

inci_by_dt_brgh <- sqldf("SELECT date_borough.Date as Date, date_borough.Borough_Code as Borough_Code, inci_dt_brgh.Fire_Number as Fire_Number
                         FROM date_borough 
                         LEFT JOIN inci_dt_brgh ON date_borough.Date = inci_dt_brgh.Date 
                         AND date_borough.Borough_Code = inci_dt_brgh.Borough_Code")
View(inci_by_dt_brgh)

#convert NAs to zero values indicating no fire occured in certain day&borough combination
inci_by_dt_brgh[is.na(inci_by_dt_brgh)] <- 0
View(inci_by_dt_brgh)

#then follow normal process to join weather and demographics
inci_join_w <- sqldf("SELECT inci_by_dt_brgh.Fire_Number as Fire_Number, inci_by_dt_brgh.Date as Date, inci_by_dt_brgh.Borough_Code as Borough_Code, weather.TAVG as TAVG, weather.PRCP as PRCP
                     FROM inci_by_dt_brgh LEFT JOIN weather ON inci_by_dt_brgh.Date = weather.DATE_Fmtd")
inci_join_w_d <- sqldf("SELECT inci_join_w.Fire_Number as Fire_Number, inci_join_w.Date as Date, inci_join_w.Borough_Code as Borough_Code, inci_join_w.TAVG as TAVG, inci_join_w.PRCP as PRCP, 
                       demographics.Total_Population as Total_Population, demographics.Inland_Area as Inland_Area, demographics.Population_Density as Population_Density, demographics.Population_Age as Population_Age, demographics.Median_Pay as Median_Pay, demographics.Building_Age as Building_Age
                       FROM inci_join_w LEFT JOIN demographics ON inci_join_w.Borough_Code = demographics.Borough_Code")

LFB_dataset <- subset(inci_join_w_d, select = c("Fire_Number", "TAVG", "PRCP", "Total_Population", "Inland_Area", "Population_Density", "Population_Age", "Median_Pay", "Building_Age"))
View(LFB_dataset) #the one for modeling
write.csv(LFB_dataset, "LFB Project.csv")

#check for records with no fire
checks <- subset(inci_join_w_d, Fire_Number == 0)
View(checks)
nrow(checks) #result: 23761 out of 108471
#Since the number of records with no fire is not that small, 
#it is reasonable to transform "Fire_number" to dummy variable

## DV: dummy variable
Log_dataset <- LFB_dataset
Log_dataset$Fire <- ifelse(Log_dataset$Fire_Number > 0, 1, 0)
View(Log_dataset)
write.csv(Log_dataset,"proj1.csv")


df <- read.csv("proj1.csv", header = T)
head(df)

table(df$Fire)
df$Fire <- as.factor(df$Fire)

library(ggplot2)

ggplot(df, 
       aes(x = Fire, fill = Fire)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


library(reshape2)
df_cor <- melt(cor(subset(df, select = -c(Fire))))
head(df_cor)
g <- ggplot(data = df_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

df_cor>0.7# identify multicollinearity issue.
#more visualization please see in report.


library(ROSE)
data(df)
dfun <- ovun.sample(Fire~., data = df, p = 0.5, method = "under")$data
table(dfun$Fire)

write.csv(dfun,"proj2.csv")


#End of Data Preparation
#######################################################
#Model Building

library(h2o)
h2o.init(nthreads = -1)
h2o.removeAll()

args(h2o.deeplearning)

projdata <- "proj2.csv"
rawdata <- h2o.importFile(projdata)

rawdata$Fire <- as.factor(rawdata$Fire) 
h2o.levels(rawdata$Fire) 

splits <- h2o.splitFrame(data = rawdata, 
                         ratios = c(0.7, 0.15)) 
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

nrow(train) #33271
nrow(valid) #7145
nrow(test)  #7147

names(rawdata)
y <- "Fire"
x <- setdiff(names(rawdata), c(y,"Fire_Number", "C1", "X", "Population_Density"))
print(x)


#GLM
glm_fit1 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train,
                    validation_frame = valid,
                    model_id = "glm_fit1",
                    family = "binomial",
                    standardize = FALSE) 

print(glm_fit1)

glm_fit2 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train,
                    model_id = "glm_fit2",
                    validation_frame = valid,
                    family = "binomial",
                    lambda_search = TRUE)

print(glm_fit2)

glm_fit3 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train,
                    validation_frame = valid,
                    model_id = "glm_fit3",
                    family = "binomial",
                    standardize = TRUE) 
print(glm_fit3)

glm_perf1 <- h2o.performance(model = glm_fit1,
                             newdata = test)
glm_perf2 <- h2o.performance(model = glm_fit2,
                             newdata = test)
glm_perf3 <- h2o.performance(model = glm_fit3,
                             newdata = test)

h2o.auc(glm_perf1)  # 0.6822158
h2o.auc(glm_perf2)  # 0.7071392
h2o.auc(glm_perf3)  # 0.7075298 with standardization

glm_fit3@model$coefficients
glm_fit3@model$standardized_coefficient_magnitudes

##Random Forest
#ntree = 50
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            validation_frame = valid,
                            model_id = "rf_fit1")
#ntree = 100
rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            validation_frame = valid,
                            model_id = "rf_fit2",
                            ntrees = 100)

rf_fit3 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            validation_frame = valid,
                            model_id = "rf_fit3",
                            ntrees = 200)
rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = test)
rf_perf2 <- h2o.performance(model = rf_fit2,
                            newdata = test)
rf_perf3 <- h2o.performance(model = rf_fit3,
                            newdata = test)
rf_perf1
rf_perf2
rf_perf3

h2o.auc(rf_perf1)  #0.7270245
h2o.auc(rf_perf2)  #0.7285115
h2o.auc(rf_perf3)  #0.7284638
        
plot(rf_fit1)
plot(rf_perf1)

## Deep Learning

#GBM Grid Search
# GBM hyperparamters
gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

# Train and validate a cartesian grid of GBMs
gbm_grid1 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1", 
                             sort_by = "auc", 
                             decreasing = TRUE)
print(gbm_gridperf1)
#  col_sample_rate learn_rate max_depth sample_rate          model_ids                auc
#1             0.2        0.1         5         1.0 gbm_grid1_model_27 0.7266610374775381
#2             0.5        0.1         3         1.0 gbm_grid1_model_22 0.7266363507249981

# Grab the top GBM model, chosen by validation AUC
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = test)
h2o.auc(best_gbm_perf1) 
# 0.7359763

print(best_gbm1@model[["model_summary"]])
#Model Summary: 
#  number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth mean_depth min_leaves max_leaves mean_leaves
#1             100                      100               25304         0         5    3.44000          1         31    15.06000

best_gbm2 <- h2o.getModel(gbm_gridperf1@model_ids[[2]])
best_gbm_perf2 <- h2o.performance(model = best_gbm2,
                                  newdata = test)
h2o.auc(best_gbm_perf2) 
# 0.7356292


#End of Model Building
#######################################################