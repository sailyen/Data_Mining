###Assignment 002
###Shuwen WANG, Jiafeng XU, Tingyu XIE

df <- read.csv("train_set.csv", header = T)
head(df)

##problem 1
df$Claim_Amount <- ifelse(df$Claim_Amount>0, 1, 0)
table(df$Claim_Amount)

library(ROSE)
##problem 2
data(df)
dfun <- ovun.sample(Claim_Amount~., data = df, p = 0.5, method = "under")$data
table(dfun$Claim_Amount)


##problem 3
library(caret)
trainIndex <- createDataPartition(dfun$Claim_Amount, p = .7, list = F, times = 1)
train <- dfun[trainIndex,]
test <- dfun[-trainIndex,]


##problem 4
library(mice)
library(VIM)
library(missForest)
#train
train[train == "?"] <- NA
sapply(train, function(x) sum(is.na(x)) )
md.pattern(train)
mis_plot <- aggr(train, col=c('navyblue','red'),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(train), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))
miss <- c("Cat11", "Cat1", "Cat6","OrdCat","Cat10", "Cat3", "Cat8")
miss_drop <- c("Cat7", "Cat5", "Cat4", "Cat2")
#drop columns contain too many missing values
train_miss_drop <- train[, -which(names(train) %in% miss_drop)]
train_miss_drop <- train_miss_drop[complete.cases(train_miss_drop[ , "Blind_Make"]), ]
sapply(train_miss_drop, function(x) sum(is.na(x)) )

#impute missing values
train_miss_imp <- train_miss_drop[,which(names(train_miss_drop) %in% miss)]
train_imp <- missForest(train_miss_imp)
train_imp$ximp
train_imp$OOBerror
train.imp <- data.frame(train_imp$ximp)
train_excMissing <- cbind(train_miss_drop[, -which(names(train_miss_drop) %in% miss)], train.imp)


#test
test[test == "?"] <- NA
sapply(test, function(x) sum(is.na(x)) )

#drop columns contain too many missing values
test_miss_drop <- test[, -which(names(test) %in% miss_drop)]
test_miss_drop <- test_miss_drop[complete.cases(test_miss_drop[ , "Blind_Make"]), ]
sapply(test_miss_drop, function(x) sum(is.na(x)) )

#impute missing values
test_miss_imp <- test_miss_drop[,which(names(test_miss_drop) %in% miss)]
test_imp <- missForest(test_miss_imp)
test_imp$ximp
test_imp$OOBerror
test.imp <- data.frame(test_imp$ximp)
test_excMissing <- cbind(test_miss_drop[, -which(names(test_miss_drop) %in% miss)], test.imp)



##Problem 5
library(sqldf)
##target encoding
#train
enc_B1 <- sqldf("SELECT Blind_Make, AVG(Claim_Amount) as encoding_BMake
                From train_excMissing
                Group by Blind_Make;")

enc_B2 <- sqldf("SELECT Blind_Model, AVG(Claim_Amount) as encoding_BModel
                From train_excMissing
                Group by Blind_Model;")

enc_B3 <- sqldf("SELECT Blind_Submodel, AVG(Claim_Amount) as encoding_BSubModel
                From train_excMissing
                Group by Blind_Submodel;")

train_enc1 <- sqldf("SELECT train_excMissing.*, enc_B1.encoding_BMake
                    From train_excMissing
                    JOIN enc_B1
                    ON train_excMissing.Blind_Make = enc_B1.Blind_Make;")

train_enc2 <- sqldf("SELECT train_enc1.*, enc_B2.encoding_BModel
                    From train_enc1
                    JOIN enc_B2
                    ON train_enc1.Blind_Model = enc_B2.Blind_Model;")

train_enc3 <- sqldf("SELECT train_enc2.*, enc_B3.encoding_BSubModel
                    From train_enc2
                    JOIN enc_B3
                    ON train_enc2.Blind_Submodel = enc_B3.Blind_Submodel;")

encode <- c("Blind_Submodel", "Blind_Model", "Blind_Make")
train_enc <- train_enc3[, -which(names(train_enc3) %in% encode)]

#test
enc_B1_te <- sqldf("SELECT Blind_Make, AVG(Claim_Amount) as encoding_BMake
                   From test_excMissing
                   Group by Blind_Make;")

enc_B2_te <- sqldf("SELECT Blind_Model, AVG(Claim_Amount) as encoding_BModel
                   From test_excMissing
                   Group by Blind_Model;")

enc_B3_te <- sqldf("SELECT Blind_Submodel, AVG(Claim_Amount) as encoding_BSubModel
                   From test_excMissing
                   Group by Blind_Submodel;")

test_enc1 <- sqldf("SELECT test_excMissing.*, enc_B1_te.encoding_BMake
                   From test_excMissing
                   JOIN enc_B1_te
                   ON test_excMissing.Blind_Make = enc_B1_te.Blind_Make;")

test_enc2 <- sqldf("SELECT test_enc1.*, enc_B2_te.encoding_BModel
                   From test_enc1
                   JOIN enc_B2_te
                   ON test_enc1.Blind_Model = enc_B2_te.Blind_Model;")

test_enc3 <- sqldf("SELECT test_enc2.*, enc_B3_te.encoding_BSubModel
                   From test_enc2
                   JOIN enc_B3_te
                   ON test_enc2.Blind_Submodel = enc_B3_te.Blind_Submodel;")

test_enc <- test_enc3[, -which(names(test_enc3) %in% encode)]



##Problem 6
model.logistic <- glm(formula = Claim_Amount ~ ., family =binomial(link='logit'), data = train_enc)
summary(model.logistic)

model.logistic.fitted <- predict(model.logistic,test_enc,type='response')
model.logistic.fitted <- ifelse(model.logistic.fitted > 0.5,1,0)
misClassificationError <- mean(model.logistic.fitted != test_enc$Claim_Amount)
print(paste('Accuracy',1-misClassificationError))
#Accuracy 0.597449336128581

##Problem 7 
#AUC
library(ROCR)
p <- predict(model.logistic, test_enc, type="response")
pr <- prediction(p, test_enc$Claim_Amount)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b= 1)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# [1] 0.6399068
#There might be a little fluctuation due to the missForest missing value imputing.

##Problem 8
#Stepwise AIC (variable selection)
library(MASS)
step <- stepAIC(model.logistic, direction="both")
step$anova
