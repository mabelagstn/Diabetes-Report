# DATA PREPARATION
dbts = read.csv("diabetes-dataset.csv")
head(dbts)
attach(dbts)

dbts$hypertension = as.factor(hypertension)
dbts$heart_disease = as.factor(heart_disease)
dbts$diabetes = as.factor(diabetes)

head(dbts)
dim(dbts)
summary(dbts)

set.seed(1101)
# train data = 80%
train.index = sample(1:nrow(dbts))[1:(0.8*nrow(dbts))]
train.data = dbts[train.index,]
dim(train.data) # 80000 rows
# test data = 20%
test.data = dbts[-train.index,]
dim(test.data) # 20000 rows

# THE ASSOCIATION BETWEEN THE RESPONSE AND INPUT VARIABLES
### The association between gender and diabetes
table(diabetes)
gender.table1 = table(gender, diabetes)
gender.table1
gender.table2 = prop.table(gender.table1,"gender")
gender.table2

# Comments:
# Among females, around 7.6% of them have diabetes
# Among males, around 9.7% of them have diabetes
# Among the other genders, none of them has diabetes

### The association between age and diabetes
# Make age into three categories: young, middle, and old
# Young category: <15 years old
# Middle category: 15 - 64 years old
# Old category = >65 years old

age.group = ifelse(age<15, "young", ifelse(age>=15 & age<=64, "middle", "old"))
age.table1 = table(age.group, diabetes)
age.table1
age.table2 = prop.table(age.table1,"age.group")
age.table2 

# Comments:
# Among the young age group, 0.3% of them have diabetes
# Among the middle age group, 6.9% of them have diabetes
# Among the old age group, 20.5% of them have diabetes

### The association between hypertension and diabetes
# 0 is no hypertension
# 1 is have hypertension

hyper.table1 = table(hypertension, diabetes)
hyper.table1 
hyper.table2 = prop.table(hyper.table1, "hypertension")
hyper.table2

# Comments:
# Among the people who have no hypertension, 6.9% of them have diabetes
# Among the people who have hypertension, 27.9% of them have diabetes

### The association of between heart disease and diabetes
# 0 is no heart disease
# 1 is have heart disease

heart.table1 = table(heart_disease, diabetes)
heart.table1
heart.table2 = prop.table(heart.table1, "heart_disease")
heart.table2

# Comments:
# Among the people who have no heart disease, 7.5% of them have diabetes
# Among the people who have heart disease, 32.1% of them have diabetes

### The association between smoking history and diabetes

smoke.table1 = table(smoking_history, diabetes)
smoke.table1
smoke.table2 = prop.table(smoke.table1, "smoking_history")
smoke.table2

# Comments:
# Among those who are currently smoking, 10.2% of them have diabetes
# Among those who smoke sometimes, 11.8% of them have diabetes
# Among those who quit smoking, 17% of them have diabetes
# Among those who never smoke, 9.5% of them have diabetes
# Among those who has never smoked, 10.7% of them have diabetes
# Among the rest who are not included above, 4% of them have diabetes

### The association between bmi and diabetes
# Make bmi into 4 categories: underweight, healthyweight, overweight, obese
# Underweight: <18.5
# Healthyweight: 18.5-24.9
# Overweight: 25-30 
# Obese: >30 

bmi.group = ifelse(bmi<18.5,"underweight",
                   ifelse(bmi>=18.5 & bmi<25, "healthyweight",
                          ifelse(bmi>=25 & bmi<=30, "overweight",
                                 "obese")))
bmi.table1 = table(bmi.group, diabetes)
bmi.table1
bmi.table2 = prop.table(bmi.table1, "bmi.group")
bmi.table2

# Comments:
# Among those who are underweight, 0.75% of them have diabetes
# Among those who are of healthy weight, 3.9% of them have diabetes
# Among those who are overweight, 7.3% of them have diabetes
# Among those who are obese, 18% of them have diabetes

### The association between HbA1c level and diabetes
# HbA1c level is classified with three categories: low, medium, high
# Low: <5.7%
# Medium: 5.7% - 6.49%
# High: >=6.5%

Hb.group = ifelse(HbA1c_level<5.7, "low", 
                  ifelse(HbA1c_level>=5.7 & HbA1c_level<6.5, "medium","high"))
Hb.table1 = table(Hb.group, diabetes)
Hb.table1
Hb.table2 = prop.table(Hb.table1, "Hb.group")
Hb.table2

# Comments:
# Among those who have low HbA1c level, 0% of them have diabetes
# Among those who have medium HbA1c level, 8% of them have diabetes
# Among those who have high HbA1c level, 24.96% of them have diabetes

### The association between blood glucose level and diabetes
# Blood glucose level is classified with three categories: low, medium, high
# Low: <140 mg/dL
# Medium: 140-199 mg/dL
# High: >=200 mg/dL

bgl.group = ifelse(blood_glucose_level<140, "low",
                  ifelse(blood_glucose_level>=140 & blood_glucose_level<200, "medium", "high"))
bgl.table1 = table(bgl.group, diabetes)
bgl.table1
bgl.table2 = prop.table(bgl.table1, "bgl.group")
bgl.table2

# Comments:
# Among those who have low blood glucose level, 3% of them have diabetes
# Among those who have medium blood glucose level, 7.1% of them have diabetes
# Among those who have high blood glucose level, 36% of them have diabetes



# CLASSIFIER 1: NAIVE BAYES
library(e1071)
nb.model = naiveBayes(diabetes ~ ., train.data)

### Testing the goodness-of-fit (Naive Bayes)
### Evaluation method 1: ROC curve
library(ROCR)
nb.rawresult = predict(nb.model, test.data[,-9], type = "raw")
nb.rawresult

nb.score = nb.rawresult[,2] # only take the probability of yes

nb.pred = prediction(nb.score, test.data$diabetes)
nb.roc = performance(nb.pred, measure ="tpr", x.measure = "fpr")
plot(nb.roc, main = "ROC Curve of Naive Bayes", col = "red")

### Evaluation method 2: AUC
nb.auc = performance(nb.pred, "auc")@y.values[[1]]
nb.auc # 0.9506937

### Evaluation method 3: False Negative Rate (FNR)
nb.threshold = round(as.numeric(unlist(nb.roc@alpha.values)), 4)
nb.fpr = round(as.numeric(unlist(nb.roc@x.values)),4)
nb.tpr = round(as.numeric(unlist(nb.roc@y.values)),4)
nb.bind = cbind(nb.threshold, nb.fpr, nb.tpr)

prop.table(table(diabetes))
nb.bind[3700:3800,]
nb.bind[3692,] # threshold = 0.0823, fpr = 0.1223, tpr = 0.8648

dt.cm = table(test.data$diabetes, ifelse(nb.rawresult[,2] > 0.0823, "1", "0"))
dt.cm
dt.fnr = dt.cm[2,1]/sum(dt.cm[2,])
dt.fnr # 0.1351512

# CLASSIFIER 2: DECISION TREES
library("rpart")
library("rpart.plot")

dt.model = rpart(diabetes ~ .,
                 method = "class",
                 data = train.data,
                 control = rpart.control(),
                 parms = list(split = 'information'))

rpart.plot(dt.model, type = 4, extra = 2, clip.right.labs = FALSE, varlen = 0, faclen = 0)

dt.result = predict(dt.model, newdata = test.data[,-9], type = 'prob')
dt.result

data.frame(test.data$diabetes, dt.result)

### Testing the goodness-of-fit (Decision Tree)
### Evaluation method 1: ROC curve
library(ROCR)
dt.result = predict(dt.model, newdata = test.data[,-9], type = 'prob')
dt.pred = prediction(dt.result[,2], test.data$diabetes)
dt.roc = performance(dt.pred, measure = "tpr", x.measure = "fpr")
plot(dt.roc, main = "ROC Curve of Decision Trees", col = "blue")

### Evaluation method 2: AUC
dt.auc = performance(dt.pred, "auc")@y.values[[1]]
dt.auc # 0.8355068

### Evaluation method 3: False Negative Rate (FNR)
# Determine a threshold
dt.threshold = round(as.numeric(unlist(dt.roc@alpha.values)), 4)
dt.fpr = round(as.numeric(unlist(dt.roc@x.values)),4)
dt.tpr = round(as.numeric(unlist(dt.roc@y.values)),4)
dt.bind = cbind(dt.threshold, dt.fpr, dt.tpr)
dt.bind
dt.bind[2,] # threshold = 1.0000, fpr = 0, tpr = 0.6

dt.cm = table(test.data$diabetes, ifelse(dt.result[,2] > 1, "1", "0"))
dt.cm
dt.fnr = dt.cm[2,1]/sum(dt.cm[2,])
dt.fnr # 1

dt.result2 = predict(dt.model, newdata = test.data[,-9], type = 'class')

dt.cm2 = table(test.data$diabetes, dt.result2)
dt.cm2
dt.fnr2 = dt.cm2[2,1]/sum(dt.cm2[2,])
dt.fnr2 # 0.3289864

# CLASSIFER 3: LOGISTIC REGRESSION
log.model = glm(diabetes~., data=train.data, family = binomial(link="logit"))
summary(log.model)

# Comments:
# The p-values for gender(other) and smoking history(ever, former, never, not current) are large, suggesting that gender doesn't contribute significantly to the model when predicting the response

log.result = predict(log.model, newdata = test.data[,-9], type = "response")
log.result
data.frame(test.data$diabetes, log.result)

### Testing the goodness-of-fit (Logistic Regression)
### Evaluation method 1: ROC curve
library(ROCR)
log.pred = prediction(log.result, test.data$diabetes)
log.roc = performance(log.pred, "tpr", "fpr")
plot(log.roc, main = "ROC Curve of Logistic Regression", col = "black")

### Evaluation method 2: AUC
log.auc = performance(log.pred, "auc")@y.values[[1]]
log.auc # 0.962497

### Evaluation method 3: FNR
# Determine a threshold
log.threshold = round(as.numeric(unlist(log.roc@alpha.values)), 4)
log.fpr = round(as.numeric(unlist(log.roc@x.values)),4)
log.tpr = round(as.numeric(unlist(log.roc@y.values)),4)
log.bind = cbind(log.threshold, log.fpr, log.tpr)

prop.table(table(diabetes)) # 0.085 = 8.5% of them have diabetes
log.bind[3621,] # threshold = 0.0823, fpr = 0.1171, tpr = 0.8892

log.cm = table(test.data$diabetes, ifelse(log.result > 0.0821, "1", "0"))
log.cm
log.fnr = log.cm[2,1]/sum(log.cm[2,])
log.fnr # 0.1108477 



# Comparison of ROC curve
plot(dt.roc, col = "blue")
plot(nb.roc, col = "red", add=TRUE)
plot(log.roc, col = "black", add=TRUE)
legend("bottomright", legend = c("Naive Bayes", "Decision Trees", "Logistic Regression"), col = c("red","blue","black"), pch = c(20,20,20))
