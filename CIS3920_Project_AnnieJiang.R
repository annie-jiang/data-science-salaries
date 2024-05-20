data = read.csv(file.choose(),header=T,stringsAsFactors=TRUE)

head(data)
dim(data)
str(data)

summary(data)

attach(data)

# check for missing values
colSums(is.na(data))

# change salary_in_usd to be in thousands
data$salary_in_usd <- as.integer(data$salary_in_usd / 1000)

# rename remote ratio values
data$work_mode <- as.factor(ifelse(grepl("100", data$remote_ratio), "Fully Remote",
                                   ifelse(grepl("50", data$remote_ratio), "Hybrid",
                                          "In Office")))

# convert data type from int to factor
data$work_year <- as.factor(data$work_year)

# add a new column called work_country to simplify employee residence
data$work_country <- as.factor(ifelse(grepl("US", data$employee_residence), "US", "International"))

# rename job titles
data$job_type <- as.factor(ifelse(grepl("Scientist", data$job_title), "Scientist",
                                  ifelse(grepl("Analyst", data$job_title), "Analyst",
                                         ifelse(grepl("Engineer", data$job_title), "Engineer",
                                                ifelse(grepl("Manager", data$job_title), "Manager",
                                                       "Other")))))

# subset columns
data <- subset(data, select= c(work_year, experience_level, employment_type, salary_in_usd, company_size, work_mode, work_country, job_type))

# plotting histogram of salaries
hist(data$salary_in_usd, breaks=30, col='chartreuse3', xlab='Salaries in thousands (USD)', main='Histogram of Salaries')

par(mfrow=c(2,4))

# plotting experience level vs. salary
experience_level = with(data, reorder(experience_level, data$salary_in_usd, FUN = median))
plot(experience_level, data$salary_in_usd, col ="chartreuse3", xlab="Experience Level",ylab ="Salary in thousands (USD)",
     main="Experience Level vs. Salary", xaxt = 'n')
# rename x axis values
axis(1,at=1:length(levels(experience_level)), labels= c('Entry-Level','Mid-Level','Senior-Level','Executive-Level'))

# company size vs. salary
company_size = with(data, reorder(company_size, data$salary_in_usd, FUN = median))
plot(company_size, data$salary_in_usd, col ="chartreuse3", xlab="Company Size",ylab ="Salary in thousands (USD)",
     main="Company Size vs. Salary")

# employment_type vs. salary
boxplot(data$salary_in_usd ~ employment_type, data=data, col ="chartreuse3", xlab="Employment Type",ylab ="Salary in thousands (USD)",
        main="Employment Type vs. Salary")

# remote ratio vs. salary
boxplot(data$salary_in_usd ~ work_mode, data=data, col ="chartreuse3", xlab="Remote Ratio",ylab ="Salary in thousands (USD)",
     main="Remote Ratio vs. Salary")

# work year vs. salary
boxplot(data$salary_in_usd ~ work_year, data=data, col ="chartreuse3", xlab="Work Year",ylab ="Salary in thousands (USD)",
        main="Work Year vs. Salary")

# work_country vs. salary
boxplot(data$salary_in_usd ~ work_country, data=data, col ="chartreuse3", xlab="Work Country",ylab ="Salary in thousands (USD)",
        main="Work Country vs. Salary")

# plot job titles vs. salary
boxplot(data$salary_in_usd ~ job_type, data = data, col ="chartreuse3", 
        xlab="Job Title", ylab ="Salary in thousands (USD)", main="Job Title vs. Salary")

# fit all input variables into a multiple linear regression model
# model 1 using all the variables
model1 = lm(salary_in_usd ~ work_year + experience_level + employment_type + job_type + work_mode + company_size + work_country, data=data)
summary(model1)

# diagnostic plots
par(mfrow=c(1,2))
plot(predict(model1), residuals(model1))
plot(predict(model1), rstudent(model1))

# 10-fold cross validation
k=10
M1CVMSE = rep(0,k)

set.seed(1)
folds = sample(1:k,nrow(data),replace=TRUE)

for(j in 1:k)
{
  M1CV=lm(salary_in_usd ~ .,data=data[folds!=j,])
  M1CVMSE [j]=mean((data$salary_in_usd-predict(M1CV,data))[folds==j]^2)
}


MeanM1MSE=mean(M1CVMSE)
MeanM1MSE # 2656.822

# perform backward stepwise selection
library(MASS)

stepwise_model = step(lm(salary_in_usd ~ ., data=data), direction='backward')
summary(stepwise_model)

# diagnostic plots
par(mfrow=c(1,2))
plot(predict(stepwise_model), residuals(stepwise_model))
plot(predict(stepwise_model), rstudent(stepwise_model))

# 10-fold cross validation
k=10
STEPCVMSE = rep(0,k)

set.seed(1)
folds = sample(1:k,nrow(data),replace=TRUE)

for(j in 1:k)
{
  STEPCV=step(lm(salary_in_usd ~ ., data=data[folds!=j,]), direction='backward')
  STEPCVMSE [j]=mean((data$salary_in_usd-predict(STEPCV,data))[folds==j]^2)
}

MeanSTEPMSE=mean(STEPCVMSE)
MeanSTEPMSE # 2646.827
# salary ~ experience_level + company_size + work_country + job_type

# collinearity
library(car)
vif(model1)
vif(stepwise_model)

# regression tree
library(tree)

# create a training set with 50/50 hold out
set.seed(1)

train = sample(1:nrow(data), nrow(data)/2)

tree.data=tree(data$salary_in_usd ~ .,data,subset=train)

cv.data=cv.tree(tree.data,K=10)
cv.data

# prune tree to 10, lowest cross-validation error rate
prune.data=prune.tree(tree.data,best=10)

par(mfrow=c(1,1))
plot(prune.data)
text(prune.data,pretty=0)

data.test=data[-train,"salary_in_usd"]

tree.pred=predict(prune.data,newdata=data[-train,])
mean((tree.pred-data.test)^2)
# MSE: 2609.618

# random forest
library(randomForest)

set.seed(1)

bag.data=randomForest(salary_in_usd ~ ., data=data, subset=train, mtry=7, importance=TRUE)
bag.data

yhat.bag = predict(bag.data,newdata=data[-train,])
mean((yhat.bag-data.test)^2)
# MSE: 2767.48

set.seed(1)
# use 3 for mtry since p/3
rf.data=randomForest(salary_in_usd~.,data=data,subset=train,mtry=3,importance=TRUE)
yhat.rf = predict(rf.data,newdata=data[-train,])
mean((yhat.rf-data.test)^2)
# MSE: 2518.181

importance(rf.data)
varImpPlot(rf.data)


