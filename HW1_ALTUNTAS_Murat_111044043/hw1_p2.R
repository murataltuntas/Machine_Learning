#####################################################################
############################### Library #############################
#####################################################################

library(kernlab)
library(pROC)
library(ROCR)
library(class)

#####################################################################
############################## Functions ############################
#####################################################################
CreateTable <- function(x1,x2) {
	total <- matrix(0,36,36)

	for(i in 1:34) {
		total[x1[i],x2[i]] <- total[x1[i],x2[i]] + 1
	}
	return(total)
}

#####################################################################
########################### iris(linear svm) ########################
#####################################################################

#Randomly shuffle the data
iris<-iris[sample(nrow(iris)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(iris)),breaks=10,labels=FALSE)
gp <- runif(nrow(iris))   # random siralama
iris <- iris[order(gp),]
totalMatLinear <- matrix(0,3,3)
all_predict_L <- c()
all_iris_test_target <- c()
for(i in 1:10){
	#-- train ve test olarak ayırma --#
	#Segement your data by fold using the which() function 
	testIndexes <- which(folds==i,arr.ind=TRUE)
	iris_test <- iris[testIndexes, ]
	iris_train <- iris[-testIndexes, ]
	iris_train_target <- iris[-testIndexes, 5]   # 5 => kolon numarası
	iris_test_target <- iris[testIndexes, 5]
	all_iris_test_target <- c(all_iris_test_target, iris_test_target)

	filter_Linear <- ksvm(Species~.,data=iris_train, kernel="vanilladot", prob.model = TRUE)	

	iris_type_L <- predict(filter_Linear,iris_test, type = "prob")

	all_predict_L <- rbind(all_predict_L, iris_type_L)

	iris_type_L <- predict(filter_Linear,iris_test, type = "response")
	
	table(iris_test_target, iris_type_L)
	totalMatLinear <- as.matrix(table(iris_test_target, iris_type_L)) + totalMatLinear
}

pred <- prediction( all_predict_L[,1], all_iris_test_target == 1)
perf <- performance( pred, "tpr", "fpr" )
xValues <- unlist(perf@x.values)
yValues <- unlist(perf@y.values)
aValues <- unlist(perf@alpha.values)
for(i in 2:length(levels(factor(iris[[5]])))){
	pred <- prediction( all_predict_L[,i], all_iris_test_target == i)
	perf <- performance( pred, "tpr", "fpr" )
	xValues <- xValues + unlist(perf@x.values)
	yValues <- yValues + unlist(perf@y.values)
	aValues <- aValues + unlist(perf@alpha.values)
}
perf@x.values <- list(xValues / length(levels(factor(iris[[5]]))))
perf@y.values <- list(yValues / length(levels(factor(iris[[5]]))))
perf@alpha.values <- list(aValues / length(levels(factor(iris[[5]]))))
plot( perf, col = "blue")

print(totalMatLinear)
CrossTable(totalMatLinear, prop.chisq = FALSE)
cat("Linear SVM: %", (sum(diag(totalMatLinear)) / sum(totalMatLinear) * 100))

#####################################################################
###################### iris(polynomial svm) #########################
#####################################################################

#Randomly shuffle the data
iris<-iris[sample(nrow(iris)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(iris)),breaks=10,labels=FALSE)
gp <- runif(nrow(iris))   # random siralama
iris <- iris[order(gp),]
totalMatPoly <- matrix(0,3,3)
all_predict_P <- c()
all_iris_test_target <- c()
for(i in 1:10){
	#-- train ve test olarak ayırma --#
	#Segement your data by fold using the which() function 
	testIndexes <- which(folds==i,arr.ind=TRUE)
	iris_test <- iris[testIndexes, ]
	iris_train <- iris[-testIndexes, ]
	iris_train_target <- iris[-testIndexes, 5]   # 5 => kolon numarası
	iris_test_target <- iris[testIndexes, 5]
	all_iris_test_target <- c(all_iris_test_target, iris_test_target)

	filter_Poly <- ksvm(Species~.,data=iris_train, kernel="polydot", prob.model = TRUE)

	iris_type_P <- predict(filter_Poly,iris_test, type = "prob")

	all_predict_P <- rbind(all_predict_P, iris_type_P)

	iris_type_P <- predict(filter_Poly,iris_test, type = "response")
	
	table(iris_test_target, iris_type_P)
	totalMatPoly <- as.matrix(table(iris_test_target, iris_type_P)) + totalMatPoly

}

pred <- prediction( all_predict_P[,1], all_iris_test_target == 1)
perf <- performance( pred, "tpr", "fpr" )
xValues <- unlist(perf@x.values)
yValues <- unlist(perf@y.values)
aValues <- unlist(perf@alpha.values)
for(i in 2:length(levels(factor(iris[[5]])))){
  pred <- prediction( all_predict_P[,i], all_iris_test_target == i)
  perf <- performance( pred, "tpr", "fpr" )
  xValues <- xValues + unlist(perf@x.values)
  yValues <- yValues + unlist(perf@y.values)
  aValues <- aValues + unlist(perf@alpha.values)
}
perf@x.values <- list(xValues / length(levels(factor(iris[[5]]))))
perf@y.values <- list(yValues / length(levels(factor(iris[[5]]))))
perf@alpha.values <- list(aValues / length(levels(factor(iris[[5]]))))
plot( perf, col = "blue")

print(totalMatPoly)
CrossTable(totalMatPoly, prop.chisq = FALSE)
cat("Polynomial SVM: %", (sum(diag(totalMatPoly)) / sum(totalMatPoly) * 100))




#####################################################################
########################## leaf(Linear svm) #########################
#####################################################################

leaf <- read.csv("C:\\Users\\Murat\\Desktop\\ss\\ML\\HW\\HW1\\leaf.csv", header = FALSE)
#Randomly shuffle the data
leaf<-leaf[sample(nrow(leaf)),]

#set class as factor
leaf[[1]]=factor(leaf[[1]])

#Create 10 equally size folds
folds <- cut(seq(1,nrow(leaf)),breaks=10,labels=FALSE)
gp <- runif(nrow(leaf))   # random siralama
leaf <- leaf[order(gp),]
totalMatLinear <- matrix(0,36,36)
all_predict_L <- c()
all_leaf_test_target <- c()

for(i in 1:10){
	#-- train ve test olarak ayırma --#
	#Segement your data by fold using the which() function 
	testIndexes <- which(folds==i,arr.ind=TRUE)
	leaf_test <- leaf[testIndexes, ]
	leaf_train <- leaf[-testIndexes, ]
	leaf_train_target <- leaf[-testIndexes, 1]   # 1 => kolon numarası
	leaf_test_target <- leaf[testIndexes, 1]

	all_leaf_test_target <- c(all_leaf_test_target, leaf_test_target)

	filter_Linear <- ksvm(V1~.,data=leaf_train,kernel="vanilladot", prob.model = TRUE)	

	leaf_type_L <- predict(filter_Linear,leaf_test, type = "prob")

	all_predict_L <- rbind(all_predict_L, leaf_type_L)

	leaf_type_L <- predict(filter_Linear,leaf_test, type = "response")
	
	table(leaf_test_target, leaf_type_L)
	totalMatLinear <- as.matrix(CreateTable(leaf_test_target, leaf_type_L)) + totalMatLinear
}

pred <- prediction( all_predict_L[,1], all_leaf_test_target == 1)
perf <- performance( pred, "tpr", "fpr" )
xValues <- unlist(perf@x.values)
yValues <- unlist(perf@y.values)
aValues <- unlist(perf@alpha.values)
for(i in 2:length(levels(factor(leaf[[1]])))){
  pred <- prediction( all_predict_L[,i], all_leaf_test_target == i)
  perf <- performance( pred, "tpr", "fpr" )
  xValues <- xValues + unlist(perf@x.values)
  yValues <- yValues + unlist(perf@y.values)
  aValues <- aValues + unlist(perf@alpha.values)
}
perf@x.values <- list(xValues / length(levels(factor(leaf[[1]]))))
perf@y.values <- list(yValues / length(levels(factor(leaf[[1]]))))
perf@alpha.values <- list(aValues / length(levels(factor(leaf[[1]]))))
plot( perf, col = "red")

cat("Linear SVM: %", (sum(diag(totalMatLinear)) / sum(totalMatLinear) * 100))
write.table(totalMatLinear, "Linear_SVM_Result.txt", sep="\t")



#####################################################################
######################## leaf(polynomial svm) #######################
#####################################################################

leaf <- read.csv("C:\\Users\\Murat\\Desktop\\ss\\ML\\HW\\HW1\\leaf.csv", header = FALSE)
#Randomly shuffle the data
leaf<-leaf[sample(nrow(leaf)),]

#set class as factor
leaf[[1]]=factor(leaf[[1]])

#Create 10 equally size folds
folds <- cut(seq(1,nrow(leaf)),breaks=10,labels=FALSE)
gp <- runif(nrow(leaf))   # random siralama
leaf <- leaf[order(gp),]
totalMatPoly <- matrix(0,36,36)
all_predict_L <- c()
all_leaf_test_target <- c()

for(i in 1:10){
	#-- train ve test olarak ayırma --#
	#Segement your data by fold using the which() function 
	testIndexes <- which(folds==i,arr.ind=TRUE)
	leaf_test <- leaf[testIndexes, ]
	leaf_train <- leaf[-testIndexes, ]
	leaf_train_target <- leaf[-testIndexes, 1]   # 1 => kolon numarası
	leaf_test_target <- leaf[testIndexes, 1]

	all_leaf_test_target <- c(all_leaf_test_target, leaf_test_target)

	filter_Linear <- ksvm(V1~.,data=leaf_train, kernel="polydot", prob.model = TRUE)	

	leaf_type_L <- predict(filter_Linear,leaf_test, type = "prob")

	all_predict_L <- rbind(all_predict_L, leaf_type_L)

	leaf_type_L <- predict(filter_Linear,leaf_test, type = "response")
	
	table(leaf_test_target, leaf_type_L)
	totalMatPoly <- as.matrix(CreateTable(leaf_test_target, leaf_type_L)) + totalMatPoly
}

pred <- prediction( all_predict_L[,1], all_leaf_test_target == 1)
perf <- performance( pred, "tpr", "fpr" )
xValues <- unlist(perf@x.values)
yValues <- unlist(perf@y.values)
aValues <- unlist(perf@alpha.values)
for(i in 2:length(levels(factor(leaf[[1]])))){
  pred <- prediction( all_predict_L[,i], all_leaf_test_target == i)
  perf <- performance( pred, "tpr", "fpr" )
  xValues <- xValues + unlist(perf@x.values)
  yValues <- yValues + unlist(perf@y.values)
  aValues <- aValues + unlist(perf@alpha.values)
}
perf@x.values <- list(xValues / length(levels(factor(leaf[[1]]))))
perf@y.values <- list(yValues / length(levels(factor(leaf[[1]]))))
perf@alpha.values <- list(aValues / length(levels(factor(leaf[[1]]))))
plot( perf, col = "red")

cat("Polynomial SVM: %", (sum(diag(totalMatPoly)) / sum(totalMatPoly) * 100))
write.table(totalMatPoly, "Polynomial_SVM_Result.txt", sep="\t")









#bitti



#####################################################################
#################### 2. Partın Sonu The END #########################
#####################################################################






# deneme kısmı #









#leaf#

leaf <- read.csv("C:\\Users\\Murat\\Desktop\\ss\\ML\\HW\\HW1\\leaf.csv", header = FALSE)
#Randomly shuffle the data
leaf<-leaf[sample(nrow(leaf)),]
#set class as factor
leaf[[1]]=factor(leaf[[1]])

#Create 10 equally size folds
folds <- cut(seq(1,nrow(leaf)), breaks=10, labels=FALSE)
gp <- runif(nrow(leaf))   # random siralama
leaf <- leaf[order(gp),]
totalMatLinear <- matrix(0,36,36)
totalMatPoly <- matrix(0,36,36)

for(i in 1:10){
	#-- train ve test olarak ayırma --#
	#Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    leaf_test <- leaf[testIndexes, ]
    leaf_train <- leaf[-testIndexes, ]
	leaf_train_target <- leaf[-testIndexes, 1]   # 1 => kolon numarası
	leaf_test_target <- leaf[testIndexes, 1]

	filter_Linear <- ksvm(V1~.,data=leaf_train,kernel="vanilladot")	
	filter_Poly <- ksvm(V1~.,data=leaf_train,kernel="polydot")

	leaf_type_L <- predict(filter_Linear,leaf_test)
	leaf_type_P <- predict(filter_Poly,leaf_test)

	table(leaf_test_target, leaf_type_L)
	table(leaf_test_target, leaf_type_P)
	totalMatLinear <- as.matrix(CreateTable(leaf_test_target, leaf_type_L)) + totalMatLinear
	totalMatPoly <- as.matrix(CreateTable(leaf_test_target, leaf_type_P)) + totalMatPoly

}

totalMatLinear <- totalMatLinear / 10
totalMatPoly <- totalMatPoly / 10

cat("Linear     SVM: %", (sum(diag(totalMatLinear)) / sum(totalMatLinear) * 100))
cat("Polynomial SVM: %", (sum(diag(totalMatPoly)) / sum(totalMatPoly) * 100))



























#Randomly shuffle the data
iris<-iris[sample(nrow(iris)),]
library("e1071")
head(iris,5)
attach(iris)
x <- subset(iris, select=-Species)
y <- Species

svm_model <- svm(x[1:120,],y[1:120])

pred <- predict(svm_model,x[121:150,])

table(pred,y[121:150])













iris_classifier <- ksvm(Species ~ ., data = iris, kernel = "vanilladot")


svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)

svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)

system.time(predict(svm_model_after_tune,x))
table(pred,y)







library(pROC)
data(iris)
 
# Basic example
multiclass.roc(iris$Sepal.Length, iris$Sepal.Width)
# Produces an innocuous warning because one level has no observation
 
# Select only 3 of the aSAH$gos6 levels:
multiclass.roc(iris$Sepal.Length, iris$Sepal.Width, levels=c(3, 4, 5))
 
# Give the result in percent
multiclass.roc(iris$Sepal.Length, iris$Sepal.Width, percent=TRUE)









library(rpart)
library(pROC)

Dat <- kyphosis
a <- rep(0, nrow(Dat))
a[which(Dat$Kyphosis == "present")] <- 1
Dat$Present <- a

a <- sample(c(1:nrow(Dat)), size = nrow(Dat) * 0.7, replace = FALSE)

Train <- Dat[a, ]  
Test <- Dat[-a, ]  

RP1 <- rpart(Present ~ Age + Number + Start, Train, control = rpart.control(minsplit = 15, cp = 1e-04))  

Pred1 <- predict(RP1, Test[, c("Age", "Number", "Start")])
Test$Prediction1 <- Pred1

ROC1 <- roc(Test$Present, Test$Prediction1)

plot(ROC1, col = "blue")






library(ROCR)
data(ROCR.simple)
head(cbind(ROCR.simple$predictions, ROCR.simple$labels), 5)
pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
class(pred)
slotNames(pred)
sn = slotNames(pred)
sapply(sn, function(x) length(slot(pred, x)))
sapply(sn, function(x) class(slot(pred, x)))

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)



data(aSAH)
roc(aSAH$outcome, aSAH$s100b,
    levels=c("Good", "Poor"))
roc(aSAH$outcome, aSAH$s100b)
roc(outcome ~ s100b, aSAH)
roc(aSAH$outcome ~ aSAH$s100b)
with(aSAH, roc(outcome, s100b))
with(aSAH, roc(outcome ~ s100b))

roc(outcome ~ s100b, data=aSAH)

roc(controls=aSAH$s100b[aSAH$outcome=="Good"], cases=aSAH$s100b[aSAH$outcome=="Poor"])
roc(controls=aSAH$wfns[aSAH$outcome=="Good"], cases=aSAH$wfns[aSAH$outcome=="Poor"])

roc(aSAH$outcome, aSAH$s100b,
    levels=c("Poor", "Good"))

roc(aSAH$outcome, aSAH$s100b,
    levels=c("Poor", "Good"), direction="<")

roc(aSAH$outcome, aSAH$s100b, percent=TRUE)

roc(aSAH$outcome, aSAH$s100b, algorithm = 1)
roc(aSAH$outcome, aSAH$s100b, algorithm = 2)
roc(aSAH$outcome, aSAH$s100b, algorithm = 3)
if (require(microbenchmark)) {
    roc(aSAH$outcome, aSAH$s100b, algorithm = 0)
}

roc(aSAH$outcome, aSAH$s100b,
    percent=TRUE, plot=TRUE, ci=TRUE)
roc(aSAH$outcome, aSAH$s100b, smooth=TRUE)
smooth(roc(aSAH$outcome, aSAH$s100b))









pred <- prediction( all_test_pred[,1], all_test_labels == 1)
perf <- performance( pred, "tpr", "fpr" )
xValues <- unlist(perf@x.values)
yValues <- unlist(perf@y.values)
aValues <- unlist(perf@alpha.values)
for(i in 2:classnum){
  pred <- prediction( all_test_pred[,i], all_test_labels == i)
  perf <- performance( pred, "tpr", "fpr" )
  xValues <- xValues + unlist(perf@x.values)
  yValues <- yValues + unlist(perf@y.values)
  aValues <- aValues + unlist(perf@alpha.values)
}
perf@x.values <- list(xValues / classnum)
perf@y.values <- list(yValues / classnum)
perf@alpha.values <- list(aValues / classnum)
plot( perf, colorize = TRUE)

#----#


forROC <- rep(0,length(iris_test_target))
for(i in 1:length(iris_test_target)){
	if(iris_test_target[i] == "virginica")
		forROC[i] <- 1
}


pred <- prediction(as.numeric(iris_type_P),forROC)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
