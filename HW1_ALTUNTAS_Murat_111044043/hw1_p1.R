# dataları böl
# labelleri ayır
# labelsiz train ve test datalarını ve train labellerini knn e ver
# knn'den test labellerini al
# gerçek test labelleri ile karşılaştır (CrossTable)
# 

#####################################################################
############################# Functions #############################
#####################################################################
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
euc_dist <- function(x1, x2) { return (sqrt(sum((x1 - x2) ^ 2))) }
manh_dist <- function(p,q){ sum(abs(p-q)) }
Mode <- function(x) {
  ls <- unique(x)
  ls[which.max(tabulate(match(x, ls)))]
}

CreateTable <- function(x1,x2) {
	total <- matrix(0,36,36)

	for(i in 1:34) {
		total[x1[i],x2[i]] <- total[x1[i],x2[i]] + 1
	}
	return(total)
}

myKnnEuc <- function(train,test,cl,k) {
	train_row <- nrow(train) # train dataset row sayısı
	test_row <- nrow(test)   # test dataset row sayısı
	eucArr <- 1:train_row    # eucledian hesaplarının row sayısı
	labelArr <- 1:k          # labeller
	result <- 1:test_row     # sonuc arrayi
	for(i in 1:test_row) {
		for(j in 1:train_row) {
			eucArr[j] <- euc_dist(train[j,],test[i,])  
		}
		for(l in 1:k) { 
			labelArr[l] <- cl[match(l,rank(eucArr))]  
		}
		result[i] <- Mode(labelArr)
	}
	return(result)
}

myKnnManh <- function(train,test,cl,k) {
	train_row <- nrow(train) # train dataset row sayısı
	test_row <- nrow(test)   # test dataset row sayısı
	eucArr <- 1:train_row    # eucledian hesaplarının row sayısı
	labelArr <- 1:k          # labeller
	result <- 1:test_row     # sonuc arrayi
	for(i in 1:test_row) {
		for(j in 1:train_row) {
			eucArr[j] <- manh_dist(train[j,],test[i,])  
		}
		for(l in 1:k) { 
			labelArr[l] <- cl[match(l,rank(eucArr))]  
		}
		result[i] <- Mode(labelArr)
	}
	return(result)
}

#####################################################################
########################## iris (Eucledian) #########################
#####################################################################

#Randomly shuffle the data
iris<-iris[sample(nrow(iris)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(iris)),breaks=10,labels=FALSE)
gp <- runif(nrow(iris))   # random siralama
iris <- iris[order(gp),]
totalMAtEuc <- matrix(0,3,3)
#Perform 10 fold cross validation
for(i in 1:10){
	iris_n <- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    iris_test <- iris_n[testIndexes, ]
    iris_train <- iris_n[-testIndexes, ]
    #Use the test and train data partitions however you desire...

	#-- train ve test olarak ayırma --#
	iris_test_target <- iris[testIndexes, 5]   # 5 => kolon numarası
	iris_train_target <- iris[-testIndexes, 5]
	test_Euc <- myKnnEuc(iris_train, iris_test, iris_train_target,5)   # part 1 
	#-- Cros Table --#
	totalMAtEuc <- as.matrix(table(iris_test_target, test_Euc)) + totalMAtEuc

}

print(totalMAtEuc)
CrossTable(totalMAtEuc, prop.chisq = FALSE)
cat("%", (sum(diag(totalMAtEuc)) / sum(totalMAtEuc) * 100))

#####################################################################
############################# iris (Manhattan) ######################
#####################################################################

#Randomly shuffle the data
iris<-iris[sample(nrow(iris)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(iris)),breaks=10,labels=FALSE)
gp <- runif(nrow(iris))   # random siralama
iris <- iris[order(gp),]
totalMAtManh <- matrix(0,3,3)
#Perform 10 fold cross validation
for(i in 1:10){
	iris_n <- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    iris_test <- iris_n[testIndexes, ]
    iris_train <- iris_n[-testIndexes, ]
    #Use the test and train data partitions however you desire...

	#-- train ve test olarak ayırma --#
	iris_test_target <- iris[testIndexes, 5]   # 5 => kolon numarası
	iris_train_target <- iris[-testIndexes, 5]
	test_Manh <- myKnnManh(iris_train, iris_test, iris_train_target,5) # part 2
	#-- Cros Table --#
	totalMAtManh <- as.matrix(table(iris_test_target, test_Manh)) + totalMAtManh

}

print(totalMAtManh)
CrossTable(totalMAtManh, prop.chisq = FALSE)
cat("%", (sum(diag(totalMAtManh)) / sum(totalMAtManh) * 100))

#####################################################################
########################## leaf (Eucledian) #########################
#####################################################################

leaf <- read.csv("C:\\Users\\Murat\\Desktop\\ss\\ML\\HW\\HW1\\leaf.csv", header = FALSE)
leaf<-leaf[sample(nrow(leaf)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(leaf)),breaks=10,labels=FALSE)
gp <- runif(nrow(leaf))   # random siralama
leaf <- leaf[order(gp),]
totalMAtEuc <- matrix(0,36,36)

#Perform 10 fold cross validation
for(i in 1:10){
	leaf_n <- as.data.frame(lapply(leaf[,2:16],normalize))

 	#Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    leaf_test <- leaf_n[testIndexes, ]
    leaf_train <- leaf_n[-testIndexes, ]

	#-- train ve test olarak ayırma --#
	leaf_train_target <- leaf[-testIndexes, 1]   # 1 => kolon numarası
	leaf_test_target <- leaf[testIndexes, 1]
	test_Euc <- myKnnEuc(leaf_train, leaf_test, leaf_train_target,5)   # part 1 

	totalMAtEuc <- as.matrix(CreateTable(leaf_test_target, test_Euc)) + totalMAtEuc

}


cat("%", (sum(diag(totalMAtEuc)) / sum(totalMAtEuc) * 100))
write.table(totalMAtEuc, file="mydataEuc.txt", sep="\t")

#write.table(totalMAtEuc, "C:/Users/Murat/Desktop/ss/ML/HW/HW1/mydataEuc.txt", sep="\t")



#####################################################################
########################## leaf (Manhattan) #########################
#####################################################################

leaf <- read.csv("C:\\Users\\Murat\\Desktop\\ss\\ML\\HW\\HW1\\leaf.csv", header = FALSE)
leaf<-leaf[sample(nrow(leaf)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(leaf)),breaks=10,labels=FALSE)
gp <- runif(nrow(leaf))   # random siralama
leaf <- leaf[order(gp),]
totalMAtManh <- matrix(0,36,36)

#Perform 10 fold cross validation
for(i in 1:10){
	leaf_n <- as.data.frame(lapply(leaf[,2:16],normalize))

 	#Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    leaf_test <- leaf_n[testIndexes, ]
    leaf_train <- leaf_n[-testIndexes, ]

	#-- train ve test olarak ayırma --#
	leaf_train_target <- leaf[-testIndexes, 1]   # 1 => kolon numarası
	leaf_test_target <- leaf[testIndexes, 1]
	test_Manh <- myKnnManh(leaf_train, leaf_test, leaf_train_target,5) # part 2

	totalMAtManh <- as.matrix(CreateTable(leaf_test_target, test_Manh)) + totalMAtManh

}


	cat("%", (sum(diag(totalMAtManh)) / sum(totalMAtManh) * 100))

write.table(totalMAtEuc, "mydataManh.txt", sep="\t")
#write.table(totalMAtEuc, "C:/Users/Murat/Desktop/ss/ML/HW/HW1/mydataManh.txt", sep="\t")











#####################################################################
#################### 1. Partın Sonu The END #########################
#####################################################################







#####################################################################
############################# iris ##################################
#####################################################################

gp <- runif(nrow(iris))   # random siralama
iris <- iris[order(gp),]
iris_n <- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
#-- train ve test olarak ayırma --#
iris_train <- iris_n[1:129,]
iris_test <- iris_n[130:150,]
iris_train_target <- iris[1:129, 5]   # 5 => kolon numarası
iris_test_target <- iris[130:150, 5]
test_Euc <- myKnnEuc(iris_train, iris_test, iris_train_target,5)   # part 1 
test_Manh <- myKnnManh(iris_train, iris_test, iris_train_target,5) # part 2
#-- Cros Table --#
library(gmodels)
CrossTable(x = iris_test_target, y = test_Euc, prop.chisq = FALSE)
CrossTable(x = iris_test_target, y = test_Manh, prop.chisq = FALSE)


#####################################################################
############################# leaf ##################################
#####################################################################

leaf <- read.csv("C:\\Users\\Murat\\Desktop\\ss\\ML\\HW\\HW1\\leaf.csv", header = FALSE)

gp <- runif(nrow(leaf))   # random siralama
leaf <- leaf[order(gp),]
leaf_n <- as.data.frame(lapply(leaf[,2:16],normalize))

#-- train ve test olarak ayırma --#
leaf_train <- leaf_n[1:300,]
leaf_test <- leaf_n[301:340,]
leaf_train_target <- leaf[1:300, 1]   # 1 => kolon numarası
leaf_test_target <- leaf[301:340, 1]
test_Euc <- myKnnEuc(leaf_train, leaf_test, leaf_train_target,5)   # part 1 
test_Manh <- myKnnManh(leaf_train, leaf_test, leaf_train_target,5) # part 2

#-- confusion Matrix --#
table(leaf_test_target, test_Euc)
table(leaf_test_target == test_Euc)
table(leaf_test_target, test_Manh)
table(leaf_test_target == test_Manh)
#-- Cros Table --#
library(gmodels)
CrossTable(x = leaf_test_target, y = test_Euc, prop.chisq = FALSE)
CrossTable(x = leaf_test_target, y = test_Manh, prop.chisq = FALSE)



