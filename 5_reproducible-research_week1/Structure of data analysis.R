library(kernlab)
data(spam)

##Perform subsampling
set.seed(56789)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5) 
table(trainIndicator)

trainSpam <- spam[trainIndicator == 1,]
testSpam <- spam[trainIndicator == 0,]


##check correlation between predictors
plot(log10(trainSpam[,1:4]+1))

##cluster data to see if there are interesting groups
hClust <- hclust(dist(t(log10(trainSpam[,1:55] + 1))))
plot(hClust)

