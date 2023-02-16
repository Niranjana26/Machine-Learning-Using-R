library(ggplot2)
library(ggpubr)
library(caTools)
library(class)
library(dplyr)
library(e1071)
library(rpart)
library(rpart.plot)
#Import dataset
dataset <- read.csv("D:/VIT/Fall_22-23/CSI3004 _Data Science Programming/LabDA/Th_DA/Sensorless_drive_diagnosis.txt",
                    header = F, sep = '',
                    col.names = c("Feature1", "Feature2", "Feature3", "Feature4", "Feature5", 
                                  "Feature6", "Feature7", "Feature8", "Feature9","Feature10","Feature11","Feature12",
                                  "Feature13","Feature14","Feature15","Feature16","Feature17","Feature18",
                                  "Feature19","Feature20","Feature21","Feature22","Feature23","Feature24",
                                  "Feature25","Feature26","Feature27","Feature28","Feature29","Feature30",
                                  "Feature31","Feature32","Feature33","Feature34","Feature35","Feature36",
                                  "Feature37","Feature38","Feature39","Feature40","Feature41","Feature42",
                                  "Feature43","Feature44","Feature45","Feature46","Feature47","Feature48","label"))
View(dataset)
str(dataset)
summary(dataset)
#Cleaning Dataset
cleanedDataset <- dataset
cleanedDataset$label <- factor(cleanedDataset$label, levels = c(0, 1)) 
str(cleanedDataset)
#Plot
boxPlot1 <- ggplot(data=cleanedDataset,
                   aes(x=as.factor(label), y=Feature2, fill = as.factor(label))) +
  geom_boxplot(alpha = 0.4,
               outlier.colour = "red") +
  ggtitle("BOXPLOT Label X Feature2 Niranjana R(20MID0182)")

boxPlot2 <- ggplot(data=cleanedDataset,
                   aes(x=as.factor(label), y=Feature4, fill = as.factor(label))) +
  geom_boxplot(alpha = 0.4,
               outlier.colour = "red") +
  ggtitle("BOXPLOT Label X Feature4 Niranjana R(20MID0182)")

boxPlot3 <- ggplot(data=cleanedDataset,
                   aes(x=as.factor(label), y=Feature6, fill = as.factor(label))) +
  geom_boxplot(alpha = 0.4,
               outlier.colour = "red") +
  ggtitle("BOXPLOT Label X Feature6 Niranjana R(20MID0182)")

boxPlot4 <- ggplot(data=cleanedDataset,
                   aes(x=as.factor(label), y=Feature8, fill = as.factor(label))) +
  geom_boxplot(alpha = 0.4,
               outlier.colour = "red") +
  ggtitle("BOXPLOT Label X Feature8 Niranjana R(20MID0182)")

ggarrange(boxPlot1, boxPlot2, boxPlot3, boxPlot4,
          ncol = 2, nrow = 2)
violinPlot1 <- ggplot(data=cleanedDataset, 
                      aes(x=as.factor(label), y=Feature2, fill = as.factor(label))) +
  geom_violin(alpha = 0.4) + 
  ggtitle("VIOLINPLOT Label X Feature2 Niranjana R(20MID0182)") 

violinPlot2 <- ggplot(data=cleanedDataset, 
                      aes(x=as.factor(label), y=Feature4, fill = as.factor(label))) +
  geom_violin(alpha = 0.4) + 
  ggtitle("VIOLINPLOT Label X Feature4 Niranjana R(20MID0182)") 

violinPlot3 <- ggplot(data=cleanedDataset, 
                      aes(x=as.factor(label), y=Feature6, fill = as.factor(label))) +
  geom_violin(alpha = 0.4) + 
  ggtitle("VIOLINPLOT Label X Feature6 Niranjana R(20MID0182)")

violinPlot4 <- ggplot(data=cleanedDataset, 
                      aes(x=as.factor(label), y=Feature8, fill = as.factor(label))) +
  geom_violin(alpha = 0.4) + 
  ggtitle("VIOLINPLOT Label X Feature8 Niranjana R(20MID0182)") 

ggarrange(violinPlot1, violinPlot2, violinPlot3, violinPlot4,
          ncol = 2, nrow = 2)
histPlot1 <- ggplot(data=cleanedDataset,
                    aes(x=Feature2, fill=as.factor(label))) +
  geom_histogram(alpha = 0.5, bins=30) + 
  ggtitle("HISTOGRAM Feature2 Niranjana R(20MID0182)") 

histPlot2 <- ggplot(data=cleanedDataset,
                    aes(x=Feature4, fill=as.factor(label))) +
  geom_histogram(alpha = 0.5, bins=30) + 
  ggtitle("HISTOGRAM Feature4 Niranjana R(20MID0182)") 

histPlot3 <- ggplot(data=cleanedDataset,
                    aes(x=Feature6, fill=as.factor(label))) +
  geom_histogram(alpha = 0.5, bins=30) + 
  ggtitle("HISTOGRAM Feature6 Niranjana R(20MID0182)") 

histPlot4 <- ggplot(data=cleanedDataset,
                    aes(x=Feature8, fill=as.factor(label))) +
  geom_histogram(alpha = 0.5, bins=30) + 
  ggtitle("HISTOGRAM Feature8 Niranjana R(20MID0182)") 

ggarrange(histPlot1, histPlot2, histPlot3, histPlot4,
          ncol = 2, nrow = 2)
scatterPlot1 <- ggplot(data=cleanedDataset,
                       aes(x=Feature2, y=Feature4, fill = as.factor(label))) +
  geom_point(alpha = 0.7,
             size=1) +
  scale_color_brewer(palette = "Spectral") +
  ggtitle("SCATTERPLOT Feature2 X Feature4 Niranjana R(20MID0182)") 


scatterPlot2 <- ggplot(data=cleanedDataset,
                       aes(x=Feature2, y=Feature6, fill=as.factor(label))) +
  geom_point(alpha = 0.7,
             size=1) +
  ggtitle("SCATTERPLOT Feature2 X Feature6 Niranjana R(20MID0182)")

scatterPlot3 <- ggplot(data=cleanedDataset,
                       aes(x=Feature2, y=Feature8, fill=as.factor(label))) +
  geom_point(alpha = 0.7,
             size=1) +
  ggtitle("SCATTERPLOT Feature2 X Feature8 Niranjana R(20MID0182)")

scatterPlot4 <- ggplot(data=cleanedDataset,
                       aes(x=Feature2, y=Feature10, fill=as.factor(label))) +
  geom_point(alpha = 0.7,
             size=1) +
  ggtitle("SCATTERPLOT Feature2 X Feature10 Niranjana R(20MID0182)") 

ggarrange(scatterPlot1, scatterPlot2, scatterPlot3, scatterPlot4,
          ncol = 2, nrow = 2)
ggplot(data=dataset,aes(x=label,fill=as.factor(label)))+geom_density(alpha=0.4)+labs(title = "Niranjana R 20MID0182")
BubblePlot1 <- ggplot(data=dataset, aes(x=Feature4, y=Feature6)) +
  geom_point(alpha=0.7)+ggtitle("Niranjana R(20MID0182)")
BubblePlot2 <- ggplot(data=dataset, aes(x=Feature8, y=Feature10)) +
  geom_point(alpha=0.7)+ggtitle("Niranjana R(20MID0182)")
BubblePlot3 <- ggplot(data=dataset, aes(x=Feature12, y=Feature14)) +
  geom_point(alpha=0.7)+ggtitle("Niranjana R(20MID0182)")
BubblePlot4 <- ggplot(data=dataset, aes(x=Feature16, y=Feature18)) +
  geom_point(alpha=0.7)+ggtitle("Niranjana R(20MID0182)")

ggarrange(BubblePlot1, BubblePlot2, BubblePlot3, BubblePlot4,
          ncol = 2, nrow = 2)
#KNN
#Training data
set.seed(123)
split <- sample.split(Y = cleanedDataset$label, SplitRatio = 2/3) 
trainSet <- subset(x = cleanedDataset, split == TRUE)
testSet <- subset(x = cleanedDataset, split == FALSE)
#Feature scaling
trainSet[-49] <- scale(trainSet[-49]) 
testSet[-49] <- scale(testSet[-49])
str(trainSet)
str(testSet)
#Creating model
yPred <- knn(trainSet[,-49], test = testSet[,-49], cl = trainSet[,49], k = 5)
#Confusion matrix
cm <- table(testSet[,49], yPred)
cm
#Accuracy
sum(diag(cm))/sum(cm)
#Algorithm 2 - Naive Bayes #Creating object
naive <- naiveBayes(label ~.,
                    data = trainSet)
#Creating model
yPred <- predict(naive, newdata = testSet, type = "class")
#Confusion matrix
cm <- table(testSet[,13], yPred)
cm
#Accuracy
sum(diag(cm))/sum(cm)
