library(lattice)
library(data.tree)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(RColorBrewer)

#1.read mushroom data
data <- read.csv(file="/Users/golf/Desktop/R/mushrooms.csv",header = TRUE)
str(data)
print(summary(data))
print(levels(data$odor))


#2.split data
index <- 1:nrow(data)
sample_size <- floor(0.8*nrow(data))
set.seed(12345)
testindex <- sample(nrow(data),size=sample_size)

train <- data[testindex,]
test <- data[-testindex,]

#3.attribute selection
Attribute_Selection <- function(data){
  ig <- sapply(colnames(data)[-1], function(x) InformationGain(table(data[,x], data[,1])))
  print(ig)
  
  #4 visualize
  data_name <- names(ig)
  data_ig <- unname(ig)
  ig_data_frame <- data.frame("Value0to1"= unname(data_ig),"Name"= data_name)
  graph <- barchart(ig_data_frame$Name ~ ig_data_frame$Value0to1, data = ig_data_frame)
  print(graph)
}


#5.1 create tree
IsPure <- function(data){
  length(unique(data[,1])) ==1 #first column = target value (class)
}
Entropy <- function(vls){
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls==0] <- 0
  -sum(res)
}

InformationGain <- function(tble){
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s/sum(s)*apply(tble,MARGIN=1,FUN=Entropy))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

TrainID3 <- function(node, data) {
  node$obsCount <- nrow(data)
  
  if (IsPure(data)) {
    child <- node$AddChild(unique(data[,1]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    ig <- sapply(colnames(data)[-1], 
                 function(x) InformationGain(
                   table(data[,x], data[,1])
                 )
    )
    feature <- names(ig)[ig == max(ig)][1]
    node$feature <- feature
    
    childObs <- split(data[,!(names(data) %in% feature)], 
                      data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      child <- node$AddChild(names(childObs)[i])
      TrainID3(child, childObs[[i]])
    }
  }
}
Predict <- function(tree,feature){
  if(tree$children[[1]]$isLeaf)
    return(tree$children[[1]]$name)
  child <- tree$children[[feature[[tree$feature]]]]
  return(Predict(child,feature))
}
mushroom_tree <- Node$new("mushroom")
TrainID3(mushroom_tree,data)
print(mushroom_tree,"feature","obsCount")
plot(mushroom_tree)

#5.2 rpart tree
fit <- rpart(class ~ . ,data = train,method = "class")
rpart.plot(fit)
pred_r <- predict(fit, test,type="class")
plot(pred_r)
print(confusionMatrix(table(pred_r,test$class)))
