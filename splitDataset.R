##### Split data to train, devlop, and test sets #####

setwd("C:\\Users\\Dyn\\Documents\\YAD_JohnHopkins_DS\\data\\capstone\\Coursera-SwiftKey\\script")
cText <- readRDS("cleanDataset.rds")

set.seed(1111)
trainIndex <- sample.int(dim(cText[1]), size = floor(dim(cText[1])*0.6))
train <- cText[trainIndex, ]
remain <- cText[-trainIndex, ]
testIndex <- sample.int(dim(remain[1]), size = floor(dim(remain[1])*0.5))
test <- remain[testIndex, ]
dev <- remain[-testIndex, ]

saveRDS(train, "train.rds")
saveRDS(test, "test.rds")
saveRDS(dev, "dev.rds")