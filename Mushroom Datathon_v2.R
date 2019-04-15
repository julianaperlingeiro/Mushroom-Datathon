#### Mushroom Dathaton 
#### Autor: Juliana Perlingeiro 
#### Date: 12/04/19 


## Libraries ------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(lattice)
library(ggplot2)

## Reading the file ----------------------------------------------------------------------

mushroom_data <- readRDS (file = "train (3).rds")

str(mushroom_data)
summary(mushroom_data)
ls(mushroom_data)
names(mushroom_data)

## Turn all character variables into factors

mushroom_data$class <- as.factor(mushroom_data$class)
mushroom_data$cap.shape <- as.factor(mushroom_data$cap.shape)
mushroom_data$cap.surface <- as.factor(mushroom_data$cap.surface)
mushroom_data$cap.color <- as.factor(mushroom_data$cap.color)
mushroom_data$odor <- as.factor(mushroom_data$odor)
mushroom_data$gill.spacing <- as.factor(mushroom_data$gill.spacing)
mushroom_data$gill.size <- as.factor(mushroom_data$gill.size)
mushroom_data$stalk.color.above.ring <- as.factor(mushroom_data$stalk.color.above.ring)
mushroom_data$stalk.color.below.ring <- as.factor(mushroom_data$stalk.color.below.ring)
mushroom_data$stalk.root <- as.factor(mushroom_data$stalk.root)
mushroom_data$stalk.shape <- as.factor(mushroom_data$stalk.shape)
mushroom_data$stalk.surface.above.ring <-as.factor(mushroom_data$stalk.surface.above.ring)
mushroom_data$stalk.surface.below.ring <- as.factor(mushroom_data$stalk.surface.below.ring)
mushroom_data$veil.color<- as.factor(mushroom_data$veil.color)
mushroom_data$veil.type <-as.factor(mushroom_data$veil.type)
mushroom_data$ring.number <- as.factor(mushroom_data$ring.number)
mushroom_data$ring.type <- as.factor(mushroom_data$ring.type)
mushroom_data$spore.print.color <- as.factor(mushroom_data$spore.print.color)
mushroom_data$population <- as.factor(mushroom_data$population)
mushroom_data$habitat <- as.factor(mushroom_data$habitat)

## Selecting attributes ------------------------------------------------------------------

mushroom_data <- select(mushroom_data, 
                        cap.shape, 
                        cap.color, 
                        bruises, 
                        stalk.color.above.ring,
                        stalk.color.below.ring, 
                        population, 
                        class)


str(mushroom_data)
summary(mushroom_data)
ls(mushroom_data)
names(mushroom_data)


## Set seed ----------------------------------------------------------------

set.seed(123)


## Create training and testing sets -----------------------------------

inTraining <- createDataPartition(mushroom_data$class, 
                                  p = .08, 
                                  list = FALSE)

training <- mushroom_data[inTraining,]

testing <- mushroom_data[-inTraining,]


## Cross Validation --------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 5,
                                repeats = 1,
                                search = "grid",
                                classProbs = TRUE)


# Training C5 Model ------------------------------------------------------

C5_Model <- train(class~., 
                  data = training, 
                  method = "C5.0", 
                  metric = "Precision",
                  trControl=CrossValidation, 
                  tuneLength = 1)



## Checking Training results --------------------------------------------------------

C5_Model

## Predictions -------------------------------------------------------------

C5Prediction <- predict(C5_Model, testing)

## Checking Predictions -------------------------------------------------------

C5Prediction

confusionMatrix(C5Prediction, testing$class)

## Save model --------------------------------------------------------------

save(C5_Model, file = "Flo&JuliAlwaysChachara3.rda")




