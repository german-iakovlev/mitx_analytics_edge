

parks_data <- read.csv("park_visits.csv")

# Subset July 2016 data
parks_july_2016 <- subset(parks_data, Month == "7" & Year == "2016")

# Subset Yellowstone NP park data
park_yellowstone <- subset(parks_data, ParkName == "Yellowstone NP")

# Create a timeseries of logVisits
park_yellowstone_ts=ts(park_yellowstone$logVisits,start=c(2010,1),freq=12)
plot(park_yellowstone_ts)

# Remove observations with missing data from parks_data
parks_data <- parks_data[rowSums(is.na(parks_data)) == 0,]

# Month variable as factor
parks_data$Month <- as.factor(parks_data$Month)

# Subset the data into training and testing sets
train <- subset(parks_data, Year < 2015)
test <- subset(parks_data, Year >= 2015)

# Linear model #1
model_1 <- lm(logVisits ~ laglogVisits, data = train)
prediction_1 <- predict(model_1, newdata = test)
SSE_1 <- sum((prediction_1 - test$logVisits) ^ 2)
SST_1 <- sum((mean(train$logVisits) - test$logVisits) ^ 2)
R_squared_1 <- 1 - SSE_1/SST_1

# Linear model #2
model_2 <- lm(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month
              + Region + ParkType + cost, data = train)
prediction_2 <- predict(model_2, newdata = test)
SSE_2 <- sum((prediction_2 - test$logVisits) ^ 2)
SST_2 <- sum((mean(train$logVisits) - test$logVisits) ^ 2)
R_squared_2 <- 1 - SSE_2/SST_2

# Regression tree model
library(rpart)
library(rpart.plot)
# Here the optimal cp value from cross-validation is used
model_rpart <- rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month
                     + Region + ParkType + cost, data = train, cp = 0.0001)
rpart.plot(model_rpart)

prediction_rpart <- predict(model_rpart, newdata = test)
SSE_rpart <- sum((prediction_rpart - test$logVisits) ^ 2)
SST_rpart <- sum((mean(train$logVisits) - test$logVisits) ^ 2)
R_squared_rpart <- 1 - SSE_rpart/SST_rpart

# Cross-validated model
library(caret)
set.seed(201)
num.folds <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp = seq(0.0001, 0.005, 0.0001))
print(train(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month
      + Region + ParkType + cost,
      data = train,
      method = "rpart",
      trControl = num.folds,
      tuneGrid = cp.grid
      )
)

# Random forest model
library(randomForest)
set.seed(201)
model_random_forest <- randomForest(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month
                                    + Region + ParkType + cost,
                                    data = train
                                    )
prediction_random_forest <- predict(model_random_forest, newdata = test)
SSE_random_forest <- sum((prediction_random_forest - test$logVisits) ^ 2)
SST_random_forest <- sum((mean(train$logVisits) - test$logVisits) ^ 2)
R_squared_random_forest <- 1 - SSE_random_forest/SST_random_forest