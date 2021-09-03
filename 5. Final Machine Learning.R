#Import libraries
library(tree)
library(randomForest)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(caret)
library(factoextra)
library(tictoc)
library(class)

############## Loading data set WITH 32 MOST COMMON MAKES due to limits in the models meaning one factor could not exceed 32 levels ###############
########## therefore dropped the 9 least commonly occuring makes from the dataset ##########
cars_32 <- read.csv(file="/Users/benkight/OneDrive - Durham University/Masters/Dissertation/Python - Scraping/Engineered Dataset for Simple and Pruned.csv",sep=",")[,-1] #remove indexing column

############## Rename columns and transform characters to factors ############## 
str(cars_32)

#rename columns
names(cars_32)[names(cars_32) == "Mean.Model.Price...."] <- "Mean.Model.Price"
names(cars_32)[names(cars_32) == "Engine.Size..L."] <- "Engine.Size"
names(cars_32)[names(cars_32) == "Actual.Car.Price...."] <- "Actual.Price"

#transform characters to factors 
cars_32$Make <- as.factor(cars_32$Make)
cars_32$Transmission <- as.factor(cars_32$Transmission)
cars_32$Fuel.Type <- as.factor(cars_32$Fuel.Type)
cars_32$Indicated.Value <- as.factor(cars_32$Indicated.Value)

########## SIMPLE TREE ON FULL DATASET *WITH* MAKES OVER 2 ITERATIONS ########## 
start_time <- Sys.time() #start timer

vector_full_with_makes <- c() 

repetitions = 2 # number of iterations
for(i in 1:repetitions)
{
  set.seed(i) # seed inside loop so that it changes 
  
  #Step 1) training
  training_full_with_makes <- tree(Indicated.Value~., data = cars_32)
  
  #Step 2) testing
  predict_full_with_makes <- predict(training_full_with_makes, data = cars_32, type = "class")
  
  #Step 3) predictive performance
  cm_full_with_makes <- table(predict_full_with_makes, cars_32$Indicated.Value)
  ac_full_with_makes <- ((cm_full_with_makes[[1,1]] + cm_full_with_makes[[2,2]] + cm_full_with_makes[[3,3]] + cm_full_with_makes[[4,4]] + cm_full_with_makes[[5,5]])/sum(cm_full_with_makes))
  vector_full_with_makes[i] = ac_full_with_makes 
}

#plotting tree
plot(training_full_with_makes); text(training_full_with_makes, pretty = 0); title("With Makes")

#mean classification rate over the 5 repetitions 
cr_full_with_makes <- mean(vector_full_with_makes)
cr_full_with_makes

end_time <- Sys.time() #end timer
time_full_with_makes <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


######## SIMPLE TREE ON FULL DATASET *WITHOUT* MAKES OVER 5 ITERATIONS ######### 
start_time <- Sys.time() #start timer

vector_full_no_makes <- c() # empty vector for predictive performance

repetitions = 5 # number of iterations
for(i in 1:repetitions)
{
  set.seed(i) # seed inside loop so that it changes 
  
  #Step 1) training
  training_full_no_makes <- tree(Indicated.Value ~ Mean.Model.Price + Year + Mileage + Transmission + Engine.Size + Fuel.Type + Actual.Price, data = cars_32)
  
  #Step 2) testing
  predict_full_no_makes <- predict(training_full_no_makes, data = cars_32, type = "class")
  
  #Step 3) predictive performance
  cm_full_no_makes <- table(predict_full_no_makes, cars_32$Indicated.Value)
  ac_full_no_makes <- ((cm_full_no_makes[[1,1]] + cm_full_no_makes[[2,2]] + cm_full_no_makes[[3,3]] + cm_full_no_makes[[4,4]] + cm_full_no_makes[[5,5]])/sum(cm_full_no_makes))
  vector_full_no_makes[i] = ac_full_no_makes 
}

#plotting tree
plot(training_full_no_makes); text(training_full_no_makes, pretty = 0); title("No Makes")

#mean classification rate over the 5 repetitions 
cr_full_no_makes <- mean(vector_full_no_makes)
cr_full_no_makes

end_time <- Sys.time() #end timer
time_full_no_makes <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
######## Table to show accuracy and time comparisons of first two models #######
par(mfrow=c(1,2)) #fit both on one screen

scores <- c(cr_full_with_makes*100, cr_full_no_makes*100) #get the accuracies in %'s
scores <- round(scores, digits = 3) #round to 5 dig

times <- c(time_full_with_makes,time_full_no_makes) #convert so both in minutes
times <- round(times, digits = 3) #round to 5 dig

x_acc <- barplot(scores, main = "Mean Classification Accuracies of two Models", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('With Makes', 'Without Makes'), ylim = c(0,100), col = c("skyblue3", "forestgreen"))
y_acc <- as.matrix(scores)
text(x_acc,y_acc+4, labels = as.character(y_acc))

x_times <- barplot(times, main = "Timings of two Models", ylab = 'Time Taken (minutes)', xlab = "Model", names.arg = c('With Makes', 'Without Makes'), ylim = c(0,150), col = c("skyblue3", "forestgreen"))
y_times <- as.matrix(times)
text(x_times,y_times+4, labels = as.character(y_times))

time_acc <- matrix(c(scores[[1]], times[[1]], scores[[2]], times[[2]]), nr=2)
y_time_acc <- matrix(c(y_acc[[1]], y_times[[1]], y_acc[[2]], y_times[[2]]))
x_time_acc <- barplot(time_acc, beside=T, main = "Timings of two Models", ylab = 'Time Taken (minutes)', xlab = "Model", names.arg = c('Without Makes', 'With Makes'),  col = c("skyblue3", "forestgreen"), ylim = c(0,150))
legend("topleft", c("Classification Accuracy (%)", "Time Taken (mins)"), pch = 15, col = c("skyblue3", "forestgreen"), bty="n")
text(x_time_acc,y_time_acc+2, labels = as.character(y_time_acc))


################################################################################
##### Load library with all car makes and models but drop the make column ######
cars_ <- read.csv(file="/Users/benkight/OneDrive - Durham University/Masters/Dissertation/Python - Scraping/Engineered Dataset.csv",sep=",")[,-c(1:2)] #drop index and make columns

############## Rename columns and transform characters to factors ############## 
str(cars_)

#rename columns
names(cars_)[names(cars_) == "Mean.Model.Price...."] <- "Mean.Model.Price"
names(cars_)[names(cars_) == "Engine.Size..L."] <- "Engine.Size"
names(cars_)[names(cars_) == "Actual.Car.Price...."] <- "Actual.Price"

#convert characters to factors 
cars_$Transmission <- as.factor(cars_$Transmission)
cars_$Fuel.Type <- as.factor(cars_$Fuel.Type)
cars_$Indicated.Value <- as.factor(cars_$Indicated.Value)
cars_$Indicated.Value <- ordered(cars_$Indicated.Value, levels = c("Lower", "Great", "Good", "Fair", "Higher")) #reorder so in ascending to make confusion matrices easier to understand

################################################################################
######################## Simple Cart on split data set #########################
start_time <- Sys.time() #start timer

vector_simple = c() # empty vector for predictive performance

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i) #seed within the loop so that it changes 
  
  #Step 1) splitting the data 80:20 train:test
  cars_.sample = sample(1:13535, 10828)
  train_simple = cars_[cars_.sample, ]
  test_simple = cars_[-cars_.sample, ]
  
  #Step 2) training
  training_simple <- tree(Indicated.Value~., data = train_simple)
  
  #Step 3) testing
  predict_simple <- predict(training_simple, newdata = test_simple, type = "class")
  
  #Step 4) predictive performance
  cm_simple <- table(predict_simple, test_simple$Indicated.Value)
  ac_simple <- ((cm_simple[[1,1]] + cm_simple[[2,2]] + cm_simple[[3,3]] + cm_simple[[4,4]] + cm_simple[[5,5]])/sum (cm_simple))
  vector_simple[i] = ac_simple
}

#plotting tree
plot(training_simple); text(training_simple, pretty = 0); title("Simple CART on split dataset")

#boxplot of the classification rates over the 100 repetitions 
boxplot(vector_simple, main = 'Boxplot of Tree Classification Rates', ylab = 'Classification Rate', xlab = "Simple", col="darkseagreen")

#mean classification rate over the 100 repetitions 
cr_simple <- mean(vector_simple)
cr_simple

end_time <- Sys.time() #end timer
time_simple <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
############################# Pruning for over-fitting #########################
c_v_tree = cv.tree(training_simple, FUN = prune.misclass) #implement tree
plot(c_v_tree, main = "Misclassification Rates with Changing Size of Tree") #plot tree

start_time <- Sys.time() #start timer

vector_pruned = c() # empty vector for predictive performance

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i) #seed within the loop so that it changes 
  
  #Step 1) splitting the data 
  cars_.sample = sample(1:13535, 10828)
  train_pruned = cars_[cars_.sample, ]
  test_pruned = cars_[-cars_.sample, ]
  
  #Step 2) pruning - using 2 trees as the plot shows this has the minimum misclass. rate
  pruned_tree <- prune.misclass(training_simple, best=2)
  
  #Step 3) testing
  predict_pruned <- predict(pruned_tree, newdata = test_pruned, type = "class")
  
  #Step 4) predictive performance
  cm_pruned <- table(predict_pruned, test_pruned$Indicated.Value)
  ac_pruned <- ((cm_pruned[[1,1]] + cm_pruned[[2,2]] + cm_pruned[[3,3]] + cm_pruned[[4,4]] + cm_pruned[[5,5]]) / sum(cm_pruned))
  vector_pruned[i] = ac_pruned
}

#plotting tree
plot(pruned_tree); text(pruned_tree, pretty = 0); title("Pruned Tree")

#Boxplot of the means over the 100 repetitions
boxplot(vector_simple,vector_pruned, main = "Boxplot of Tree Classification Rates", ylab = "Classification Rate", names=c("Simple", "Pruned"), col="darkseagreen")

#Mean classification rate over the last 100 repetitions
cr_pruned <- mean(vector_pruned)
cr_pruned #note accuracy increase

end_time <- Sys.time() #end timer
time_pruned <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins

#finding classification rate ranges
which.min(vector_pruned)
vector_pruned[[47]]
which.max(vector_pruned)
vector_pruned[[80]]

################################################################################
################## Bagging - finding optimal number of trees ###################
####### didn't go beyond 150 trees because too computationally expensive #######
set.seed(321)

ac_bag_tree = double(150)
for (ntree_t in 1:150)
{
  #Step 1) training
  training_bag_tree <- randomForest(Indicated.Value~., data = train_simple, mtry=7, ntree=ntree_t, importance=TRUE)
  
  #Step 2) testing
  predict_bag_tree <- predict(training_bag_tree, newdata = test_simple)
  
  #Step 3) predictive performance
  cm_bag_tree <- table(predict_bag_tree, test_simple$Indicated.Value)
  ac_bag_tree[ntree_t] <- ((cm_bag_tree[[1,1]] + cm_bag_tree[[2,2]] + cm_bag_tree[[3,3]] + cm_bag_tree[[4,4]] + cm_bag_tree[[5,5]]) / sum(cm_bag_tree))
}
#Finding the number of model with highest accuracy
which.max(ac_bag_tree)

#plotting
plot(ac_bag_tree, main = "Bagging with Changing Number of Trees", ylab = "Classification Rate", xlab = "Number of Trees")
points(65, ac_bag_tree[65], col = "red", cex = 2, pch = 1, lwd = 1)

#prediction accuracy of that chosen model
ac_bag_tree[65]


################################################################################
#################### Bagging with optimal number of trees ######################
start_time <- Sys.time() #start timer

vector_bag_optimal = c()

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i)
  
  #Step 1) Splitting the data
  cars_.sample = sample(1:13535, 10828)
  train_bag_optimal = cars_[cars_.sample, ]
  test_bag_optimal = cars_[-cars_.sample, ]
  
  #Step 2) training
  training_bag_optimal <- randomForest(Indicated.Value~., data = train_bag_optimal, mtry = 7, ntree = 65, importance=TRUE)
  
  #Step 3) testing
  predict_bag_optimal <- predict(training_bag_optimal, newdata = test_bag_optimal)
  
  #Step 4) predictive performance
  cm_bag_optimal <- table(predict_bag_optimal, test_bag_optimal$Indicated.Value)
  ac_bag_optimal <- ((cm_bag_optimal[[1,1]] + cm_bag_optimal[[2,2]] + cm_bag_optimal[[3,3]] + cm_bag_optimal[[4,4]] + cm_bag_optimal[[5,5]]) / sum(cm_bag_optimal))
  vector_bag_optimal[i] = ac_bag_optimal
}

#boxplot of the means over the 100 repetitions
boxplot(vector_bag_optimal, main = "Boxplot of Bagging Classification Rates", ylab = 'Classification Rate')
boxplot(vector_simple,vector_pruned, vector_bag_optimal, main = "Boxplot of Tree Classification Rates", ylab = "Classification Rate", names=c("Simple", "Pruned", "Bagged"), col="darkseagreen")

#mean classification rate from the 100 repetitions
cr_bag_optimal <- mean(vector_bag_optimal)
cr_bag_optimal

end_time <- Sys.time() #end timer
time_bagged <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
############ Random Forests - Finding optimal number of predictors #############
set.seed(321)

ac_rf = double(7) #due to there being 7 predictor variables

for(mtry_t in 1:7)
{
  #Step 1) training
  training_rf <- randomForest(Indicated.Value~., data = train_simple, mtry = mtry_t, ntree=65, importance=TRUE) ###### NTREE MUST BE THE SAME AS THE BEST FOR BAGGING!
  
  #Step 2) testing 
  predict_rf <- predict(training_rf, newdata = test_simple)
  
  #Step 3) predictive performance
  cm_rf <- table(predict_rf, test_simple$Indicated.Value)
  ac_rf[mtry_t] <- ((cm_rf[[1,1]] + cm_rf[[2,2]] + cm_rf[[3,3]] + cm_rf[[4,4]] + cm_rf[[5,5]]) / sum(cm_rf))
}

#finding the model number with the highest accuracy
which.max(ac_rf)

plot(ac_rf, main = 'Predictive Performance with Changing Number of Predictors', ylab = 'Classification Rate', xlab = 'Number of Predictors', ylim = c(0.355, 0.380))
points(1,ac_rf[1], col = "red", cex = 2, pch = 1, lwd = 1)
points(2,ac_rf[2], col = "orange", cex = 2, pch = 1, lwd = 1)

#prediction accuracy of that chosen model
ac_rf[1]
ac_rf[2]


################################################################################
######## finding the importance of the variables - this is what RF does ########
importance(training_rf)
varImpPlot(training_rf)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#tune
t <- tuneRF(train_simple[,-8], train_simple[,8],
            stepFactor = 0.5,
            plot = T,
            ntreeTry = 65,
            trace = T, 
            imporove = 0.005)

################################################################################
########### Random Forests with optimal number of predictors - 2 ###############
start_time <- Sys.time() #start timer

vector_rf_optimal = c()

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i)
  
  #Step 1) Splitting the data
  cars_.sample = sample(1:13535, 10828)
  train_rf_optimal = cars_[cars_.sample, ]
  test_rf_optimal = cars_[-cars_.sample, ]
  
  #Step 2) training
  training_rf_optimal <- randomForest(Indicated.Value~., data = train_rf_optimal, mtry = 2, ntrees=65)
  
  #Step 3) testing
  predict_rf_optimal <- predict(training_rf_optimal, newdata = test_rf_optimal, type="class")
  
  #Step 4) predictive performance
  cm_rf_optimal <- table(predict_rf_optimal, test_rf_optimal$Indicated.Value)
  ac_rf_optimal <- ((cm_rf_optimal[[1,1]] + cm_rf_optimal[[2,2]] + cm_rf_optimal[[3,3]] + cm_rf_optimal[[4,4]] + cm_rf_optimal[[5,5]]) / sum(cm_rf_optimal))
  vector_rf_optimal[i] = ac_rf_optimal
}

#boxplot of the means over the 100 repetitions
boxplot(vector_rf_optimal, main = "Boxplot of Random Forest Classification Rates", ylab = 'Classification Rate')
boxplot(vector_simple, vector_pruned, vector_bag_optimal, vector_rf_optimal, main = "Boxplot of Tree Classification Rates", ylab = "Classification Rate", names=c("Simple", "Pruned", "Bagged", "Random Forest"), col="darkseagreen")

#mean classification rate from the 100 repetitions
cr_rf_optimal <- mean(vector_rf_optimal)
cr_rf_optimal

end_time <- Sys.time() #end timer
time_rf <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
######################### Boosting using a new package #########################
############################### setting up model ###############################
#Firstly, convert the response variable categories to ints starting at 0 by copying dataset under new name
cars_boosting <- cars_
cars_boosting$Indicated.Value <- (as.integer(cars_boosting$Indicated.Value)-1)

#split data
set.seed(1)
ind = sample(2, nrow(cars_boosting), replace = T, prob = c(0.8, 0.2))
train_boost = cars_boosting[ind==1,]
test_boost = cars_boosting[ind==2,]

#create matrix and one hot encoding for factor variables
#training matrix
train_m <- sparse.model.matrix(Indicated.Value~ .-train_boost$Indicated.Value, data=train_boost)
train_label <- train_boost[,"Indicated.Value"]
train_matrix <- xgb.DMatrix(data = as.matrix(train_m), label = train_label)

#testing matrix
test_m <- sparse.model.matrix(Indicated.Value~ .-test_boost$Indicated.Value, data=test_boost)
test_label <- test_boost[,"Indicated.Value"]
test_matrix <- xgb.DMatrix(data = as.matrix(test_m), label = test_label)

#parameters
nc <- length(unique(train_label)) 
watchlist <- list(train= train_matrix, test = test_matrix)

boosting_params <- list(objective = "multi:softmax",
                   eval_metric = "merror", 
                   num_class = nc)

################ finding optimal number of boosting iterations #################
set.seed(321)
b_i <- double(100)
for(b_i_it in 1:100)
{
  #Step 1) training
  training_boosting_b_i <- xgb.train(params = boosting_params, data = train_matrix, nrounds = b_i_it, watchlist = watchlist)
  
  #Step 2) testing
  predict_boosting_b_i <-  predict(training_boosting_b_i, newdata = test_matrix)
  
  #Step 3) predictive performance
  cm_boost_b_i <- table(predict_boosting_b_i, test_label)
  b_i[b_i_it] <- ((cm_boost_b_i[[1,1]] + cm_boost_b_i[[2,2]] + cm_boost_b_i[[3,3]] + cm_boost_b_i[[4,4]] + cm_boost_b_i[[5,5]])/sum(cm_boost_b_i))
}
#finding the number of model with the highest accuracy
which.max(b_i)

plot(b_i, main = 'Classification rates with changing number of boosting iterations', ylab = 'Classification Rate', xlab = 'Iteration')
points(38, b_i[38], col = "red", cex = 2, pch = 1, lwd = 1)

####################### finding optimal learning rate ##########################
set.seed(321)
eta_sequence <- seq(from = 0.01, to = 1, by = 0.01) #going from 0.01 to 1 (the max) over 100 iterations
eta_vect <- c()
for(eta_t in 1:100)
{
  #Step 1) training
  training_boosting_lr <- xgb.train(params = boosting_params, data = train_matrix, nrounds = 38, watchlist = watchlist, eta = (eta_t/100))
  
  #Step 2) testing
  predict_boosting_lr <-  predict(training_boosting_lr, newdata = test_matrix)
  
  #Step 3) predictive performance
  cm_boost_lr <- table(predict_boosting_lr, test_label)
  eta_vect[eta_t] <- ((cm_boost_lr[[1,1]] + cm_boost_lr[[2,2]] + cm_boost_lr[[3,3]] + cm_boost_lr[[4,4]] + cm_boost_lr[[5,5]])/sum(cm_boost_lr))
}
#finding the number of model with the highest accuracy
which.max(eta_vect)

#plotting all 
plot(eta_vect, main = 'Classification rates with changing learning rate', ylab = 'Classification Rate', xlab = 'Iteration')
points(36, eta_vect[36], col = "red", cex = 2, pch = 1, lwd = 1)

#prediction accuracy of that chosen model
eta_vect[36]

#size of shrinkage for that model
eta_sequence[36]

####### Finding optimal max depth (maximum nodes per tree) - default = 6 ####### 
set.seed(123)
md <- double(10)
for(tree_depth in 1:10)
{
  #Step 1) training
  training_boosting_md <- xgb.train(params = boosting_params, data = train_matrix, max_depth = tree_depth, nrounds = 38, watchlist = watchlist, eta = 0.36)
  
  #Step 2) testing
  predict_boosting_md <-  predict(training_boosting_md, newdata = test_matrix)
  
  #Step 3) predictive performance
  cm_boost_md <- table(predict_boosting_md, test_label)
  md[tree_depth] <- ((cm_boost_md[[1,1]] + cm_boost_md[[2,2]] + cm_boost_md[[3,3]] + cm_boost_md[[4,4]] + cm_boost_md[[5,5]])/sum(cm_boost_md))
}
#finding the model with the highest accuracy
which.max(md)

#plotting all
plot(md, main = 'Misclassification rates with changing maximum depth', ylab = 'Classification Rate', xlab = 'Max depth')
points(4, md[4], col = "red", cex = 2, pch = 1, lwd = 1)

#Accuracy of that model
md[4]

################# FINAL BOOSTING MODEL WITH OPTIMAL PARAMTERS ##################
start_time <- Sys.time() #start timer

vector_boost_optimal = c()

repetitions = 100
for(i in 1:repetitions)
{
  #Step 1) Splitting the data
  set.seed(i)
  cars_boosting.sample = sample(1:13535, 10828)
  train_boost = cars_boosting[cars_boosting.sample, ]
  test_boost = cars_boosting[-cars_boosting.sample, ]
  
  #create matrix and one hot encoding for factor variables
  #training matrix
  train_m <- sparse.model.matrix(Indicated.Value~ .-train_boost$Indicated.Value, data=train_boost)
  train_label <- train_boost[,"Indicated.Value"]
  train_matrix <- xgb.DMatrix(data = as.matrix(train_m), label = train_label)
  
  #testing matrix
  test_m <- sparse.model.matrix(Indicated.Value~ .-test_boost$Indicated.Value, data=test_boost)
  test_label <- test_boost[,"Indicated.Value"]
  test_matrix <- xgb.DMatrix(data = as.matrix(test_m), label = test_label)
  
  #Step 2) training
  training_boosting_optimal <- xgb.train(params = boosting_params, data = train_matrix, max_depth = 4, nrounds = 38, watchlist = watchlist, eta = 0.36)
  
  #Step 3) testing
  predict_boosting_optimal <-  predict(training_boosting_optimal, newdata = test_matrix)
  
  #Step 3) predictive performance
  cm_boost_optimal <- table(predict_boosting_optimal, test_label)
  ac_boost_optimal <- ((cm_boost_optimal[[1,1]] + cm_boost_optimal[[2,2]] + cm_boost_optimal[[3,3]] + cm_boost_optimal[[4,4]] + cm_boost_optimal[[5,5]])/sum(cm_boost_optimal))
  vector_boost_optimal[i] = ac_boost_optimal
}

#boxplot of the means over the 100 repetitions
boxplot(vector_boost_optimal, main = "Boxplot of Optimal Boosting Classification Rates", ylab = 'Classification Rate')
boxplot(vector_simple, vector_pruned, vector_bag_optimal, vector_rf_optimal, vector_boost_optimal, main = "Boxplot of Tree Classification Rates", ylab = "Classification Rate", names=c("Simple", "Pruned", "Bagged", "Random Forest", "Boosted"), col="darkseagreen", ylim=c(0.34, 0.43))

#mean classification rate from the 100 repetitions
cr_boost_optimal <- mean(vector_boost_optimal)
cr_boost_optimal

end_time <- Sys.time() #end timer
time_boosted <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
######################## plotting the different methods ########################
#boxplots of all models
boxplot(vector_simple, vector_pruned, vector_bag_optimal, vector_rf_optimal, vector_boost_optimal, names = c('Simple', 'Pruned', 'Bagged', 'R-Forest', 'Boosted'), main = "Classification Rates of Tree-Based Methods", ylab = 'Classification Rate', xlab = "Model", col ='darkseagreen', ylim=c(0.34, 0.43))

#BARPLOT OF MEAN CLASSIFICATION RATE
class_rates = c(cr_simple*100, cr_pruned*100, cr_bag_optimal*100, cr_rf_optimal*100, cr_boost_optimal*100)
class_rates
class_rates = round(class_rates, digits = 3)

x = barplot(class_rates, main = "Mean Classification Accuracies of Tree-Based Methods", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Simple', 'Pruned', 'Bagged', 'R-Forest', 'Boosted'), ylim = c(0,100), col = "darkseagreen")
y = as.matrix(class_rates)
text(x,y+2, labels = as.character(y))

#barplots of timings for reduced models
time_5 <- c(time_simple, time_pruned, time_bagged, time_rf, time_boosted)
time_5 <- round(time_5, digits = 3)

x_time_5 <- barplot(rev(time_5), main = "Model Times", ylab = 'Model', xlab = "Time Taken (mins)", names.arg = c('Boosted', 'R-Forest', 'Bagged', 'Pruned', 'Simple'), col = "darkseagreen", xlim = c(0,20), horiz = T)
y_time_5 <- as.matrix(rev(time_5))
text(y_time_5+1,x_time_5, labels = as.character(y_time_5))


################################################################################
################ RE-DOING WITH 3 EVENLY DISTRIBUTED CATEGORIES #################
#copy the dataset to a new name for the following
cars_3 <- cars_
str(cars_3)

#Reduce to 3 categories by combined "Lower" and "Great" and "Fair" and "Higher"
cars_3$Indicated.Value <- recode_factor(cars_3$Indicated.Value, Great = "Lower", Fair = "Higher")
str(cars_3) #check it has worked

#plot this
iv_3 <- factor(cars_3$Indicated.Value, levels = c("Lower", "Good", "Higher"))
x = barplot(table(iv_3), main = "Combined Categories Before Down-Sampling", ylim = c(0,8500), col = c("green4", "lemonchiffon1", "red3"), ylab = "Quantity of Cars", xlab = "Indicated Value")
y = as.matrix(table(iv_3))
text(x,y+150, labels = as.character(y))

#down-sample so the three categories are evenly distributed in-line with the least frequent category (lower)
set.seed(1)
table(cars_3$Indicated.Value) #check uneven distribution 

cars_3 <- downSample(x = cars_3[,-8], y = cars_3$Indicated.Value) #down-sample the variable
names(cars_3)[names(cars_3) == "Class"] <- "Indicated.Value" #rename column
table(cars_3$Indicated.Value) #check it has worked

#shuffle and re-index the dataframe so it is no longer sorted by response variable
set.seed(1)
ars_3 <- cars_3[sample(1:nrow(cars_3)), ] 
row.names(cars_3) <- NULL

#plot after downsampling
iv_3_d <- factor(cars_3$Indicated.Value, levels = c("Lower", "Good", "Higher"))
x = barplot(table(iv_3_d), main = "Combined Categories After Down-Sampling", ylim = c(0,8500), col = c("green4", "lemonchiffon1", "red3"), ylab = "Quantity of Cars", xlab = "Indicated Value")
y = as.matrix(table(iv_3_d))
text(x,y+150, labels = as.character(y))

################################################################################
######################### Simple Cart on split dataset #########################
start_time <- Sys.time() #start timer

vector_simple_reduced = c() # empty vector for predictive performance

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i) #seed within the loop so that it changes 
  
  #Step 1) splitting the data 80:20 train:test
  cars_3.sample = sample(1:5820, 4656)
  train_simple_reduced = cars_3[cars_3.sample, ]
  test_simple_reduced = cars_3[-cars_3.sample, ]
  
  #Step 2) training
  training_simple <- tree(Indicated.Value~., data = train_simple_reduced)
  
  #Step 3) testing
  predict_simple <- predict(training_simple, newdata = test_simple_reduced, type = "class")
  
  #Step 4) predictive performance
  cm_simple_reduced <- table(predict_simple, test_simple_reduced$Indicated.Value)
  ac_simple_reduced <- ((cm_simple_reduced[[1,1]] + cm_simple_reduced[[2,2]] + cm_simple_reduced[[3,3]])/sum (cm_simple_reduced))
  vector_simple_reduced[i] = ac_simple_reduced
}

#plotting tree
plot(training_simple); text(training_simple, pretty = 0)

#boxplot of the means over the 100 repetitions 
boxplot(vector_simple_reduced, main = 'Boxplot of Tree Classification Rates', ylab = 'Classification Accuracy', xlab = "Simple", col = "turquoise")
boxplot(vector_simple, vector_simple_reduced, main = "Boxplot of Tree Classification Rates", ylab = "Classification Rate", names=c("Simple-5", "Simple-3"), col=c("darkseagreen", "turquoise"), ylim =c(0.35,0.48))

#mean classification rate over the 100 repetitions 
cr_simple_reduced <- mean(vector_simple_reduced)
cr_simple_reduced

end_time <- Sys.time() #end timer
time_split_no_makes_reduced <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
########################## Pruning for over-fitting ############################
c_v_tree = cv.tree(training_simple, FUN = prune.misclass)
plot(c_v_tree)

start_time <- Sys.time() #start timer

vector_pruned_reduced = c() # empty vector for predictive performance

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i) #seed within the loop so that it changes 
  
  #Step 1) splitting the data 
  cars_3.sample = sample(1:5820, 4656)
  train_pruned_reduced = cars_3[cars_3.sample, ]
  test_pruned_reduced = cars_3[-cars_3.sample, ]
  
  #Step 2) pruning - using 2 trees as the plot shows this has the minimum misclass. rate
  pruned_tree <- prune.misclass(training_simple, best=3)
  
  #Step 3) testing
  predict_pruned <- predict(pruned_tree, newdata = test_pruned_reduced, type = "class")
  
  #Step 4) predictive performance
  cm_pruned_reduced <- table(predict_pruned, test_pruned_reduced$Indicated.Value)
  ac_pruned_reduced <- ((cm_pruned_reduced[[1,1]] + cm_pruned_reduced[[2,2]] + cm_pruned_reduced[[3,3]]) / sum(cm_pruned_reduced))
  vector_pruned_reduced[i] = ac_pruned_reduced
}

#plotting tree
plot(pruned_tree); text(pruned_tree, pretty = 0)

#Boxplot of the means over the 100 repetitions
boxplot(vector_pruned_reduced, main = "Boxplot of Pruned Tree Classification Rate", ylab = "Classification Rate")
boxplot(vector_simple_reduced, vector_pruned_reduced, main = 'Boxplot of Tree Classification Rates', ylab = 'Classification Rate', names = c("Simple", "Pruned"), col = "turquoise")

#Mean classification rate over the last 100 repetitions
cr_pruned_reduced <- mean(vector_pruned_reduced)
cr_pruned_reduced #note accuracy increase

end_time <- Sys.time() #end timer
time_pruned_reduced <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins

################################################################################
################ Bagging - finding optimal number of trees #####################
###### didn't go beyond 150 trees because too computationally expensive ########
set.seed(321)

ac_bag_tree_reduced = double(150)
for (ntree_t_reduced in 1:150)
{
  #Step 1) training
  training_bag_tree <- randomForest(Indicated.Value~., data = train_simple_reduced, mtry=7, ntree=ntree_t_reduced, importance=TRUE)
  
  #Step 2) testing
  predict_bag_tree <- predict(training_bag_tree, newdata = test_simple_reduced)
  
  #Step 3) predictive performance
  cm_bag_tree_reduced <- table(predict_bag_tree, test_simple_reduced$Indicated.Value)
  ac_bag_tree_reduced[ntree_t_reduced] <- ((cm_bag_tree_reduced[[1,1]] + cm_bag_tree_reduced[[2,2]] + cm_bag_tree_reduced[[3,3]]) / sum(cm_bag_tree_reduced))
}
#Finding the number of model with highest accuracy
which.max(ac_bag_tree_reduced)

#plotting
plot(ac_bag_tree_reduced, main = "Bagging with Changing Number of Trees", ylab = "Classification Accuracy", xlab = "Number of Trees", cex = 0.5)
points(72, ac_bag_tree_reduced[72], col = "red", cex = 2, pch = 1, lwd = 1)

#prediction accuracy of that chosen model
ac_bag_tree_reduced[72]


################################# Bagging with optimal number of trees ###################################
start_time <- Sys.time() #start timer

vector_bag_optimal_reduced = c()

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i)
  
  #Step 1) Splitting the data
  cars_3.sample = sample(1:5820, 4656)
  train_bag_optimal_reduced = cars_3[cars_3.sample, ]
  test_bag_optimal_reduced = cars_3[-cars_3.sample, ]
  
  #Step 2) training
  training_bag_optimal <- randomForest(Indicated.Value~., data = train_bag_optimal_reduced, mtry = 7, ntree = 72, importance=TRUE)
  
  #Step 3) testing
  predict_bag_optimal <- predict(training_bag_optimal, newdata = test_bag_optimal_reduced)
  
  #Step 4) predictive performance
  cm_bag_optimal_reduced <- table(predict_bag_optimal, test_bag_optimal_reduced$Indicated.Value)
  ac_bag_optimal_reduced <- ((cm_bag_optimal_reduced[[1,1]] + cm_bag_optimal_reduced[[2,2]] + cm_bag_optimal_reduced[[3,3]]) / sum(cm_bag_optimal_reduced))
  vector_bag_optimal_reduced[i] = ac_bag_optimal_reduced
}

#boxplot of the means over the 100 repetitions
boxplot(vector_bag_optimal_reduced, main = "Boxplot of Bagging Classification Rates", ylab = 'Classification Accuracy')
boxplot(vector_simple_reduced, vector_pruned_reduced, vector_bag_optimal_reduced , main = 'Boxplot of Tree Classification Rates', ylab = 'Classification Rate', names = c("Simple", "Pruned", "Bagged"), col = "turquoise")

#mean classification rate from the 100 repetitions
cr_bag_optimal_reduced <- mean(vector_bag_optimal_reduced)
cr_bag_optimal_reduced

end_time <- Sys.time() #end timer
time_bagged_optimal_reduced <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


##################### Random Forests - Finding optimal number of predictors #################
set.seed(321)

ac_rf_reduced = double(7) #due to there being 7 predictor variables

for(mtry_t_reduced in 1:7)
{
  #Step 1) training
  training_rf <- randomForest(Indicated.Value~., data = train_simple_reduced, mtry = mtry_t_reduced, ntree=72, importance=TRUE)
  
  #Step 2) testing 
  predict_rf <- predict(training_rf, newdata = test_simple_reduced)
  
  #Step 3) predictive performance
  cm_rf_reduced <- table(predict_rf, test_simple_reduced$Indicated.Value)
  ac_rf_reduced[mtry_t_reduced] <- ((cm_rf_reduced[[1,1]] + cm_rf_reduced[[2,2]] + cm_rf_reduced[[3,3]]) / sum(cm_rf_reduced))
}

#finding the model number with the highest accuracy
which.max(ac_rf_reduced)

plot(ac_rf_reduced, main = 'Predictive performance with changing number of predictors', ylab = 'Classification Rate', xlab = 'Number of Predictors')
points(2, ac_rf_reduced[2], col = "red", cex = 2, pch = 1, lwd = 1)

#prediction accuracy of that chosen model
ac_rf_reduced[2]


################################################################################
######## finding the importance of the variables - this is what RF does ########
importance(training_rf)
varImpPlot(training_rf)


################################# Random Forests with optimal number of predictors ##################################
start_time <- Sys.time() #start timer

vector_rf_optimal_reduced = c()

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i)
  
  #Step 1) Splitting the data
  cars_3.sample = sample(1:5820, 4656)
  train_rf_optimal_reduced = cars_3[cars_3.sample, ]
  test_rf_optimal_reduced = cars_3[-cars_3.sample, ]
  
  #Step 2) training
  training_rf_optimal <- randomForest(Indicated.Value~., data = train_rf_optimal_reduced, mtry = 2, ntrees=72)
  
  #Step 3) testing
  predict_rf_optimal <- predict(training_rf_optimal, newdata = test_rf_optimal_reduced, type="class")
  
  #Step 4) predictive performance
  cm_rf_optimal_reduced <- table(predict_rf_optimal, test_rf_optimal_reduced$Indicated.Value)
  ac_rf_optimal_reduced <- ((cm_rf_optimal_reduced[[1,1]] + cm_rf_optimal_reduced[[2,2]] + cm_rf_optimal_reduced[[3,3]]) / sum(cm_rf_optimal_reduced))
  vector_rf_optimal_reduced[i] = ac_rf_optimal_reduced
}

#boxplot of the means over the 100 repetitions
boxplot(vector_rf_optimal_reduced, main = "Boxplot of Random Forest Classification Rates", ylab = 'Classification Accuracy')
boxplot(vector_simple_reduced, vector_pruned_reduced, vector_bag_optimal_reduced, vector_rf_optimal_reduced, main = 'Boxplot of Tree Classification Rates', ylab = 'Classification Rate', names = c("Simple", "Pruned", "Bagged", "Random Forest"), col = "turquoise")

#mean classification rate from the 100 repetitions
cr_rf_optimal_reduced <- mean(vector_rf_optimal_reduced)
cr_rf_optimal_reduced

end_time <- Sys.time() #end timer
time_rf_reduced <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


######################### Boosting using a new package #########################
############################### setting up model ###############################
#Firstly, convert the response variable categories to ints starting at 0 by copying dataset under new name
cars_boosting_reduced <- cars_3
cars_boosting_reduced$Indicated.Value <- (as.integer(cars_boosting_reduced$Indicated.Value)-1)

#split data
set.seed(1)
samples = sample(2, nrow(cars_boosting_reduced), replace = T, prob = c(0.8, 0.2))
train_boost_reduced = cars_boosting_reduced[samples==1,]
test_boost_reduced = cars_boosting_reduced[samples==2,]

#create matrix and one hot encoding for factor variables
#training matrix
train_m_reduced <- sparse.model.matrix(Indicated.Value~ .-train_boost_reduced$Indicated.Value, data=train_boost_reduced)
train_label_reduced <- train_boost_reduced[,"Indicated.Value"]
train_matrix_reduced <- xgb.DMatrix(data = as.matrix(train_m_reduced), label = train_label_reduced)

#testing matrix
test_m_reduced <- sparse.model.matrix(Indicated.Value~ .-test_boost_reduced$Indicated.Value, data=test_boost_reduced)
test_label_reduced <- test_boost_reduced[,"Indicated.Value"]
test_matrix_reduced <- xgb.DMatrix(data = as.matrix(test_m_reduced), label = test_label_reduced)

#parameters
nc <- length(unique(train_label_reduced)) 
watchlist <- list(train= train_matrix_reduced, test = test_matrix_reduced)

#same as in larger dataset but re-do anyway
boosting_params <- list(objective = "multi:softmax",
                        eval_metric = "merror", 
                        num_class = nc)

################ finding optimal number of boosting iterations #################
set.seed(321)
b_i_reduced <- double(100)
for(b_i_it_reduced in 1:100)
{
  #Step 1) training
  training_boosting_b_i <- xgb.train(params = boosting_params, data = train_matrix_reduced, nrounds = b_i_it_reduced, watchlist = watchlist)
  
  #Step 2) testing
  predict_boosting_b_i <-  predict(training_boosting_b_i, newdata = test_matrix_reduced)
  
  #Step 3) predictive performance
  cm_boost_b_i_reduced <- table(predict_boosting_b_i, test_label_reduced)
  b_i_reduced[b_i_it_reduced] <- ((cm_boost_b_i_reduced[[1,1]] + cm_boost_b_i_reduced[[2,2]] + cm_boost_b_i_reduced[[3,3]])/sum(cm_boost_b_i_reduced))
}
#finding the number of model with the highest accuracy
which.max(b_i_reduced)

plot(b_i_reduced, main = 'Classification rates with changing number of boosting iterations', ylab = 'Classification Rate', xlab = 'Iteration')
points(30, b_i_reduced[30], col = "red", cex = 2, pch = 1, lwd = 1)


######################## finding optimal learning rate  ########################
set.seed(321)
eta_sequence_reduced <- seq(from = 0.01, to = 1, by = 0.01) #going from 0.01 to 1 (the max) over 100 iterations
eta_vect_reduced <- c()
for(eta_t_reduced in 1:100)
{
  #Step 1) training
  training_boosting_lr <- xgb.train(params = boosting_params, data = train_matrix_reduced, nrounds = 30, watchlist = watchlist, eta = (eta_t_reduced/100))
  
  #Step 2) testing
  predict_boosting_lr <-  predict(training_boosting_lr, newdata = test_matrix_reduced)
  
  #Step 3) predictive performance
  cm_boost_lr_reduced <- table(predict_boosting_lr, test_label_reduced)
  eta_vect_reduced[eta_t_reduced] <- ((cm_boost_lr_reduced[[1,1]] + cm_boost_lr_reduced[[2,2]] + cm_boost_lr_reduced[[3,3]])/sum(cm_boost_lr_reduced))
}
#finding the number of model with the highest accuracy
which.max(eta_vect_reduced)

#plotting all 
plot(eta_vect_reduced, main = 'Classification rates with changing learning rate', ylab = 'Classification Rate', xlab = 'Iteration')
points(30, eta_vect_reduced[30], col = "red", cex = 2, pch = 1, lwd = 1)
points(99, eta_vect_reduced[99], col = "orange", cex = 2, pch = 1, lwd = 1)

#prediction accuracy of that chosen model
eta_vect_reduced[30]
eta_vect_reduced[99]

#size of shrinkage for that model
eta_sequence_reduced[30]


####### Finding optimal max depth (maximum nodes per tree) - default = 6 #######
set.seed(123)
md_reduced <- double(10)
for(tree_depth_reduced in 1:10)
{
  #Step 1) training
  training_boosting_md <- xgb.train(params = boosting_params, data = train_matrix_reduced, max_depth = tree_depth_reduced, nrounds = 30, watchlist = watchlist, eta = 0.3)
  
  #Step 2) testing
  predict_boosting_md <-  predict(training_boosting_md, newdata = test_matrix_reduced)
  
  #Step 3) predictive performance
  cm_boost_md_reduced <- table(predict_boosting_md, test_label_reduced)
  md_reduced[tree_depth_reduced] <- ((cm_boost_md_reduced[[1,1]] + cm_boost_md_reduced[[2,2]] + cm_boost_md_reduced[[3,3]])/sum(cm_boost_md_reduced))
}
#finding the model with the highest accuracy
which.max(md_reduced)

#plotting all
plot(md_reduced, main = 'Misclassification rates with changing interaction depth', ylab = 'Classification Rate', xlab = 'Max depth')
points(6, md_reduced[6], col = "orange", cex = 2, pch = 1, lwd = 1)
points(3, md_reduced[3], col = "red", cex = 2, pch = 1, lwd = 1)

#Accuracy of that model
md_reduced[6]

################# FINAL BOOSTING MODEL WITH OPTIMAL PARAMTERS ##################
start_time <- Sys.time() #start timer

vector_boost_optimal_reduced = c()

repetitions = 100
for(i in 1:repetitions)
{
  #Step 1) Splitting the data
  set.seed(i)
  samples = sample(2, nrow(cars_boosting_reduced), replace = T, prob = c(0.8, 0.2))
  train_boost_reduced = cars_boosting_reduced[samples==1,]
  test_boost_reduced = cars_boosting_reduced[samples==2,]
  
  #create matrix and one hot encoding for factor variables
  #training matrix
  train_m_reduced <- sparse.model.matrix(Indicated.Value~ .-train_boost_reduced$Indicated.Value, data=train_boost_reduced)
  train_label_reduced <- train_boost_reduced[,"Indicated.Value"]
  train_matrix_reduced <- xgb.DMatrix(data = as.matrix(train_m_reduced), label = train_label_reduced)
  
  #testing matrix
  test_m_reduced <- sparse.model.matrix(Indicated.Value~ .-test_boost_reduced$Indicated.Value, data=test_boost_reduced)
  test_label_reduced <- test_boost_reduced[,"Indicated.Value"]
  test_matrix_reduced <- xgb.DMatrix(data = as.matrix(test_m_reduced), label = test_label_reduced)
  
  #Step 2) training
  training_boosting_optimal <- xgb.train(params = boosting_params, data = train_matrix_reduced, max_depth = 3, nrounds = 30, watchlist = watchlist, eta = 0.3)
  
  #Step 3) testing
  predict_boosting_optimal <-  predict(training_boosting_optimal, newdata = test_matrix_reduced)
  
  #Step 3) predictive performance
  cm_boost_optimal_reduced <- table(predict_boosting_optimal, test_label_reduced)
  ac_boost_optimal_reduced <- ((cm_boost_optimal_reduced[[1,1]] + cm_boost_optimal_reduced[[2,2]] + cm_boost_optimal_reduced[[3,3]])/sum(cm_boost_optimal_reduced))
  vector_boost_optimal_reduced[i] = ac_boost_optimal_reduced
}

#boxplot of the means over the 100 repetitions
boxplot(vector_boost_optimal_reduced, main = "Boxplot of Optimal Boosting Classification Rates", ylab = 'Classification Accuracy')
boxplot(vector_simple_reduced, vector_pruned_reduced, vector_bag_optimal_reduced, vector_rf_optimal_reduced, vector_boost_optimal_reduced, main = 'Boxplot of Tree Classification Rates', ylab = 'Classification Rate', names = c("Simple", "Pruned", "Bagged", "Random Forest", "Boosted"), col = "turquoise")

#mean classification rate from the 100 repetitions
cr_boost_optimal_reduced <- mean(vector_boost_optimal_reduced)
cr_boost_optimal_reduced

end_time <- Sys.time() #end timer
time_boosted_reduced <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
######################## plotting the different methods ########################
#boxplots of all reduced models
boxplot(vector_simple_reduced, vector_pruned_reduced, vector_bag_optimal_reduced, vector_rf_optimal_reduced, vector_boost_optimal_reduced, names = c('Simple', 'Pruned', 'Bagged', 'R-Forest', 'Boosted'), main = "Classification Rates of Tree-Based Methods with Evenly Distributed Response Variables", ylab = 'Classification Rate', xlab = "Model", col ='turquoise')

#boxplots of both 5 and 3 category trees
boxplot(vector_simple, vector_pruned, vector_bag_optimal, vector_rf_optimal, vector_boost_optimal, vector_simple_reduced, vector_pruned_reduced, vector_bag_optimal_reduced, vector_rf_optimal_reduced, vector_boost_optimal_reduced, names = c('Simple-5', 'Pruned-5', 'Bagged-5', 'R-Forest-5', 'Boosted-5', 'Simple-3', 'Pruned-3', 'Bagged-3', 'R-Forest-3', 'Boosted-3'), main = "Classification Rates of all Tree-Based Methods", ylab = 'Classification Rate', xlab = "Model", col =c('darkseagreen','darkseagreen','darkseagreen','darkseagreen','darkseagreen', "turquoise", "turquoise", "turquoise", "turquoise", "turquoise"))

#BARPLOT OF MEAN CLASSIFICATION RATE
class_rates_reduced <- c(cr_simple_reduced*100, cr_pruned_reduced*100, cr_bag_optimal_reduced*100, cr_rf_optimal_reduced*100, cr_boost_optimal_reduced*100)
class_rates_reduced
class_rates_reduced <- round(class_rates_reduced, digits = 3)

x_reduced <- barplot(class_rates_reduced, main = "Mean Classification Accuracies of Tree-Based Methods with Evenly Distributed Response Variables", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Simple', 'Pruned', 'Bagged', 'R-Forest', 'Boosted'), ylim = c(0,100), col = "turquoise")
y_reduced <- as.matrix(class_rates_reduced)
text(x_reduced,y_reduced+2, labels = as.character(y_reduced))

#barplots of classification rates for both 5 and 3
class_rates_combined <- c(class_rates, class_rates_reduced)
x_combined_acc <- barplot(class_rates_combined, main = "Mean Classification Accuracies of all Tree-Based Methods", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Simple-5', 'Pruned-5', 'Bagged-5', 'R-Forest-5', 'Boosted-5', 'Simple-3', 'Pruned-3', 'Bagged-3', 'R-Forest-3', 'Boosted-3'), ylim = c(0,100), col = c('darkseagreen','darkseagreen','darkseagreen','darkseagreen','darkseagreen',"turquoise","turquoise","turquoise","turquoise","turquoise"))
y_combined_acc <- as.matrix(class_rates_combined)
text(x_combined_acc,y_combined_acc+2, labels = as.character(y_combined_acc))

#barplots of mean classification accuracy of all models within both data sets
mean_class_rates_combined <- c(mean(class_rates), mean(class_rates_reduced))
x_mean_combined_acc <- barplot(mean_class_rates_combined, main = "Mean Classification Accuracies of all models from original dataset vs all models from reduced dataset", ylab = "Classification Accuracy (%)", xlab = "Models", names.arg=c("Original Dataset", "Reduced Dataset"), ylim=c(0,100), col = c('darkseagreen', 'turquoise'))
y_combined_acc <- as.matrix(class_rates_combined)
text(x_combined_acc,y_combined_acc+2, labels = as.character(y_combined_acc))

#barplots of timings for reduced models
time_reduced <- c(time_split_no_makes_reduced, time_pruned_reduced, time_bagged_optimal_reduced, time_rf_reduced, time_boosted_reduced)
time_reduced <- round(time_reduced, digits = 3)

x_time_reduced <- barplot(time_reduced, main = "Time Taken for Model", ylab = 'Time Taken (mins)', xlab = "Model", names.arg = c('Simple', 'Pruned', 'Bagged', 'R-Forest', 'Boosted'), col = "blue", ylim = c(0,7))
y_time_reduced <- as.matrix(time_reduced)
text(x_time_reduced,y_time_reduced+0.2, labels = as.character(y_time_reduced))

#barplots of timings for both 5 and 3
times_combined <- c(time_5, time_reduced)

y_combined_time <- barplot(rev(times_combined), main = "Time Taken for all Models", ylab = 'Model', xlab = "Time Taken (mins)", names.arg = rev(c('Simple-5', 'Pruned-5', 'Bagged-5', 'R-Frst-5', 'Boosted-5', 'Simple-3', 'Pruned-3', 'Bagged-3', 'R-Frst-3', 'Boosted-3')), xlim = c(0,20), horiz = T, col = rev(c("darkseagreen","darkseagreen","darkseagreen","darkseagreen","darkseagreen","turquoise","turquoise","turquoise","turquoise","turquoise")))
x_combined_time <- as.matrix(times_combined)
text(x_combined_time+0.3, rev(y_combined_time), labels = as.character(x_combined_time))



################################################################################
############################### Trees with 1 make ##############################
################################################################################
##### Load library with all car makes and models but drop the make column ######
cars_make <- read.csv(file="/Users/benkight/Documents/Dissertation/Python - Scraping/Engineered Dataset - top make.csv",sep=",")[,-c(1:2)] #drop index and make columns

############## Rename columns and transform characters to factors ############## 
str(cars_make)

#rename columns
names(cars_make)[names(cars_make) == "Mean.Model.Price...."] <- "Mean.Model.Price"
names(cars_make)[names(cars_make) == "Engine.Size..L."] <- "Engine.Size"
names(cars_make)[names(cars_make) == "Actual.Car.Price...."] <- "Actual.Price"

#convert characters to factors 
cars_make$Transmission <- as.factor(cars_make$Transmission)
cars_make$Fuel.Type <- as.factor(cars_make$Fuel.Type)
cars_make$Indicated.Value <- as.factor(cars_make$Indicated.Value)

################################################################################
######################## Simple Cart on split data set #########################
start_time <- Sys.time() #start timer

vector_make_simple = c() # empty vector for predictive performance

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i) #seed within the loop so that it changes 
  
  #Step 1) splitting the data 80:20 train:test
  cars_make.sample = sample(1:13535, 10828)
  train_make_simple = cars_make[cars_make.sample, ]
  test_make_simple = cars_make[-cars_make.sample, ]
  
  #Step 2) training
  training_make_simple <- tree(Indicated.Value~., data = train_make_simple)
  
  #Step 3) testing
  predict_make_simple <- predict(training_make_simple, newdata = test_make_simple, type = "class")
  
  #Step 4) predictive performance
  cm_make_simple <- table(predict_make_simple, test_make_simple$Indicated.Value)
  ac_make_simple <- ((cm_make_simple[[1,1]] + cm_make_simple[[2,2]] + cm_make_simple[[3,3]] + cm_make_simple[[4,4]] + cm_make_simple[[5,5]])/sum (cm_make_simple))
  vector_make_simple[i] = ac_make_simple
}

#plotting tree
plot(training_make_simple); text(training_make_simple, pretty = 0)

#boxplot of the classification rates over the 100 repetitions 
boxplot(vector_make_simple, main = 'Boxplot of Simple Tree Classification Rates', ylab = 'Classification Rate', xlab = "Simple", col = "plum2")

#mean classification rate over the 100 repetitions 
cr_make_simple <- mean(vector_make_simple)
cr_make_simple

end_time <- Sys.time() #end timer
time_make_simple <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins

c_v_tree = cv.tree(training_make_simple, FUN = prune.misclass)
plot(c_v_tree)

################################################################################
######################### Boosting using a new package #########################
############################### setting up model ###############################
#Firstly, convert the response variable categories to ints starting at 0 by copying dataset under new name
cars_make_boosting <- cars_make
cars_make_boosting$Indicated.Value <- (as.integer(cars_make_boosting$Indicated.Value)-1)

#split data
set.seed(1)
ind = sample(2, nrow(cars_make_boosting), replace = T, prob = c(0.8, 0.2))
train_make_boost = cars_make_boosting[ind==1,]
test_make_boost = cars_make_boosting[ind==2,]

#create matrix and one hot encoding for factor variables
#training matrix
train_m <- sparse.model.matrix(Indicated.Value~ .-train_make_boost$Indicated.Value, data=train_make_boost)
train_label <- train_make_boost[,"Indicated.Value"]
train_matrix <- xgb.DMatrix(data = as.matrix(train_m), label = train_label)

#testing matrix
test_m <- sparse.model.matrix(Indicated.Value~ .-test_make_boost$Indicated.Value, data=test_make_boost)
test_label <- test_make_boost[,"Indicated.Value"]
test_matrix <- xgb.DMatrix(data = as.matrix(test_m), label = test_label)

#parameters
nc <- length(unique(train_label)) 
watchlist <- list(train= train_matrix, test = test_matrix)

boosting_params <- list(objective = "multi:softmax",
                        eval_metric = "merror", 
                        num_class = nc)

################ finding optimal number of boosting iterations #################
set.seed(321)
b_i_make <- double(100)
for(b_make_i_it in 1:100)
{
  #Step 1) training
  training_make_boosting_b_i <- xgb.train(params = boosting_params, data = train_matrix, nrounds = b_make_i_it, watchlist = watchlist)
  
  #Step 2) testing
  predict_make_boosting_b_i <-  predict(training_make_boosting_b_i, newdata = test_matrix)
  
  #Step 3) predictive performance
  cm_make_boost_b_i <- table(predict_make_boosting_b_i, test_label)
  b_i_make[b_make_i_it] <- ((cm_make_boost_b_i[[1,1]] + cm_make_boost_b_i[[2,2]] + cm_make_boost_b_i[[3,3]] + cm_make_boost_b_i[[4,4]] + cm_make_boost_b_i[[5,5]])/sum(cm_make_boost_b_i))
}
#finding the number of model with the highest accuracy
which.max(b_i_make)

plot(b_i_make, main = 'Classification rates with changing number of boosting iterations', ylab = 'Classification Rate', xlab = 'Iteration')
points(98, b_i_make[98], col = "orange", cex = 2, pch = 1, lwd = 1)
points(17, b_i_make[17], col = "red", cex = 2, pch = 1, lwd = 1)

####################### finding optimal learning rate ##########################
set.seed(321)
eta_make_sequence <- seq(from = 0.01, to = 1, by = 0.01) #going from 0.01 to 1 (the max) over 100 iterations
eta_make_vect <- c()
for(eta_make_t in 1:100)
{
  #Step 1) training
  training_make_boosting_lr <- xgb.train(params = boosting_params, data = train_matrix, nrounds = 17, watchlist = watchlist, eta = (eta_make_t/100))
  
  #Step 2) testing
  predict_make_boosting_lr <-  predict(training_make_boosting_lr, newdata = test_matrix)
  
  #Step 3) predictive performance
  cm_make_boost_lr <- table(predict_make_boosting_lr, test_label)
  eta_make_vect[eta_make_t] <- ((cm_make_boost_lr[[1,1]] + cm_make_boost_lr[[2,2]] + cm_make_boost_lr[[3,3]] + cm_make_boost_lr[[4,4]] + cm_make_boost_lr[[5,5]])/sum(cm_make_boost_lr))
}
#finding the number of model with the highest accuracy
which.max(eta_make_vect)

#plotting all 
plot(eta_make_vect, main = 'Classification rates with changing learning rate', ylab = 'Classification Rate', xlab = 'Iteration')
points(97, eta_make_vect[97], col = "red", cex = 2, pch = 1, lwd = 1)

#prediction accuracy of that chosen model
eta_make_vect[97]

#size of shrinkage for that model
eta_make_sequence[97]

####### Finding optimal max depth (maximum nodes per tree) - default = 6 ####### 
set.seed(123)
md_make <- double(10)
for(tree_depth_make in 1:10)
{
  #Step 1) training
  training_make_boosting_md <- xgb.train(params = boosting_params, data = train_matrix, max_depth = tree_depth_make, nrounds = 17, watchlist = watchlist, eta = 0.97)
  
  #Step 2) testing
  predict_make_boosting_md <-  predict(training_make_boosting_md, newdata = test_matrix)
  
  #Step 3) predictive performance
  cm_make_boost_md <- table(predict_make_boosting_md, test_label)
  md_make[tree_depth_make] <- ((cm_make_boost_md[[1,1]] + cm_make_boost_md[[2,2]] + cm_make_boost_md[[3,3]] + cm_make_boost_md[[4,4]] + cm_make_boost_md[[5,5]])/sum(cm_make_boost_md))
}
#finding the model with the highest accuracy
which.max(md_make)

#plotting all
plot(md_make, main = 'Misclassification rates with changing interaction depth', ylab = 'Classification Rate', xlab = 'Max depth')
points(6, md_make[6], col = "red", cex = 2, pch = 1, lwd = 1)

#Accuracy of that model
md_make[6]

################# FINAL BOOSTING MODEL WITH OPTIMAL PARAMTERS ##################
start_time <- Sys.time() #start timer

vector_make_boost_optimal = c()

repetitions = 100
for(i in 1:repetitions)
{
  #Step 1) Splitting the data
  set.seed(i)
  samples = sample(2, nrow(cars_make_boosting), replace = T, prob = c(0.8, 0.2))
  train_make_boost = cars_make_boosting[samples==1,]
  test_make_boost = cars_make_boosting[samples==2,]
  
  #create matrix and one hot encoding for factor variables
  #training matrix
  train_make_m <- sparse.model.matrix(Indicated.Value~ .-train_make_boost$Indicated.Value, data=train_make_boost)
  train_make_label <- train_make_boost[,"Indicated.Value"]
  train_make_matrix <- xgb.DMatrix(data = as.matrix(train_make_m), label = train_make_label)
  
  #testing matrix
  test_make_m <- sparse.model.matrix(Indicated.Value~ .-test_make_boost$Indicated.Value, data=test_make_boost)
  test_make_label <- test_make_boost[,"Indicated.Value"]
  test_make_matrix <- xgb.DMatrix(data = as.matrix(test_make_m), label = test_make_label)
  
  #Step 2) training
  training_make_boosting_optimal <- xgb.train(params = boosting_params, data = train_make_matrix, max_depth = 6, nrounds = 17, watchlist = watchlist, eta = 0.97)
  
  #Step 3) testing
  predict_make_boosting_optimal <-  predict(training_make_boosting_optimal, newdata = test_make_matrix)
  
  #Step 3) predictive performance
  cm_make_boost_optimal <- table(factor(predict_make_boosting_optimal, levels=min(test_make_label):max(test_make_label)), factor(test_make_label, levels=min(test_make_label):max(test_make_label)))
  ac_make_boost_optimal <- ((cm_make_boost_optimal[[1,1]] + cm_make_boost_optimal[[2,2]] + cm_make_boost_optimal[[3,3]] + cm_make_boost_optimal[[4,4]] + cm_make_boost_optimal[[5,5]])/sum(cm_make_boost_optimal))
  vector_make_boost_optimal[i] = ac_make_boost_optimal
}

#boxplot of the means over the 100 repetitions
boxplot(vector_make_boost_optimal, main = "Boxplot of Optimal Boosting Classification Rates", ylab = 'Classification Rate')

#mean classification rate from the 100 repetitions
cr_make_boost_optimal <- mean(vector_make_boost_optimal)
cr_make_boost_optimal

end_time <- Sys.time() #end timer
time_make_boosted <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
########################### plotting results ###################################

#boxplots of both models
boxplot(vector_make_simple, vector_make_boost_optimal, names = c('Simple','Boosted'), main = "Classification Rates of Tree-Based Methods with Most Popular Make", ylab = 'Classification Rate', xlab = "Model", col ='plum2')

#boxplots of all trees
boxplot(vector_simple, vector_boost_optimal, vector_make_simple, vector_make_boost_optimal, names = c('Simple-5', 'Boosted-5', 'Simple-1-Make', 'Boosted-1-Make'), main = "Classification Rates of Models", ylab = 'Classification Rate', xlab = "Model", col =c('darkseagreen', 'darkseagreen','plum2','plum2' ))

#barplot of MEAN classification rates for 1 make
class_rates_make <- c(cr_make_simple*100, cr_make_boost_optimal*100)
class_rates_make
class_rates_make <- round(class_rates_make, digits = 3)

x_make <- barplot(class_rates_make, main = "Mean Classification Accuracies of Tree-Based Methods with Evenly Distributed Response Variables", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Simple','Boosted'), ylim = c(0,100), col = "khaki")
y_make <- as.matrix(class_rates_make)
text(x_make, y_make+2, labels = as.character(y_make))

#barplots of MEAN classification rates for all tree-based models
class_rates_combined <- c(class_rates[1], class_rates[2], class_rates_make)
x_combined_acc <- barplot(class_rates_combined, main = "Mean Classification Accuracies of Models", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Simple-5', 'Boosted-5', 'Simple-1-Make', 'Boosted-1-Make'), ylim = c(0,100), col = c('darkseagreen', 'darkseagreen','plum2','plum2' ))
y_combined_acc <- as.matrix(class_rates_combined)
text(x_combined_acc,y_combined_acc+2, labels = as.character(y_combined_acc))

#barplots of timings for 1 make models
time_make <- c(time_make_simple, time_make_boosted)
time_make <- round(time_make, digits = 3)

x_time_make <- barplot(time_make, main = "Time Taken for Model", ylab = 'Time Taken (mins)', xlab = "Model", names.arg = c('Simple', 'Boosted'), col = "blue", ylim = c(0,3))
y_time_make <- as.matrix(time_make)
text(x_time_make, y_time_make+0.2, labels = as.character(y_time_make))

#barplots of timings for all tree-based models
times_combined <- c(time_5[1], time_5[5], time_make)

y_combined_time <- barplot(rev(times_combined), main = "Time Taken for all Models", ylab = 'Model', xlab = "Time Taken (mins)", names.arg = rev(c('Simple-5', 'Boosted-5', 'Simple-1-Make', 'Boosted-1-Make')), xlim = c(0,5), horiz = T, col = rev(c("darkseagreen",'darkseagreen', 'plum2', 'plum2')))
x_combined_time <- as.matrix(times_combined)
text(x_combined_time+0.3, rev(y_combined_time), labels = as.character(x_combined_time))



################################################################################
###################################### KNN #####################################
################################################################################
#set up the data accordingly

#one hot encode the data set
dummy_knn <- dummyVars("~.", cars_[,1:7])
cars_knn <- data.frame(predict(dummy_knn, cars_[,1:7]))
cars_knn <- cbind(cars_knn, cars_$Indicated.Value) #add indicated value back in
names(cars_knn)[names(cars_knn) == "cars_$Indicated.Value"] <- "Indicated Value" #rename

#scale the data
cars_knn[-12] <- scale(cars_knn[,-12])
cars_knn <- cars_knn[-10] #converted petrol plug in hybrid to all NaN's - assume because there were none in the data set - therefore had to remove

################################################################################
########################## find the optimal size of k ########################## 
#split the data
set.seed(1)
knn_sample = sample(1:13645, 10916)
train_knn = cars_knn[knn_sample, ]
test_knn = cars_knn[-knn_sample, ]

k_size <- double(300)
for(k_t in 1:300)
{
  #Step 1) Predicting
  y_pred <- knn(train = train_knn[, -11], test = test_knn[, -11], cl = train_knn[, 11], k = k_t) 
  
  #Step 2) Predictive performance
  cm_knn <- table(y_pred, test_knn[,11])
  k_size[k_t] <- ((cm_knn[[1,1]] + cm_knn[[2,2]] + cm_knn[[3,3]] + cm_knn[[4,4]] + cm_knn[[5,5]]) / sum(cm_knn))
}

plot(k_size, main = "Predictive Performance with Changing Value of k", ylab = "Classification Rate", xlab = "Size of k", cex = 0.5)
which.max(k_size)
points(68,k_size[68], col = "red", cex = 1.25, pch = 1, lwd = 1) #note this is close to the theory that the best size of k is sqrt(length of training set/2)

################################################################################
######################## KNN with the optimal size of K ######################## 
start_time <- Sys.time() #start timer

vector_knn_optimal = c()

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i)
  
  #Step 1) Splitting the data
  knn_sample = sample(1:13645, 10916)
  train_knn_optimal = cars_knn[knn_sample, ]
  test_knn_optimal = cars_knn[-knn_sample, ]
  
  #Step 2) Predicting with optimal size of k
  y_pred_optimal <- knn(train = train_knn_optimal[, -11], test = test_knn_optimal[, -11], cl = train_knn_optimal[, 11], k = 68) 
  
  #Step 3) predictive performance
  cm_knn_optimal <- table(y_pred_optimal, test_knn_optimal[,11])
  ac_knn_optimal <- ((cm_knn_optimal[[1,1]] + cm_knn_optimal[[2,2]] + cm_knn_optimal[[3,3]] + cm_knn_optimal[[4,4]] + cm_knn_optimal[[5,5]]) / sum(cm_knn_optimal))
  vector_knn_optimal[i] = ac_knn_optimal
}

#boxplot of the means over the 100 repetitions
boxplot(vector_knn_optimal, main = "Boxplot of Classification Rates with optimal k", ylab = 'Classification Accuracy', xlab = "Optimal k", col = "yellow1")

#mean classification rate from the 100 repetitions
cr_knn_optimal <- mean(vector_knn_optimal)
cr_knn_optimal

end_time <- Sys.time() #end timer
time_knn_optimal <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins



################################################################################
########## KNN with conventional size of K - sqrt(len(training data) ###########
start_time <- Sys.time() #start timer

vector_knn_sqrt = c()

repetitions = 100
for(i in 1:repetitions)
{
  set.seed(i)
  
  #Step 1) Splitting the data
  knn_sample = sample(1:13645, 10916)
  train_knn_sqrt = cars_knn[knn_sample, ]
  test_knn_sqrt = cars_knn[-knn_sample, ]
  
  #Step 2) Predicting with optimal size of k
  y_pred_sqrt <- knn(train = train_knn_sqrt[, -11], test = test_knn_sqrt[, -11], cl = train_knn_sqrt[, 11], k = round(sqrt(nrow(train_knn_sqrt)))) 
  
  #Step 3) predictive performance
  cm_knn_sqrt <- table(y_pred_sqrt, test_knn_sqrt[,11])
  ac_knn_sqrt <- ((cm_knn_sqrt[[1,1]] + cm_knn_sqrt[[2,2]] + cm_knn_sqrt[[3,3]] + cm_knn_sqrt[[4,4]] + cm_knn_sqrt[[5,5]]) / sum(cm_knn_sqrt))
  vector_knn_sqrt[i] = ac_knn_sqrt
}

#boxplot of the means over the 100 repetitions
boxplot(vector_knn_sqrt, main = "Boxplot of Classification Rates with sqrt K", ylab = 'Classification Rate')
boxplot(vector_knn_optimal, vector_knn_sqrt, main = "Boxplot of Classification Rates of KNN models", ylab = 'Classification Rate', xlnames = c("Optimal k", "Square Root k"), col = "yellow1", ylim = c(0.36,0.41))


#mean classification rate from the 100 repetitions
cr_knn_sqrt <- mean(vector_knn_sqrt)
cr_knn_sqrt

end_time <- Sys.time() #end timer
time_knn_sqrt <- difftime(end_time, start_time, units = "mins")[[1]] #hold time within a variable in mins


################################################################################
###################### plotting predictive performances ########################
#boxplots of knn models
boxplot(vector_simple, vector_pruned, vector_bag_optimal, vector_rf_optimal, vector_boost_optimal, vector_knn_optimal, vector_knn_sqrt, names = c('Simple', 'Pruned', 'Bagged', 'Random Forest', 'Boosted','Optimal k', 'Square Root'), main = "Classification Rates of Tree-based and KNN Models", ylab = 'Classification Rate', xlab = "Model", col =c('darkseagreen','darkseagreen','darkseagreen','darkseagreen','darkseagreen','yellow1','yellow1'))

#boxplots of ALL models
boxplot(vector_simple, vector_pruned, vector_bag_optimal, vector_rf_optimal, vector_boost_optimal, vector_simple_reduced, vector_pruned_reduced, vector_bag_optimal_reduced, vector_rf_optimal_reduced, vector_boost_optimal_reduced, vector_make_simple, vector_make_boost_optimal, vector_knn_optimal, vector_knn_sqrt, names = c('Simple-5', 'Pruned-5', 'Bagged-5', 'R-Forest-5', 'Boosted-5', 'Simple-3', 'Pruned-3', 'Bagged-3', 'R-Forest-3', 'Boosted-3', 'Simple-1-Mk', 'Boosted-1-Mk', 'KNN-Optimal', 'KNN-SqRt'), main = "Classification Rates of all Models Used", ylab = 'Classification Rate', xlab = "Model", col =c('darkseagreen','darkseagreen','darkseagreen','darkseagreen','darkseagreen','turquoise', 'turquoise', 'turquoise', 'turquoise', 'turquoise', 'plum2', 'plum2', 'yellow1', 'yellow1'),ylim = c(0.30, 0.55))

#barplots of MEAN classification rates for KNN models
class_rates_knn <- c(cr_knn_optimal*100, cr_knn_sqrt*100)
class_rates_knn
class_rates_knn <- round(class_rates_knn, digits = 3)

x_knn <- barplot(class_rates_knn, main = "Mean Classification Accuracies of KNN Models", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Optimal k', 'Square Root'), ylim = c(0,100), col = "yellow1")
y_knn <- as.matrix(class_rates_knn)
text(x_knn, y_knn+2, labels = as.character(y_knn))

#barplot of main tb and knn models
class_rates_dt_knn <- c(class_rates, class_rates_knn)
x_knn <- barplot(class_rates_dt_knn, main = "Mean Classification Accuracies of Models", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Simple', 'Pruned', 'Bagged', 'Random Forest', 'Boosted', 'Optimal k', 'Square Root'), ylim = c(0,100), col = c('darkseagreen','darkseagreen','darkseagreen','darkseagreen','darkseagreen','yellow1','yellow1'))
y_knn <- as.matrix(class_rates_dt_knn)
text(x_knn, y_knn+2, labels = as.character(y_knn))


#barplots of MEAN classification rates for ALL models
class_rates_combined <- c(class_rates, class_rates_reduced, class_rates_make , class_rates_knn)
x_combined_acc <- barplot(class_rates_combined, main = "Mean Classification Accuracies of all Models", ylab = 'Classification Accuracy (%)', xlab = "Model", names.arg = c('Simple-5', 'Pruned-5', 'Bagged-5', 'R-Forest-5', 'Boosted-5', 'Simple-3', 'Pruned-3', 'Bagged-3', 'R-Forest-3', 'Boosted-3', 'Simple-1-Mk', 'Boosted-1-Mk', 'KNN-Optimal', 'KNN-SqRt'), ylim = c(0,100), col =c('darkseagreen','darkseagreen','darkseagreen','darkseagreen','darkseagreen','turquoise', 'turquoise', 'turquoise', 'turquoise', 'turquoise', 'plum2', 'plum2', 'yellow1', 'yellow1'))
y_combined_acc <- as.matrix(class_rates_combined)
text(x_combined_acc, y_combined_acc+2, labels = as.character(y_combined_acc))

#barplots of timings for knn models
time_knn <- c(time_knn_optimal, time_knn_sqrt)
time_knn <- round(time_knn, digits = 3)

x_time_knn <- barplot(time_knn, main = "Time Taken for Model", ylab = 'Time Taken (mins)', xlab = "Model", names.arg = c('Optimal k', 'Square Root'), col = "blue", ylim = c(0,2))
y_time_knn <- as.matrix(time_knn)
text(x_time_knn,y_time_knn+0.2, labels = as.character(y_time_knn))

#timings of KNN and DT models
times_dt_knn <- c(time_5, time_knn)

y_combined_time <- barplot(rev(times_dt_knn), main = "Time Taken for Models", ylab = 'Model', xlab = "Time Taken (mins)", names.arg = rev(c('Simple-5', 'Pruned-5', 'Bagged-5', 'R-Forest-5', 'Boosted-5', 'KNN-Optimal', 'KNN-sqrt')), xlim = c(0,20), horiz=T, col = rev(c("darkseagreen","darkseagreen","darkseagreen","darkseagreen","darkseagreen", "yellow1", "yellow1")))
x_combined_time <- as.matrix(times_dt_knn)
text(x_combined_time+0.7, rev(y_combined_time), labels = as.character(x_combined_time))

#barplots of timings for ALL models
times_combined <- c(time_5, time_reduced, time_make, time_knn)
par(mar=c(5,8,3,3)) #change margin sizes
y_combined_time <- barplot(rev(times_combined), main = "Time Taken for all Models", xlab = "Time Taken (mins)", names.arg = rev(c('Simple-5', 'Pruned-5', 'Bagged-5', 'R-Forest-5', 'Boosted-5', 'Simple-3', 'Pruned-3', 'Bagged-3', 'R-Forest-3', 'Boosted-3', 'Simple-1-Make', 'Boosted-1-Make', 'KNN-Optimal', 'KNN-SqRt')), xlim = c(0,20), horiz=T, col = rev(c("darkseagreen","darkseagreen","darkseagreen","darkseagreen","darkseagreen","turquoise","turquoise","turquoise","turquoise","turquoise", "plum2","plum2", "yellow1", "yellow1")), las=1)
x_combined_time <- as.matrix(times_combined)
text(x_combined_time+0.4, rev(y_combined_time), labels = as.character(x_combined_time))



################################################################################
################################## CLUSTERING ################################## 
################################# 5 CATEGORIES #################################
#extract the indicated values from the dataset

clustering_y <- cars_$Indicated.Value
table(clustering_y)

#have the rest of the data under one variable
clustering_x <- cars_[1:7]

#one hot encode the data set
dummy <- dummyVars("~.", clustering_x)
oh_cars_ <- data.frame(predict(dummy, clustering_x))

#scale the data
oh_cars_scaled <- scale(oh_cars_)

#calculate the distances
clustering_x <- dist(oh_cars_scaled)

#calculate number of clusters required - within sum of squares
fviz_nbclust(oh_cars_scaled, kmeans, method = "wss")

#kmeans 
km <- kmeans(oh_cars_scaled, centers = 5, nstart = 10)
km

#visualise the clustering results
km_clusters <- km$cluster
rownames(oh_cars_scaled) <- paste(clustering_y, 1:dim(cars_)[1], sep="<<-value_car.row.number->>")
fviz_cluster(list(data=oh_cars_scaled, cluster = km_clusters))

#predictive performance
cm_k_means = table(km_clusters, clustering_y)
ac_k_means = ((cm_k_means[[1,1]] + cm_k_means[[2,2]] + cm_k_means[[3,3]] + cm_k_means[[4,4]] + cm_k_means[[5,5]])/sum(cm_k_means))
ac_k_means




## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## extra ## ## ## ## ## ## ## ## ## ## ## ##
################################################################################
################################# 3 CATEGORIES #################################
#extract the indicated values from the data set
clustering_y_3 <- cars_3$Indicated.Value
table(clustering_y_3)

#have the rest of the data under one variable
clustering_x_3 <- cars_3[1:7]

#one hot encode the data set
dummy_3 <- dummyVars("~.", clustering_x_3)
oh_cars_3 <- data.frame(predict(dummy_3, clustering_x_3))

#scale the data
oh_cars_scaled_3 <- scale(oh_cars_3)

#calculate the distances
clustering_x_3 <- dist(oh_cars_scaled_3)

#calculate number of clusters required - within sum of squares -- Elbow Plot
fviz_nbclust(oh_cars_scaled_3, kmeans, method = "wss")

#kmeans 
km_out_3 = kmeans(oh_cars_scaled_3, centers = 3, nstart = 10)
km_out_3

#visualise the clustering results
km_clusters_3 = km_out_3$cluster
rownames(oh_cars_scaled_3) = paste(clustering_y_3, 1:dim(cars_3)[1], sep="<<-value_car.row.number->>")
fviz_cluster(list(data=oh_cars_scaled_3, cluster = km_clusters_3))

#predictive performance
cm_k_means_3 = table(km_clusters_3, clustering_y_3)
ac_k_means_3 = ((cm_k_means_3[[1,1]] + cm_k_means_3[[2,2]] + cm_k_means_3[[3,3]])/sum(cm_k_means_3))
ac_k_means_3

