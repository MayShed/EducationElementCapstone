setwd("C:/Users/Layla/Downloads/MSBA/Capstone")

rm(list = ls())
library(tidyverse)
library(glmnet)
set.seed(1)
dei <- read.csv("data_cleansed_dei survey_v2_lm.csv", na.strings = c(""," ",".")) #"#DIV/0!"
colnames(dei)[1] <- "grade"
colnames(dei)[23] <- "G_School_Climate_DEI"
colnames(dei)[28] <- "G_School_Climate_Overall"
colnames(dei)[37] <- "G_Classroom"
colnames(dei)[41] <- "G_Cocurricular"
colnames(dei)[45] <- "G_School_Commitment_DEI"
colnames(dei)[52] <- "G_Discrimination_Students"
colnames(dei)[59] <- "G_Discrimination_Teachers"
colnames(dei)[64] <- "G_Outcomes"
colnames(dei)[72] <- "G_Interactions"

options(scipen=999)
dei[is.na(dei)]<-"No Response"
dei <- dei %>% mutate(across(c(23,28,37,41,45,52,59,64,72), na_if, "#DIV/0!"))
num_col <- c("gpa","G_School_Climate_DEI","G_School_Climate_Overall","G_Classroom","G_School_Commitment_DEI","G_Discrimination_Students","G_Discrimination_Teachers","G_Outcomes","G_Interactions")
#names(dei)
dei_1 <- dei %>% filter(district == "1")
dei_1 <- dei_1[,c("grade","gender","transgender","religion","ethnicity","parent_guardian","parent_other_than_English_frequency","book_in_home_number","computer_at_home_number","district","gpa",
                  "G_School_Climate_DEI","G_School_Climate_Overall","G_Classroom","G_School_Commitment_DEI","G_Discrimination_Students","G_Discrimination_Teachers","G_Outcomes","G_Interactions")]
dei_1_gpa <-  dei_1 %>% filter(gpa != "No Response")
dei_1_gpa <- na.omit(dei_1)
dei_1_gpa[num_col] <- sapply(dei_1_gpa[num_col], as.numeric)
dei_1_gpa <- na.omit(dei_1_gpa)
#str(dei_1_gpa)
dei_1_gpa <- as.data.frame(unclass(dei_1_gpa),                     # Convert all columns to factor
                       stringsAsFactors = TRUE)
#str(dei_1_gpa)
x <- model.matrix(gpa~., dei_1_gpa)[,-1]
y <- dei_1_gpa$gpa
grid <- 10^seq(10, -2, length = 100)
set.seed(1)
train <- sample(1:nrow(x), nrow(x)*0.8)
y.test <- y[-train]
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
summary(lasso.mod)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam #0.03238484
lasso.pred <- predict(lasso.mod , s = bestlam ,
                      newx = x[-train, ])
mean (( lasso.pred - y.test)^2)
#2.377437
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients",
                      s = bestlam)[1:55, ]
lasso.coef
