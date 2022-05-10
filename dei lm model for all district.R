setwd("C:/Users/Layla/Downloads/MSBA/Capstone")

rm(list=ls())
dei <- read.csv("dei data for r3.csv", na.strings = c(""," ","I donÃ¢Â???ÂTt know","I donÃ¢Â???ÂTt know","I don't know","#DIV/0!"))
options(scipen=999)

dei_c <- na.omit(dei)

summary(dei_c)
lmdei <- lm(gpa~., data=dei_c)
plot(gpa~., data=dei_c)
summary(lmdei)
lmtest::bptest(lmdei) #0.0006637

#The p-value is below 0.05, indicating heteroskedasticity in the data. This finding decreases the overall accuracy of our model.

car::vif(lmdei)

plot(lmdei) 

