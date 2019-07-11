setwd("C:/Frank Zhou/UM-MADISON/4/628/Module1")
install.packages("leaps")
install.packages("faraway")
library("leaps")
library("faraway")
BodyFat = read.table("BodyFat.csv", sep = ",", header = T)
summary(BodyFat)
BodyFat = BodyFat[,-c(1,3)]

#####Data Cleaning#####
#step 1 Check whether the BMI index is calculated coreect or not
height <- BodyFat$HEIGHT / 39.37007874#change in to meter
weight <- BodyFat$WEIGHT * 0.453592#change in to kilogram
BMI <- weight / (height^2)#calculate the BMI index in theory
BMI_diff <- NULL
for (i in 1:length(BMI)) {
  BMI_diff[i] <- BMI[i] - BodyFat$ADIPOSITY[i]
  if (BMI_diff[i] > 0.1|BMI_diff[i] < -0.1){#without42,163,221, the average of difference is 0.03, we choose 0.1 as the standard.
    print(paste0("i = ",i,"      BMI_difference = ",BMI_diff[i]))
  }
}

sqrt(BodyFat$WEIGHT[42]*0.453592/BodyFat$ADIPOSITY[42])*39.37007874#Check the BMI difference is made by weight or height.
BodyFat$ADIPOSITY[42]*((BodyFat$HEIGHT[42]/39.37007874)^2)/0.453592

sqrt(BodyFat$WEIGHT[163]*0.453592/BodyFat$ADIPOSITY[163])*39.37007874
BodyFat$ADIPOSITY[163]*((BodyFat$HEIGHT[163]/39.37007874)^2)/0.453592

sqrt(BodyFat$WEIGHT[221]*0.453592/BodyFat$ADIPOSITY[221])*39.37007874
BodyFat$ADIPOSITY[221]*((BodyFat$HEIGHT[221]/39.37007874)^2)/0.453592

BodyFat$HEIGHT[42] <- 69.5
BodyFat$WEIGHT[221] <- 173.25
BodyFat$WEIGHT[163] <- 164.25

model <- lm(BodyFat$BODYFAT ~ ., data=BodyFat)
plot(model, which = 4)


BodyFat <- BodyFat[-c(39,172,182),]#delete the outlier and the unapporiate point after the backgroud research.

model <- lm(BodyFat$BODYFAT ~ ., data=BodyFat)
layout(matrix(1:4, ncol=2))
plot(model)

#####Mallows's Cp#####
model <- lm(BodyFat$BODYFAT ~ ., data=BodyFat)
X <- model.matrix(model)[,-1]
Y <- BodyFat[,1]
g <- leaps(X,Y, nbest=1)
Cpplot(g)#Choose variables according to the plot
cp_choice <- c(1,3,6,7,13,14)+1
BodyFat <- BodyFat[,c(1,cp_choice)]
model.cp <- lm(BodyFat$BODYFAT ~ ., data=BodyFat)
summary(model.cp)
