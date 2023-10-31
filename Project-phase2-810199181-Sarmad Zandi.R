#================= ProjectPhase2 ====================
#Author: Sarmad Zandi
#S.Number: 810199181
#Course: Statistical Inference
#====================================================

# Importing the Healthcare dataset
HealthCare <- read.csv("H:/Second Term/Statistical Inference/Projects/ProjectPhase2/HealthCare.csv")
View(HealthCare)

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================


#=============================== Question 1 : Part A ===========================

# randomly select a sample (n = 500) without replacement from HealthCare
Sample_1_A <- HealthCare[sample(nrow(HealthCare), size = 500, replace = FALSE),]

# to understand the frequency
table_1_A <- table(unlist(Sample_1_A$ever_married),unlist(Sample_1_A$smoking_status))
table_1_A_1 <- table(Sample_1_A$ever_married)

# checking the success-failure condition 
#--------------------------- 1 -----------------------------
#===========================================================
#no ~ formerly smoked
(table_1_A[1] * (table_1_A[1]/table_1_A_1[1])) >= 10 
(table_1_A[1] * (1 - (table_1_A[1]/table_1_A_1[1]))) >= 10 

#yes ~ formerly smoked
(table_1_A[2] * (table_1_A[2]/table_1_A_1[2])) >= 10 
(table_1_A[2] * (1 - (table_1_A[2]/table_1_A_1[2]))) >= 10 
#===========================================================
#--------------------------- 1 -----------------------------

#--------------------------- 2 -----------------------------
#===========================================================
#no ~ never smoked
(table_1_A[3] * (table_1_A[3]/table_1_A_1[1])) >= 10 
(table_1_A[3] * (1 - (table_1_A[3]/table_1_A_1[1]))) >= 10 

#yes ~ never smoked
(table_1_A[4] * (table_1_A[4]/table_1_A_1[2])) >= 10 
(table_1_A[4] * (1 - (table_1_A[4]/table_1_A_1[2]))) >= 10 
#===========================================================
#--------------------------- 2 -----------------------------

#--------------------------- 3 -----------------------------
#===========================================================
#no ~ smokes
(table_1_A[5] * (table_1_A[5]/table_1_A_1[1])) >= 10 
(table_1_A[5] * (1 - (table_1_A[5]/table_1_A_1[1]))) >= 10 

#yes ~ smokes
(table_1_A[6] * (table_1_A[6]/table_1_A_1[2])) >= 10 
(table_1_A[6] * (1 - (table_1_A[6]/table_1_A_1[2]))) >= 10
#===========================================================
#--------------------------- 3 -----------------------------

#--------------------------- 4 -----------------------------
#===========================================================
#no ~ unknown
(table_1_A[7] * (table_1_A[7]/table_1_A_1[1])) >= 10 
(table_1_A[7] * (1 - (table_1_A[7]/table_1_A_1[1]))) >= 10 

#yes ~ unknown
(table_1_A[8] * (table_1_A[8]/table_1_A_1[2])) >= 10 
(table_1_A[8] * (1 - (table_1_A[8]/table_1_A_1[2]))) >= 10 
#===========================================================
#--------------------------- 4 -----------------------------

#===========================================================================================================
# Z* for 95% CL
Z <- qnorm(0.975)

# CI for smokes
SE_smokes <- sqrt( ( ((table_1_A[5]/table_1_A_1[1]) * (1 - (table_1_A[5]/table_1_A_1[1]))) / table_1_A[5] ) +
                   ( ((table_1_A[6]/table_1_A_1[2]) * (1 - (table_1_A[6]/table_1_A_1[2]))) / table_1_A[6] ) )
# point estimate
point_estimate_smokes <- ((table_1_A[5]/table_1_A_1[1]) - (table_1_A[6]/table_1_A_1[2]))

# point estimate ± Z*SE
CI_smokes <- c(point_estimate_smokes - (Z * SE_smokes) , point_estimate_smokes + (Z * SE_smokes))
as.numeric(CI_smokes)
#===========================================================================================================

#===========================================================================================================
# Z* for 95% CL
Z <- qnorm(0.975)

# CI for never smoked
SE_never <- sqrt( ( ((table_1_A[3]/table_1_A_1[1]) * (1 - (table_1_A[3]/table_1_A_1[1]))) / table_1_A[3] ) +
                     ( ((table_1_A[4]/table_1_A_1[2]) * (1 - (table_1_A[4]/table_1_A_1[2]))) / table_1_A[4] ) )
# point estimate
point_estimate_never <- ((table_1_A[3]/table_1_A_1[1]) - (table_1_A[4]/table_1_A_1[2]))

# point estimate ± Z*SE
CI_never <- c(point_estimate_never - (Z * SE_never) , point_estimate_never + (Z * SE_never))
as.numeric(CI_never)
#===========================================================================================================

#===========================================================================================================
# Z* for 95% CL
Z <- qnorm(0.975)

# CI for formerly smoked
SE_formerly <- sqrt( ( ((table_1_A[1]/table_1_A_1[1]) * (1 - (table_1_A[1]/table_1_A_1[1]))) / table_1_A[1] ) +
                    ( ((table_1_A[2]/table_1_A_1[2]) * (1 - (table_1_A[2]/table_1_A_1[2]))) / table_1_A[2] ) )
# point estimate
point_estimate_formerly <- ((table_1_A[1]/table_1_A_1[1]) - (table_1_A[2]/table_1_A_1[2]))

# point estimate ± Z*SE
CI_formerly <- c(point_estimate_formerly - (Z * SE_formerly) , point_estimate_formerly + (Z * SE_formerly))
as.numeric(CI_formerly)
#===========================================================================================================

#===========================================================================================================
# Z* for 95% CL
Z <- qnorm(0.975)

# CI for unknown
SE_unknown <- sqrt( ( ((table_1_A[7]/table_1_A_1[1]) * (1 - (table_1_A[7]/table_1_A_1[1]))) / table_1_A[7] ) +
                       ( ((table_1_A[8]/table_1_A_1[2]) * (1 - (table_1_A[8]/table_1_A_1[2]))) / table_1_A[8] ) )
# point estimate
point_estimate_unknown <- ((table_1_A[7]/table_1_A_1[1]) - (table_1_A[8]/table_1_A_1[2]))

# point estimate ± Z*SE
CI_unknown <- c(point_estimate_unknown - (Z * SE_unknown) , point_estimate_unknown + (Z * SE_unknown))
as.numeric(CI_unknown)
#===========================================================================================================



#=============================== Question 1 : Part B ===========================


# randomly select a sample (n=400) without replacement from HealthCare
my_sample_1_B <- HealthCare[sample(nrow(HealthCare), size = 400, replace = FALSE),]


# to understand the frequency
table_1_B <- table(my_sample_1_B$ever_married,my_sample_1_B$smoking_status)
table_1_B_B <- addmargins(table_1_B)
# to calculate the expected count of each cell
chii <- chisq.test(table_1_B_B)
expcount_1 <- table_1_B_B
expcount_1[] <- paste(table_1_B_B,paste0("(",round(chii$expected),")"))
expcount_1

# independence test using the chisq.test() function
chisq.test(table(my_sample_1_B$ever_married,my_sample_1_B$smoking_status))

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================


#================================== Question 2 =================================

# randomly select a small sample (n = 15) without replacement from HealthCare
my_sample <- HealthCare[sample(nrow(HealthCare), size = 15, replace = FALSE),]

# To calculate the point estimate, 
# we first get the ratio of people who are married 
# and the ratio of people who are single in our sample.
rat <- table(my_sample$ever_married)
vec <- c(( rat[1] / 15 ) , ( rat[2] / 15 ))
vec

# It is a fair coin that returns a value of 1 or 0 whenever it is called
# value of 1 = success 
funcc <- function(){
  rand <- as.integer(runif(1, min=0, max=2))
  return(rand)
}
# define a null vector for final result
res <- c()

# Perform 100 simulations
# in each simulation : i flip the coin 15 times
# and record the proportion of success
# and recording the proportion of success at each iteration
# in res vector
for (y in c(1:100)) {
  my_vector <- c()
  for (z in c(1:15)) {
    my_vector <- c(my_vector,funcc())
  }
  p_hat_sim <- (as.numeric(table(my_vector)[2]))/15
  res <- c(res,p_hat_sim) 
}

# create a data frame for all results
dff <- data.frame(res)

library(ggplot2)
#I use dotplot to display the results of all simulations
ggplot(dff, aes(x = res)) + geom_dotplot(fill="skyblue") +
  labs(title="Dot plot") +         # the title of the plot
  theme_bw() + 
  # the grid and background removed from plot,
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # put the title location in the center of the plot.
        plot.title = element_text(size=12,face="bold",hjust = 0.5))
  


#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#============================== Question 3 : Part A ============================

# find the distribution of my categorical variable.
real_dis <- table(HealthCare$smoking_status)/nrow(HealthCare) * 100 

# to find the expected #
Expected <- real_dis
EXp <- as.integer(Expected)

# randomly select a sample (n=100) without replacement from HealthCare
my_sample_3 <- HealthCare[sample(nrow(HealthCare), size = 100, replace = FALSE),]


#  select a Biased sample (n=100) with replacement from HealthCare
my_sample_3_Bias <- HealthCare[sample(HealthCare[3,10,], size = 100, replace = TRUE),]

# find the distribution of my Non-Biased sample.
first_dis <- table(my_sample_3$smoking_status)

# find the distribution of my Biased sample.
sec_dis <- table(my_sample_3_Bias$smoking_status)


Expected[4] <- Expected[4]+1

# Goodness of fit test #1
chisq.test(x=as.integer(first_dis),p=as.integer(Expected)/100)

# Goodness of fit test #2
chisq.test(x=as.integer(sec_dis),p=as.integer(Expected)/100)


#============================== Question 3 : Part B ============================

# randomly select a sample (n=300) without replacement from HealthCare
my_sample_3_B <- HealthCare[sample(nrow(HealthCare), size = 300, replace = FALSE),]


# to understand the frequency
table_3_B <- table(my_sample_3_B$ever_married,my_sample_3_B$smoking_status)
table_3_B_B <- addmargins(table_3_B)
# to calculate the expected count of each cell
chi <- chisq.test(table_3_B_B)
expcount[] <- paste(table_3_B_B,paste0("(",round(chi$expected),")"))
expcount

# independence test using the chisq.test() function
chisq.test(table(my_sample_3_B$ever_married,my_sample_3_B$smoking_status))


#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#================================= Question 4 ==================================
# at first lets replace missing values with the median
HealthCare2 <- HealthCare
HealthCare2$health_bills[is.na(HealthCare2$health_bills)]<-median(HealthCare2$health_bills,na.rm=TRUE)

# randomly select a sample (n=100) without replacement from HealthCare
my_sample_4 <- HealthCare[sample(nrow(HealthCare), size = 100, replace = FALSE),]

#============================== Question 4 : Part B --> a.b  ===================
# 1
# checking the linearity condition :
library(ggplot2)
# plot a scatter plot
ggplot(data = my_sample_4,aes(x=avg_glucose_level,y=health_bills)) + 
  geom_point(aes(x = avg_glucose_level, y = health_bills),
             color = "skyblue3", size = 2)  + 
  labs(title = "health bills ~ avg glucose level") + 
  # put the title location in the center of the plot.
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  theme_classic() +
  # for create a linear model
  geom_smooth(method=lm,se=FALSE,color="red")

# checking the Nearly normal residuals condition
library(ggplot2)
#create histogram of residuals
ggplot(data = my_sample_4, aes(x = modelnum1$residuals)) +
  geom_histogram(bins = 10, 
                 fill = 'skyblue3', 
                 color = 'gray') +
  labs(title = 'Histogram of Residuals', x = 'Residuals') +
  theme_classic()

# create my Regression models
modelnum1 <- summary(lm(health_bills ~ avg_glucose_level,data=my_sample_4))

# 2
# checking the linearity condition :
library(ggplot2)
# plot a scatter plot
ggplot(data = my_sample_4,aes(x=age,y=health_bills)) + 
  geom_point(aes(x = age, y = health_bills),
             color = "skyblue3", size = 2)  + 
  labs(title = "health bills ~ age") + 
  # put the title location in the center of the plot.
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  theme_classic() +
  # for create a linear model
  geom_smooth(method=lm,se=FALSE,color="red")

# checking the Nearly normal residuals condition
library(ggplot2)
#create histogram of residuals
ggplot(data = my_sample_4, aes(x = modelnum2$residuals)) +
  geom_histogram(bins = 10, 
                 fill = 'skyblue3', 
                 color = 'gray') +
  labs(title = 'Histogram of Residuals', x = 'Residuals') +
  theme_classic()

# create my Regression models
modelnum2 <- summary(lm(health_bills ~ age,data=my_sample_4))

#============================== Question 4 : Part B --> c  =====================

library(ggplot2)
# plot a scatter plot
ggplot(data = my_sample_4,aes(x=avg_glucose_level,y=health_bills)) + 
  geom_point(aes(x = avg_glucose_level, y = health_bills),
             color = "skyblue3", size = 2)  + 
  labs(title = "health bills ~ avg glucose level") + 
  # put the title location in the center of the plot.
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  theme_classic() +
  # for create a linear model with dashed line
  geom_smooth(method=lm,se=FALSE,color="red",lty=2,size=1.5)

library(ggplot2)
# plot a scatter plot
ggplot(data = my_sample_4,aes(x=age,y=health_bills)) + 
  geom_point(aes(x = age, y = health_bills),
             color = "skyblue3", size = 2)  + 
  labs(title = "health bills ~ age") + 
  # put the title location in the center of the plot.
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  theme_classic() +
  # for create a linear model with dashed line
  geom_smooth(method=lm,se=FALSE,color="red",lty=2,size=1)

#============================== Question 4 : Part D   ==========================
# 1- ANOVA for model number one : health_bills ~ avg_glucose_level
m1 <- lm(health_bills ~ avg_glucose_level,data=my_sample_4)
anova(m1)

# 2- ANOVA for model number two : health_bills ~ age
m2 <- lm(health_bills ~ age,data=my_sample_4)
anova(m2)

#============================== Question 4 : Part F --> a  =====================
# i considered 90% of the data as new_data
ned <- sample(nrow(my_sample_4),nrow(my_sample_4)*.90)
new_data <- my_sample_4[ned,]

# create my Regression models
modelnum11 <- summary(lm(health_bills ~ avg_glucose_level,data=new_data))

# create my Regression models
modelnum22 <- summary(lm(health_bills ~ age,data=new_data))

#============================== Question 4 : Part F --> b  =====================
# create 95% CI for health_bills ~ avg_glucose_level
# df
dff <- 90 - 2
# to calculate the t-statistic
t_sta <- qt(0.025,df=dff)
# CI : point estimate ± ME 
CI_1 <- c(modelnum11$coefficients[2] - (abs(t_sta) * modelnum11$coefficients[4]),
          modelnum11$coefficients[2] + (abs(t_sta) * modelnum11$coefficients[4])) 
CI_1
#-------------------------------------------------------------------------------
# create 95% CI for health_bills ~ age
# df
dff <- 90 - 2
# to calculate the t-statistic
t_sta <- qt(0.025,df=dff)
# CI : point estimate ± ME 
CI_2 <- c(modelnum22$coefficients[2] - (abs(t_sta) * modelnum22$coefficients[4]),
          modelnum22$coefficients[2] + (abs(t_sta) * modelnum22$coefficients[4])) 
CI_2

#============================== Question 4 : Part F --> c  =====================
# the remaining percent of samples.
new_data_2 <- my_sample_4[-ned,]

# create my Regression models --> health_bills ~ avg_glucose_level
modelnum111 <- summary(lm(health_bills ~ avg_glucose_level,data=new_data_2))

# create my Regression models --> health_bills ~ age
modelnum222 <- summary(lm(health_bills ~ age,data=new_data_2))

#============================== Question 4 : Part F --> d  =====================

# Obtain actual values for response variable from the data set
vec_1 <- c(unlist(new_data$health_bills))
# Obtain predicted values for response variable from my Linear model
vec_p <- c(predict(lm(health_bills ~ avg_glucose_level,data=new_data))) 

# first compare :
vec_p == vec_1

vec_a <- c(predict(lm(health_bills ~ age,data=new_data))) 

# second compare :
vec_a == vec_1

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#================================= Question 5 ==================================
# at first lets replace missing values with the median
HealthCare2 <- HealthCare
HealthCare2$health_bills[is.na(HealthCare2$health_bills)]<-median(HealthCare2$health_bills,na.rm=TRUE)

# randomly select a sample (n=100) without replacement from HealthCare
my_sample_5 <- HealthCare[sample(nrow(HealthCare), size = 100, replace = FALSE),]

#================================= Question 5 : Part A =========================

# to create the Correlation panel and texts
funcc <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  value <- round(cor(x, y), digits=2)
  text(0.5, 0.5, value, cex = 2,col="red")
}
# to create scatterplots 
funcc2 <- function(x, y){
  points(x,y, pch = 18, col = "skyblue")
}
# Create the pairwise scatter plot 
pairs(health_bills ~ avg_glucose_level + age + bmi, 
      data=my_sample_4,
      upper.panel = funcc2,
      lower.panel = funcc)


#================================= Question 5 : Part B =========================
# fit a multiple linear regression model :
bills_model <- summary(lm(health_bills ~ avg_glucose_level + age + bmi , data=my_sample_4))
bills_model

#================================= Question 5 : Part E =========================
######## Forward Selection Method with Adjusted R2 #######
# STEP 1
# variable included : health_bills ~ avg_glucose_level
summary(lm(health_bills ~ avg_glucose_level , data=my_sample_4))

# variable included : health_bills ~ age
summary(lm(health_bills ~ age , data=my_sample_4))

# variable included : health_bills ~ bmi
summary(lm(health_bills ~ bmi , data=my_sample_4))

# STEP 2
# variable included : health_bills ~ bmi + avg_glucose_level
summary(lm(health_bills ~ bmi + avg_glucose_level , data=my_sample_4))

# variable included : health_bills ~ bmi + age
summary(lm(health_bills ~ bmi + age , data=my_sample_4))

# STEP 3
# variable included : health_bills ~ bmi + avg_glucose_level + age
summary(lm(health_bills ~ bmi + avg_glucose_level + age , data=my_sample_4))

######## Forward Selection Method with P-value #######
# STEP 1 :
# variable included : health_bills ~ avg_glucose_level
summary(lm(health_bills ~ avg_glucose_level , data=my_sample_4))

# variable included : health_bills ~ age
summary(lm(health_bills ~ age , data=my_sample_4))

# variable included : health_bills ~ bmi
summary(lm(health_bills ~ bmi , data=my_sample_4))

# STEP 2 :
# variable included : health_bills ~ bmi + avg_glucose_level 
summary(lm(health_bills ~ bmi + avg_glucose_level , data=my_sample_4))

# variable included : health_bills ~ bmi + age 
summary(lm(health_bills ~ bmi + age , data=my_sample_4))

# STEP 3 :
# variable included : health_bills ~ bmi + avg_glucose_level + age
summary(lm(health_bills ~ bmi + avg_glucose_level + age , data=my_sample_4))
#-----------------------------------------------------------------------------

######## Backward Elimination Method with Adjusted R2 #######
# STEP 1 :
# variable included : health_bills ~ bmi + avg_glucose_level + age
summary(lm(health_bills ~ bmi + avg_glucose_level + age , data=my_sample_4))

# variable included : health_bills ~ bmi + avg_glucose_level 
summary(lm(health_bills ~ bmi + avg_glucose_level , data=my_sample_4))

# variable included : health_bills ~ bmi  + age
summary(lm(health_bills ~ bmi + age , data=my_sample_4))

# variable included : health_bills ~  avg_glucose_level + age
summary(lm(health_bills ~  avg_glucose_level + age , data=my_sample_4))

# STEP 2 :
# variable included : health_bills ~ bmi  
summary(lm(health_bills ~ bmi , data=my_sample_4))

# variable included : health_bills ~  avg_glucose_level 
summary(lm(health_bills ~ avg_glucose_level , data=my_sample_4))

######## Backward Elimination Method with P-value #######
# STEP 1 :
# variable included : health_bills ~ bmi + avg_glucose_level + age
summary(lm(health_bills ~ bmi + avg_glucose_level + age , data=my_sample_4))

# STEP 2 :
# variable included : health_bills ~ bmi + avg_glucose_level 
summary(lm(health_bills ~ bmi + avg_glucose_level , data=my_sample_4))

#================================= Question 5 : Part F =========================
# 1
# checking the linearity condition for health bills ~ avg_glucose_level:
library(ggplot2)
# plot a scatter plot
ggplot(data = my_sample_4,aes(x=avg_glucose_level,y=health_bills)) + 
  geom_point(aes(x = avg_glucose_level, y = health_bills),
             color = "skyblue3", size = 2)  + 
  labs(title = "health bills ~ avg glucose level") + 
  # put the title location in the center of the plot.
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  theme_classic() +
  # for create a linear model
  geom_smooth(method=lm,se=FALSE,color="orange2")

# checking the linearity condition for health bills ~ bmi:
library(ggplot2)
# plot a scatter plot
ggplot(data = my_sample_4,aes(x=bmi,y=health_bills)) + 
  geom_point(aes(x = bmi, y = health_bills),
             color = "skyblue3", size = 2)  + 
  labs(title = "health bills ~ bmi") + 
  # put the title location in the center of the plot.
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  theme_classic() +
  # for create a linear model
  geom_smooth(method=lm,se=FALSE,color="orange2")

final_model <- lm(health_bills ~ bmi + avg_glucose_level , data=my_sample_4)

# checking the Nearly normal residuals condition
library(ggplot2)
#create histogram of residuals
ggplot(data = my_sample_4, aes(x = final_model$residuals)) +
  geom_histogram(bins = 10, 
                 fill = 'skyblue3', 
                 color = 'gray') +
  labs(title = 'Histogram of Residuals', x = 'Residuals') +
  theme_classic()

# checking the Cinstant variability of residuals
plot(final_model$residuals ~ final_model$fitted.values,
     pch=20,col="skyblue")
abline(h=0,col="red2",lwd=3,lty=2)

#================================= Question 5 : Part G =========================
# importing libraries :
library(tidyverse)
library(caret)

# to divide my sample into "training" and "test" parts :
my_sample_44 <- sample(nrow(my_sample_4),nrow(my_sample_4)*0.2)
test_part <- my_sample_4[my_sample_44,]
train_part <- my_sample_4[-my_sample_44,]

# build the model that I created in part B with train data :
model_B <- lm(health_bills ~ avg_glucose_level + age + bmi , data=train_part)

# doing predictions with test part with my predictors 
# and computing the R2 and RMSE metrics 
model_BB <- train(health_bills ~ avg_glucose_level + age + bmi, 
                  data = train_part, method = "lm",
                  trControl = trainControl(number = 5,method = "cv"))
# Root mean square error (RMSE) for first model
model_BB$results[2]

# build the model that I created in part E with train data :
model_E <- lm(health_bills ~ avg_glucose_level + bmi , data=train_part)

# doing predictions with test part with my predictors 
# and computing the R2 and RMSE metrics 
model_EE <- train(health_bills ~ avg_glucose_level + bmi, 
                  data = train_part, method = "lm",
                  trControl = trainControl(number = 5,method = "cv"))
# Root mean square error (RMSE) for best model
model_EE$results[2]


#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#================================= Question 6 ==================================
# randomly select a sample (n=100) without replacement from HealthCare
my_sample_6 <- HealthCare[sample(nrow(HealthCare), size = 100, replace = FALSE),]

#================================= Question 6 : Part A =========================
# To fit a logistic Regression model
log_model<-summary(glm(stroke ~ avg_glucose_level + hypertension + gender, 
                       data = my_sample_4 , family = binomial))

#================================= Question 6 : Part B =========================
# define 2 empty vectors
vec_1 <- c()
vec_2 <- c()

# to repeat 50 times...
for (i in c(1:50)) {
  # calculate the odds ratio for selected variable :
  e <- 2.718
  odds_ratio <- e^1.330107 
  
  # generate a random number beetwen 0 and 1 for P(stroke|no hypertension) :
  randd <- runif(1,0,1)
  
  # calculate the probability of P(stroke|hypertension) :
  P_stroke_hypertension <- ( (odds_ratio * (randd/(1-randd))) / (1 + (odds_ratio * (randd/(1-randd)))) )
  
  # save all values to two vectors :
  vec_1 <- c(vec_1,randd)
  vec_2 <- c(vec_2,P_stroke_hypertension)
}

daf <- data.frame(vec_1,vec_2)

# plot the OR curve :) 
library(ggplot2)
ggplot(data=daf, aes(x=vec_1, y=vec_2, group=1)) +
  # OR curve
  geom_line(color="skyblue",size=1.2)+
  # y = x curve
  geom_abline(slope=1, intercept=0,color="gray") +
  geom_point(color="red", size=1)+
  # to put titles
  labs(title = 'OR curve', y = 'P(stroke|hypertension)' ,
                          x ='P(stroke|no hypertension)') +
  theme_classic()

#================================= Question 6 : Part C =========================
#load necessary packages
library(ggplot2)
library(plotROC)
library(pROC)

#use model to make predictions
log_model22 <- glm(stroke ~ avg_glucose_level + hypertension + gender, 
                   data = my_sample_4 , family = binomial)
my_per <- predict(log_model22,
                  my_sample_4, 
                  type="response")

#create ROC curve with predictions and calculate the AUC
ROC_curve <- roc(my_sample_4$stroke, my_per)
AUC <- round(auc(my_sample_4$stroke, my_per),2)

#to create ROC plot
ggroc(ROC_curve, colour = 'skyblue', legacy.axes = TRUE,size = 1.2)  +
  # # to put titles
  labs(title = paste0('ROC Curve ', '(AUC = ', AUC, ')'), y = 'Sensitivity' ,
       x ='1 - Specificity') +
  # y = x curve
  geom_abline(slope=1, intercept=0,color="gray")+
  theme_classic()

#================================= Question 6 : Part E =========================
# To fit a logistic Regression model
log_modelll<-summary(glm(stroke ~  hypertension , 
                       data = my_sample_4 , family = binomial))

# to compare two logistic models with AUC :
logmodelnum1 <- glm(stroke ~ avg_glucose_level + hypertension + gender, 
                    data = my_sample_4 , family = binomial)

logmodelnum2 <- glm(stroke ~ avg_glucose_level + hypertension , 
                    data = my_sample_4 , family = binomial)

my_per_num1 <- predict(logmodelnum1,
                  my_sample_4, 
                  type="response")
my_per_num2 <- predict(logmodelnum2,
                       my_sample_4, 
                       type="response")

# calculate the AUC
AUC_num1 <- round(auc(my_sample_4$stroke, my_per_num1),4)
AUC_num2 <- round(auc(my_sample_4$stroke, my_per_num2),4)

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#================================= Question 7 ==================================
# at first lets replace missing values with the median
HealthCare2 <- HealthCare
HealthCare2$health_bills[is.na(HealthCare2$health_bills)]<-median(HealthCare2$health_bills,na.rm=TRUE)

# Get the median of health_bills variable in dataset 
# and specifying the Threshold :
Threshold <- median(HealthCare2$health_bills)

# To determine whether a person is in category 1 or in category 0.
high_cost <- function(inp){
  if(inp > Threshold){
    return(1)
  }else{
    return(0)
  }
}

# Add a new column to dataset and named it "high_medical_costs" 
# call function on each row of the dataset with lapply.
# To determine whether a person is in category 1 or in category 0 in new variable.
HealthCare2$high_medical_costs <- lapply(HealthCare2$health_bills,high_cost)

# To fit a logistic Regression model
log_model_q7 <- summary(glm(unlist(high_medical_costs) ~ hypertension + age +
                              stroke + heart_disease , 
                       data = HealthCare2 , family = binomial))

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================






