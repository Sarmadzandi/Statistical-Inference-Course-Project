#================= ProjectPhase1 ====================
#Author: Sarmad Zandi
#S.Number: 810199181
#Date: 2021-05-22 
#Course: Statistical Inference
#====================================================

# Importing the Healthcare dataset
HealthCare <- read.csv("H:/Second Term/Statistical Inference/Projects/ProjectPhase1/HealthCare.csv")
View(HealthCare)

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#=============================== Question 1 : Part A ===========================
# importing libraries
library(ggplot2)
library(plyr)
# HISTOGRAM and probability density function for age of observations
ggplot(HealthCare, aes(x = age)) +
  # plot a histogram
  geom_histogram(aes(y=..density..),
                 binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)) ,
                 colour = "black", fill = "skyblue") +   
  # distribution of age (Density Curve)
  geom_density(color="orange2",size=1.4) +
  labs(title="Histogram for age of patients") +         # the title of the plot
  theme_bw() + 
  # the grid and background removed from plot,
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # put the title location in the center of the plot.
        plot.title = element_text(size=12,face="bold",hjust = 0.5))   

#=============================== Question 1 : Part B ===========================
# importing libraries
library(ggplot2)
# Q-Q plot for Comparison of age distribution with normal distribution  
qplot(sample = age, data = HealthCare)+
  # the title of the plot 
  labs(title="Comparison of age distribution with normal distribution",
       y = "age",x="Theoretical Quantiles")+
  # draw a line with size=1.3 in Q-Q plot
  stat_qq_line(size=1.3,color="skyblue") +
  theme_classic()
#=============================== Question 1 : Part C ===========================
# importing library
library(moments)

# calculate skewness in r
skewness(HealthCare$age)

# Non-parametric 
sk <- (mean(HealthCare$age)-median(HealthCare$age))/sd(HealthCare$age)
sk
#=============================== Question 1 : Part D ===========================
# importing libraries
library(ggplot2)

# plot a Boxplot for "age" variable
ggplot(HealthCare) +
  aes(y = age) +
  geom_boxplot(fill = "skyblue1") +
  theme_minimal() + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

# plot a Boxplot for "avg_glucose_level" variable
ggplot(HealthCare) +
  aes(y=avg_glucose_level) +
  geom_boxplot(fill = "skyblue1") +
  theme_minimal() + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

#=============================== Question 1 : Part E ===========================
mean(HealthCare$age)
median(HealthCare$age)
var(HealthCare$age)
sd(HealthCare$age)

#=============================== Question 1 : Part F ===========================
# plot a density plot for age 
ggplot(HealthCare, aes(x=age))+
  geom_density(color="skyblue3", fill="skyblue",alpha = 0.2)+ 
  theme_minimal() +
  # add lines for the mean and median
  geom_vline(aes(xintercept=mean(age)),
             color="red1", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(age)),
             color="green3", linetype="dashed", size=1)  +
  # add texts for the mean and median
  annotate(geom="text", x=mean(HealthCare$age)-6, y=0.01, 
           label=paste0("mean\n",mean(HealthCare$age)),
           color="red1") +
  annotate(geom="text", x=median(HealthCare$age)+6, y=0.008,
           label=paste0("median\n",median(HealthCare$age)),
           color="green3")
  
#=============================== Question 1 : Part G ===========================
# this function Categorizing the age variable into four intervals based on its mean.
func <- function(inp){
  if(inp <= 21)
    return("0-21")
  else if(inp >= 22 & inp <=43)
    return("22-43")
  else if(inp >=44 & inp <=64)
    return("44-64")
  else if(inp >=65)
    return("65-82")
}

# Add a new column to HealthCare dataset and call function on each row of age variable
# to Categorize this variable into four intervals based on its mean
HealthCare$age_interval <- lapply(HealthCare$age,func)

# create a dataframe 
dataa <- data.frame(data = prop.table(table(unlist(HealthCare$age_interval)))*100)

#importing libraries
library(ggplot2)
library(scales)  # for percentage scales

# plot a pie chart that visualizes the frequency of these four categories (age intervals)
ggplot(data = dataa, aes(x = "", y = data.Freq, fill = data.Var1)) + 
  geom_bar(stat = "identity", color = "black",width=1) +
  labs(fill = "Intervals", title = "Age intervals") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette = "Dark2")+
  theme_void() + theme(axis.line = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       plot.title = element_text(hjust = 0.5, color = "#666666"))+
  geom_text(aes(y = data.Freq/3 + c(0, cumsum(data.Freq)[-length(data.Freq)]), 
                label = percent(data.Freq/100)), size=5)

#=============================== Question 1 : Part H ===========================
# importing libraries
library(ggplot2)

# fivenum function can return min , Q1 , Q2 , Q3 , max for age variable
five <- fivenum(HealthCare$age)
IQR <- five[4] - five[2]

# plot a Boxplot to specify upper and lower quartiles, whiskers, and the IQR
ggplot(HealthCare) +
  aes(x=age) +
  geom_boxplot( alpha=0.5,fill = "skyblue1",lwd=1) +
  theme_minimal() + 
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  # Q2 (median) 
  geom_vline(aes(xintercept=median(age)),color="red1",size=1.3)+
  annotate(geom="text", x=median(HealthCare$age)-6, y=0.05, 
           label=paste0("median\n",median(HealthCare$age)),
           color="red1") +
  # Q3 (third quartile)
  geom_vline(aes(xintercept=quantile(age,0.75)),color="purple1", size=1.3) +
  annotate(geom="text", x=quantile(HealthCare$age,0.75)-3, y=0.05, 
           label=paste0("Q3"),
           color="purple1") +
  # Q1 (first quartile)
  geom_vline(aes(xintercept=quantile(age,0.25)),color="orange1", size=1.3) +
  annotate(geom="text", x=quantile(HealthCare$age,0.25)-3, y=0.05, 
           label=paste0("Q1"),
           color="orange1")+
  # lower Whisker    
  geom_vline(aes(xintercept=max(five[2]-(1.5*IQR),five[1])),color="green4", size=1.3)+
  annotate(geom="text", x=max(five[2]-(1.5*IQR),five[1])+6, y=0.05, 
           label=paste0("lower Whisker"),
           color="green4")+
  # upper Whisker
  geom_vline(aes(xintercept=min(five[4]+(1.5*IQR),five[5])),color="green4", size=1.3)+
  annotate(geom="text", x=min(five[4]+(1.5*IQR),five[5])+6, y=0.05, 
           label=paste0("upper Whisker"),
           color="green4") +
  # IQR
  geom_segment(aes(x = five[2], y = -0.45 , xend = five[4], yend = -0.45)) +
  annotate(geom="text", x=median(HealthCare$age)-6, y=-0.47, 
           label=paste0("IQR"),
           color="black")

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#=============================== Question 2 : Part A ===========================
# importing libraries
library(ggplot2)
library(scales)  # for percentage scales

# find the frequency of each category with Barplot
ggplot(HealthCare, aes(x = smoking_status)) +
  geom_bar(aes(x = smoking_status, fill = smoking_status), position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  labs(title="Barplot for Frequency of each Category ",
       x="Smoking Status",y="Frequency",fill="Status") +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

# find the frequency of each category with table function  
table(HealthCare$smoking_status)

# find the percentage of each category with prop.table function  
prop.table(table(HealthCare$smoking_status))*100

#=============================== Question 2 : Part B ===========================
# importing libraries
library(ggplot2)
library(scales)  # for percentage scales

# find the percentage of each category with Barplot with different colors
# and add percentage marks to each bar
ggplot(data = HealthCare, aes(x = smoking_status, 
                          y = prop.table(stat(count)), 
                          fill = smoking_status, 
                          label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = 1.5, 
            size = 4, colour = "white") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Barplot for Percentage of each Category ",
       x="Smoking Status",y="Percentage",fill="Status") +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

#=============================== Question 2 : Part C ===========================
# importing libraries
library(ggplot2)
library(scales)  # for percentage scales

# A function that sorts my categorical variable categories by their frequencies.
reordering <- function(inp) {
  factor(inp, levels = names(sort(table(inp), increasing = TRUE)))
 }

# plotting a Horizontal BarPlot for my categorical variable that Sorted by their frequencies.
ggplot(HealthCare, aes(x = reordering(smoking_status))) +
  geom_bar(aes(x = reordering(smoking_status), fill = smoking_status), position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", hjust = 1.2,size = 4, colour = "white") +
  labs(title="Horizontal Barplot for Frequency of each Category that Sorted by their frequencies",
       x="Smoking Status",y="Frequency",fill="Status") +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  coord_flip()

#=============================== Question 2 : Part D ===========================
# importing libraries
library(ggplot2)

# Plotting a violin plot for smoking status and age variables
ggplot(HealthCare, aes(x = smoking_status,y = age )) +
  geom_violin(trim = FALSE,aes(fill = smoking_status)) +
  geom_boxplot(width = 0.12,)+
  labs(title="Violin Plot of age by smoking status",
       x="Smoking Status",y="Age",fill="Status") +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#=============================== Question 3 : Part B ===========================
# replace missing values with the mean
samp <- HealthCare
sampp <- data.frame(sapply(samp,function(x) ifelse(is.na(x),mean(x, na.rm = TRUE),x)))

# importing library
library(ggplot2)
# plot a scatter plot
ggplot(sampp, aes(x=unlist(age), y=unlist(health_bills))) +
  geom_point(size=1, shape=23,color="skyblue2") +
  geom_smooth(method=lm, se=FALSE,color="red2",size=1.3)+
  theme_classic()

#=============================== Question 3 : Part C ===========================
library("ggpubr")
# Calculate the correlation coefficient for these two variables
cor(unlist(sampp$age),unlist(sampp$health_bills), method = c("pearson", "kendall", "spearman"))

#=============================== Question 3 : Part E ===========================
# Correlation testing
cor.test(unlist(sampp$age),unlist(sampp$health_bills), method = c("pearson", "kendall", "spearman"))

#=============================== Question 3 : Part F ===========================
# importing library
library(ggplot2)
# plot a scatter plot
ggplot(sampp, aes(x=unlist(age), y=unlist(health_bills),
                  colour = unlist(work_type))) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE,color="green3",size=1.6)+
  theme_classic()+
  labs(x="age",y="health bills",colour="work type")

#=============================== Question 3 : Part G ===========================
# importing libraries
library(ggplot2)
library(hexbin)
library(RColorBrewer)

# plot a hexbin plot with marginal distribution and fitting curve for 
# age and health bills variables
plot <- ggplot(sampp, aes(unlist(age),unlist(health_bills))) + stat_bin2d(bins=80) + 
  scale_fill_gradientn(colours=r) + geom_point(size=-1) + 
  theme(legend.position="left") +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), 
              se = TRUE,color="red2",size=1.3) +
  labs(x="age",y="health bills",title="Hexbin plot")
ggMarginal(plot,type="histogram", fill = "slateblue", xparams = list(  bins=40))

#=============================== Question 3 : Part H ===========================
# importing libraries
library(ggplot2)
# Draw the 2D density plot for age and health bills variables
ggplot(sampp, aes(x=unlist(age), 
                  y=unlist(health_bills)) ) +
  stat_density_2d(aes(fill = ..level..), 
                  geom = "polygon", colour="orange")+
  labs(x="age",y="health bills",title="2D density plot")

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================


#=============================== Question 4 : Part A ===========================


#=============================== Question 4 : Part B ===========================


#=============================== Question 4 : Part C ===========================
# importing libraries
library("scatterplot3d") 
# my categorical variable is gender 
catt <- c("red","green","blue")
catt <- catt[transform(unlist(sampp22$gender ),
                       id=as.numeric(factor(unlist(sampp22$gender ))))$id]
# Draw a 3D scatterplot for the numerical variables 
# and use the categorical variable as points’ color
scatterplot3d(unlist(sampp22$age),unlist(sampp22$health_bills),
              unlist(sampp22$avg_glucose_level),main="3D Scatter Plot",
              xlab = "age",ylab = "average glucose level",zlab = "health bills",
              color=catt, pch = 16)
# plot a legend for gender
legend("right", legend = c("Male","Female","Other"),
       col =  c("red", "green","blue"), pch = 16)

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#=============================== Question 5 : Part A ===========================
# importing libraries
library(gridExtra)
library(grid)

# draw Frequency/Contingency table for gender and work type variables
my_table = table(gender=HealthCare$gender,worktype=HealthCare$work_type)
grid.table(addmargins(my_table,FUN = sum),theme=ttheme_minimal(
  core=list(bg_params = list(fill = "azure2", col=NA),
            fg_params=list(fontface=1)),
  colhead=list(fg_params=list(col="skyblue")),
  rowhead=list(fg_params=list(col="orange"))))

#=============================== Question 5 : Part B ===========================
# importing library
library(ggplot2)
# Grouped bar chart  for gender and work type variables
ggplot(HealthCare, aes(x = work_type,group = gender)) +
  geom_bar(aes(x = work_type, fill = gender), position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 1.5, colour = "black") +
  labs(title="Grouped bar chart",
       x="work type by gender",y="Frequency",fill="gender")

#=============================== Question 5 : Part C ===========================
# importing library
library(ggplot2)
# Segmented bar plot for gender and work type variables
ggplot(HealthCare, aes(x = work_type,group = gender)) +
  geom_bar(aes(x = work_type, fill = gender), 
           position = position_stack(vjust = .1)) +
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = .01, colour = "black") +
  labs(title="Segmented bar chart",
       x="work type by gender",y="Frequency",fill="gender")

#=============================== Question 5 : Part D ===========================
# importing library
library(ggplot2) 
library(ggmosaic) 

# Mosaic plot
ggplot(data = HealthCare) +
  geom_mosaic(aes(x = product(work_type),
                  group = gender,
                  weight = prop.table(stat(count)), 
                  fill = gender, 
                  label = scales::percent(prop.table(stat(count))))) + 
  scale_y_continuous(labels = scales::percent) +
  # for labeling
  labs(title="Mosaic plot ",
       x="work type",y="Percentage",fill="gender") +
  annotate("text",x=0.07,y=0.25,label="47%",lwd=3)+
  annotate("text",x=0.07,y=0.75,label="52%",lwd=3)+
  annotate("text",x=0.07,y=1,label="0%",lwd=3)+
  
  annotate("text",x=0.2,y=0.25,label="60%",lwd=3)+
  annotate("text",x=0.2,y=0.75,label="40%",lwd=3)+
  annotate("text",x=0.2,y=1,label="0%",lwd=3) +
  
  annotate("text",x=0.275,y=0.25,label="50%",lwd=3)+
  annotate("text",x=0.275,y=0.75,label="50%",lwd=3)+
  annotate("text",x=0.275,y=1,label="0%",lwd=3)+
  
  annotate("text",x=0.6,y=0.25,label="59%",lwd=3)+
  annotate("text",x=0.6,y=0.75,label="40.97%",lwd=3)+
  annotate("text",x=0.6,y=1,label="0.03%",lwd=3)+
  
  annotate("text",x=0.93,y=0.25,label="61%",lwd=3)+
  annotate("text",x=0.93,y=0.75,label="39%",lwd=3)+
  annotate("text",x=0.93,y=1,label="0%",lwd=3)

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

# Choose a random sample of 50 data points from the HealthCare dataset
My_sam <- HealthCare[sample(nrow(HealthCare), size = 50, replace = FALSE),]

#=============================== Question 6 : Part A ===========================
# 95% Confidence Interval for mean 
# Point estimate ± Z* SE

# Point estimate (X)
point_estimate <- mean(My_sam$age)

# standard error (SE)
SE <- sd(HealthCare$age)/sqrt(length(My_sam$age))

# Z*
Z <- qnorm(0.975)

# # 95% Confidence Interval for mean
CI <- c(point_estimate - (Z*SE),point_estimate + (Z*SE))
CI
#=============================== Question 6 : Part C ===========================
# importing libraries
library(ggplot2)
# HISTOGRAM and 95% Confidence Interval for mean age of observations
ggplot(My_sam, aes(x = age)) +
  # plot a histogram
  geom_histogram(aes(y=..density..),
                 binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)) ,
                 colour = "black", fill = "skyblue") +
  labs(title="Histogram for age of patients and 95% CI for meu") +         # the title of the plot
  theme_bw() + 
  # the grid and background removed from plot,
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # put the title location in the center of the plot.
        plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  # mean
  geom_vline(aes(xintercept=mean(age)),color="red1",size=1.3)+
  annotate(geom="text", x=mean(My_sam$age)-1, y=0.02, 
           label=paste0("mean\n",mean(My_sam$age)),
           color="red1")+
  # Confidence interval
  geom_vline(aes(xintercept=point_estimate + (Z*SE)),color="green3",size=1.3)+
  annotate(geom="text", x=point_estimate + (Z*SE)+5, y=0.018, 
           label="95% CI",
           color="green3")+
  geom_vline(aes(xintercept=point_estimate - (Z*SE)),color="green3",size=1.3)+
  annotate(geom="text", x=point_estimate - (Z*SE)-5, y=0.018, 
           label="95% CI",
           color="green3")

#=============================== Question 6 : Part D ===========================
# Hypothesis testing for mean of age variable
# 1 set the hypotheses
  null_value <- 43
# 2 calculate the point estimate
  n <- length(My_sam$age)
  # Point estimate (X)
  X <- mean(My_sam$age)
  # standard error (SE)
  SE <- sd(HealthCare$age)/sqrt(n)
# 3 check conditions
# 4 calculate test statistic
  Z_statistic <- (X-null_value)/SE
# 5 calculate the p-value
  p_value <- 2 * (1- pnorm(Z_statistic))
  p_value

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================
  
#=============================== Question 7 : Part A ===========================
# Choose a random sample of 25 data points from the dataset
My_sample <- HealthCare[sample(nrow(HealthCare), size = 25, replace = FALSE),]
  
    
# Paired Hypothesis test Framework
# 1) Convert 2 numeric variables to one variable called diff
  
     # Adding a column with consecutive numbers
     My_sample$observation <- 1:25
    # do calculate difference between the age and avg_glucose_level variables
    funct <- function(inp){
      diff <- My_sample$avg_glucose_level[inp] - My_sample$age[inp]
      return(diff)
    }
    
    # Add a new column to sample and named it "diff" 
    # call function on each row of the sample with lapply.
    # calculate difference between the age and avg_glucose_level variables
    My_sample$diff <- lapply(My_sample$observation,funct)
  
# 2) set the hypothesis
# 3) calculate the point estimate
    n_diff <- length(My_sample$observation)
    X_diff <- mean(unlist(My_sample$diff))
    s_diff <- sd(unlist(My_sample$diff))
    SE_diff <- s_diff/sqrt(n_diff)
# 4) calculate test statistic
    T_statistic <- (X_diff-0)/SE_diff
# 5) df
    dfree <- 24
# 6) p-value
    P_VALUE <- 2 * pt(T_statistic,df=dfree,lower.tail = FALSE)
    P_VALUE
# with t-test      
t.test(unlist(My_sample$diff))  
  
#=============================== Question 7 : Part B ===========================
# Choose a random sample
My_age_sample <- HealthCare[sample(nrow(HealthCare[1:2555,]), 
                                   size = 100, replace = FALSE),]
My_glucose_sample <- HealthCare[sample(nrow(HealthCare[2556:5110]), 
                                       size = 100, replace = FALSE),]
  
My_age_sample_1 <- My_age_sample$age
My_glucose_sample_1 <- My_glucose_sample$avg_glucose_level

# 1) set the hypothesis
# 2) calculate the point estimate
n_glucose <- length(My_glucose_sample_1)
X_glucose <- mean(My_glucose_sample_1)
s_glucose <- sd(My_glucose_sample_1)

n_age <- length(My_age_sample_1)
X_age <- mean(My_age_sample_1)
s_age <- sd(My_age_sample_1)

p_estim <- X_glucose - X_age

SE_age_glucose <- sqrt( ( (s_glucose^2)/(n_glucose) ) + ( (s_age^2)/(n_age) ) )

# 3) calculate test statistic
T_statisticc <- (p_estim-0)/SE_age_glucose
# 5) df
dfreee <- 99
# 6) p-value
P_VALUEE <- 2 * pt(T_statisticc,df=dfreee,lower.tail = FALSE)
P_VALUEE

# 95% confidence interval
t_ <- abs(qt(0.025,df=99))
CO_IN <- c(p_estim - (t_ * SE_age_glucose),p_estim + (t_ * SE_age_glucose))
CO_IN
  

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================  
  
#=============================== Question 8 : Part A ===========================
CI_95 <- c(quantile(HealthCare$avg_glucose_level,0.025),
           quantile(HealthCare$avg_glucose_level,0.975))

# importing libraries
library(ggplot2)
library(plyr)
# HISTOGRAM and 95% Confidence Interval for mean avg_glucose_level of observations
ggplot(HealthCare, aes(x = avg_glucose_level)) +
  # plot a histogram
  geom_histogram(aes(y=..density..),
                 binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)) ,
                 colour = "black", fill = "skyblue") +
   labs(title="Histogram for average glucose level of patients and 95% CI for mean") +
   theme_bw() + 
  # the grid and background removed from plot,
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # put the title location in the center of the plot.
        plot.title = element_text(size=12,face="bold",hjust = 0.5)) +
  # 95% Confidence interval
  geom_vline(aes(xintercept=quantile(avg_glucose_level,0.025)),color="red2",size=1.3)+
  annotate(geom="text", x=quantile(HealthCare$avg_glucose_level,0.025)+2, y=0.018, 
            label="95% CI",
            color="red2")+
  geom_vline(aes(xintercept=quantile(avg_glucose_level,0.975)),color="red2",size=1.3)+
  annotate(geom="text", x=quantile(HealthCare$avg_glucose_level,0.975)-2, y=0.018, 
            label="95% CI",
            color="red2")
#=============================== Question 8 : Part B ===========================
#create an Empty Vector
bootstrap_dist <- c()

#for 100 times resampling
for (i in c(1:100)) {
  #Sampling of the original sample with placement
  bootstrap_sample <- HealthCare[sample(nrow(HealthCare), size = 20, replace = TRUE),]
  
  #Using the following commands, the average of my variable is calculated.
  mean_boo <- mean(bootstrap_sample$avg_glucose_level)
  
  bootstrap_dist <- append(bootstrap_dist,mean_boo)
}

#To be able to visualize data with the ggplot Library, I create a data frame.
mydata_frame <- data.frame(data = bootstrap_dist)

library(ggplot2)
#I use dotplot to display the distribution shape.
ggplot(mydata_frame, aes(x = data)) + geom_dotplot(binwidth = 3) +
  stat_function(fun = function(curve) 16* (dnorm(curve,mean = mean(mydata_frame$data), 
                                                  sd = sd(mydata_frame$data))) ,
                color = "deepskyblue" ,size=1)

# ***standard error method*** --> bootstrap samples confidence interval
# point estimate ± t* X SE
# point estimate -->  mean of original sample
# df = 99
# SE --> bootstrap distribution

# calculate the mean
point_estimateee <- mean(HealthCare$avg_glucose_level)

# df = n-1 = 100-1
df <- 99

# SE --> bootstrap distribution
SEE <- sd(mydata_frame$data)

# t-score with df=999
t_score <- abs(qt(0.025,df=99))

# confidence interval
# point estimate ± t* X SE
CI_boot <- c(point_estimateee - (t_score * SEE),point_estimateee + (t_score * SEE))
CI_boot
  
#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================

#=============================== Question 9 ====================================

# Choose a random sample
mynewsample <- HealthCare[sample(nrow(HealthCare),size=500,replace=FALSE),]

# replace missing values with the mean
mynewsample_1 <- data.frame(sapply(mynewsample,
                                 function(x) ifelse(is.na(x),mean(x, na.rm = TRUE),x)))
count <- 1

Private_group <- c()
slef_group <- c()
never_group <- c()
govt_group <- c()
chi_group <- c()

for (i in 1:500){
   if(mynewsample_1$work_type[count] == "Private"){
    Private_group <- c(Private_group,mynewsample_1$health_bills[count])
    count <- count + 1
  }else if(mynewsample_1$work_type[count] == "Self-employed"){
    slef_group <- c(slef_group,mynewsample_1$health_bills[count])
    count <- count + 1
  }else if(mynewsample_1$work_type[count] == "Never_worked"){
    never_group <- c(never_group,mynewsample_1$health_bills[count])
    count <- count + 1
  }else if(mynewsample_1$work_type[count] == "Govt_job"){
    govt_group <- c(govt_group,mynewsample_1$health_bills[count])
    count <- count + 1
  }else if(mynewsample_1$work_type[count] == "children"){
    chi_group <- c(chi_group,mynewsample_1$health_bills[count])
    count <- count + 1
  }
}

# importing libraries
library(ggplot2)
library(scales)  # for percentage scales

# find the frequency of each work type with Barplot
ggplot(mynewsample_1, aes(x = unlist(work_type))) +
  geom_bar(aes(x = unlist(work_type), fill = unlist(work_type)), position = "dodge") +
  geom_text(aes(label = ..count..), 
            stat = "count", vjust = 1.5, colour = "black") +
  labs(title="Barplot for Frequency of each work type ",
       x="Work type",y="Frequency",fill="Type") +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


# plot a Q-Q plot for all work types 
qqnorm(unlist(Private_group),pch = 3, frame = FALSE,col="red2",
       main = "Private work type" )  
qqline(unlist(Private_group), lwd = 2,col = "sky blue2")     

qqnorm(unlist(slef_group),pch = 3, frame = FALSE,col="red2",
       main = "Self-employed work type" )  
qqline(unlist(slef_group), lwd = 2,col = "sky blue2")  

qqnorm(unlist(never_group),pch = 3, frame = FALSE,col="red2",
       main = "Never_worked work type" )  
qqline(unlist(never_group), lwd = 2,col = "sky blue2")  

qqnorm(unlist(govt_group),pch = 3, frame = FALSE,col="red2",
       main = "Govt-job type work" )  
qqline(unlist(govt_group), lwd = 2,col = "sky blue2")  

qqnorm(unlist(chi_group),pch = 3, frame = FALSE,col="red2",
       main = "children type work" )  
qqline(unlist(chi_group), lwd = 2,col = "sky blue2")  


# Boxplot for all work type
boxplot(unlist(mynewsample_1$health_bills)~unlist(mynewsample_1$work_type),
        main='Health bills by work type',xlab='work type',ylab='health bills')


# ANOVA in R
anova_test<-aov(unlist(mynewsample_1$health_bills)~unlist(mynewsample_1$work_type))
summary(anova_test)

# p-value
pf(12.32,4,495,lower.tail = FALSE)

#==========================================================================================================================================================
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#==========================================================================================================================================================