
#In this exercise, I will use data set called "cushny" in package "psychTools"

#Data import
#install.packages("psychTools")
library(psychTools)
df <- psychTools::cushny
head(df)

#Data description
str(df)
summary(df)
sum(is.na(df)) #no missing data
#mean
mean_ct <-mean(df$Control) 
mean_d1 <- mean(df$drug1)
#median of control group,drug1
median_ct <- median(df$Control)
median_d1 <- median(df$drug1)
#quartiles
quart_ct <- quantile(df$Control, probs = c(0.25, 0.5, 0.75))
quart_d1 <- quantile(df$drug1, probs = c(0.25, 0.5, 0.75))
#sample variance and sample standard deviation
var(df$Control)
sd(df$Control)
var(df$drug1)
sd(df$drug1)
#range
range(df$Control)
range(df$drug1)
#IQR
IQR(df$Control)
summary(df$Control)

#Distribution
# In base R
hist(df$Control, breaks =3,freq = FALSE,
    main = "Distribution of control group")
# We can add mean (red) and median (blue): abline draws a straight line from a to b 
# (or vertical as here)
abline(v = mean_ct, col = "red") 
abline(v = median_ct, col = "blue")



#box plot
boxplot(df$Control, ylab = "Hours of sleep",
        xlab = "control")
points(df$Control)

boxplot(df$drug1, ylab = "Hours of sleep",
        xlab = "drug1")
points(df$drug1)

myseq <- as.character(c("control","drug1","drug2L", "drug2R","delta1", "delta2L","delta2R"))
boxplot(list(df$Control,df$drug1,df$drug2L,df$drug2R,df$delta1,df$delta2L,df$delta2R), ylab = "Hours of sleep",
        xlab = "group", names=myseq)


#SE of different groups, we know that the length of each group is 10,so df=9
n <- length(df$Control)
SE_ct <- sd(df$Control) / sqrt(10) 
SE_d1 <- sd(df$drug1) / sqrt(10) 



#t-distribution (exact CI)
str(df)

tQuantile <- qt(p = 0.975, df =  9)

#control group
lowerCI_mean_ct <- mean_ct - tQuantile * SE_ct

upperCI_mean_ct <- mean_ct + tQuantile * SE_ct

cbind(mean_ct, lowerCI_mean_ct, upperCI_mean_ct)

t.test(df$Control, data = df, var.equal = FALSE)


#drug1 group
lowerCI_mean_d1 <- mean_d1 - tQuantile * SE_d1

upperCI_mean_d1 <- mean_d1 + tQuantile * SE_d1

cbind(mean_d1, lowerCI_mean_d1, upperCI_mean_d1)

#CIs for difference in group means
diff_mean_d1c <- mean_d1-mean_ct
var_diff_mean_d1c <- 
  (9 * var(df$drug1) + 9 * var(df$Control)) / 
  (9 + 9 - 2)
SE_diff_mean_d1c <- sqrt(var_diff_mean_d1c) * sqrt(1/9 + 1/9) 

tQuantile_diff <- qt(p = 0.975, df = 9 + 9 - 2)

lowerCI_diff_mean_d1c <- diff_mean_d1c - tQuantile_diff * SE_diff_mean_d1c
upperCI_diff_mean_d1c <- diff_mean_d1c + tQuantile_diff * SE_diff_mean_d1c
cbind(diff_mean_d1c, lowerCI_diff_mean_d1c, upperCI_diff_mean_d1c)
#The mean drug1 group sleep 0.75 hour than control group. 
#We are 95% confident that the true difference in the drug group lies between-1.31 hour and 2.81 hour.


#a paired t-test and a two group t-test
#H0:the difference in the control group and drug1 group is zero
t.test(df$Control, df$drug1, data = df, var.equal = FALSE)
t.test(df$Control, df$drug1, data = df, var.equal = TRUE)
#p=0.4007>0.05, acceptH0, which shows there is no significant difference between the control group and drug1 group.
t.test(df$Control,df$drug2L, data = df, var.equal = FALSE)
t.test(df$Control,df$drug2L, data = df, var.equal = TRUE)
#p=0.0072647<0.05, rejuectH0, which shows there is significant difference between the control group and drug2L group.
t.test(df$Control,df$drug2R, data = df, var.equal = FALSE)
t.test(df$Control,df$drug2R, data = df, var.equal = TRUE)
#p=0.01155<0.05, rejuectH0, which shows there is significant difference between the control group and drug2L group.
