
# setup
install.packages("tidyverse")
install.packages("gtsummary")
install.packages("GGally")
install.packages("gridExtra")
install.packages("ggfortify")
install.packages("olsrr")
install.packages("MASS")
install.packages("plyr")
install.packages("lmtest")
install.packages("pracma")
library(tidyverse)
library(GGally)
library(ggfortify)
library(MASS)
library(car)
library(gridExtra)
library(plyr)
library(lmtest)
library(pracma)
library(olsrr)
   

      
data <- read.csv("302data.csv",header = TRUE)
   

      
glimpse(data)
   
Creating training and test set
      
set.seed(1008124245)
   

   {r data_cleaning_aggregate}
# add aggregate variables
data <- data %>% 
  mutate(study_avg = rowMeans(cbind(Studying, Studying2, Studying3, Studying4))) %>% 
  mutate(COVID_avg = rowMeans(cbind(COVID, COVID2, COVID3, COVID4)))
   

      
# random split train 70% test 30%
dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]
   

      
data %>%
  ggplot(aes(x=Studying))+geom_histogram(
    color="gray", 
    fill="steelblue")+ 
  labs(title="Number of Studying hours in Week 1")+
  theme(plot.title=element_text(hjust = 0.5))
   
      
data %>%
  # by default bins=30
  ggplot(aes(x=Studying2))+geom_histogram(
    color="gray", 
    fill="steelblue")+ 
  labs(title="Number of Studying hours in Week 2")+
  theme(plot.title=element_text(hjust = 0.5))
   
      
data %>%
  # by default bins=30
  ggplot(aes(x=Studying3))+geom_histogram(
    color="gray", 
    fill="steelblue")+ 
  labs(title="Number of Studying hours in Week 3")+
  theme(plot.title=element_text(hjust = 0.5))
   
      
data %>%
  # by default bins=30
  ggplot(aes(x=Studying4))+geom_histogram(
    color="gray", 
    fill="steelblue")+ 
  labs(title="Number of Studying hours in Week 4")+
  theme(plot.title=element_text(hjust = 0.5))
   

   {r,out.width="20%", fig.width=5, fig.height=4, fig.show='hold'}
boxplot(data$Studying, data$Studying2, data$Studying3, data$Studying4, 
        names=c("week 1", "week 2", "week 3", "week 4"), 
        ylab="Numumber of Studying hours")
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=Miscellaneous))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of Miscellaneous hours in Week 1")+
  theme(plot.title = element_text(hjust = 0.5))
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=Miscellaneous2))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of Miscellaneous hours in Week 2")+
  theme(plot.title = element_text(hjust = 0.5))
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=Miscellaneous3))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of Miscellaneous hours in Week 3")+
  theme(plot.title = element_text(hjust = 0.5))
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=Misceallenous4))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of Miscellaneous hours in Week 4")+
  theme(plot.title = element_text(hjust = 0.5))
   

      
boxplot(data$Miscellaneous, data$Miscellaneous2, data$Miscellaneous3, data$Misceallenous4, 
        names=c("week 1", "week 2", "week 3", "week 4"),
        ylab="Numumber of Miscellaneous hours")
   
      
boxplot(data$COVID, data$COVID2, data$COVID3, data$COVID4, 
        names=c("week 1", "week 2", "week 3", "week 4"),
        ylab="Numumber of COVID concern hours")
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=COVID))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of COVID concern hours in Week 1")+
  theme(plot.title = element_text(hjust = 0.5))
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=COVID2))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of COVID concern hours in Week 2")+
  theme(plot.title = element_text(hjust = 0.5))
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=COVID3))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of COVID concern hours in Week 3")+
  theme(plot.title = element_text(hjust = 0.5))
   
   {r,}
data %>%
  # by default bins=30
  ggplot(aes(x=COVID4))+
  geom_histogram(color="gray", fill="steelblue")+
  labs(title="Number of COVID concern hours in Week 4")+
  theme(plot.title = element_text(hjust = 0.5))
   

      
data %>%
  # by default bins=30
  ggplot(aes(x=Term.Test))+geom_histogram(color="gray", fill="steelblue")+
  labs(title="Term Test Grade")+
  theme(plot.title = element_text(hjust = 0.5))
   
      
data$Term.Test.percentage <- data$Term.Test/55 * 100
boxplot(data$Term.Test.percentage, xlab="Term.Test", ylab="Term Test Grade")
   

      
data %>%
  # by default bins=30
  ggplot(aes(x=Term.Test.percentage))+geom_histogram(color="gray", fill="steelblue")+
  labs(title="Term Test Percentage Grade")+
  theme(plot.title = element_text(hjust = 0.5))
   

      
data %>%
  ggplot(aes(x=Famiiliar))+
  geom_bar(color="gray", fill="steelblue")+
  labs(title="Familiarity on Course Materials")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()
   

      
data %>%
  ggplot(aes(x=OH))+
  geom_bar(color="gray",fill="steelblue")+
  labs(title="Office Hour Attendance")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()
   
      
# corrolation maps
corr_study <- ggcorr(cbind(
  w1 = data$Studying, 
  w2 = data$Studying2, 
  w3 = data$Studying3, 
  w4 = data$Studying4
)) + ggtitle("Correlation between 
Weekly Studying Hours")
corr_covid <- ggcorr(cbind(
  w1 = data$COVID, 
  w2 = data$COVID2, 
  w3 = data$COVID3, 
  w4 = data$COVID4
)) + ggtitle("Correlation between 
Weekly COVID-19 Concern
Hours")
corr_miscel <- ggcorr(cbind(
  w1 = data$Miscellaneous, 
  w2 = data$Miscellaneous2, 
  w3 = data$Miscellaneous3, 
  w4 = data$Misceallenous4
)) + ggtitle("Correlation between 
Weekly Miscellaneous 
Hours")
grid.arrange(corr_study, corr_covid, corr_miscel, nrow = 1)

data %>%
  ggplot(aes(x=(Studying+Studying2+Studying3+Studying4)/4,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to average Studying Time")

data %>%
  ggplot(aes(x=Studying,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to Week 1 Studying Time")

data %>%
  ggplot(aes(x=Miscellaneous,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to Miscellaneous Activities")

data %>%
  ggplot(aes(x=Miscellaneous2,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to Miscellaneous Activities in Week 2")

data %>%
  ggplot(aes(x=Miscellaneous3,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to Miscellaneous Activities in Week 3")

data %>%
  ggplot(aes(x=Misceallenous4,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to Miscellaneous Activities in Week 4")

data %>%
  ggplot(aes(x=COVID,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to COVID")

data %>%
  ggplot(aes(x=COVID2,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to COVID2")

data %>%
  ggplot(aes(x=COVID3,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to COVID3")

data %>%
  ggplot(aes(x=COVID4,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to COVID")

data %>%
  ggplot(aes(x=(COVID+COVID2+COVID3+COVID4)/4,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to COVID_avg")

data %>%
  ggplot(aes(x=(Miscellaneous+Miscellaneous2+Miscellaneous3+Misceallenous4)/4,y=Term.Test.percentage))+
  geom_point()+
  geom_smooth(se=FALSE,method="lm")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Term Test Grade in relation to miscel_avg")

data %>%
  ggplot(aes(x=OH,y=Term.Test.percentage))+
  geom_boxplot(color="gray",fill="steelblue")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Office Hour Attendance in relation to Term Test Grades")+
  coord_flip()

data %>%
  ggplot(aes(x=Famiiliar,y=Term.Test.percentage))+
  geom_boxplot(color="gray",fill="steelblue")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Famiiliarity in relation to Term Test Grades")+
  coord_flip()

data %>%
  ggplot(aes(x=(Studying+Studying2+Studying3+Studying4)/4,y=OH))+
  geom_boxplot(color="gray",fill="steelblue")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Average Studying Time in relation to Office Hour Attendance")+
  coord_flip()

data %>%
  ggplot(aes(x=(Studying+Studying2+Studying3+Studying4)/4,y=Famiiliar))+
  geom_boxplot(color="gray",fill="steelblue")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Average Studying Time in relation to Familiarity")+
  coord_flip()

summary(data$Term.Test.percentage)

data %>%
  ggpairs(data, columns = c("study_avg","COVID_avg","Term.Test","OH","Famiiliar"), title = "Pair Plot") + theme(plot.title = element_text(hjust = 0.4))


Model Development


full_model <- lm(Term.Test~.-ID-study_avg-COVID_avg-Miscellaneous-Miscellaneous2-Miscellaneous3-Misceallenous4,data=train)
summary(full_model)

vif(full_model)

mean(vif(full_model))

reduced_model = lm(full_model, data=train)
ols_step_backward_aic(reduced_model, progress = FALSE, details = FALSE)

stepwise_AIC_model <- stepAIC(full_model, trace = FALSE)
summary(stepwise_AIC_model)



candidate_model_1 <- lm(Term.Test~COVID3+Studying, data = train)
summary(candidate_model_1)

candidate_model_2 <- lm(Term.Test~COVID3+Studying+Famiiliar:Studying, data=train)
summary(candidate_model_2)

candidate_model_3 <- lm(Term.Test~COVID4+Studying, data = train)
summary(candidate_model_3)

install.packages("AICcmodavg")
library(AICcmodavg)
#define list of models
models <- list(full_model, reduced_model, candidate_model_1,candidate_model_2,candidate_model_3)
#specify model names
mod.names <- c('full model', 'AIC reduced model', 'candidate model 1','candidate model 2','candidate model 3')
#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)


Diagnostic Plots

# diagnostic plots
autoplot(reduced_model)+theme(plot.title = element_text(hjust = 0.5))

# Leverage point
h <- hatvalues(candidate_model_1)
threshold <- 2*(length(candidate_model_1$coefficients)/nrow(train))
length(which(h>threshold))
# Outlier
std_res <- rstandard(candidate_model_1)
length(which(abs(std_res)>2))
# Cook's distance
D <-  cooks.distance(candidate_model_1)
cutoff_D <- 4/(nrow(train)-2)
length(which(D>cutoff_D))

vif(candidate_model_1)



AIC(candidate_model_1)

BIC(candidate_model_1)

summary(candidate_model_1)$adj.r.squared

train_model <- lm(Term.Test~COVID3+Studying,data=train)
summary(train_model)

test_model <- lm(Term.Test~COVID3+COVID4+Studying,data=test)
summary(test_model)
autoplot(test_model)

# Outlying and influential for Y
n <- nrow(train)
p_prime <- length(coef(reduced_model))
# calculate studentized deleted residual
model_studentized_deleted_residual <- rstudent(reduced_model)
# alpha value
alpha <- 0.05
# construct report
model_outlying_influential_y <- cbind(
  row_index = seq(1:n), 
  Y = train$Term.Test, 
  Y_hat = fitted(reduced_model), 
  t = model_studentized_deleted_residual, 
  cooks_distance = cooks.distance(reduced_model)
) %>% 
  as_tibble() %>% 
  mutate(is_outlier = abs(t) > qt(1 - alpha/(2*n), n - p_prime - 1)) %>% 
  mutate(is_influential = cooks_distance > qf(0.5, p_prime, n - p_prime))
head(model_outlying_influential_y)




model_outlying_influential_y %>% filter(is_outlier & is_influential)



bc <- boxcox(reduced_model)
(lambda <- bc$x[which.max(bc$Term.Test)])

# Partial F Test
final_model <- lm(Term.Test~Studying+COVID4+COVID3,data=train)
anova(final_model)