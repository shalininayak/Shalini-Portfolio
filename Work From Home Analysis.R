rm(list=ls())

# Packages
library(haven)
library(stargazer)
library(Hmisc)

#Import data
satisfaction171 <- read_dta("Satisfaction.dta")
s171<-data.frame(satisfaction171)

#View table
View(s171)

#Summary
summary(s171)
stargazer(s171,type="text",align=TRUE,out="Summary.txt")

#Hypothesis
library(tidyverse)
s171 %>% group_by(children,satisfaction) %>% summarise(n=n()) %>% summarise(n=sum(n))
#people with no children are more satisfied
t.test(satisfaction ~ children , data = s171)
#Based on the type of data you collected, we perform a one-tailed t-test to test whether people with children are more satisfied than people without. This test gives you:
#an estimate of the difference in average satisfaction between the two groups.
#a p-value showing how likely you are to see this difference if the null hypothesis of no difference is true.
#Your t-test shows an average satisfaction of 4.726241 for group 0 (no child) and an average satisfaction of 4.546667 cm for group1(child), with an estimate of the true difference ranging from 0.179574 to infinity. The p-value is 0.1334.

s171 %>% group_by(volunteer,satisfaction) %>% summarise(n=n())%>% summarise(n=sum(n))
#people who wfh are more satisfied
t.test(satisfaction ~ volunteer , data = s171)

#Your t-test shows an average satisfaction of 4.725333 for group 1 (wfh) and an average satisfaction of 4.476190 cm for group0(not wfh), with an estimate of the true difference ranging from 0.249143 to infinity. The p-value is 0.07117.

m1 = lm(satisfaction ~ volunteer ,data = s171)
m2 = lm(satisfaction ~ children ,data = s171)
summary(m1)

stargazer(m1, m2, type="text", align=TRUE)

m3 = lm(satisfaction ~ volunteer + married + high_educ + expgroup_treatment ,data = s171)
m4 = lm(satisfaction ~ children + married + high_educ + expgroup_treatment,data = s171)
stargazer(m3, m4, type="text", align=TRUE)

#descriptive analysis
#Descriptive stats
library(vtable)
st(s171)

#Correlation and corrplot
m = cor(s171[, unlist(lapply(s171, is.numeric))]) 
m

#Correlation matrix has a value between -1 and 1 where: -1 indicates a perfectly negative linear correlation between two variables. 0 indicates no linear correlation between two variables. 1 indicates a perfectly positive linear correlation between two variables
library(corrplot)
corrplot(m, method = 'number') 

#EDA
ggplot(s171, aes(x=age, y=satisfaction)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth()+ggtitle("age vs satisfaction")

s171$high_educ = as.factor(s171$high_educ)
ggplot(s171, aes(x=age, y=satisfaction,color=high_educ)) + 
  geom_point()+
  geom_smooth()+ggtitle("age vs satisfaction")

s171$married = as.factor(s171$married)
ggplot(s171, aes(x=grosswage, y=satisfaction,color=married)) + 
  geom_point()+
  geom_smooth()+ggtitle("grosswage vs satisfaction")

ggplot(s171, aes(x=tenure, y=satisfaction,color=married)) + 
  geom_point()+
  geom_smooth()+ggtitle("tenure vs satisfaction")

s171$men = as.factor(s171$men)
ggplot(s171, aes(x=age, y=satisfaction,color=men)) + 
  geom_point()+
  geom_smooth()+ggtitle("age vs satisfaction")

ggplot(s171, aes(x=commute, y=satisfaction)) + 
  geom_point()+
  geom_smooth()+ggtitle("commute vs satisfaction")

ggplot(data=s171, aes(x=volunteer, y=satisfaction,color = volunteer)) +
  geom_boxplot()

ggplot(data=s171, aes(x=bedroom, y=satisfaction,color = bedroom)) +
  geom_boxplot()

ggplot(data=s171, aes(x=men, y=satisfaction,color = men)) +
  geom_boxplot()

ggplot(data=s171, aes(x=high_educ, y=satisfaction, fill=men)) +
  geom_bar(stat="identity")

s171$children <- ifelse(s171$children == 0,"No children","Children")

#DATA ANALYSIS
densityplot(~ satisfaction | children, data=s171, layout=c(1,2),
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red") 
              panel.abline(v=mean(x), col.line="green")
            })

s171$volunteer <- ifelse(s171$volunteer == 0,"No volunteer","volunteer")
densityplot(~ satisfaction | volunteer, data=s171, layout=c(1,2),
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red") 
              panel.abline(v=mean(x), col.line="green")
            })

#### run regressions

s171$marriage <- factor(s171$married)
s171$children <- factor(s171$children)

m0<-lm(data=s171,satisfaction~ expgroup_treatment)
m1<-lm(data=s171,satisfaction~ children)
m2<-lm(data=s171,satisfaction~ children+married)

stargazer(m0, m1, m2, type="text", align=TRUE, out="Rsummary.txt")

#### Is satisfaction different for men and women?
m3<-lm(data=s171,satisfaction~ expgroup_treatment+men+surveynoF+personid )
m4<-lm(data=s171,satisfaction~ expgroup_treatment*men+surveynoF+personid  )

stargazer(m2, m3, m4, type="text", align=TRUE,omit=c("personid"))

#### Making the data shorter to work out when the main effect arises
m2<-lm(data=subset(s171,surveyno<4),satisfaction~ expgroup_treatment+surveynoF+personid)
m4<-lm(data=subset(s171,surveyno<4),satisfaction~ expgroup_treatment*men+surveynoF+personid)

stargazer(m2, m4, type="text", align=TRUE,omit=c("personid"))

m2<-lm(data=subset(s171,surveyno<3),satisfaction~ expgroup_treatment+surveynoF+personid)
m4<-lm(data=subset(s171,surveyno<3),satisfaction~ expgroup_treatment*men+surveynoF+personid)

stargazer(m2, m4, type="text", align=TRUE,omit=c("personid"))

# Same for all measures of satisfaction?

m4<-lm(data=s171,satisfaction~ men)
m5<-lm(data=s171,satisfaction~ commute)
m6<-lm(data=s171,satisfaction~ bedroom)
m7<-lm(data=s171,satisfaction~ general)
m8<-lm(data=s171,satisfaction~ life)
m9<-lm(data=s171,satisfaction~ personid)
m10<-lm(data=s171,satisfaction~ age)
m11<-lm(data=s171,satisfaction~ tenure)
m12<-lm(data=s171,satisfaction~ grosswage)
m13<-lm(data=s171,satisfaction~ volunteer)


stargazer(m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, type="text",
          align=FALSE, out="Dependent.txt")












