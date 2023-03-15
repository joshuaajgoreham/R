## Chad Williams Get R Done tutorial
# https://www.youtube.com/watch?v=pJOhYPBYn5U&list=PLmNgrNF3pZqg2GPmcIAWDa1A2-cAfbX37&index=30


#### Episode 30 - Linear Mixed Effect Model ####
#clear GE
rm(list = ls()) 
#create data set to use
reg_data= data.frame(
  Scores=c(rnorm(50,200,20),rnorm(50,300,20)),
  Condition=c(rep(-1,50),rep(1,50)),
  Subject=1:50
)

#convert to factors
reg_data[,2]=as.factor(reg_data[,2])
reg_data[,3]=as.factor(reg_data[,3])

install.packages("lme4")
library(lme4)

install.packages("lmerTest")
library(lmerTest)

reg_results=lmer(Scores~Condition+(1|Subject),data=reg_data)
reg_results
anova(reg_results)

#F(1,49)=636.4, p<0.0001
#### Episode 31 - Linear Mixed Effect Model/Multilevel Model (RM Multiple Regression) ####
#clear GE
rm(list = ls()) 
#create data set to use
reg_data= data.frame(
  Scores=c(rnorm(50,100,20),rnorm(50,200,20),rnorm(50,500,20),rnorm(50,250,20)),
  Factor1=c(rep(-1,100),rep(1,100)), #factors are conditions
  Factor2=rep(c(rep(-1,50),rep(1,50)),2), #factors are conditions
  Subject=1:50
)

#convert to factors
reg_data[,2]=as.factor(reg_data[,2])
reg_data[,3]=as.factor(reg_data[,3])
reg_data[,4]=as.factor(reg_data[,4])

library(lme4)
library(lmerTest) #gives us p-values too

#Two ways to do the anova (first one writes the interaction effect out, second one assumes main effects with interactions)
reg_results=lmer(Scores~Factor1+Factor2+Factor1*Factor2+(1|Subject),data=reg_data)
anova(reg_results)

reg_results=lmer(Scores~Factor1*Factor2+(1|Subject),data=reg_data)
anova(reg_results)



#### Episode 32 - Hierarchical Regression (Multiple Regression & Mixed Effect Models) ####
# Goal: comparing different regression models

#clear GE
rm(list = ls()) 
#create data set to use
reg_data= data.frame(
  Scores=c(rnorm(50,10,2),rnorm(50,15,2),rnorm(50,12,2),rnorm(50,8,2)),
  Factor1=sort(rep(c(-1,1),100)),
  Factor2=rep(sort(rep(c(-1,1),50)),2),
  Subject=1:50
)
##Multiple Regression
#Simple Model
simple_model=lm(Scores~Factor1+Factor2,data=reg_data)
anova(simple_model)

#Complex Model
complex_model=lm(Scores~Factor1*Factor2,data=reg_data)
anova(complex_model)

#compare the two models
anova(simple_model,complex_model)

#F(1,196)=198.33, p<0.0001... this means the complex model is different than the simple model... so we should use the complex model

##Linear Mixed Effect Model

library(lme4)
library(lmerTest)

simple_lme=lmer(Scores~Factor1+Factor2+(1|Subject),data=reg_data)
anova(simple_lme)

complex_lme=lmer(Scores~Factor1*Factor2+(1|Subject),data=reg_data)
anova(complex_lme)

#compare the two models
anova(simple_lme,complex_lme)

#X2(Chi^2)(1,n=50)=139.9, p,0.0001...this means complex lme is different than simple lme


#### Episode 33 - Linear Mixed Effect Model with a Random Intercept and Slope ####

#clear GE
rm(list = ls()) 
#create data set to use
reg_data= data.frame(
  Scores=c(rnorm(50,100,20),rnorm(50,150,20),rnorm(50,170,20)),
  Conditions=sort(rep(c(-1,0,1),50)),
  Subject=1:50
)

library(lme4)
library(lmerTest)

lme_results=lmer(Scores~Conditions+(Conditions|Subject),data=reg_data) #added random slope effects 
anova(lme_results)

#F(1,148)=352.62, p<0.0001
#### Episode 34 - Bar Plot (w/ggplot2) ####

#clear GE
rm(list = ls()) 
#create data set to use
plot_data= data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2)),
  Groups=c(rep("First",10),rep("Second",10))
)

library(ggplot2)
install.packages("Hmisc")
library(Hmisc)

ggplot(aes(x=Groups,y=Values,fill=Groups),data=plot_data)+
  stat_summary(fun=mean,geom='bar')+
  scale_y_continuous(expand = c(0,0))+ #sets the y axis limit to 0...
  theme_classic()+ #removes grey background
  theme(legend.position = 'none')

#### Episode 35 - Bar plot with 95% CI's (w/ggplot2)####

#clear GE
rm(list = ls()) 
#create data set to use
plot_data= data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2)),
  Groups=c(rep("First",10),rep("Second",10))
)

library(ggplot2)
library(Hmisc)

ggplot(aes(x=Groups,y=Values,fill=Groups),data=plot_data)+
  stat_summary(fun=mean,geom='bar')+
  stat_summary(fun.data=mean_cl_normal, geom='errorbar',width=0.5)+
  theme_classic()+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = 'none')
  
#### Episode 36 - Grouped Bar plot (w/ggplot2)####                    

#clear GE
rm(list = ls()) 
#create data set to use
plot_data= data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2),rnorm(10,12,2),rnorm(10,6,2)),
  Groups=c(rep("First",20),rep("Second",20)),
  Conditions=c(rep("Early",10),rep("Late",10),rep("Early",10),rep("Late",10))
)

library(ggplot2)
library(Hmisc)

ggplot(aes(x=Groups,y=Values,group=Conditions,fill=Conditions),data=plot_data)+
  geom_bar(position='dodge',stat='identity')+
  theme_classic()+ #removes grey background
  scale_y_continuous(expand=c(0,0)) #sets ylim to 0

#### Episode 37 - Grouped Bar plot with 95% CI's (w/ggplot2)#### 
#clear GE
rm(list = ls()) 
#create data set to use
plot_data= data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2),rnorm(10,12,2),rnorm(10,6,2)),
  Groups=c(rep("First",20),rep("Second",20)),
  Conditions=c(rep("Early",10),rep("Late",10),rep("Early",10),rep("Late",10))
)

library(ggplot2)
library(Hmisc)

ggplot(aes(x=Groups,y=Values,group=Conditions,fill=Conditions),data=plot_data)+
  stat_summary(fun=mean, geom='bar',position=position_dodge())+
  stat_summary(fun.data=mean_cl_normal,geom='errorbar',width=0.5, position = position_dodge(0.9))+
  theme_classic()+ #removes grey background
  scale_y_continuous(expand=c(0,0)) #sets ylim to 0

#### Episode 38 - Effects Plot (w/ggplot2) ####
#clear GE
rm(list = ls()) 
#create data set to use
plot_data= data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2),rnorm(10,12,2),rnorm(10,6,2)),
  Groups=c(rep("First",20),rep("Second",20)),
  Conditions=c(rep("Early",10),rep("Late",10),rep("Early",10),rep("Late",10))
)

library(ggplot2)
library(Hmisc)

ggplot(aes(x=Groups,y=Values,group=Conditions,color=Conditions),data=plot_data)+
  stat_summary(fun=mean,geom='point')+
  stat_summary(fun=mean,geom='line')+
  theme_classic()

#### Episode 39 - Effects Plot with 95% CI's (w/ggplot2) ####

#clear GE
rm(list = ls()) 
#create data set to use
plot_data= data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2),rnorm(10,12,2),rnorm(10,6,2)),
  Groups=c(rep("First",20),rep("Second",20)),
  Conditions=c(rep("Early",10),rep("Late",10),rep("Early",10),rep("Late",10))
)

library(ggplot2)
library(Hmisc)

ggplot(aes(x=Groups,y=Values,group=Conditions,color=Conditions),data=plot_data)+
  stat_summary(fun=mean,geom='point')+
  stat_summary(fun=mean,geom='line')+
  stat_summary(fun.data = mean_cl_normal,geom='errorbar',width=0.2)+
  theme_classic()


#### Episode 40 - Scatterplot with Regression Line (w/ggplot2) ####

#clear GE
rm(list = ls()) 

plot_data=data.frame(
  Values_x=rnorm(100,10,2),
  Values_y=rnorm(100,10,2)
)

library(ggplot2)

ggplot(aes(x=Values_x,y=Values_y),data=plot_data)+ #creates the plot, but plots nothing
  geom_point()+ #includes the markers
  geom_smooth(method=lm)+ #includes a line... include "se=FALSE" if you don't want error
  theme_classic()


#### Episode 41 - Line Plot (w/ggplot2) ####

#clear GE
rm(list = ls()) 

plot_data=data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2),rnorm(10,18,2),rnorm(10,8,2),rnorm(10,2,2)),
  Time=sort(rep(1:5,10))
)

library(ggplot2)
library(Hmisc)

ggplot(aes(x=Time,y=Values),data=plot_data)+
  stat_summary(fun=mean,geom='line')+
  theme_classic()
  
#### Episode 42 - Line Plot with 95% CI's (w/ggplot2) ####

#clear GE
rm(list = ls()) 

plot_data=data.frame(
  Values=c(rnorm(10,10,2),rnorm(10,15,2),rnorm(10,18,2),rnorm(10,8,2),rnorm(10,2,2)),
  Time=sort(rep(1:5,10))
)

library(ggplot2)
library(Hmisc)

ggplot(aes(x=Time,y=Values),data=plot_data)+
  stat_summary(fun=mean,geom='line')+ #includes lines
  stat_summary(fun=mean,geom='point')+ #includes markers in the plot
  stat_summary(fun.data = mean_cl_normal,geom = 'errorbar',width=0.3)+ #includes errorbars
  theme_classic() #removes grey background