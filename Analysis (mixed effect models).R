## Title: The Association between Primary Care Quality and Diseases Burden
## DATA SETS: OECD Statistics (hospital rate of NCD),  Global Burden of Disease (DALYs)
## Methods: GEE, fixed effects model, random effects model, sensitivity analysis
## PROGRAMMER: Gloria Xiang 
## Date: July 20, 2022 (last modified)


library(readr)
library(readxl)
library(reshape2)
library(geepack)
library(MESS)
library(gee)

#################### Data process ##############################
asthma <- read_csv("~/Desktop/asthma_all.CSV", col_types = cols(quality = col_number(), 
                  YLL = col_number(),
                 DALY = col_number(), share_GDP = col_number(), share_ppp = col_number(), 
                 govern_exp = col_number(),  insurance = col_number(), oop = col_number(), 
                  influenza = col_number(), DTP = col_number(), measles = col_number(), 
                 nurse_d = col_number(), physician_D = col_number(), hospital_bed = col_number(), 
                 poverty = col_number(),obese = col_number(), smoking = col_number(), alcohol = col_number()))


## hospital admission group by yearly average (high/low)
#asthma
q_media<-asthma %>% 
  group_by(year) %>% 
  summarize(median= median(quality,na.rm=TRUE))%>%
  ungroup() 

quality2017<-na.omit(subset(asthma,year==2017)$quality)
distribution<-c()
for (ii in 1:22) {
  if(quality2017[ii]<=30.50){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(quality2017,distribution)

## !!!!!!!!!!! Reminder: change data file for different disease!!!!!!!!!!!
#COPD
q_media<-COPD %>% 
  group_by(year) %>% 
  summarize(median= median(quality,na.rm=TRUE))%>%
  ungroup() 

quality2017<-na.omit(subset(COPD,year==2017)$quality)
distribution<-c()
for (ii in 1:21) {
  if(quality2017[ii]<=171.7){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(quality2017,distribution)

#heart disease
q_media<-heart %>% 
  group_by(year) %>% 
  summarize(median= median(quality,na.rm=TRUE))%>%
  ungroup() 

quality2017<-na.omit(subset(heart,year==2017)$quality)
distribution<-c()
for (ii in 1:19) {
  if(quality2017[ii]<=169.50){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(quality2017,distribution)
which(distribution==1)


#DM
q_media<-DM %>% 
  group_by(year) %>% 
  summarize(median= median(quality,na.rm=TRUE))%>%
  ungroup() 

quality2017<-na.omit(subset(DM,year==2017)$quality)
distribution<-c()
for (ii in 1:18) {
  if(quality2017[ii]<=100.15){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(quality2017,distribution)
which(distribution==1)

#hypertension
q_media<-ht %>% 
  group_by(year) %>% 
  summarize(median= median(quality,na.rm=TRUE))%>%
  ungroup() 

quality2017<-na.omit(subset(ht,year==2017)$quality)
distribution<-c()
for (ii in 1:19) {
  if(quality2017[ii]<=45.4){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(quality2017,distribution)
which(distribution==1)


## disease burden group by yearly average (high/low)
#asthma
df_mean<-asthma %>% 
  group_by(year) %>% 
  summarize(average = mean(burden, na.rm=TRUE))%>%
  ungroup() 

burden2017<-na.omit(subset(asthma,year==2017)$burden)
distribution<-c()
for (ii in 1:24) {
  if(burden2017[ii]<=203.1198){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(burden2017,distribution)
which(distribution==1)

#COPD
df_media<-COPD %>% 
  group_by(year) %>% 
  summarize(median= median(burden,na.rm=TRUE))%>%
  ungroup() 

burden2017<-na.omit(subset(COPD,year==2017)$burden)
distribution<-c()
for (ii in 1:23) {
  if(burden2017[ii]<=430.1164){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(burden2017,distribution)
which(distribution==1)


#heart disease
df_media<-heart %>% 
  group_by(year) %>% 
  summarize(median= median(burden,na.rm=TRUE))%>%
  ungroup() 

burden2017<-na.omit(subset(heart,year==2017)$burden)
distribution<-c()
for (ii in 1:21) {
  if(burden2017[ii]<=1184.974){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(burden2017,distribution)
which(distribution==1)

#DM
df_media<-DM %>% 
  group_by(year) %>% 
  summarize(median= median(burden,na.rm=TRUE))%>%
  ungroup() 

burden2017<-na.omit(subset(DM,year==2017)$burden)
distribution<-c()
for (ii in 1:20) {
  if(burden2017[ii]<=565.5072){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(burden2017,distribution)
which(distribution==1)

#hypertension 
df_media<-ht %>% 
  group_by(year) %>% 
  summarize(median= median(burden,na.rm=TRUE))%>%
  ungroup() 

burden2017<-na.omit(subset(ht,year==2017)$burden)
distribution<-c()
for (ii in 1:21) {
  if(burden2017[ii]<=43.27382){
    0-> distribution[ii]
  }
  else{
    1-> distribution[ii]
  }
}
cbind(burden2017,distribution)
which(distribution==1)

#################### Visualization: boxplot ##############################
# boxplot: hospital admission,
ggplot(asthma_all, aes(x = as.factor(year), y = YLL)) + geom_boxplot() + # boxplot
  xlab("Year") +   
  ylab("Hypertension YLL") + 
  geom_dotplot(binaxis = 'y', stackdir ='center',method = "dotdensity",
               stackratio = 1,dotsize = 0.2)  +  
  geom_point(data = df_mean,mapping = aes(x =as.factor(year), y = average), color="red") + #mean value
  geom_line(data = df_mean, mapping = aes(x = as.factor(year), y = average, group=1)) #mean line connect



#################### Statistical analysis ##############################
#GEE model ####  all country, one indicator   ####  #### 
data_exch <-geeglm(data = asthma, burden ~ quality, id =id,family = gaussian(link = "identity"),
                   corstr = "exchangeable")
summary(data_exch)

data_exch = geeglm(data = asthma, burden ~ quality+share_GDP+share_ppp+govern_exp+volunteer_exp+
                     insurance+oop, id =id,family = gaussian(link = "identity"),corstr = "exchangeable")
summary(data_exch)
QIC(data_exch)[1]

data_exch2 = geeglm(data = asthma, burden ~ quality+nurse_d+physician_D+hospital_bed,
                    id =id,family = gaussian(link = "identity"),corstr = "exchangeable")
summary(data_exch2)

data_exch3 = geeglm(data = asthma, burden ~ quality+life_ex+influenza+DTP+measles+poverty,
                    id =id,family = gaussian(link = "identity"),corstr = "exchangeable")
summary(data_exch3)


#####fixed effects model###
library(plm)
library(readxl)
library(reshape2)
library(MESS)

## model selection
###pool test significnat
#H0：the same coefficients apply to each individual，reject H0: the coefficients are varying in different countries
pooltest(DALY ~ quality, data=asthma, index = c("id"),model="within")

pooling<-plm(DALY ~ quality, data=asthma, index = c("id"), model='pooling') #Tests for individual effect,OLS
summary(pooling)

pooling<-plm(YLL ~ quality, data=asthma, index = c("id"),model='pooling') #Tests for individual effect,OLS
summary(pooling)

# Breusch-Godfrey LM test 
plmtest(pooling) #significant effects, do not trust pooling model

FXM<-plm(burden ~ quality, data=asthma, model='within') #fixed effects model
random <- plm(burden ~ quality, data=asthma, model="random")
phtest(FXM, random) #p=0.3924

summary(FXM)
summary(random)
## use fixed effects model over others

coeftest(FXM, vcov. = vcovHC, type = "HC1")  
coeftest(random, vcov. = vcovHC, type = "HC1")

## binery group by healthcare quality：
#model 1: fixed effects model
FXM<-plm(burden ~ quality+share_GDP+share_ppp+govern_exp+volunteer_exp+
           insurance+oop, data=asthma, model='within') 

random <- plm(burden ~ quality+share_GDP+share_ppp+govern_exp+volunteer_exp+
                insurance+oop, data=asthma, model="random")
phtest(FXM, random) 

summary(FXM)
summary(random)

####partial variables 1:
FXM<-plm(YLL ~ quality+share_GDP+insurance, data=asthma, model='within') 
summary(FXM)


#model2:
FXM<-plm(burden ~ quality+nurse_d+physician_D+hospital_bed, data=asthma, model='within') 
#random effects model
random <- plm(burden ~ quality+nurse_d+physician_D+hospital_bed, data=asthma, model="random")
phtest(FXM, random) 

summary(FXM)
summary(random)

####partial variables 2:
FXM<-plm(burden ~ quality+hospital_bed, data=asthma, model='within') 
summary(FXM)


#model3:
FXM<-plm(burden ~ quality+influenza+DTP+measles+poverty, data=asthma, model='within') 
#random effects model
random <- plm(burden ~ quality+influenza+DTP+measles+poverty, data=asthma, model="random")
phtest(FXM, random) 

summary(FXM)
summary(random)

####partial variables 3:
FXM<-plm(burden ~ quality+life_ex+DTP, data=asthma, model='within') 
summary(FXM)


#mixed effects model
library(lme4)
quality_mixed = lmer(burden ~ quality + (1 | id), data = asthma)
summary(quality_mixed)
confint(quality_mixed)
plot(quality_mixed) 
#Estimates of the random effects for the first 20 ids(intercept + random effect)
ranef(quality_mixed)$id %>% head(20)
#Estimates of the random coefficient for the first 20 ids(intercept + random effect)
coef(quality_mixed)$id %>% head(20)
#prediction
predict(quality_mixed, re.form=NA) %>% head(20)
#predict_with_re = predict(quality_mixed)


#################Repeat for 5 diseases, respectively#################
#################Compare partial models, choose the best model for each disease################

## Sensitivity analysis: 
## Use sampled data for sensitivity analysis, repeat process above. The results are robust 

#Final model for five NCDs:
#asthma
FXM <- plm(DALY ~ quality+share_GDP+share_ppp+govern_exp+nurse_d+measles+obese+smoking+alcohol,  
           data = asthma,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(DALY ~ quality+share_GDP+share_ppp+govern_exp+nurse_d+measles,  
           data = asthma,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL~ quality+share_ppp+govern_exp+insurance+oop+physician_D+hospital_bed+
             obese+smoking+alcohol,  
           data = asthma,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL~ quality+share_ppp+govern_exp+insurance+oop+physician_D+hospital_bed,  
           data = asthma,index = c("id"), model = "within")
summary(FXM)

#COPD:
FXM <- plm(YLL ~ quality+share_ppp+insurance+nurse_d+physician_D+DTP+obese+smoking+alcohol,  
           data = COPD,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL ~ quality+share_ppp+insurance+nurse_d+physician_D+DTP,  
           data = COPD,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(DALY~ quality+share_GDP+share_ppp+nurse_d+physician_D+obese+smoking+alcohol,  
           data = COPD,index = c("id"), model = "within")
summary(FXM)
FXM <- plm(DALY~ quality+share_GDP+share_ppp,  
           data = COPD,index = c("id"), model = "within")
summary(FXM)

#heart disease:
FXM <- plm(DALY~ quality+share_GDP+share_ppp+insurance+oop+nurse_d+physician_D+hospital_bed+
             influenza+DTP+measles+obese+smoking+alcohol,  
           data = heart,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(DALY~ quality+share_GDP+share_ppp+insurance+oop+nurse_d+physician_D+hospital_bed+
             influenza+DTP+measles,  
           data = heart,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL~ quality+share_GDP+share_ppp+insurance+oop+nurse_d+physician_D+hospital_bed+
             influenza+DTP+measles+obese+smoking+alcohol,  
           data = heart,index = c("id"), model = "within")
summary(FXM)

#DM:
FXM <- plm(DALY~ quality+share_GDP+hospital_bed+DTP+measles+obese+smoking+alcohol,  
           data = DM,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL ~ quality+share_ppp+nurse_d+hospital_bed,  
           data = DM,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL ~ quality+share_ppp+nurse_d+hospital_bed+DTP+poverty+obese+smoking+alcohol,  
           data = DM,index = c("id"), model = "within")
summary(FXM)

#hypertension:
FXM <- plm(DALY~ quality+nurse_d+DTP+measles+obese+smoking+alcohol,  
           data = ht,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL~ quality+govern_exp+nurse_d+DTP+measles,  
           data = ht,index = c("id"), model = "within")
summary(FXM)

FXM <- plm(YLL~ quality+govern_exp+nurse_d+DTP+measles+obese+smoking+alcohol,  
           data = ht,index = c("id"), model = "within")
summary(FXM)



