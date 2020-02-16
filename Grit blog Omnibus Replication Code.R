#Grit blog replication code
library(foreign)
library(haven)
library(Matching)
library(rgenoud)
library(survey)
library(psych)
library(haven)
library(descr)
library(ggeffects)
setwd("d:/dustin/Desktop/ceu stats folder")
ani<-read_dta("Omnibus_2019_Dec_03.02.20.dta")

names(ani)

freq(as_factor(ani$so9_1), w= ani$indwt)
freq(ani$so9_2, w= ani$indwt)
freq(ani$so9_3, w= ani$indwt)
freq(ani$so9_4, w= ani$indwt)
freq(ani$so9_5, w= ani$indwt)
freq(ani$so9_6, w= ani$indwt)
freq(ani$so9_7, w= ani$indwt)
freq(ani$so9_8, w= ani$indwt)
freq(ani$so9_9, w= ani$indwt)
freq(ani$so9_10, w= ani$indwt)
freq(ani$so9_11, w= ani$indwt)
freq(ani$so9_12, w= ani$indwt)
table(ani$sex)
table(ani$age)
ani$age_r<-ani$age
ani$age_r[ani$age_r<=35]<-1
ani$age_r[ani$age_r>=56]<-3
ani$age_r[ani$age_r>=10]<-2
ani$age_r<-as_factor(ani$age_r)
table(as_factor(ani$d1))
table(as_factor(ani$d2))
ani$d2_r<-ani$d2
ani$d2_r[ani$d2_r==2]<-1
ani$d2_r[ani$d2_r==3]<-2
ani$d2_r[ani$d2_r==4]<-3
ani$d2_r[ani$d2_r==5]<-3
ani$d2_r_r<-ani$d2_r
ani$d2_r_r[ani$d2_r_r==1]<-0
ani$d2_r_r[ani$d2_r_r==2]<-0
ani$d2_r_r[ani$d2_r_r==3]<-1
ani$d2_r<-as_factor(ani$d2_r)
ani$d2_r_r<-as_factor(ani$d2_r_r)

table(as_factor(ani$d3))
ani$d3_r<-ani$d3
ani$d3_r[ani$d3_r<=-1]<-NA
freq(as_factor(ani$d5))
ani$d5_r<-ani$d5
ani$d5_r<-as_factor(ani$d5_r)
freq(as_factor(ani$stratum))
ani$stratum<-as_factor(ani$stratum)
freq(as_factor(ani$d1))
freq(ani$d1)
ani$d1_r<-ani$d1
ani$d1_r[ani$d1_r<=3]<-1
ani$d1_r[ani$d1_r>=4]<-0

#reverse code variables

#2R Due to new projects/ideas I often don't pay attention to the old ones 
#3R My interests change from year to year 
#5R I had been interested with an idea, but later I lost interest. 
#7R I often set goals, but later prefer to follow other goals
#8R It's hard to me being focused on projects that take more than several months to 
#11R I have new interests once in several months 
ani[ani==-1]<-NA
ani[ani==-2]<-NA
ani$so9_2_r<-ani$so9_2
ani$so9_2_r<-(ani$so9_2_r*10)
ani$so9_2_r[ani$so9_2_r==10]<-5
ani$so9_2_r[ani$so9_2_r==20]<-4
ani$so9_2_r[ani$so9_2_r==30]<-3
ani$so9_2_r[ani$so9_2_r==40]<-2
ani$so9_2_r[ani$so9_2_r==50]<-1
ani$so9_3_r<-ani$so9_3
ani$so9_3_r<-(ani$so9_3_r*10)
ani$so9_3_r[ani$so9_3_r==10]<-5
ani$so9_3_r[ani$so9_3_r==20]<-4
ani$so9_3_r[ani$so9_3_r==30]<-3
ani$so9_3_r[ani$so9_3_r==40]<-2
ani$so9_3_r[ani$so9_3_r==50]<-1
ani$so9_5_r<-ani$so9_5
ani$so9_5_r<-(ani$so9_5_r*10)
ani$so9_5_r[ani$so9_5_r==10]<-5
ani$so9_5_r[ani$so9_5_r==20]<-4
ani$so9_5_r[ani$so9_5_r==30]<-3
ani$so9_5_r[ani$so9_5_r==40]<-2
ani$so9_5_r[ani$so9_5_r==50]<-1
ani$so9_7_r<-ani$so9_7
ani$so9_7_r<-(ani$so9_7_r*10)
ani$so9_7_r[ani$so9_7_r==10]<-5
ani$so9_7_r[ani$so9_7_r==20]<-4
ani$so9_7_r[ani$so9_7_r==30]<-3
ani$so9_7_r[ani$so9_7_r==40]<-2
ani$so9_7_r[ani$so9_7_r==50]<-1
ani$so9_8_r<-ani$so9_8
ani$so9_8_r<-(ani$so9_8_r*10)
ani$so9_8_r[ani$so9_8_r==10]<-5
ani$so9_8_r[ani$so9_8_r==20]<-4
ani$so9_8_r[ani$so9_8_r==30]<-3
ani$so9_8_r[ani$so9_8_r==40]<-2
ani$so9_8_r[ani$so9_8_r==50]<-1

ani$so9_11_r<-ani$so9_11
ani$so9_11_r<-(ani$so9_11_r*10)
ani$so9_11_r[ani$so9_11_r==10]<-5
ani$so9_11_r[ani$so9_11_r==20]<-4
ani$so9_11_r[ani$so9_11_r==30]<-3
ani$so9_11_r[ani$so9_11_r==40]<-2
ani$so9_11_r[ani$so9_11_r==50]<-1



ani$grit<-(ani$so9_1+
    ani$so9_2_r+
    ani$so9_3_r+
    ani$so9_4+
    ani$so9_5_r+
    ani$so9_6+
    ani$so9_7_r+
    ani$so9_8_r+
    ani$so9_9+
    ani$so9_10+
    ani$so9_11_r+
    ani$so9_12)
mean(ani$grit, na.rm = TRUE)
anisvy<-svydesign(id=~psu,weights=~indwt, strat=~substratum, data=ani)
anisvy$variables$grit<-(anisvy$variables$grit/12)

svymean(anisvy$variables$grit, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_1, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_2_r, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_3_r, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_4, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_5_r, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_6, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_7_r, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_8_r, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_9, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_10, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_11_r, design = anisvy, na.rm = TRUE)
svymean(anisvy$variables$so9_12, design = anisvy, na.rm = TRUE)
table(as_factor(ani$d1))
model1<-svyglm(grit~sex+age_r+stratum+d5_r, design =anisvy)
summary(model1)

plot(ggemmeans(model1, terms = ("stratum"), data = anisvy$variables))
plot(ggemmeans(model1, terms = ("d5_r"), data = anisvy$variables))


freq(as_factor(ani$d1), w = ani$indwt)
freq(ani$d1, w = ani$indwt)
anisvy$variables$d1_r<-anisvy$variables$d1
anisvy$variables$d1_r[anisvy$variables$d1_r==6]<-5
anisvy$variables$d1_r[anisvy$variables$d1_r!=5]<-0
anisvy$variables$d1_r[anisvy$variables$d1_r==5]<-1
freq(anisvy$variables$d1_r, w = ani$indwt)

model2<-svyglm(d1_r~grit+sex+age+stratum+d5_r, design =anisvy, family = "binomial")
summary(model2)

plot(ggemmeans(model2, terms = ("grit[1, 2,3,4,5]"), data = anisvy$variables))

model3<-svyglm(d3_r~grit+sex+age+stratum+d5_r, design =anisvy)
summary(model3)

plot(ggemmeans(model3, terms = ("grit[1, 2,3,4,5]"), data = anisvy$variables))

table(anisvy$variables$d2_r_r)
anisvy$variables$d2_r_r<-as.numeric(anisvy$variables$d2_r_r)
anisvy$variables$d2_r_r<-(anisvy$variables$d2_r_r-6)
model4<-svyglm(d2_r_r~grit+sex+age+stratum+d5_r, design =anisvy)
summary(model4)

plot(ggemmeans(model4, terms = ("grit[1, 2,3,4,5]"), data = anisvy$variables))

model5<-svyglm(d6~grit+sex+age+stratum+d5_r, design =anisvy)
summary(model5)

plot(ggemmeans(model5, terms = ("grit[1, 2,3,4,5]"), data = anisvy$variables))
