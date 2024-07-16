library(dplyr)
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)

setwd('E:\\ResearchProject\\Sorowar Sir\\Breast Cancer')
BrData <- read.csv("BreastCancerDiagnost_DATA_Raw data.csv")

library(pastecs)
stat.desc(BrData$PtD_months)
stat.desc(BrData$PrD_months)
stat.desc(BrData$TD_months)

#1.	Form Number / Identification Number
BrData$Record_ID <- factor(BrData$Record.ID)

# BrData$Survey_Identifier <- BrData$Survey.Identifier #all missing
# BrData$X  #not important
# BrData$Form.no...Patient.ID #not important

#2.	Date of Interview (today’s date)
BrData$Date.of.interview <- BrData$Date_of_interview

#3.	Patient’s Name
BrData$Patients_Name <- factor(BrData$X1..Patient.s.Name)

#4.	Contact: XX, XX
BrData$Contact_Telephone <- factor(BrData$X2..Contact.Telephone)

#9.	Age in years (whole year)
BrData$Age <- BrData$X3..Age..in.whole.year.

BrData$AgeCat[BrData$Age < 40]  = 1
BrData$AgeCat[BrData$Age >= 40 & BrData$Age <=  49]  = 2
BrData$AgeCat[BrData$Age >= 50 & BrData$Age <= 59] = 3
BrData$AgeCat[BrData$Age >= 60] = 4

BrData$AgeCat <- factor(BrData$AgeCat,levels=c(1,2,3,4),labels = c('<40','40-49','50-59','>=60'))
BrData$AgeCat

summary(BrData$AgeCat)
x <- table(BrData$AgeCat)
x
round(prop.table(x),4)*100

describe.by(BrData$PtD_weeks, BrData$AgeCat)

#Division
BrData$Division <- factor(BrData$Division)

BrData$Division_cat <- factor(BrData$Division,levels=c("Barisal","Chittagong","Dhaka","Khulna","Mymensingh","Rajshahi","Rangpur","Sylhet"),
                        labels = c("Barisal","Chittagong","Dhaka","Khulna","Mymensingh","Rajshahi","Rangpur","Sylhet"))
BrData$Division_cat

summary(BrData$Division_cat)
x <- table(BrData$Division_cat)
x
round(prop.table(x),4)*100

describe.by(BrData$PtD_weeks, BrData$Division_cat)

#6.	Current place of residence (Home District)
# BrData$Home_district <- factor(BrData$X4..Home.District..permanent.residence.)
# 
# summary(BrData$Home_district)
# x <- table(BrData$Home_district)
# x
# round(prop.table(x),4)*100

#5.	Current place of residence (Rural/Urban)
BrData$residence <- factor(BrData$X5..Location.of.of.residence)
summary(BrData$residence)
BrData$residence <- factor(BrData$X5..Location.of.of.residence,levels=c("Rural","Urban"),labels = c('Rural','Urban'))
summary(BrData$residence)

x <- table(BrData$residence)
x
round(prop.table(x),4)*100

describeBy(BrData$PtD_weeks, BrData$residence)

#5.	Current place of residence (Current District)
BrData$Current_eistrict <- factor(BrData$X6..Current.place.of.residence..district.)

#7.	Education (highest level completed) (primary, secondary, higher secondary, university)
BrData$Patients_education <- factor(BrData$X1..Education.completed)
summary(BrData$Patients_education)
BrData$Patients_education <- factor(BrData$Patients_education,levels=c("Graduate","Higher secondary (College)","Illiterate","Primary","Secondary"),labels = c("Secondary/higher","Secondary/higher","Illiterate","Primary","Secondary/higher"))
summary(BrData$Patients_education)

x <- table(BrData$Patients_education)
x
round(prop.table(x),4)*100

describeBy(BrData$PtD_weeks, BrData$residence)

#11.	Marital status (single/widowed/never married/ married)
BrData$Marital_status <- factor(BrData$X2..Marital.status)
summary(BrData$Marital_status)
BrData$Marital_status <- factor(BrData$Marital_status,levels=c("Married","Never married","Single","Widowed"),labels = c("Married","Single","Single","Single"))
summary(BrData$Marital_status)

x <- table(BrData$Marital_status)
x
round(prop.table(x),4)*100

#8.	Husband’s education (Primary, secondary, higher secondary, university)
BrData$Husbands_education <- factor(BrData$X3..Husband.s.education..completed.)
summary(BrData$Husbands_education)
BrData$Husbands_education <- factor(BrData$Husbands_education,levels=c("Graduate","Higher secondary (College)","Illiterate","Primary","Secondary"),labels = c("Secondary/higher","Secondary/higher","Illiterate","Primary","Secondary/higher"))
summary(BrData$Husbands_education)

x <- table(BrData$Husbands_education)
x
round(prop.table(x),4)*100

#Monthly.family.income.in.Taka
BrData$Family_income <- factor(BrData$X4..Monthly.family.income.in.Taka)
summary(BrData$Family_income)
BrData$Family_income <- factor(BrData$Family_income,levels=c("<5,000","10,000","20,000","Others"),labels = c("<5,000","10,000","20,000",">20,000"))
summary(BrData$Family_income)

x <- table(BrData$Family_income)
x
round(prop.table(x),4)*100

#Monthly.income.Others
BrData$Others_income <- factor(BrData$Monthly.income.Others)

summary(BrData$PortableElectronicDevices)
BrData$PortableElectronicDevices <- factor(BrData$PortableElectronicDevices,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$PortableElectronicDevices)

x <- table(BrData$PortableElectronicDevices)
x
round(prop.table(x),4)*100

summary(BrData$MassMediaAccess)
BrData$MassMediaAccess <- factor(BrData$MassMediaAccess,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$MassMediaAccess)

x <- table(BrData$MassMediaAccess)
x
round(prop.table(x),4)*100

#Social Media
BrData$social_media_access <- factor(BrData$X5..Access.to.communication.and.media..choice.Social.media.)

#Mobile
BrData$Mobile_access <- factor(BrData$X5..Access.to.communication.and.media..choice.Mobile.)

#Smartphone
BrData$smartphone_access <- factor(BrData$X5..Access.to.communication.and.media..choice.Smartphone.)

#computer
BrData$computer_access <- factor(BrData$X5..Access.to.communication.and.media..choice.Personal.Computer..PC..)

#TV
BrData$TV_access <- factor(BrData$X5..Access.to.communication.and.media..choice.TV.)

#Newspaper
BrData$Newspaper_access <- factor(BrData$X5..Access.to.communication.and.media..choice.Newspaper.)

#13.	Family history of breast cancer: yes/no
BrData$Hist_br <- factor(BrData$X6..Family.history.of.breast.cancer)
summary(BrData$Hist_br)
BrData$Hist_br <- factor(BrData$Hist_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Hist_br)

x <- table(BrData$Hist_br)
x
round(prop.table(x),4)*100

#When did you first realize that you haveproblem with your breast? 
BrData$problem_identify_br <- factor(BrData$X1.3.1.When.did.you.first.realize.that.you.problem.with.your.breast.)
# BrData$SymptomptoDay
# 
# 
# summary(BrData$symptom_Lump_br)
# x <- table(BrData$symptom_Lump_br)
# x
# round(prop.table(x),4)*100


#Lump
BrData$symptom_Lump_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Lump.)
BrData$symptom_Lump_br

summary(BrData$symptom_Lump_br)
x <- table(BrData$symptom_Lump_br)
x
round(prop.table(x),4)*100

#Skin changes
BrData$symptom_SkinChanges_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Skin.changes.)

summary(BrData$symptom_SkinChanges_br)
x <- table(BrData$symptom_SkinChanges_br)
x
round(prop.table(x),4)*100

#Breast Pain
BrData$symptom_breastPain_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Breast.pain.)

summary(BrData$symptom_breastPain_br)
x <- table(BrData$symptom_breastPain_br)
x
round(prop.table(x),4)*100

#Nipple discharge
BrData$symptom_nippleDischarge_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Nipple.discharge.)

summary(BrData$symptom_nippleDischarge_br)
x <- table(BrData$symptom_nippleDischarge_br)
x
round(prop.table(x),4)*100

#Bone Pain
BrData$symptom_BonePain_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Bone.pain.)

summary(BrData$symptom_BonePain_br)
x <- table(BrData$symptom_BonePain_br)
x
round(prop.table(x),4)*100

#Others
BrData$symptom_Others_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Other.)

summary(BrData$symptom_SkinChanges_br)
x <- table(BrData$symptom_SkinChanges_br)
x
round(prop.table(x),4)*100

#Others_name
BrData$symptom_Othersname_br <- factor(BrData$If.other..please.specify.other.symptoms.noticed)

#2.3 When you noticed symptom for the first time did you think that it could be cancer? 
BrData$symptom_cancer_br <- factor(BrData$X2.1.When.you.noticed.symptom.for.the.first.time.did.you.think.that.it.could.be.cancer.)

#2.4 Have you experienced following discomfort? 
#a.	Lump in the armpit, neck or trunk? Yes/No 
BrData$discomfort_Lump_br <- factor(BrData$Lump.in.the.armpit..neck.or.trunk.)
BrData$discomfort_Lump_br <- factor(BrData$discomfort_Lump_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_Lump_br)

summary(BrData$discomfort_Lump_br)
x <- table(BrData$discomfort_Lump_br)
x
round(prop.table(x),4)*100


#b.	Pain in breast  (Yes/No)
BrData$discomfort_breastPain_br <- factor(BrData$Pain.in.breast)
BrData$discomfort_breastPain_br <- factor(BrData$discomfort_breastPain_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_breastPain_br)

summary(BrData$discomfort_breastPain_br)
x <- table(BrData$discomfort_breastPain_br)
x
round(prop.table(x),4)*100

#c.	Pain in arm on the same side as the affected? (Yes/No)
BrData$discomfort_armPain_br <- factor(BrData$Pain.in.arm.on.the.same.side.as.the.affected.)
BrData$discomfort_armPain_br <- factor(BrData$discomfort_armPain_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_armPain_br)

summary(BrData$discomfort_armPain_br)
x <- table(BrData$discomfort_armPain_br)
x
round(prop.table(x),4)*100

#d.	Color changes in the breast skin (like red, brown or purple)? (Yes/No)
BrData$discomfort_skinColor_br <- factor(BrData$Color.changes.in.the.breast.skin..like.red..brown.or.purple..)
BrData$discomfort_skinColor_br <- factor(BrData$discomfort_skinColor_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_skinColor_br)

summary(BrData$discomfort_skinColor_br)
x <- table(BrData$discomfort_skinColor_br)
x
round(prop.table(x),4)*100

#e.	Ulcer or sore on the skin of the breast? (Yes/No)
BrData$discomfort_skinUlcer_br <- factor(BrData$Ulcer.or.sore.on.the.skin.of.the.breast.)
BrData$discomfort_skinUlcer_br <- factor(BrData$discomfort_skinUlcer_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_skinUlcer_br)

summary(BrData$discomfort_skinUlcer_br)
x <- table(BrData$discomfort_skinUlcer_br)
x
round(prop.table(x),4)*100

#f.	Itching in the breast?(Yes/No)
BrData$discomfort_Itching_br <- factor(BrData$Itching.in.the.breast.)
BrData$discomfort_Itching_br <- factor(BrData$discomfort_Itching_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_Itching_br)

summary(BrData$discomfort_Itching_br)
x <- table(BrData$discomfort_Itching_br)
x
round(prop.table(x),4)*100

#g.	Changes in breast shape?(Yes/No)
BrData$discomfort_shape_br <- factor(BrData$Changes.in.breast.shape.)
BrData$discomfort_shape_br <- factor(BrData$discomfort_shape_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_shape_br)

summary(BrData$discomfort_shape_br)
x <- table(BrData$discomfort_shape_br)
x
round(prop.table(x),4)*100

#h.	Liquid or blood came out from the nipple?(Yes/No)
BrData$discomfort_liquidBlood_br <- factor(BrData$Liquid.or.blood.came.out.from.the.nipple.)
BrData$discomfort_liquidBlood_br <- factor(BrData$discomfort_liquidBlood_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_liquidBlood_br)

summary(BrData$discomfort_liquidBlood_br)
x <- table(BrData$discomfort_liquidBlood_br)
x
round(prop.table(x),4)*100

#2.3 Once you realized your problem when did you go to doctor:________(in days) 
BrData$doctorVisit_days <- BrData$X2.3.Once.you.realized.your.problem.when.did.you.go.to.doctor.________.in.days.
BrData$PtD <- factor(BrData$PtD,levels=c("Yes","No"),labels = c("Yes","No"))

summary(BrData$PtD)
x <- table(BrData$PtD)
x
round(prop.table(x),4)*100


c <- table(BrData$AgeCat ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$AgeCat), ref = ">=60"),
              family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$Division_cat ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$Division_cat), ref = "Barisal"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$residence ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$residence), ref = "Urban"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Marital_status ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$Marital_status), ref = "Married"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Patients_education ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$Patients_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Husbands_education ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$Husbands_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Family_income ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$Family_income), ref = ">20,000"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$PortableElectronicDevices ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$PortableElectronicDevices), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$MassMediaAccess ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$MassMediaAccess), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_Lump_br ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$symptom_Lump_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$symptom_breastPain_br ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$symptom_breastPain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_nippleDischarge_br ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$symptom_nippleDischarge_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_SkinChanges_br ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$symptom_SkinChanges_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_BonePain_br ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$symptom_BonePain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$KP_checkBreast ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$KP_checkBreast), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Hist_br ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$Hist_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

#adjusted model
model <- glm(relevel(factor(BrData$PtD), ref = "No")~ relevel(factor(BrData$Patients_education), ref = "Secondary/higher")
             + relevel(factor(BrData$Family_income), ref = ">20,000")
             +relevel(factor(BrData$PortableElectronicDevices), ref = "Unchecked")
             +relevel(factor(BrData$MassMediaAccess), ref = "Unchecked")
             +relevel(factor(BrData$symptom_breastPain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

#hoslem
hoslem.test(model$y, fitted(model)) #hosmer and lemeshow goodness of fit  test

#auc value
# 
# prob <- predict(model,type="response")
# pred <- prediction(as.numeric(prob),as.numeric(model$y))
# perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
# auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
# auc

library("verification")

# For model without ties
roc.area(model$y, fitted(model))
ci.auc(model$y, fitted(model))

#roc curve

# plot(perf, main="ROC Curve ", xlab="specificity",  ylab="sensitivity")
# grid()
# abline(0,1, col="blue", lty=2)

D.ex <- model$y
M.ex <- fitted(model)
mu1 <- mean(M.ex[D.ex == 1])
mu0 <- mean(M.ex[D.ex == 0])
s1 <- sd(M.ex[D.ex == 1])
s0 <- sd(M.ex[D.ex == 0])
c.ex <- seq(min(M.ex), max(M.ex), length.out = 300)

binorm.roc <- data.frame(c = c.ex, 
                         FPF = pnorm((mu0 - c.ex)/s0), 
                         TPF = pnorm((mu1 - c.ex)/s1)
)
library(survivalROC)
library(plotROC)
binorm.plot1 <- ggplot(binorm.roc, aes(x = FPF, y = TPF, label = c)) + 
  geom_roc(stat = "identity") +  
  ggtitle("ROC Curves (Model 1)") + 
  annotate("text", x = .75, y = .50, 
           label = paste("AUC =", round(calc_auc(binorm.plot)$AUC*(-1)*100, 2),"%")) +
#  scale_x_continuous("False positive fraction (1 - Specificity)", breaks = seq(0, 1, by = .1))+
#  scale_y_continuous("True positive fraction (Sensitivity)", breaks = seq(0, 1, by = .1)) 
  style_roc(theme = theme_grey, xlab = "False positive fraction (1 - Specificity)", ylab = "True positive fraction (Sensitivity)")
binorm.plot1 <- binorm.plot1 + 
  theme(plot.title = element_text(size = 16,hjust=0.5),
                  legend.title = element_text(size=16),
                  legend.text = element_text(size=16),
                  axis.text = element_text(size = 16),
                  axis.title = element_text(size = 16))
binorm.plot1

#Provider_Delay
BrData$PrD <- factor(BrData$PrD,levels=c("Yes","No"),labels = c("Yes","No"))
BrData$PrD
summary(BrData$PrD)
x <- table(BrData$PrD)
x
round(prop.table(x),4)*100


c <- table(BrData$AgeCat ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$AgeCat), ref = ">=60"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$Division_cat ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$Division_cat), ref = "Barisal"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$residence ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$residence), ref = "Urban"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Marital_status ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$Marital_status), ref = "Married"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Patients_education ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$Patients_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Husbands_education ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$Husbands_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Family_income ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$Family_income), ref = ">20,000"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$PortableElectronicDevices ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$PortableElectronicDevices), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$MassMediaAccess ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$MassMediaAccess), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_Lump_br ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$symptom_Lump_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$symptom_breastPain_br ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$symptom_breastPain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_nippleDischarge_br ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$symptom_nippleDischarge_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_SkinChanges_br ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$symptom_SkinChanges_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_BonePain_br ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$symptom_BonePain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$KP_checkBreast ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$KP_checkBreast), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Hist_br ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$Hist_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$pathos_cancerStage ,BrData$PrD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$pathos_cancerStage), ref = "Stage I"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

#adjusted model
model <- glm(relevel(factor(BrData$PrD), ref = "No")~ relevel(factor(BrData$Division_cat), ref = "Barisal")
             + relevel(factor(BrData$residence), ref = "Urban")
             + relevel(factor(BrData$symptom_breastPain_br), ref = "Unchecked")
             + relevel(factor(BrData$symptom_nippleDischarge_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

#hoslem
hoslem.test(model$y, fitted(model)) #hosmer and lemeshow goodness of fit  test

#auc value
# 
# prob <- predict(model,type="response")
# pred <- prediction(as.numeric(prob),as.numeric(model$y))
# perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
# auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
# auc

library("verification")

# For model without ties
roc.area(model$y, fitted(model))
ci.auc(model$y, fitted(model))

# roc curve

# plot(perf, main="ROC Curve ", xlab="specificity",  ylab="sensitivity")
# grid()
# abline(0,1, col="blue", lty=2)

D.ex <- model$y
M.ex <- fitted(model)
mu1 <- mean(M.ex[D.ex == 1])
mu0 <- mean(M.ex[D.ex == 0])
s1 <- sd(M.ex[D.ex == 1])
s0 <- sd(M.ex[D.ex == 0])
c.ex <- seq(min(M.ex), max(M.ex), length.out = 300)

binorm.roc <- data.frame(c = c.ex, 
                         FPF = pnorm((mu0 - c.ex)/s0), 
                         TPF = pnorm((mu1 - c.ex)/s1)
)
library(survivalROC)
library(plotROC)
binorm.plot2 <- ggplot(binorm.roc, aes(x = FPF, y = TPF, label = c)) + 
  geom_roc(stat = "identity") +  
  ggtitle("ROC Curves (Model 2)") + 
  annotate("text", x = .75, y = .50, 
           label = paste("AUC =", round(calc_auc(binorm.plot2)$AUC*(-1)*100, 2),"%")) +
  #  scale_x_continuous("False positive fraction (1 - Specificity)", breaks = seq(0, 1, by = .1))+
  #  scale_y_continuous("True positive fraction (Sensitivity)", breaks = seq(0, 1, by = .1)) 
  style_roc(theme = theme_grey, xlab = "False positive fraction (1 - Specificity)", ylab = "True positive fraction (Sensitivity)")
binorm.plot2 <- binorm.plot2 + 
  theme(plot.title = element_text(size = 16,hjust=0.5),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))
binorm.plot2


#Total_Delay
BrData$TD <- factor(BrData$TD,levels=c("Yes","No"),labels = c("Yes","No"))
BrData$TD
summary(BrData$TD)
x <- table(BrData$TD)
x
round(prop.table(x),4)*100

c <- table(BrData$AgeCat ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$AgeCat), ref = ">=60"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$Division_cat ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$Division_cat), ref = "Barisal"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$residence ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$residence), ref = "Urban"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Marital_status ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$Marital_status), ref = "Married"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Patients_education ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$Patients_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Husbands_education ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$Husbands_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Family_income ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$Family_income), ref = ">20,000"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$PortableElectronicDevices ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$PortableElectronicDevices), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$MassMediaAccess ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$MassMediaAccess), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_Lump_br ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$symptom_Lump_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$symptom_breastPain_br ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$symptom_breastPain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_nippleDischarge_br ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$symptom_nippleDischarge_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_SkinChanges_br ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$symptom_SkinChanges_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_BonePain_br ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$symptom_BonePain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$KP_checkBreast ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$KP_checkBreast), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Hist_br ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$Hist_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))



c <- table(BrData$pathos_cancerStage ,BrData$TD)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$pathos_cancerStage), ref = "Stage I"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

#adjusted model
model <- glm(relevel(factor(BrData$TD), ref = "No")~ relevel(factor(BrData$Division_cat), ref = "Barisal")
             + relevel(factor(BrData$residence), ref = "Urban")
             + relevel(factor(BrData$Marital_status), ref = "Married")
             + relevel(factor(BrData$Patients_education), ref = "Secondary/higher")
             + relevel(factor(BrData$Family_income), ref = ">20,000")
             + relevel(factor(BrData$PortableElectronicDevices), ref = "Unchecked")
             + relevel(factor(BrData$MassMediaAccess), ref = "Unchecked")
             + relevel(factor(BrData$symptom_breastPain_br), ref = "Unchecked")
             + relevel(factor(BrData$symptom_nippleDischarge_br), ref = "Unchecked")
             + relevel(factor(BrData$symptom_BonePain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

#hoslem
hoslem.test(model$y, fitted(model)) #hosmer and lemeshow goodness of fit  test

#auc value
# 
# prob <- predict(model,type="response")
# pred <- prediction(as.numeric(prob),as.numeric(model$y))
# perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
# auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
# auc

library("verification")

# For model without ties
roc.area(model$y, fitted(model))
ci.auc(model$y, fitted(model))

#roc curve

# plot(perf, main="ROC Curve ", xlab="specificity",  ylab="sensitivity")
# grid()
# abline(0,1, col="blue", lty=2)

D.ex <- model$y
M.ex <- fitted(model)
mu1 <- mean(M.ex[D.ex == 1])
mu0 <- mean(M.ex[D.ex == 0])
s1 <- sd(M.ex[D.ex == 1])
s0 <- sd(M.ex[D.ex == 0])
c.ex <- seq(min(M.ex), max(M.ex), length.out = 300)

binorm.roc <- data.frame(c = c.ex, 
                         FPF = pnorm((mu0 - c.ex)/s0), 
                         TPF = pnorm((mu1 - c.ex)/s1)
)
library(survivalROC)
library(plotROC)
binorm.plot3 <- ggplot(binorm.roc, aes(x = FPF, y = TPF, label = c)) + 
  geom_roc(stat = "identity") +  
  ggtitle("ROC Curves (Model 3)") + 
  annotate("text", x = .75, y = .50, 
           label = paste("AUC =", round(calc_auc(binorm.plot3)$AUC*(-1)*100, 2),"%")) +
  #  scale_x_continuous("False positive fraction (1 - Specificity)", breaks = seq(0, 1, by = .1))+
  #  scale_y_continuous("True positive fraction (Sensitivity)", breaks = seq(0, 1, by = .1)) 
  style_roc(theme = theme_grey, xlab = "False positive fraction (1 - Specificity)", ylab = "True positive fraction (Sensitivity)")
binorm.plot3 <- binorm.plot3 + 
  theme(plot.title = element_text(size = 16,hjust=0.5),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))
binorm.plot3

library(gridExtra)
tiff("ROC.tiff", units="in", width=18, height=10, res=300)
gridExtra::grid.arrange(binorm.plot1,binorm.plot2,binorm.plot3, nrow=1, ncol=3)
dev.off()

#Why did not seek attention sooner? Please respond for each of the queries (Yes/No)
#2.6 Emotional barriers: a.	Because you thought that the problem would disappear by itself? Yes/No 
BrData$Emotional_barriers_disappearItself <- factor(BrData$a..Because.you.thought.that.the.problem.would.disappear.by.itself.)
summary(BrData$Emotional_barriers_disappearItself)
BrData$Emotional_barriers_disappearItself <- factor(BrData$Emotional_barriers_disappearItself,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_disappearItself)

x <- table(BrData$Emotional_barriers_disappearItself)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Emotional_barriers_disappearItself)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Emotional barriers: b.	Fear/ too scared?(Yes/No)
BrData$Emotional_barriers_scared <- factor(BrData$b..Fear..too.scared.)
summary(BrData$Emotional_barriers_scared)
BrData$Emotional_barriers_scared <- factor(BrData$Emotional_barriers_scared,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_scared)

x <- table(BrData$Emotional_barriers_scared)
x
round(prop.table(x),4)*100


c <- table(BrData$Hist_br ,BrData$Emotional_barriers_scared)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Emotional barriers: c.	Too embarrassed(Yes/No)
BrData$Emotional_barriers_embarrassed <- factor(BrData$c..Too.embarrassed.)
summary(BrData$Emotional_barriers_embarrassed)
BrData$Emotional_barriers_embarrassed <- factor(BrData$Emotional_barriers_embarrassed,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_embarrassed)

x <- table(BrData$Emotional_barriers_embarrassed)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Emotional_barriers_embarrassed)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Emotional barriers: d.	Negligence or carelessness?(Yes/No)
BrData$Emotional_barriers_negligence <- factor(BrData$d..Negligence.or.carelessness.)
summary(BrData$Emotional_barriers_negligence)
BrData$Emotional_barriers_negligence <- factor(BrData$Emotional_barriers_negligence,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_negligence)

x <- table(BrData$Emotional_barriers_negligence)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Emotional_barriers_negligence)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Practical barriers: e.	h.	Because I had to take care of the family (children, elderly or sick)?(Yes/No)
BrData$Practical_barriers_Carefamily <- factor(BrData$e..Because.I.had.to.take.care.of.the.family..children..elderly.or.sick..)
summary(BrData$Practical_barriers_Carefamily)
BrData$Practical_barriers_Carefamily <- factor(BrData$Practical_barriers_Carefamily,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_Carefamily)

x <- table(BrData$Practical_barriers_Carefamily)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Practical_barriers_Carefamily)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Practical barriers: g.	Too busy?(Yes/No)
BrData$Practical_barriers_toobusy <- factor(BrData$f..Too.busy.)
summary(BrData$Practical_barriers_toobusy)
BrData$Practical_barriers_toobusy <- factor(BrData$Practical_barriers_toobusy,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_toobusy)

x <- table(BrData$Practical_barriers_toobusy)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Practical_barriers_toobusy)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Practical barriers: f.	Lack of money to use health services? (Yes/No)
BrData$Practical_barriers_lackMoney <- factor(BrData$g..Lack.of.money.to.use.health.services.)
summary(BrData$Practical_barriers_lackMoney)
BrData$Practical_barriers_lackMoney <- factor(BrData$Practical_barriers_lackMoney,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_lackMoney)

x <- table(BrData$Practical_barriers_lackMoney)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Practical_barriers_lackMoney)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Health-Service barriers: k.	Difficult to arrange transport?(Yes/No)
BrData$Practical_barriers_transportDifficulties <- factor(BrData$h..Difficult.to.arrange.transport.)
summary(BrData$Practical_barriers_transportDifficulties)
BrData$Practical_barriers_transportDifficulties <- factor(BrData$Practical_barriers_transportDifficulties,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_transportDifficulties)

x <- table(BrData$Practical_barriers_transportDifficulties)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Practical_barriers_transportDifficulties)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Health-Service barriers: i.	Because I did not know where should I go?
BrData$Practical_barriers_wheretoGo <- factor(BrData$i..Because.I.did.not.know.where.should.I.go)
summary(BrData$Practical_barriers_wheretoGo)
BrData$Practical_barriers_wheretoGo <- factor(BrData$Practical_barriers_wheretoGo,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_wheretoGo)

x <- table(BrData$Practical_barriers_wheretoGo)
x
round(prop.table(x),4)*100


c <- table(BrData$Hist_br ,BrData$Practical_barriers_wheretoGo)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)


#2.6 Health-Service barriers: j.	Difficult to make appointment?(Yes/No)
BrData$Practical_barriers_appoinmentDifficulties <- factor(BrData$j..Difficult.to.make.appointment.)
summary(BrData$Practical_barriers_appoinmentDifficulties)
BrData$Practical_barriers_appoinmentDifficulties <- factor(BrData$Practical_barriers_appoinmentDifficulties,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_appoinmentDifficulties)

x <- table(BrData$Practical_barriers_appoinmentDifficulties)
x
round(prop.table(x),4)*100

c <- table(BrData$Hist_br ,BrData$Practical_barriers_appoinmentDifficulties)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Health-Service barriers: l.	For some other reason? _____________
BrData$barriers_others <- factor(BrData$K..For.some.other.reason.)
summary(BrData$barriers_others)
BrData$barriers_others <- factor(BrData$barriers_others,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$barriers_others)

x <- table(BrData$barriers_others)
x
round(prop.table(x),4)*100


c <- table(BrData$Hist_br ,BrData$barriers_others)
c
round(prop.table(c,2)*100,2)
summary(c)
fisher.test(c)

#2.6 Health-Service barriers: l.	For some other reason? specify
BrData$barriers_othersSpecify <- factor(BrData$If.other..please.specify)

#3.1 What medical centerdid you visit before coming to the cancer treatment centre?  (Select ONE)
BrData$treatmentCenter_cat <- factor(BrData$X3.1.What.medical.center.did.you.visit.before.coming.to.the.cancer.treatment.center.)

#3.1 What medical centerdid you visit before coming to the cancer treatment centre?  (Select ONE)
BrData$treatmentCenter_other <- factor(BrData$If.other..please.specify.1)

#3.1 Do you remember the date when you first visited a medical center? 
BrData$treatmentCenter_visitDate <- BrData$X3.2.Do.you.remember.the.date.when.you.first.visited.a.medical.center..Enter.the.date.

#3.2 Have you tried to treat at home or taken alternative remedy for this problem? Yes/No
BrData$treatmenthome_alternativeRemedy <- factor(BrData$X3.3.Have.you.tried.to.treat.at.home.or.taken.alternative.remedy.for.this.problem.)
summary(BrData$treatmenthome_alternativeRemedy)
BrData$treatmenthome_alternativeRemedy <- factor(BrData$treatmenthome_alternativeRemedy,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$treatmenthome_alternativeRemedy)

x <- table(BrData$treatmenthome_alternativeRemedy)
x
round(prop.table(x),4)*100

#3.2 Have you tried to treat at home or taken alternative remedy for this problem? Yes/No
BrData$treatmenthome_alternativeRemedy_specify <- factor(BrData$X3.3.1.if.yes..which.one...)

#3.2 Have you tried to treat at home or taken alternative remedy for this problem? Yes/No
BrData$treatmenthome_alternativeRemedy_specify_other <- factor(BrData$X3.3.1.1.Other.alternative.remedy)

#Family a support  
#4.1 Who is the person you talked first about your health problem? (Select ONE)
BrData$FamilySupport_firstTalked <- factor(BrData$X4.1.Who.is.the.person.you.talked.first.about.your.health.problem.)

#4.1 Who is the person you talked first about your health problem? (Select ONE)
BrData$FamilySupport_firstTalked_other <- factor(BrData$If.other..please.specify.2)

#4.2 Who recommended you to consult with a doctor?  (Select ONE)
BrData$FamilySupport_WhoRecommendDoc <- factor(BrData$X4.2.Who.recommended.you.to.consult.with.a.doctor.)

#4.2 Who recommended you to consult with a doctor?  (Select ONE)
BrData$FamilySupport_WhoRecommendDoc_other <- factor(BrData$If.other..please.specify.3)

#4.3 Did you fear or uncomfortable to tell about the problem to your spouse? Yes/No
BrData$FamilySupport_uncomfortabletoShareHusband <- factor(BrData$X4.3.Did.you.fear.or.uncomfortable.to.tell.about.the.problem.to.your.spouse.)

#4.4 Did you receive support from spouse after diagnosis?  yes/no
BrData$FamilySupport_supportHusband <- factor(BrData$X4.4.Did.you.receive.support.from.spouse.after.diagnosis.)

#4.5 If no, did you receive negative behavior from spouse? (Yes/No)
BrData$FamilySupport_supportHusband_negative <- factor(BrData$X4.5.If.no..did.you.receive.negative.behavior.from.spouse.)

#4.6 Did you receive support from social circle?  yes/no
BrData$FamilySupport_supportSocialCircle <- factor(BrData$X4.6.Did.you.receive.support.from.social.circle.)

#5. Knowledge and practices of early detection of cancer
#5.1 Did you usually check your own breasts? (Yes/No)
BrData$KP_checkBreast <- factor(BrData$X5.1.Did.you.usually.check.your.own.breasts.)
summary(BrData$KP_checkBreast)
BrData$KP_checkBreast <- factor(BrData$KP_checkBreast,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$KP_checkBreast)

x <- table(BrData$KP_checkBreast)
x
round(prop.table(x),4)*100

#5.2 Before this health problem, did a doctor or nurse check your breasts Yes/No
BrData$KP_checkBreast_Doc_Nurse <- factor(BrData$X5.2.Before.this.health.problem..did.a.doctor.or.nurse.check.your.breasts.)

#5.3 Before your breast problem have you heard of mammography or mammogram? Yes/No
BrData$KP_heardMammogram <- factor(BrData$X5.3.Before.your.breast.problem.have.you.heard.of.mammography.or.mammogram.)

#5.3 Before your breast problem have you heard of mammography or mammogram? Yes/No
BrData$KP_heardMammogram <- factor(BrData$X5.3.Before.your.breast.problem.have.you.heard.of.mammography.or.mammogram.)

#5.4 Do you know someone close to you who had or has cancer? Yes/No (THIS IS ABOUT CANCER, NOT BREAST CANCER)
BrData$KP_knowCancerPT <- factor(BrData$X5.4.Do.you.know.someone.close.to.you.who.had.or.has.cancer...THIS.IS.ABOUT.CANCER..NOT.BREAST.CANCER.)

#5.5 Did you know about breast cancer before?
BrData$KP_knowCancerBefore <- factor(BrData$X5.5.Did.you.know.about.breast.cancer.before.)

#5.6 Any particular information you wish you knew before and want others to know?
BrData$KP_wishknowCancerBefore <- factor(BrData$X5.6.Any.particular.information.you.wish.you.knew.before.and.want.others.to.know.)

#6. Pathological status after diagnosis of breast cancer 
#Lump Category
BrData$pathos_lumpCat <- factor(BrData$Lump.category)

#Cancer Stage
BrData$pathos_cancerStage <- factor(BrData$Stage.of.cancer)
summary(BrData$pathos_cancerStage)
BrData$pathos_cancerStage <- factor(BrData$pathos_cancerStage,levels=c("Stage I","Stage II","Stage III","Stage IV"),
                                    labels = c("Stage I","Stage II","Stage III","Stage IV"))
summary(BrData$pathos_cancerStage)

x <- table(BrData$pathos_cancerStage)
x
round(prop.table(x),4)*100

describeBy(BrData$PtD_months, BrData$pathos_cancerStage)
stat.desc(BrData$PtD_months)
kruskal.test(PtD_months ~ pathos_cancerStage,BrData)


describeBy(BrData$PrD_months, BrData$pathos_cancerStage)
stat.desc(BrData$PrD_months)
kruskal.test(PrD_months ~ pathos_cancerStage,BrData)

describeBy(BrData$TD_months, BrData$pathos_cancerStage)
stat.desc(BrData$TD_months)
kruskal.test(TD_months ~ pathos_cancerStage,BrData)

#Tumer Size
BrData$pathos_tumerSize <- factor(BrData$Size.of.tumor..cm.)


#Figure 1
library(ggplot2)
library("stringr") 

df <- data.frame(Discomfort=c("Breast pain", "Lump", "Arm pain","Itching", "Shape changes", "Skin changes","Ulcer or sore skin", "Nipple discharge"),
                 Percentage=c(52.06, 45.86, 34.81, 33.82, 31.36, 12.72, 11.50, 12.13))
head(df)

b<-ggplot(data=df, aes(x=Discomfort, y=Percentage)) +
  geom_bar(stat="identity", fill="pink3")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Percentage), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=5)+
  xlab("Physical Presentation") + ylab("Percentage of Patients") + 
  theme(axis.text = element_text(size = 15,angle = 0, vjust = 1, hjust=0.5),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
b <- b +scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
b
library(gridExtra)
tiff("Discomfort.tiff", units="in", width=10, height=8, res=300)
gridExtra::grid.arrange(b, nrow=1, ncol=1)
dev.off()


c <- table(BrData$pathos_cancerStage ,BrData$PtD)
c
round(prop.table(c,2)*100,2)

c <- table(BrData$pathos_cancerStage ,BrData$PrD)
c
round(prop.table(c,2)*100,2)

c <- table(BrData$pathos_cancerStage ,BrData$TD)
c
round(prop.table(c,2)*100,2)


library(ggplot2)
library(tibble)
library(scales)
library(ggrepel)
library(forcats)

df = data.frame(type = c(" Patient Delay"," Patient Delay"," Patient Delay"," Patient Delay",
                         " Provider Delay"," Provider Delay"," Provider Delay"," Provider Delay",
                         "Diagnosis Delay","Diagnosis Delay","Diagnosis Delay","Diagnosis Delay"), 
                Stages = c("Stage I","Stage II","Stage III","Stage IV",
                              "Stage I","Stage II","Stage III","Stage IV",
                              "Stage I","Stage II","Stage III","Stage IV"), 
                value = c(2.02, 44.45, 51.53, 2.01,
                          2.48, 52.75, 41.79, 2.99, 
                          1.43, 48.57, 47.86, 2.14))


library(ggplot2)
SAC <- ggplot(df, aes(x = factor(1), y = -value, fill = Stages)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(x=1.55, label=paste0(round(value), "%")),cex=2.1,
            position = position_stack(vjust=0.5)) +
  scale_x_discrete(NULL, expand = c(0,0)) +
  scale_y_continuous(NULL, expand = c(0,0)) + 
  coord_polar(theta = "y") +
  facet_wrap(~type) +
  theme_void()+ theme_bw()+
  xlab(" ") + ylab("") + ggtitle("")+
  theme(        legend.position= "bottom",
                plot.title = element_text(size = 15,hjust=0.5),
                legend.title = element_text(size=15),
                legend.text = element_text(size=15),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
                
  )
SAC

library(gridExtra)
tiff("Stages.tiff", units="in", width=8, height=4, res=300)
gridExtra::grid.arrange(SAC)
dev.off()


