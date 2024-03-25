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

#Division
BrData$Division <- factor(BrData$Division)

BrData$AgeCat <- factor(BrData$AgeCat,levels=c("Barisal",2,3,4),labels = c('<40','40-49','50-59','>=60'))
BrData$AgeCat

summary(BrData$AgeCat)
x <- table(BrData$AgeCat)
x
round(prop.table(x),4)*100

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

#13.	Family history of breast cancer: yes/no
BrData$Hist_br <- factor(BrData$X6..Family.history.of.breast.cancer)

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

summary(BrData$discomfort_Lump_br)
x <- table(BrData$discomfort_Lump_br)
x
round(prop.table(x),4)*100


#b.	Pain in breast  (Yes/No)
BrData$discomfort_breastPain_br <- factor(BrData$Pain.in.breast)

summary(BrData$discomfort_breastPain_br)
x <- table(BrData$discomfort_breastPain_br)
x
round(prop.table(x),4)*100

#c.	Pain in arm on the same side as the affected? (Yes/No)
BrData$discomfort_armPain_br <- factor(BrData$Pain.in.arm.on.the.same.side.as.the.affected.)

summary(BrData$discomfort_armPain_br)
x <- table(BrData$discomfort_armPain_br)
x
round(prop.table(x),4)*100

#d.	Color changes in the breast skin (like red, brown or purple)? (Yes/No)
BrData$discomfort_skinColor_br <- factor(BrData$Color.changes.in.the.breast.skin..like.red..brown.or.purple..)

summary(BrData$discomfort_skinColor_br)
x <- table(BrData$discomfort_skinColor_br)
x
round(prop.table(x),4)*100

#e.	Ulcer or sore on the skin of the breast? (Yes/No)
BrData$discomfort_skinUlcer_br <- factor(BrData$Ulcer.or.sore.on.the.skin.of.the.breast.)

summary(BrData$discomfort_skinUlcer_br)
x <- table(BrData$discomfort_skinUlcer_br)
x
round(prop.table(x),4)*100

#f.	Itching in the breast?(Yes/No)
BrData$discomfort_Itching_br <- factor(BrData$Itching.in.the.breast.)

summary(BrData$discomfort_Itching_br)
x <- table(BrData$discomfort_Itching_br)
x
round(prop.table(x),4)*100

#g.	Changes in breast shape?(Yes/No)
BrData$discomfort_shape_br <- factor(BrData$Changes.in.breast.shape.)

summary(BrData$discomfort_shape_br)
x <- table(BrData$discomfort_shape_br)
x
round(prop.table(x),4)*100

#h.	Liquid or blood came out from the nipple?(Yes/No)
BrData$discomfort_liquidBlood_br <- factor(BrData$Liquid.or.blood.came.out.from.the.nipple.)

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

c <- table(BrData$residence ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(BrData$Marital_status ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(BrData$Patients_education ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(BrData$Husbands_education ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(BrData$Family_income ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(BrData$Division ,BrData$PtD)
c
round(prop.table(c,1)*100,2)
summary(c)



#Provider_Delay
BrData$PrD <- factor(BrData$PrD,levels=c("Yes","No"),labels = c("Yes","No"))
BrData$PrD
summary(BrData$PrD)
x <- table(BrData$PrD)
x
round(prop.table(x),4)*100

#Total_Delay
BrData$TD <- factor(BrData$TD,levels=c("Yes","No"),labels = c("Yes","No"))
BrData$TD
summary(BrData$TD)
x <- table(BrData$TD)
x
round(prop.table(x),4)*100

#Why did not seek attention sooner? Please respond for each of the queries (Yes/No)
#2.6 Emotional barriers: a.	Because you thought that the problem would disappear by itself? Yes/No 
BrData$Emotional_barriers_disappearItself <- factor(BrData$a..Because.you.thought.that.the.problem.would.disappear.by.itself.)
summary(BrData$Emotional_barriers_disappearItself)
BrData$Emotional_barriers_disappearItself <- factor(BrData$Emotional_barriers_disappearItself,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_disappearItself)

x <- table(BrData$Emotional_barriers_disappearItself)
x
round(prop.table(x),4)*100

c <- table(BrData$AgeCat ,BrData$Emotional_barriers_disappearItself)
c
round(prop.table(c,2)*100,2)
summary(c)

c <- table(BrData$residence ,BrData$Emotional_barriers_disappearItself)
c
round(prop.table(c,2)*100,2)
summary(c)

c <- table(BrData$Marital_status ,BrData$Emotional_barriers_disappearItself)
c
round(prop.table(c,2)*100,2)
summary(c)

#2.6 Emotional barriers: b.	Fear/ too scared?(Yes/No)
BrData$Emotional_barriers_scared <- factor(BrData$b..Fear..too.scared.)
summary(BrData$Emotional_barriers_scared)
BrData$Emotional_barriers_scared <- factor(BrData$Emotional_barriers_scared,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_scared)

x <- table(BrData$Emotional_barriers_scared)
x
round(prop.table(x),4)*100


#2.6 Emotional barriers: c.	Too embarrassed(Yes/No)
BrData$Emotional_barriers_embarrassed <- factor(BrData$c..Too.embarrassed.)
summary(BrData$Emotional_barriers_embarrassed)
BrData$Emotional_barriers_embarrassed <- factor(BrData$Emotional_barriers_embarrassed,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_embarrassed)

x <- table(BrData$Emotional_barriers_embarrassed)
x
round(prop.table(x),4)*100

#2.6 Emotional barriers: d.	Negligence or carelessness?(Yes/No)
BrData$Emotional_barriers_negligence <- factor(BrData$d..Negligence.or.carelessness.)
summary(BrData$Emotional_barriers_negligence)
BrData$Emotional_barriers_negligence <- factor(BrData$Emotional_barriers_negligence,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Emotional_barriers_negligence)

x <- table(BrData$Emotional_barriers_negligence)
x
round(prop.table(x),4)*100

#2.6 Practical barriers: e.	h.	Because I had to take care of the family (children, elderly or sick)?(Yes/No)
BrData$Practical_barriers_Carefamily <- factor(BrData$e..Because.I.had.to.take.care.of.the.family..children..elderly.or.sick..)
summary(BrData$Practical_barriers_Carefamily)
BrData$Practical_barriers_Carefamily <- factor(BrData$Practical_barriers_Carefamily,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_Carefamily)

x <- table(BrData$Practical_barriers_Carefamily)
x
round(prop.table(x),4)*100


#2.6 Practical barriers: g.	Too busy?(Yes/No)
BrData$Practical_barriers_toobusy <- factor(BrData$f..Too.busy.)
summary(BrData$Practical_barriers_toobusy)
BrData$Practical_barriers_toobusy <- factor(BrData$Practical_barriers_toobusy,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_toobusy)

x <- table(BrData$Practical_barriers_toobusy)
x
round(prop.table(x),4)*100


#2.6 Practical barriers: f.	Lack of money to use health services? (Yes/No)
BrData$Practical_barriers_lackMoney <- factor(BrData$g..Lack.of.money.to.use.health.services.)
summary(BrData$Practical_barriers_lackMoney)
BrData$Practical_barriers_lackMoney <- factor(BrData$Practical_barriers_lackMoney,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_lackMoney)

x <- table(BrData$Practical_barriers_lackMoney)
x
round(prop.table(x),4)*100

#2.6 Health-Service barriers: k.	Difficult to arrange transport?(Yes/No)
BrData$Practical_barriers_transportDifficulties <- factor(BrData$h..Difficult.to.arrange.transport.)
summary(BrData$Practical_barriers_transportDifficulties)
BrData$Practical_barriers_transportDifficulties <- factor(BrData$Practical_barriers_transportDifficulties,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_transportDifficulties)

x <- table(BrData$Practical_barriers_transportDifficulties)
x
round(prop.table(x),4)*100

#2.6 Health-Service barriers: i.	Because I did not know where should I go?
BrData$Practical_barriers_wheretoGo <- factor(BrData$i..Because.I.did.not.know.where.should.I.go)
summary(BrData$Practical_barriers_wheretoGo)
BrData$Practical_barriers_wheretoGo <- factor(BrData$Practical_barriers_wheretoGo,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_wheretoGo)

x <- table(BrData$Practical_barriers_wheretoGo)
x
round(prop.table(x),4)*100

#2.6 Health-Service barriers: j.	Difficult to make appointment?(Yes/No)
BrData$Practical_barriers_appoinmentDifficulties <- factor(BrData$j..Difficult.to.make.appointment.)
summary(BrData$Practical_barriers_appoinmentDifficulties)
BrData$Practical_barriers_appoinmentDifficulties <- factor(BrData$Practical_barriers_appoinmentDifficulties,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Practical_barriers_appoinmentDifficulties)

x <- table(BrData$Practical_barriers_appoinmentDifficulties)
x
round(prop.table(x),4)*100

#2.6 Health-Service barriers: l.	For some other reason? _____________
BrData$barriers_others <- factor(BrData$K..For.some.other.reason.)
summary(BrData$barriers_others)
BrData$barriers_others <- factor(BrData$barriers_others,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$barriers_others)

x <- table(BrData$barriers_others)
x
round(prop.table(x),4)*100

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

#Tumer Size
BrData$pathos_tumerSize <- factor(BrData$Size.of.tumor..cm.)


#Figure 1
library(ggplot2)
library("stringr") 

df <- data.frame(Discomfort=c("Breast pain", "Lump", "Arm pain","Itching", "Shape changes", "Skin changes","Ulcer or sore skin", "Nipple discharge"),
                 Percentage=c(49.86, 43.66, 33.24, 32.39, 29.86, 12.11, 10.99, 11.55))
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
