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
BrData$Record_ID <- BrData$Record.ID

# BrData$Survey_Identifier <- BrData$Survey.Identifier #all missing
# BrData$X  #not important
# BrData$Form.no...Patient.ID #not important

#2.	Date of Interview (today’s date)
BrData$Date.of.interview <- BrData$Date_of_interview

#3.	Patient’s Name
BrData$Patients_Name <- BrData$X1..Patient.s.Name

#4.	Contact: XX, XX
BrData$Contact_Telephone <- BrData$X2..Contact.Telephone

#9.	Age in years (whole year)
BrData$Age <- BrData$X3..Age..in.whole.year.

#6.	Current place of residence (Home District)
BrData$Home_district <- BrData$X4..Home.District..permanent.residence.

#5.	Current place of residence (Rural/Urban)
BrData$residence <- BrData$X5..Location.of.of.residence

#5.	Current place of residence (Current District)
BrData$Current_eistrict <- BrData$X6..Current.place.of.residence..district.

#7.	Education (highest level completed) (primary, secondary, higher secondary, university)
BrData$Patients_education <- BrData$X1..Education.completed

#11.	Marital status (single/widowed/never married/ married)
BrData$Marital_status <- BrData$X2..Marital.status

#8.	Husband’s education (Primary, secondary, higher secondary, university)
BrData$Husbands_education <- BrData$X3..Husband.s.education..completed.

#Monthly.family.income.in.Taka
BrData$Family_income <- BrData$X4..Monthly.family.income.in.Taka

#Monthly.income.Others
BrData$Others_income <- BrData$Monthly.income.Others

#Social Media
BrData$social_media_access <- BrData$X5..Access.to.communication.and.media..choice.Social.media.

#Mobile
BrData$Mobile_access <- BrData$X5..Access.to.communication.and.media..choice.Mobile.

#Smartphone
BrData$smartphone_access <- BrData$X5..Access.to.communication.and.media..choice.Smartphone.

#computer
BrData$computer_access <- BrData$X5..Access.to.communication.and.media..choice.Personal.Computer..PC..

#TV
BrData$TV_access <- BrData$X5..Access.to.communication.and.media..choice.TV.

#Newspaper
BrData$Newspaper_access <- BrData$X5..Access.to.communication.and.media..choice.Newspaper.

#13.	Family history of breast cancer: yes/no
BrData$Hist_br <- BrData$X6..Family.history.of.breast.cancer

#13.	Family history of breast cancer: yes/no
BrData$Hist_br <- BrData$X6..Family.history.of.breast.cancer

#When did you first realize that you haveproblem with your breast? 
BrData$problem_identify_br <- BrData$X1.3.1.When.did.you.first.realize.that.you.problem.with.your.breast.

#Lump
BrData$symptom_Lump_br <- BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Lump.

#Skin changes
BrData$symptom_SkinChanges_br <- BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Skin.changes.

#Breast Pain
BrData$symptom_breastPain_br <- BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Breast.pain.

#Nipple discharge
BrData$symptom_nippleDischarge_br <- BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Nipple.discharge.

#Bone Pain
BrData$symptom_BonePain_br <- BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Bone.pain.

#Others
BrData$symptom_Others_br <- BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Other.

#Others_name
BrData$symptom_Othersname_br <- BrData$If.other..please.specify.other.symptoms.noticed

#2.3 When you noticed symptom for the first time did you think that it could be cancer? 
BrData$symptom_cancer_br <- BrData$X2.1.When.you.noticed.symptom.for.the.first.time.did.you.think.that.it.could.be.cancer.

#2.4 Have you experienced following discomfort? 
#a.	Lump in the armpit, neck or trunk? Yes/No 
BrData$discomfort_Lump_br <- BrData$Lump.in.the.armpit..neck.or.trunk.

#b.	Pain in breast  (Yes/No)
BrData$discomfort_breastPain_br <- BrData$Pain.in.breast











