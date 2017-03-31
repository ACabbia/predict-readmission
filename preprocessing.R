
#Read .csv of preprocessed clinical data and assign to 'diabetic data'
diabetic_data<- read.csv(file="Diabetes_130_US_hospitals_99_08_dataset.csv",head=TRUE,sep=",")


#####turn diagnosis 1 into 'Musculoskeletal [disease]' or 'Other'###################################################
##### TO DO: turn diagnosis 1 code into name ######################################################

diabetic_data$diag_1=as.numeric(diabetic_data$diag_1)

##### Delete some features and encounters#########################################################################
library(plyr)
library(dplyr)

#delete features 'encounter_id', 'patient_id', 'payer code' ,'medical specialty', 'glucose serum', 'weight'
# ( missing data > 50% )
diabetic_data=dplyr::select(diabetic_data,-encounter_id)
diabetic_data=dplyr::select(diabetic_data,-patient_nbr)
diabetic_data=dplyr::select(diabetic_data,-payer_code) 
diabetic_data=dplyr::select(diabetic_data,-medical_specialty)
diabetic_data=dplyr::select(diabetic_data, -max_glu_serum)
diabetic_data=dplyr::select(diabetic_data,-weight)
#remove diagnoses 2 and 3
diabetic_data=dplyr::select(diabetic_data,-diag_2)
diabetic_data=dplyr::select(diabetic_data,-diag_3)
#remove all medications
meds=c('examide','citoglipton','nateglinide','glimepiride.pioglitazone','chlorpropamide','acetohexamide','tolbutamide','pioglitazone','acarbose','miglitol',
                      'troglitazone','tolazamide','glyburide.metformin','glipizide.metformin','metformin.rosiglitazone','metformin.pioglitazone','metformin', 'repaglinide','glimepiride',
                      'glipizide','glyburide','rosiglitazone') 
for (feature in meds)
{diabetic_data=dplyr::select(diabetic_data,-eval(parse(text = feature)))}

### drop patients with gender = unknown/invalid 
diabetic_data$gender<-plyr::revalue(diabetic_data$gender, c("Unknown/Invalid"="Male"))

#delete patient encounters with discharge_disposition_id 11, 13, 14, 19, 20 or 21
for (out in c(11, 13, 14, 19, 20, 21))
{diabetic_data=diabetic_data[diabetic_data$discharge_disposition_id != out ,]}


##################################################################################################################

######## Make 'id' features into their corresponding qualitative values  ######## 

#turn admission_type_id into corresponding qualitive values
admission_types=c('Emergency','Urgent','Elective','Newborn','Not Available','NULL','Trauma Center','Not Mapped')
for (index in 1:length(diabetic_data$admission_type_id))
{i=1
for (admissiontype in admission_types)
{if (diabetic_data$admission_type_id[index]==i)
{diabetic_data$admission_type_id[index]=admissiontype
break}
  else {i=i+1}}}
diabetic_data$admission_type_id=factor(diabetic_data$admission_type_id)

#turn discharge_disposition_id into corresponding qualitive values if value is among the 6 most frequently occurring IDs, 
#else assigned 'other'

for (index in 1:length(diabetic_data$discharge_disposition_id))
{if (diabetic_data$discharge_disposition_id[index]==1)
{diabetic_data$discharge_disposition_id[index]='Discharged to home'}
  else if (diabetic_data$discharge_disposition_id[index]==2)
  {diabetic_data$discharge_disposition_id[index]='Discharged/transferred to another short term hospital'}
  else if (diabetic_data$discharge_disposition_id[index]==3)
  {diabetic_data$discharge_disposition_id[index]='Discharged/transferred to SNF'}
  else if (diabetic_data$discharge_disposition_id[index]==6)
  {diabetic_data$discharge_disposition_id[index]='Discharged/transferred to home with home health service'}
  else if (diabetic_data$discharge_disposition_id[index]==18)
  {diabetic_data$discharge_disposition_id[index]='NULL'}
  else if (diabetic_data$discharge_disposition_id[index]==22)
  {diabetic_data$discharge_disposition_id[index]='Discharged/transferred to another rehab fac including rehab units of a hospital .'}
  else {diabetic_data$discharge_disposition_id[index]='Other'}}

diabetic_data$discharge_disposition_id=factor(diabetic_data$discharge_disposition_id)

#turn admission_source_id into corresponding qualitive values if value is among the 6 most frequently occurring IDs, 
#else assigned 'other'
for (index in 1:length(diabetic_data$admission_source_id))
{if (diabetic_data$admission_source_id[index]==1)
{diabetic_data$admission_source_id[index]='Physician Referral'}
  else if (diabetic_data$admission_source_id[index]==2)
  {diabetic_data$admission_source_id[index]='Clinic Referral'}
  else if (diabetic_data$admission_source_id[index]==4)
  {diabetic_data$admission_source_id[index]='Transfer from a hospital'}
  else if (diabetic_data$admission_source_id[index]==6)
  {diabetic_data$admission_source_id[index]='Transfer from another health care facility'}
  else if (diabetic_data$admission_source_id[index]==7)
  {diabetic_data$admission_source_id[index]='Emergency Room'}
  else if (diabetic_data$admission_source_id[index]==18)
  {diabetic_data$admission_source_id[index]='Transfer From Another Home Health Agency'}
  else {diabetic_data$admission_source_id[index]='Other'}}

diabetic_data$admission_source_id=factor(diabetic_data$admission_source_id)

#############################################################################
### change readmitted  (3 levels) from "<30", ">30" , "NO" to "YES", "NO" (2 levels)
############################################################################

diabetic_data$readmitted<-plyr::revalue(diabetic_data$readmitted, c("<30"="YES", ">30"="YES"))


#############################################################################
#### subset of patients who have the A1c measure 
#############################################################################

A1c <- filter(diabetic_data, A1Cresult != "None")
###############################################################

# Clean up #
rm(admission_types, admissiontype, feature, i, index, meds, out)

###call feature selection script (uncomment next line)
#source('~/feature-selection.R')
