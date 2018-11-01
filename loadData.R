library(foreign) #read.spss function

# read SPSS data file
dat_in = read.spss("/Users/Fred/Documents/Aarhus/5. Semester/Bachelor/Gender in politics data/DK2017_Lea.sav")

#######################################################
####------Parsing SPSS file into responses to each question

#----------Question 1 ---------------------
# judgements of candidate in vignette
# check with Lea to verify titles
cond <- dat_in$basic_design
competence <-as.numeric(substr(dat_in$Q1_1,1,1))
ambition <-as.numeric(substr(dat_in$Q1_2,1,1))
project <-as.numeric(substr(dat_in$Q1_3,1,1))
obedient <-as.numeric(substr(dat_in$Q1_4,1,1))
respectful<-as.numeric(substr(dat_in$Q1_5,1,1))
self_erase<-as.numeric(substr(dat_in$Q1_6,1,1))
independent<-as.numeric(substr(dat_in$Q1_7,1,1))
self_reliant<-as.numeric(substr(dat_in$Q1_8,1,1))
decisive<-as.numeric(substr(dat_in$Q1_9,1,1))
warm<-as.numeric(substr(dat_in$Q1_10,1,1))
friendly<-as.numeric(substr(dat_in$Q1_11,1,1))
cooperative<-as.numeric(substr(dat_in$Q1_12,1,1))
sharp<-as.numeric(substr(dat_in$Q1_13,1,1))
intuitive<-as.numeric(substr(dat_in$Q1_14,1,1))
insightful<-as.numeric(substr(dat_in$Q1_15,1,1))
confident<-as.numeric(substr(dat_in$Q1_16,1,1))
dominent<-as.numeric(substr(dat_in$Q1_17,1,1))
forceful<-as.numeric(substr(dat_in$Q1_18,1,1))
analytic<-as.numeric(substr(dat_in$Q1_19,1,1))
logical<-as.numeric(substr(dat_in$Q1_20,1,1))
objective<-as.numeric(substr(dat_in$Q1_21,1,1))
friendly2<-as.numeric(substr(dat_in$Q1_22,1,1))
caring<-as.numeric(substr(dat_in$Q1_23,1,1))
considerate<-as.numeric(substr(dat_in$Q1_24,1,1))

#------Question 2 and 3 ---------------------------
# representation
foreign_me <- as.numeric(substr(dat_in$Q2_1,1,1))
education_me <- as.numeric(substr(dat_in$Q2_2,1,1))
health_me <- as.numeric(substr(dat_in$Q2_3,1,1))
law_me <- as.numeric(substr(dat_in$Q2_4,1,1))

foreign_dk <- as.numeric(substr(dat_in$Q3_1,1,1))
education_dk <- as.numeric(substr(dat_in$Q3_2,1,1))
health_dk <- as.numeric(substr(dat_in$Q3_3,1,1))
law_dk <- as.numeric(substr(dat_in$Q3_4,1,1))

#------Question 4 ---------------------------
# judgements of sociability and trustworthyness
sociability <- rowSums(cbind(as.numeric(substr(dat_in$Q4a_socialibility,1,1)),
                             as.numeric(substr(dat_in$Q4b_socialibility,1,1)),
                             as.numeric(substr(dat_in$Q4c_socialibility,1,1)),
                             as.numeric(substr(dat_in$Q4d_socialibility,1,1))))

trustworthy <- rowSums(cbind(as.numeric(substr(dat_in$Q4e_moral_trustworthy,1,1)),
                             as.numeric(substr(dat_in$Q4f_moral_trustworthy,1,1)),
                             as.numeric(substr(dat_in$Q4g_moral_trustworthy,1,1)),
                             as.numeric(substr(dat_in$Q4h_moral_trustworthy,1,1))))

#------Question 5, 6, and 7 ---------------------------
# voting, suitability, and ability to explicitly recall target gender
vote <- as.numeric(substr(dat_in$Q5,1,1))
gender_suitability <- as.numeric(substr(dat_in$Q6,1,1))
gender_recall <- dat_in$Q7

gender_correct <- rep(1,length(gender_recall))
gender_correct[cond=="Standard female candidate"&gender_recall == "Mand"]=0
gender_correct[cond=="Warm female candidate"&gender_recall == "Mand"]=0
gender_correct[cond=="Competent female candidate"&gender_recall == "Mand"]=0
gender_correct[cond=="Standard male candidate"&gender_recall == "Kvinde"]=0
gender_correct[gender_recall == "Ved ikke"]=0

#------ participant demographics and experiment/condition information
gender <- dat_in$gender

experiment <- rep(1,length(competence))
experiment[cond=="Warm female candidate"|cond=="Competent female candidate"] <- 2

sub <- dat_in$RecordNo




#Dataframe

participant = list(1:1006)

dataframe = data.frame(participant, cond, 
                       
                       competence, ambition, project, obedient, respectful, self_erase, independent, 
                       self_reliant, decisive, warm, friendly, cooperative, sharp, intuitive, insightful, confident, 
                       dominent, forceful, analytic, logical, objective, friendly2, caring, considerate, 
                       
                       foreign_me, education_me, health_me, law_me,
                       foreign_dk, education_dk, health_dk, law_dk,
                       
                       sociability, trustworthy,
                       
                       vote, gender_suitability, gender_recall, gender_correct, gender)

dataframe = plyr::rename(dataframe, c("X1.1006" = "participant"))

library(pastecs)
by(dataframe$vote, dataframe$cond, FUN = mean)


#Subset of dataframe with only "judgment questions"
newdf = subset(dataframe, select = c(competence, ambition, project, obedient, respectful, self_erase, independent, 
                                     self_reliant, decisive, warm, friendly, cooperative, sharp, intuitive, insightful, 
                                     confident, dominent, forceful, analytic, logical, objective, friendly2, caring, considerate))

library(psych)
#Alpha??

achievement_orientation = subset(dataframe, select = c(competence, ambition, project))
achcor = cor(achievement_orientation)
round(achcor, 2)
alpha(achievement_orientation)
achievement_orientation_mean = mean(rowMeans(achievement_orientation))
achievement_orientation_mean

deference = subset(dataframe, select = c(obedient, respectful, self_erase))
defcor = cor(deference)
round(defcor, 2)


autonomy = subset(dataframe, select = c(independent, self_reliant, decisive))
autcor = cor(autonomy)
round(autcor, 2)

affiliative = subset(dataframe, select = c(warm, friendly, cooperative))
affcor = cor(affiliative)
round(affcor, 2)

emotionally_sensitive = subset(dataframe, select = c(sharp, intuitive, insightful))
emocor = cor(emotionally_sensitive)
round(emocor, 2)

take_charge = subset(dataframe, select = c(confident, dominent, forceful))
takcor = cor(take_charge)
round(takcor, 2)

rational = subset(dataframe, select = c(analytic, logical, objective))
ratcor = cor(rational)
round(ratcor, 2)

concern_for_others = subset(dataframe, select = c(friendly2, caring, considerate))
concor = cor(concern_for_others)
round(concor, 2)

df2 = data.frame(c(achievement_orientation, deference, autonomy, affiliative, emotionally_sensitive, take_charge, rational, concern_for_others))

cor(df2)

