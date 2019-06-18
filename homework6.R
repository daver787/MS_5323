#read in the file and set working directory
setwd("C:/Users/nopassword/Desktop")
bike<-read.csv("buybike2.csv")

#convert to factor variables
bike$TerritoryID<-factor(bike$TerritoryID)
bike$MaritalStatus<-factor(bike$MaritalStatus)
bike$YearlyIncome<-factor(bike$YearlyIncome)
bike$Gender<-factor(bike$Gender)
bike$Education<-factor(bike$Education)
bike$Occupation<-factor(bike$Occupation)
bike$HomeOwnerFlag<-factor(bike$HomeOwnerFlag)
bike$CommuteDistance<-factor(bike$CommuteDistance)
bike$bike_buyer<-factor(bike$bike_buyer)

#fit the initial model
mylogit<-glm(bike_buyer~TerritoryID+MaritalStatus+YearlyIncome+Gender+TotalChildren+NumberChildrenAtHome+Education+Occupation+HomeOwnerFlag+NumberCarsOwned+CommuteDistance+Age,family="binomial",data=bike)

#check the fit of the model
summary(mylogit)

#recode factor variables in distinct groups
bike$edu_bach<-factor(ifelse(bike$Education=="Bachelors",1,0))
bike$edu_grad<-factor(ifelse(bike$Education=="Graduate Degree",1,0))
bike$edu_hs<-factor(ifelse(bike$Education=="High School",1,0))
bike$edu_partcoll<-factor(ifelse(bike$Education=="Partial College",1,0))
bike$edu_parths<-factor(ifelse(bike$Education=="Partial High School",1,0))

bike$terr1<-factor(ifelse(bike$TerritoryID==1,1,0))
bike$terr2<-factor(ifelse(bike$TerritoryID==2,1,0))
bike$terr3<-factor(ifelse(bike$TerritoryID==3,1,0))
bike$terr4<-factor(ifelse(bike$TerritoryID==4,1,0))
bike$terr5<-factor(ifelse(bike$TerritoryID==5,1,0))
bike$terr6<-factor(ifelse(bike$TerritoryID==6,1,0))
bike$terr7<-factor(ifelse(bike$TerritoryID==7,1,0))
bike$terr8<-factor(ifelse(bike$TerritoryID==8,1,0))
bike$terr9<-factor(ifelse(bike$TerritoryID==9,1,0))
bike$terr10<-factor(ifelse(bike$TerritoryID==10,1,0))

bike$male<-factor(ifelse(bike$Gender=="M",1,0))

bike$single<-factor(ifelse(bike$MaritalStatus=="S",1,0))

bike$inc_lt25k<-factor(ifelse(bike$YearlyIncome=="0-25000",1,0))
bike$inc_25_50k<-factor(ifelse(bike$YearlyIncome=="25001-50000",1,0))
bike$inc_50_75k<-factor(ifelse(bike$YearlyIncome=="50001-75000",1,0))
bike$inc_75_100k<-factor(ifelse(bike$YearlyIncome=="75001-100000",1,0))
bike$inc_mt100k<-factor(ifelse(bike$YearlyIncome=="greater than 100000",1,0))

bike$dist0_1<-factor(ifelse(bike$CommuteDistance=="0-1 Miles",1,0))
bike$dist1_2<-factor(ifelse(bike$CommuteDistance=="1-2 Miles",1,0))
bike$dist2_5<-factor(ifelse(bike$CommuteDistance=="2-5 Miles",1,0))
bike$dist5_10<-factor(ifelse(bike$CommuteDistance=="5-10 Miles",1,0))
bike$dist10plus<-factor(ifelse(bike$CommuteDistance=="10+ Miles",1,0))

bike$occ_cleric<-factor(ifelse(bike$Occupation=="Clerical",1,0))
bike$occ_mgmt<-factor(ifelse(bike$Occupation=="Management",1,0))
bike$occ_man<-factor(ifelse(bike$Occupation=="Manual",1,0))
bike$occ_prof<-factor(ifelse(bike$Occupation=="Professional",1,0))
bike$occ_skill<-factor(ifelse(bike$Occupation=="Skilled Manual",1,0))

#Checking out the new variables
summary(bike)


#fit the models
full<-glm(bike_buyer~terr2+terr3+terr4+terr5+terr6+terr7+terr8+terr9+terr10+single+inc_25_50k+inc_50_75k+inc_75_100k+inc_mt100k+
            TotalChildren+NumberChildrenAtHome+edu_grad+edu_hs+edu_partcoll+edu_hs+occ_mgmt+occ_man+occ_skill+
            HomeOwnerFlag+NumberCarsOwned+dist1_2+dist2_5+dist5_10+dist10plus+Age,family="binomial",data=bike)
null<-glm(bike_buyer~1,family="binomial",data=bike)

steplogit<-step(null,list(lower=formula(null),upper=formula(full)),direction="both",trace=0)

#check and see how the models performed
summary(full)
summary(steplogit)

#generate odds ratio and confidence intervals for stepwise logistic regression
exp(cbind(OR=coef(steplogit),confint(steplogit)))

#compare the full model and new full model
anova(steplogit,full,test="Chisq")







