##Packages:
##Akhil Deshmukh
## loading packages
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(zoo)
install.packages(zoo)
install.packages('caret', dependencies = TRUE)
library(caret)
install.packages('naivebayes', dependencies = TRUE)
library(naivebayes)
install.packages('Metrics', dependencies = TRUE)
library(Metrics)
library(plyr)
#install.packages("lattice")
library(lattice)
library(caret)
#install.packages("MASS")
library(MASS)
#install.packages("klaR")
library(klaR)
library(class)
install.packages("dummies", dependencies = TRUE)
library(dummies)
install.packages("openair")
library(openair)
install.packages("boot")
library(boot)
install.packages("BSDA")
library(BSDA)








Hypothesis Testing.

data_HT = read_csv("/Users/srikanthmelugiri/Documents/Renu chauhan/Data Analytics/Project/database_year.csv")

#####Hypothesis Testing########

AD = data_HT$`Aircraft Damage`

sum(is.na(AD))

#two sample hypothesis testing
install.packages("BSDA")
library("BSDA")

DamangeSpeed = 0; NotDamangeSpeed = 0;k=1;j=1;

Speed = data_HT$Speed

for(i in 1:length(AD)){
  if(AD[i]==1){
    DamangeSpeed[j]=Speed[i]
    j=j+1
  }else{
    NotDamangeSpeed[k]=Speed[i]
    k=k+1
  }
}
DamangeSpeed=na.omit(DamangeSpeed)
NotDamangeSpeed=na.omit(NotDamangeSpeed)
z.test(DamangeSpeed,NotDamangeSpeed,alternative="two.sided",paired = F,mu=0,sigma.x=sd(DamangeSpeed),sigma.y=sd(NotDamangeSpeed),conf.level=0.95)













Data Preprocessing.


## Preprocessing Data
## Aircraft Type and converting NA to A as A occurs the most.
summary(mydata_KNN$`Aircraft Type`) ## check for most occuroured valued to replace missing values.

###  checking for NA values
is.na(mydata_KNN$`Aircraft Type`)
### cheking the data type
str(mydata_KNN$`Aircraft Type`)
### converting character to factor.
mydata_KNN$`Aircraft Type` = as.factor(mydata_KNN$`Aircraft Type`)
summary(mydata_KNN$`Aircraft Type`)
mydata_KNN$`Aircraft Type`[is.na(mydata_KNN$`Aircraft Type`)] <- "A"
sum(is.na(mydata_KNN$`Aircraft Type`))


AircraftDamage = as.integer(mydata_KNN$`Aircraft Damage`)





###  creating a date variable
mydata_KNN <- mydata_KNN %>% mutate(date=paste0(as.character(`Incident Month`),"-",as.character(`Incident Day`),"-",as.character(`Incident Year`)))
mydata_KNN$date <- as.Date(mydata_KNN$date,format="%m-%d-%Y")
mydata_KNN$monthdate <- as.yearmon(format(as.Date(mydata_KNN$date, "%m-%d-%Y"),"%Y-%m"))
is.null(mydata_KNN$date)
str(mydata_KNN$date)

datevar = as.factor(mydata_KNN$date)
str(datevar)

### Filling blank rows of Aircraft Make by mode values

summary(mydata_KNN$`Aircraft Make`)
mydata_KNN$`Aircraft Make`=as.factor(mydata_KNN$`Aircraft Make`)
sum(is.na(mydata_KNN$`Aircraft Make`))
mydata_KNN$`Aircraft Make`[is.na(mydata_KNN$`Aircraft Make`)] <- "148"

### Aircraft model

is.na(mydata_KNN$`Aircraft Model`)
mydata_KNN$`Aircraft Model` = as.factor(mydata_KNN$`Aircraft Model`)
summary(mydata_KNN$`Aircraft Model`)
mydata_KNN$`Aircraft Model`[is.na(mydata_KNN$`Aircraft Model`)] <- "24"

### Aircraft Mass 

sum(is.na(mydata_KNN$`Aircraft Mass`))
mydata_KNN$`Aircraft Mass` = as.factor(mydata_KNN$`Aircraft Mass`)
summary(mydata_KNN$`Aircraft Mass`)
mydata_KNN$`Aircraft Mass`[is.na(mydata_KNN$`Aircraft Mass`)] <- "4"

### Engine make

sum(is.na(mydata_KNN$`Engine Make`))
mydata_KNN$`Engine Make` = as.factor(mydata_KNN$`Engine Make`)
summary(mydata_KNN$`Engine Make`)
mydata_KNN$`Engine Make`[is.na(mydata_KNN$`Engine Make`)] <- "10"


### Engine model - recheck

is.na(mydata_KNN$`Engine Model`)
mydata_KNN$`Engine Model` = as.factor(mydata_KNN$`Engine Model`)
summary(mydata_KNN$`Engine Model`)
mydata_KNN$`Engine Model`[is.na(mydata_KNN$`Engine Model`)] <- "1"

mydata_KNN$`Engine Model`=revalue(mydata_KNN$`Engine Model`,c("??"="1"))
sum(is.na(mydata_KNN$`Engine Model`))



### Airport Name

is.na(mydata_KNN$Airport)
mydata_KNN$Airport = as.factor(mydata_KNN$Airport)
summary(mydata_KNN$Airport)
mydata_KNN$Airport[is.na(mydata_KNN$Airport)] <- "UNKNOWN"
sum(is.na(mydata_KNN$Airport))

### Species Name

#find max occurence of bird species in the given column
ll<-data.frame(table(mydata_KNN$`Species Name`))
ll[which.max(ll$Freq),]
mydata_KNN$`Species Name` = as.factor(mydata_KNN$`Species Name`)
# replacing blank with max occurence of species name i.e UNKNOWN MEDIUM BIRD
mydata_KNN$`Species Name`[is.na(mydata_KNN$`Species Name`)] <- "UNKNOWN MEDIUM BIRD"
summary(mydata_KNN$`Species Name`)

sum(is.na(mydata_KNN$`Species Name`))

### Speed

#replcaing values in speed with mean
mydata_KNN$Speed=ifelse(is.na(mydata_KNN$Speed),ave(mydata_KNN$Speed,FUN = function(x)
  mean(x,na.rm = TRUE)),mydata_KNN$Speed)
summary(mydata_KNN$Speed)
sum(is.na(mydata_KNN$Speed))
### Height

#replcaing values in height with mean
mydata_KNN$Height=ifelse(is.na(mydata_KNN$Height),ave(mydata_KNN$Height,FUN = function(x)
  mean(x,na.rm = TRUE)),mydata_KNN$Height)

summary(mydata_KNN$Height)
sum(is.na(mydata_KNN$Height))


### Distance

#replcaing values in distance with mean

mydata_KNN$Distance=ifelse(is.na(mydata_KNN$Distance),ave(mydata_KNN$Distance,FUN = function(x)
  mean(x,na.rm = TRUE)),mydata_KNN$Distance)

summary(mydata_KNN$Distance)

sum(is.na(mydata_KNN$Distance))
### Precipitation

llpre<-data.frame(table(mydata_KNN$Precipitation))
llpre[which.max(llpre$Freq),]
#replacing blank with max occurence of precitation i.e NONE
mydata_KNN$Precipitation[is.na(mydata_KNN$Precipitation)] <- "NONE"
sum(is.na(mydata_KNN$Precipitation))


### Visibility

llvi<-data.frame(table(mydata_KNN$Visibility))
llvi[which.max(llvi$Freq),]
mydata_KNN$Visibility = as.factor(mydata_KNN$Visibility)
# replacing blank with max occurence of visibilty i.e DAY
mydata_KNN$Visibility[is.na(mydata_KNN$Visibility)] <- "DAY"
summary(mydata_KNN$Visibility)
sum(is.na(mydata_KNN$Visibility))


### Flight phase
llpha<-data.frame(table(mydata_KNN$`Flight Phase`))
llpha[which.max(llpha$Freq),]
mydata_KNN$`Flight Phase`=as.factor(mydata_KNN$`Flight Phase`)
# replacing blank with max occurence of Flight Phase i.e APPROACH
mydata_KNN$`Flight Phase`[is.na(mydata_KNN$`Flight Phase`)] <- "APPROACH"
summary(mydata_KNN$`Flight Phase`)
sum(is.na(mydata_KNN$`Flight Phase`))


### WARNING ISSUED ----

llwi<-data.frame(table(mydata_KNN$`Warning Issued`))
llwi[which.max(llwi$Freq),]
mydata_KNN$`Warning Issued`
mydata_KNN$`Warning Issued` = as.factor(mydata_KNN$`Warning Issued`)
### replacing blank with max occurence of Warning Issued i.e n
mydata_KNN$`Warning Issued`[is.na(mydata_KNN$`Warning Issued`)] <- "N"

mydata_KNN$`Warning Issued`=revalue(mydata_KNN$`Warning Issued`,c("n"="N"))
mydata_KNN$`Warning Issued`=revalue(mydata_KNN$`Warning Issued`,c("y"="Y"))


summary(mydata_KNN$`Warning Issued`)

sum(is.na(mydata_KNN$`Warning Issued`))

### FAA Region

llfar<-data.frame(table(mydata_KNN$`FAA Region`))
llfar[which.max(llfar$Freq),]
# replacing blank with max occurence of FAA region i.e ASO
mydata_KNN$`FAA Region`[is.na(mydata_KNN$`FAA Region`)] <- "ASO"
mydata_KNN$`FAA Region` = as.factor(mydata_KNN$`FAA Region`)
summary(mydata_KNN$`FAA Region`)
sum(is.na(mydata_KNN$`FAA Region`))

### State variable

llst<-data.frame(table(mydata_KNN$State))
llst[which.max(llst$Freq),]
#replacing UNKNOWN values from Aircraft in State with UNKNOWN 
mydata_KNN$State[is.na(mydata_KNN$State)] <- "TX"
mydata_KNN$State = as.factor(mydata_KNN$State)
sum(is.na(mydata_KNN$State))


##mydata$State=revalue(mydata$Aircraft,c("UNKNOWN"="UNKNOWN"))
##mydata$State = ifelse(mydata$Aircraft=="UNKNOWN",mydata$State<-"UNKNOWN",mydata$State)




### Engines

llfar<-data.frame(table(mydata_KNN$Engines))
llfar[which.max(llfar$Freq),]
# replacing blank with max occurence of Engines
mydata_KNN$Engines[is.na(mydata_KNN$Engines)] <- "2"
mydata_KNN$Engines= as.factor(mydata_KNN$Engines)
sum(is.na(mydata_KNN$Engines))


### Engine Type

llfar<-data.frame(table(mydata_KNN$`Engine Type`))
llfar[which.max(llfar$Freq),]
# replacing blank with max occurence of Engine Type
mydata_KNN$`Engine Type`[is.na(mydata_KNN$`Engine Type`)] <- "D"
mydata_KNN$`Engine Type` = as.factor(mydata_KNN$`Engine Type`)
summary(mydata_KNN$`Engine Type`)


sum(is.na(mydata_KNN$`Engine Type`))


### Species Number/Quantity
llsq<-data.frame(table(mydata_KNN$`Species Quantity`))
llsq[which.max(llsq$Freq),]

mydata_KNN$`Species Quantity`=revalue(mydata_KNN$`Species Quantity`,c("1"="Low"))
mydata_KNN$`Species Quantity`=revalue(mydata_KNN$`Species Quantity`,c("11-100"="Med"))
mydata_KNN$`Species Quantity`=revalue(mydata_KNN$`Species Quantity`,c("Over 100"="High"))

mydata_KNN$`Species Quantity`=revalue(mydata_KNN$`Species Quantity`,c("2-10"="Low"))

mydata_KNN$`Species Quantity`[is.na(mydata_KNN$`Species Quantity`)] <- "Low"
sum(is.na(mydata_KNN$`Species Quantity`))
summary(mydata_KNN$`Species Quantity`)

mydata_KNN$`Species Quantity` = as.factor(mydata_KNN$`Species Quantity`)



#### date month and year factor

dayName= weekdays(as.Date(mydata_KNN$date,'%Y-%m-%d'))
dayName = as.factor(dayName)
str(dayName)

monthname = as.factor(months(as.Date(mydata_KNN$date,'%Y-%m-%d')))
str(monthname)
### year
Year= as.factor(mydata_KNN$`Incident Year`)
str(Year)

##################### KNN ###################
 

AircraftDamage<-as.factor(AircraftDamage)

AircraftDamage<-ifelse(AircraftDamage=='0','NO','Yes')
AircraftType = mydata_KNN$`Aircraft Type`
AircraftMake = mydata_KNN$`Aircraft Make`
AircraftModel = mydata_KNN$`Aircraft Model`
AircraftMass = mydata_KNN$`Aircraft Mass`
EngineMake = mydata_KNN$`Engine Make`
EngineModel= mydata_KNN$`Engine Model`
Engines= mydata_KNN$Engines
Airport = mydata_KNN$Airport
State = mydata_KNN$State
FAARegion = mydata_KNN$`FAA Region`
WarningIssued = mydata_KNN$`Warning Issued`
FlightPhase= mydata_KNN$`Flight Phase`
Visibility = mydata_KNN$Visibility
Precipitation= mydata_KNN$Precipitation
Height= mydata_KNN$Height
Speed = mydata_KNN$Speed
Distance = mydata_KNN$Distance
SpeciesName = mydata_KNN$`Species Name`
SpeciesQuantity = mydata_KNN$`Species Quantity`
monthname
Year

project_KNN = data.frame(AircraftDamage,AircraftType,AircraftMake,AircraftModel,AircraftMass,
                         EngineMake,EngineModel,Engines,State,FAARegion,WarningIssued,
                         FlightPhase,Visibility,Precipitation,Height,Speed,Distance,monthname)



project_KNN_Dummy = dummy.data.frame(project_KNN,names = c("AircraftType","AircraftMake","AircraftModel","AircraftMass",
                                                           "EngineMake","EngineModel","Engines","State",
                                                           "FAARegion","WarningIssued","FlightPhase","Visibility",
                                                           "Precipitation","monthname"))


head(project_KNN_Dummy)


summary(project_KNN_Dummy)


num.vars <- sapply(project_KNN_Dummy,is.numeric)


project_KNN_Dummy[num.vars]<- lapply(project_KNN_Dummy[num.vars], scale)

head(project_KNN_Dummy)

summary(project_KNN_Dummy)

ncol(project_KNN_Dummy)


######### N fold for KNN ##############

xKnn=project_KNN_Dummy[2:274]
nrow(xKnn)
str(xKnn)
yKnn=project_KNN_Dummy$AircraftDamage
modelkNN=train(xKnn,yKnn,'knn',trControl=trainControl(method='cv',number=10),tunedGrid=expand.grid(k=1:5))


########### end of n fold KNN #############

################ NAIVE BAYES################

Height_NB = (cut(mydata_KNN$Height,3))
Spee_NB= (cut(mydata_KNN$Speed,3))
Distance_NB= (cut(mydata_KNN$Distance,3))


project_NB = data.frame(AircraftDamage,AircraftType,AircraftMake,AircraftModel,AircraftMass,
                        EngineMake,EngineModel,Engines,State,FAARegion,WarningIssued,
                        FlightPhase,Visibility,Precipitation,Height_NB,Spee_NB,Distance_NB,monthname)

str(project_NB)

xn=project_NB[,-1]
yn=project_NB[,1]
naivemodel = train(xn,yn,'nb',trControl = trainControl(method = 'cv',number = 20),na.action=na.omit)
predict(naivemodel$finalModel,xn)
print(naivemodel)

################ END OF NAIVE BAYES CODE ##############

###### N fold for LR ##########
fit1 = glm(project_KNN$AircraftDamage~ project_KNN$AircraftMake+project_KNN$AircraftModel+
           project_KNN$AircraftMass+project_KNN$EngineMake+project_KNN$EngineModel+project_KNN$Engines+project_KNN$Airport
           +project_KNN$State+project_KNN$FAARegion+project_KNN$FlightPhase+project_KNN$Visibility
           +project_KNN$Height+project_KNN$Speed+project_KNN$Distance+project_KNN$SpeciesName
           +project_KNN$monthname
           ,family = binomial(),data = project_KNN)

project_KNN_Dummy$AircraftDamage <- revalue(project_KNN_Dummy$AircraftDamage, c("1"="Yes"))
project_KNN_Dummy$AircraftDamage <- revalue(project_KNN_Dummy$AircraftDamage, c("0"="No"))

cv.glm(fit1,data=mydata,K=5)$delta

##############################




