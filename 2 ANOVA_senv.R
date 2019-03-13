# Sudan Kariuki. MS Thesis
# Conduct 2 factor ANOVAs on soil environmental variables

rm(list=ls(all= TRUE))

mydata<-read.csv("MASTER.csv")

# force R to recognize your categorical variables as "factors"
mydata$dist<-as.factor(mydata$dist)
mydata$canopy<-as.factor(mydata$canopy)
mydata$reps<-as.factor(mydata$reps)
mydata$basal_class<-as.factor(mydata$basal_class)
mydata$campsite<-as.factor(mydata$campsite)
mydata$Site_ID<-as.factor(mydata$Site_ID)
mydata$SAC<-as.numeric(mydata$SAC)
mydata$SAN<-as.numeric(mydata$SAN)

summary(mydata)

# creates a data frame from mydata including only specified values
hcover.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                         canopy=NA,
                         herbaceous=NA,
                         campsite=NA)

# calculate means of replicates for herbaceous cover data
Ca.1.485 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$basal_percent[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$basal_percent[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$basal_percent[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
hcover.df<-rbind(hcover.df,list("Ca","1",Ca.1.485,"485"))
hcover.df<-rbind(hcover.df,list("Ca","2",Ca.2.485,"485"))
hcover.df<-rbind(hcover.df,list("Re","1",Re.1.485,"485"))
hcover.df<-rbind(hcover.df,list("Re","2",Re.2.485,"485"))
hcover.df<-rbind(hcover.df,list("Co","1",Co.1.485,"485"))
hcover.df<-rbind(hcover.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
hcover.df<-rbind(hcover.df,list("Ca","1",Ca.1.413,"413"))
hcover.df<-rbind(hcover.df,list("Ca","2",Ca.2.413,"413"))
hcover.df<-rbind(hcover.df,list("Re","1",Re.1.413,"413"))
hcover.df<-rbind(hcover.df,list("Re","2",Re.2.413,"413"))
hcover.df<-rbind(hcover.df,list("Co","1",Co.1.413,"413"))
hcover.df<-rbind(hcover.df,list("Co","2",Co.2.413,"413"))

hcover.df<-rbind(hcover.df,list("Ca","1",Ca.1.4174,"4174"))
hcover.df<-rbind(hcover.df,list("Ca","2",Ca.2.4174,"4174"))
hcover.df<-rbind(hcover.df,list("Re","1",Re.1.4174,"4174"))
hcover.df<-rbind(hcover.df,list("Re","2",Re.2.4174,"4174"))
hcover.df<-rbind(hcover.df,list("Co","1",Co.1.4174,"4174"))
hcover.df<-rbind(hcover.df,list("Co","2",Co.2.4174,"4174"))

hcover.df<-rbind(hcover.df,list("Ca","1",Ca.1.4175,"4175"))
hcover.df<-rbind(hcover.df,list("Ca","2",Ca.2.4175,"4175"))
hcover.df<-rbind(hcover.df,list("Re","1",Re.1.4175,"4175"))
hcover.df<-rbind(hcover.df,list("Re","2",Re.2.4175,"4175"))
hcover.df<-rbind(hcover.df,list("Co","1",Co.1.4175,"4175"))
hcover.df<-rbind(hcover.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
hcover.df<-na.omit(hcover.df)


#run ANOVA on herbaceous cover
herbcover<-aov(herbaceous~dist+canopy+dist:canopy,data=hcover.df)
summary.aov(herbcover)

# calculate % difference in significantly different means
mean(mydata$basal_percent[mydata$dist=="Ca"],na.rm=T)
mean(mydata$basal_percent[mydata$dist=="Co"])

# creates a data frame from mydata including only specified values
litter.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                           canopy=NA,
                           litter=NA,
                           campsite=NA)


# calculate means of replicates for herbaceous cover data
Ca.1.485 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$litter[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$litter[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$litter[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
litter.df<-rbind(litter.df,list("Ca","1",Ca.1.485,"485"))
litter.df<-rbind(litter.df,list("Ca","2",Ca.2.485,"485"))
litter.df<-rbind(litter.df,list("Re","1",Re.1.485,"485"))
litter.df<-rbind(litter.df,list("Re","2",Re.2.485,"485"))
litter.df<-rbind(litter.df,list("Co","1",Co.1.485,"485"))
litter.df<-rbind(litter.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
litter.df<-rbind(litter.df,list("Ca","1",Ca.1.413,"413"))
litter.df<-rbind(litter.df,list("Ca","2",Ca.2.413,"413"))
litter.df<-rbind(litter.df,list("Re","1",Re.1.413,"413"))
litter.df<-rbind(litter.df,list("Re","2",Re.2.413,"413"))
litter.df<-rbind(litter.df,list("Co","1",Co.1.413,"413"))
litter.df<-rbind(litter.df,list("Co","2",Co.2.413,"413"))

litter.df<-rbind(litter.df,list("Ca","1",Ca.1.4174,"4174"))
litter.df<-rbind(litter.df,list("Ca","2",Ca.2.4174,"4174"))
litter.df<-rbind(litter.df,list("Re","1",Re.1.4174,"4174"))
litter.df<-rbind(litter.df,list("Re","2",Re.2.4174,"4174"))
litter.df<-rbind(litter.df,list("Co","1",Co.1.4174,"4174"))
litter.df<-rbind(litter.df,list("Co","2",Co.2.4174,"4174"))

litter.df<-rbind(litter.df,list("Ca","1",Ca.1.4175,"4175"))
litter.df<-rbind(litter.df,list("Ca","2",Ca.2.4175,"4175"))
litter.df<-rbind(litter.df,list("Re","1",Re.1.4175,"4175"))
litter.df<-rbind(litter.df,list("Re","2",Re.2.4175,"4175"))
litter.df<-rbind(litter.df,list("Co","1",Co.1.4175,"4175"))
litter.df<-rbind(litter.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
litter.df<-na.omit(litter.df)

#run ANOVA on litter
litter<-aov(litter~dist+canopy+dist:canopy,data=litter.df)
summary.aov(litter)

# calculate % difference in significantly different means
mean(mydata$litter[mydata$dist=="Ca"],na.rm=T)
mean(mydata$litter[mydata$dist=="Co"])
mean(mydata$litter[mydata$canopy=="1"],na.rm=T)
mean(mydata$litter[mydata$canopy=="2"])

#this creates a data frame from mydata including only specified values
GWC.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                       canopy=NA,
                       GWC=NA,
                       campsite=NA)

# calculate means of replicates for GWC data
Ca.1.485 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$GWC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$GWC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$GWC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
GWC.df<-rbind(GWC.df,list("Ca","1",Ca.1.485,"485"))
GWC.df<-rbind(GWC.df,list("Ca","2",Ca.2.485,"485"))
GWC.df<-rbind(GWC.df,list("Re","1",Re.1.485,"485"))
GWC.df<-rbind(GWC.df,list("Re","2",Re.2.485,"485"))
GWC.df<-rbind(GWC.df,list("Co","1",Co.1.485,"485"))
GWC.df<-rbind(GWC.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
GWC.df<-rbind(GWC.df,list("Ca","1",Ca.1.413,"413"))
GWC.df<-rbind(GWC.df,list("Ca","2",Ca.2.413,"413"))
GWC.df<-rbind(GWC.df,list("Re","1",Re.1.413,"413"))
GWC.df<-rbind(GWC.df,list("Re","2",Re.2.413,"413"))
GWC.df<-rbind(GWC.df,list("Co","1",Co.1.413,"413"))
GWC.df<-rbind(GWC.df,list("Co","2",Co.2.413,"413"))

GWC.df<-rbind(GWC.df,list("Ca","1",Ca.1.4174,"4174"))
GWC.df<-rbind(GWC.df,list("Ca","2",Ca.2.4174,"4174"))
GWC.df<-rbind(GWC.df,list("Re","1",Re.1.4174,"4174"))
GWC.df<-rbind(GWC.df,list("Re","2",Re.2.4174,"4174"))
GWC.df<-rbind(GWC.df,list("Co","1",Co.1.4174,"4174"))
GWC.df<-rbind(GWC.df,list("Co","2",Co.2.4174,"4174"))

GWC.df<-rbind(GWC.df,list("Ca","1",Ca.1.4175,"4175"))
GWC.df<-rbind(GWC.df,list("Ca","2",Ca.2.4175,"4175"))
GWC.df<-rbind(GWC.df,list("Re","1",Re.1.4175,"4175"))
GWC.df<-rbind(GWC.df,list("Re","2",Re.2.4175,"4175"))
GWC.df<-rbind(GWC.df,list("Co","1",Co.1.4175,"4175"))
GWC.df<-rbind(GWC.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
GWC.df<-na.omit(GWC.df)


#run ANOVA on GWC
GWC<-aov(GWC~dist+canopy+dist:canopy,data=GWC.df)
summary.aov(GWC)

# calculate % difference in means
mean(mydata$GWC[mydata$canopy=="1"],na.rm=T)
mean(mydata$GWC[mydata$canopy=="2"],na.rm=T)

#this creates a data frame from mydata including only specified values
pH.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    pH=NA,
                    campsite=NA)

# calculate means of replicates for pH data
Ca.1.485 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$pH[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$pH[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$pH[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
pH.df<-rbind(pH.df,list("Ca","1",Ca.1.485,"485"))
pH.df<-rbind(pH.df,list("Ca","2",Ca.2.485,"485"))
pH.df<-rbind(pH.df,list("Re","1",Re.1.485,"485"))
pH.df<-rbind(pH.df,list("Re","2",Re.2.485,"485"))
pH.df<-rbind(pH.df,list("Co","1",Co.1.485,"485"))
pH.df<-rbind(pH.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
pH.df<-rbind(pH.df,list("Ca","1",Ca.1.413,"413"))
pH.df<-rbind(pH.df,list("Ca","2",Ca.2.413,"413"))
pH.df<-rbind(pH.df,list("Re","1",Re.1.413,"413"))
pH.df<-rbind(pH.df,list("Re","2",Re.2.413,"413"))
pH.df<-rbind(pH.df,list("Co","1",Co.1.413,"413"))
pH.df<-rbind(pH.df,list("Co","2",Co.2.413,"413"))

pH.df<-rbind(pH.df,list("Ca","1",Ca.1.4174,"4174"))
pH.df<-rbind(pH.df,list("Ca","2",Ca.2.4174,"4174"))
pH.df<-rbind(pH.df,list("Re","1",Re.1.4174,"4174"))
pH.df<-rbind(pH.df,list("Re","2",Re.2.4174,"4174"))
pH.df<-rbind(pH.df,list("Co","1",Co.1.4174,"4174"))
pH.df<-rbind(pH.df,list("Co","2",Co.2.4174,"4174"))

pH.df<-rbind(pH.df,list("Ca","1",Ca.1.4175,"4175"))
pH.df<-rbind(pH.df,list("Ca","2",Ca.2.4175,"4175"))
pH.df<-rbind(pH.df,list("Re","1",Re.1.4175,"4175"))
pH.df<-rbind(pH.df,list("Re","2",Re.2.4175,"4175"))
pH.df<-rbind(pH.df,list("Co","1",Co.1.4175,"4175"))
pH.df<-rbind(pH.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
pH.df<-na.omit(pH.df)

#run ANOVA on pH
pH<-aov(pH~dist+canopy+dist:canopy,data=pH.df)
summary.aov(pH)

#this creates a data frame from mydata including only specified values
DOC.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                   canopy=NA,
                   DOC=NA,
                   campsite=NA)

# calculate means of replicates for DOC data
Ca.1.485 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$DOC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$DOC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$DOC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
DOC.df<-rbind(DOC.df,list("Ca","1",Ca.1.485,"485"))
DOC.df<-rbind(DOC.df,list("Ca","2",Ca.2.485,"485"))
DOC.df<-rbind(DOC.df,list("Re","1",Re.1.485,"485"))
DOC.df<-rbind(DOC.df,list("Re","2",Re.2.485,"485"))
DOC.df<-rbind(DOC.df,list("Co","1",Co.1.485,"485"))
DOC.df<-rbind(DOC.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
DOC.df<-rbind(DOC.df,list("Ca","1",Ca.1.413,"413"))
DOC.df<-rbind(DOC.df,list("Ca","2",Ca.2.413,"413"))
DOC.df<-rbind(DOC.df,list("Re","1",Re.1.413,"413"))
DOC.df<-rbind(DOC.df,list("Re","2",Re.2.413,"413"))
DOC.df<-rbind(DOC.df,list("Co","1",Co.1.413,"413"))
DOC.df<-rbind(DOC.df,list("Co","2",Co.2.413,"413"))

DOC.df<-rbind(DOC.df,list("Ca","1",Ca.1.4174,"4174"))
DOC.df<-rbind(DOC.df,list("Ca","2",Ca.2.4174,"4174"))
DOC.df<-rbind(DOC.df,list("Re","1",Re.1.4174,"4174"))
DOC.df<-rbind(DOC.df,list("Re","2",Re.2.4174,"4174"))
DOC.df<-rbind(DOC.df,list("Co","1",Co.1.4174,"4174"))
DOC.df<-rbind(DOC.df,list("Co","2",Co.2.4174,"4174"))

DOC.df<-rbind(DOC.df,list("Ca","1",Ca.1.4175,"4175"))
DOC.df<-rbind(DOC.df,list("Ca","2",Ca.2.4175,"4175"))
DOC.df<-rbind(DOC.df,list("Re","1",Re.1.4175,"4175"))
DOC.df<-rbind(DOC.df,list("Re","2",Re.2.4175,"4175"))
DOC.df<-rbind(DOC.df,list("Co","1",Co.1.4175,"4175"))
DOC.df<-rbind(DOC.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
DOC.df<-na.omit(DOC.df)

#run ANOVA on DOC
DOC<-aov(DOC~dist+canopy+dist:canopy,data=DOC.df)
summary.aov(DOC)

# calculate % difference in significantly different means
mean(mydata$DOC[mydata$canopy=="1"],na.rm=T)
mean(mydata$DOC[mydata$canopy=="2"])


#this creates a data frame from mydata including only specified values
TDN.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    TDN=NA,
                    campsite=NA)

# calculate means of replicates for TDN data
Ca.1.485 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$TDN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$TDN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$TDN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
TDN.df<-rbind(TDN.df,list("Ca","1",Ca.1.485,"485"))
TDN.df<-rbind(TDN.df,list("Ca","2",Ca.2.485,"485"))
TDN.df<-rbind(TDN.df,list("Re","1",Re.1.485,"485"))
TDN.df<-rbind(TDN.df,list("Re","2",Re.2.485,"485"))
TDN.df<-rbind(TDN.df,list("Co","1",Co.1.485,"485"))
TDN.df<-rbind(TDN.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
TDN.df<-rbind(TDN.df,list("Ca","1",Ca.1.413,"413"))
TDN.df<-rbind(TDN.df,list("Ca","2",Ca.2.413,"413"))
TDN.df<-rbind(TDN.df,list("Re","1",Re.1.413,"413"))
TDN.df<-rbind(TDN.df,list("Re","2",Re.2.413,"413"))
TDN.df<-rbind(TDN.df,list("Co","1",Co.1.413,"413"))
TDN.df<-rbind(TDN.df,list("Co","2",Co.2.413,"413"))

TDN.df<-rbind(TDN.df,list("Ca","1",Ca.1.4174,"4174"))
TDN.df<-rbind(TDN.df,list("Ca","2",Ca.2.4174,"4174"))
TDN.df<-rbind(TDN.df,list("Re","1",Re.1.4174,"4174"))
TDN.df<-rbind(TDN.df,list("Re","2",Re.2.4174,"4174"))
TDN.df<-rbind(TDN.df,list("Co","1",Co.1.4174,"4174"))
TDN.df<-rbind(TDN.df,list("Co","2",Co.2.4174,"4174"))

TDN.df<-rbind(TDN.df,list("Ca","1",Ca.1.4175,"4175"))
TDN.df<-rbind(TDN.df,list("Ca","2",Ca.2.4175,"4175"))
TDN.df<-rbind(TDN.df,list("Re","1",Re.1.4175,"4175"))
TDN.df<-rbind(TDN.df,list("Re","2",Re.2.4175,"4175"))
TDN.df<-rbind(TDN.df,list("Co","1",Co.1.4175,"4175"))
TDN.df<-rbind(TDN.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
TDN.df<-na.omit(TDN.df)

#run ANOVA on DON
TDN<-aov(TDN~dist+canopy+dist:canopy,data=TDN.df)
summary.aov(TDN)

# calculate % difference in significantly different means
mean(mydata$TDN[mydata$canopy=="1"],na.rm=T)
mean(mydata$TDN[mydata$canopy=="2"])

#this creates a data frame from mydata including only specified values
temp.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    temp=NA,
                    campsite=NA)

# calculate means of replicates for temperature data
Ca.1.485 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$temp[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$temp[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$temp[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
temp.df<-rbind(temp.df,list("Ca","1",Ca.1.485,"485"))
temp.df<-rbind(temp.df,list("Ca","2",Ca.2.485,"485"))
temp.df<-rbind(temp.df,list("Re","1",Re.1.485,"485"))
temp.df<-rbind(temp.df,list("Re","2",Re.2.485,"485"))
temp.df<-rbind(temp.df,list("Co","1",Co.1.485,"485"))
temp.df<-rbind(temp.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
temp.df<-rbind(temp.df,list("Ca","1",Ca.1.413,"413"))
temp.df<-rbind(temp.df,list("Ca","2",Ca.2.413,"413"))
temp.df<-rbind(temp.df,list("Re","1",Re.1.413,"413"))
temp.df<-rbind(temp.df,list("Re","2",Re.2.413,"413"))
temp.df<-rbind(temp.df,list("Co","1",Co.1.413,"413"))
temp.df<-rbind(temp.df,list("Co","2",Co.2.413,"413"))

temp.df<-rbind(temp.df,list("Ca","1",Ca.1.4174,"4174"))
temp.df<-rbind(temp.df,list("Ca","2",Ca.2.4174,"4174"))
temp.df<-rbind(temp.df,list("Re","1",Re.1.4174,"4174"))
temp.df<-rbind(temp.df,list("Re","2",Re.2.4174,"4174"))
temp.df<-rbind(temp.df,list("Co","1",Co.1.4174,"4174"))
temp.df<-rbind(temp.df,list("Co","2",Co.2.4174,"4174"))

temp.df<-rbind(temp.df,list("Ca","1",Ca.1.4175,"4175"))
temp.df<-rbind(temp.df,list("Ca","2",Ca.2.4175,"4175"))
temp.df<-rbind(temp.df,list("Re","1",Re.1.4175,"4175"))
temp.df<-rbind(temp.df,list("Re","2",Re.2.4175,"4175"))
temp.df<-rbind(temp.df,list("Co","1",Co.1.4175,"4175"))
temp.df<-rbind(temp.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
temp.df<-na.omit(temp.df)

#run ANOVA on temperature
temp<-aov(temp~dist+canopy+dist:canopy,data=temp.df)
summary.aov(temp)

#this creates a data frame from mydata including only specified values
CN.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                     canopy=NA,
                     CN=NA,
                     campsite=NA)

# calculate means of replicates for C:N ratio data
Ca.1.485 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$C_N[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$C_N[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$C_N[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
CN.df<-rbind(CN.df,list("Ca","1",Ca.1.485,"485"))
CN.df<-rbind(CN.df,list("Ca","2",Ca.2.485,"485"))
CN.df<-rbind(CN.df,list("Re","1",Re.1.485,"485"))
CN.df<-rbind(CN.df,list("Re","2",Re.2.485,"485"))
CN.df<-rbind(CN.df,list("Co","1",Co.1.485,"485"))
CN.df<-rbind(CN.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
CN.df<-rbind(CN.df,list("Ca","1",Ca.1.413,"413"))
CN.df<-rbind(CN.df,list("Ca","2",Ca.2.413,"413"))
CN.df<-rbind(CN.df,list("Re","1",Re.1.413,"413"))
CN.df<-rbind(CN.df,list("Re","2",Re.2.413,"413"))
CN.df<-rbind(CN.df,list("Co","1",Co.1.413,"413"))
CN.df<-rbind(CN.df,list("Co","2",Co.2.413,"413"))

CN.df<-rbind(CN.df,list("Ca","1",Ca.1.4174,"4174"))
CN.df<-rbind(CN.df,list("Ca","2",Ca.2.4174,"4174"))
CN.df<-rbind(CN.df,list("Re","1",Re.1.4174,"4174"))
CN.df<-rbind(CN.df,list("Re","2",Re.2.4174,"4174"))
CN.df<-rbind(CN.df,list("Co","1",Co.1.4174,"4174"))
CN.df<-rbind(CN.df,list("Co","2",Co.2.4174,"4174"))

CN.df<-rbind(CN.df,list("Ca","1",Ca.1.4175,"4175"))
CN.df<-rbind(CN.df,list("Ca","2",Ca.2.4175,"4175"))
CN.df<-rbind(CN.df,list("Re","1",Re.1.4175,"4175"))
CN.df<-rbind(CN.df,list("Re","2",Re.2.4175,"4175"))
CN.df<-rbind(CN.df,list("Co","1",Co.1.4175,"4175"))
CN.df<-rbind(CN.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
CN.df<-na.omit(CN.df)

#run ANOVA on C:N ratio
CN<-aov(CN~dist+canopy+dist:canopy,data=CN.df)
summary.aov(CN)
