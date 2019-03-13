# Sudan Kariuki. MS Thesis
# Run 2 factor ANOVAs on microbial activity measures

rm(list=ls(all= TRUE))

mydata<-read.csv("MASTER.csv")

# force R to recognize your categorical variables as "factors"
mydata$dist<-as.factor(mydata$dist)
mydata$canopy<-as.factor(mydata$canopy)
mydata$reps<-as.factor(mydata$reps)
mydata$basal_class<-as.factor(mydata$basal_class)
mydata$campsite<-as.factor(mydata$campsite)
mydata$Site_ID<-as.factor(mydata$Site_ID)
mydata$SAN<-as.numeric(mydata$SAN)


summary(mydata)

# create a data frame from mydata including only specified values
micC.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                         canopy=NA,
                         micC=NA,
                         campsite=NA)

na.rm=T

# calculate means of replicates for microbial biomass C data
Ca.1.485 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$micC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$micC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$micC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
micC.df<-rbind(micC.df,list("Ca","1",Ca.1.485,"485"))
micC.df<-rbind(micC.df,list("Ca","2",Ca.2.485,"485"))
micC.df<-rbind(micC.df,list("Re","1",Re.1.485,"485"))
micC.df<-rbind(micC.df,list("Re","2",Re.2.485,"485"))
micC.df<-rbind(micC.df,list("Co","1",Co.1.485,"485"))
micC.df<-rbind(micC.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
micC.df<-rbind(micC.df,list("Ca","1",Ca.1.413,"413"))
micC.df<-rbind(micC.df,list("Ca","2",Ca.2.413,"413"))
micC.df<-rbind(micC.df,list("Re","1",Re.1.413,"413"))
micC.df<-rbind(micC.df,list("Re","2",Re.2.413,"413"))
micC.df<-rbind(micC.df,list("Co","1",Co.1.413,"413"))
micC.df<-rbind(micC.df,list("Co","2",Co.2.413,"413"))

micC.df<-rbind(micC.df,list("Ca","1",Ca.1.4174,"4174"))
micC.df<-rbind(micC.df,list("Ca","2",Ca.2.4174,"4174"))
micC.df<-rbind(micC.df,list("Re","1",Re.1.4174,"4174"))
micC.df<-rbind(micC.df,list("Re","2",Re.2.4174,"4174"))
micC.df<-rbind(micC.df,list("Co","1",Co.1.4174,"4174"))
micC.df<-rbind(micC.df,list("Co","2",Co.2.4174,"4174"))

micC.df<-rbind(micC.df,list("Ca","1",Ca.1.4175,"4175"))
micC.df<-rbind(micC.df,list("Ca","2",Ca.2.4175,"4175"))
micC.df<-rbind(micC.df,list("Re","1",Re.1.4175,"4175"))
micC.df<-rbind(micC.df,list("Re","2",Re.2.4175,"4175"))
micC.df<-rbind(micC.df,list("Co","1",Co.1.4175,"4175"))
micC.df<-rbind(micC.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
micC.df<-na.omit(micC.df)

#run ANOVA on micC
micc<-aov(micC~dist+canopy+dist:canopy,data=micC.df)
summary.aov(micc)

# calculate % difference in significantly different means
mean(micC.df$micC[mydata$canopy=="1"],na.rm=T)
mean(micC.df$micC[mydata$canopy=="2"],na.rm=T)
mean(micC.df$micC[mydata$dist=="Ca"],na.rm=T)
mean(micC.df$micC[mydata$dist=="Co"],na.rm=T)


#create a data frame
micN.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                     canopy=NA,
                     micN=NA,
                     campsite=NA)

# calculate means of replicates for microbial biomass N data
Ca.1.485 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$micN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$micN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$micN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
micN.df<-rbind(micN.df,list("Ca","1",Ca.1.485,"485"))
micN.df<-rbind(micN.df,list("Ca","2",Ca.2.485,"485"))
micN.df<-rbind(micN.df,list("Re","1",Re.1.485,"485"))
micN.df<-rbind(micN.df,list("Re","2",Re.2.485,"485"))
micN.df<-rbind(micN.df,list("Co","1",Co.1.485,"485"))
micN.df<-rbind(micN.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
micN.df<-rbind(micN.df,list("Ca","1",Ca.1.413,"413"))
micN.df<-rbind(micN.df,list("Ca","2",Ca.2.413,"413"))
micN.df<-rbind(micN.df,list("Re","1",Re.1.413,"413"))
micN.df<-rbind(micN.df,list("Re","2",Re.2.413,"413"))
micN.df<-rbind(micN.df,list("Co","1",Co.1.413,"413"))
micN.df<-rbind(micN.df,list("Co","2",Co.2.413,"413"))

micN.df<-rbind(micN.df,list("Ca","1",Ca.1.4174,"4174"))
micN.df<-rbind(micN.df,list("Ca","2",Ca.2.4174,"4174"))
micN.df<-rbind(micN.df,list("Re","1",Re.1.4174,"4174"))
micN.df<-rbind(micN.df,list("Re","2",Re.2.4174,"4174"))
micN.df<-rbind(micN.df,list("Co","1",Co.1.4174,"4174"))
micN.df<-rbind(micN.df,list("Co","2",Co.2.4174,"4174"))

micN.df<-rbind(micN.df,list("Ca","1",Ca.1.4175,"4175"))
micN.df<-rbind(micN.df,list("Ca","2",Ca.2.4175,"4175"))
micN.df<-rbind(micN.df,list("Re","1",Re.1.4175,"4175"))
micN.df<-rbind(micN.df,list("Re","2",Re.2.4175,"4175"))
micN.df<-rbind(micN.df,list("Co","1",Co.1.4175,"4175"))
micN.df<-rbind(micN.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
micN.df<-na.omit(micN.df)

#run ANOVA on micN
micn<-aov(micN~dist+canopy+dist:canopy,data=micN.df)
summary.aov(micn)

# calculate % difference in significantly different means
mean(micN.df$micN[mydata$canopy=="1"],na.rm=T)
mean(micN.df$micN[mydata$canopy=="2"],na.rm=T)
mean(micN.df$micN[mydata$dist=="Ca"],na.rm=T)
mean(micN.df$micN[mydata$dist=="Co"],na.rm=T)

#create a data frame
C25.df <-data.frame(dist=factor(NA, levels=c("Ca","Re","Co")),
                         canopy=NA,
                         C25=NA,
                         campsite=NA)


# calculate means of replicates for C25 data
Ca.1.485 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean((mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
C25.df<-rbind(C25.df,list("Ca","1",Ca.1.485,"485"))
C25.df<-rbind(C25.df,list("Ca","2",Ca.2.485,"485"))
C25.df<-rbind(C25.df,list("Re","1",Re.1.485,"485"))
C25.df<-rbind(C25.df,list("Re","2",Re.2.485,"485"))
C25.df<-rbind(C25.df,list("Co","1",Co.1.485,"485"))
C25.df<-rbind(C25.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
C25.df<-rbind(C25.df,list("Ca","1",Ca.1.413,"413"))
C25.df<-rbind(C25.df,list("Ca","2",Ca.2.413,"413"))
C25.df<-rbind(C25.df,list("Re","1",Re.1.413,"413"))
C25.df<-rbind(C25.df,list("Re","2",Re.2.413,"413"))
C25.df<-rbind(C25.df,list("Co","1",Co.1.413,"413"))
C25.df<-rbind(C25.df,list("Co","2",Co.2.413,"413"))

C25.df<-rbind(C25.df,list("Ca","1",Ca.1.4174,"4174"))
C25.df<-rbind(C25.df,list("Ca","2",Ca.2.4174,"4174"))
C25.df<-rbind(C25.df,list("Re","1",Re.1.4174,"4174"))
C25.df<-rbind(C25.df,list("Re","2",Re.2.4174,"4174"))
C25.df<-rbind(C25.df,list("Co","1",Co.1.4174,"4174"))
C25.df<-rbind(C25.df,list("Co","2",Co.2.4174,"4174"))

C25.df<-rbind(C25.df,list("Ca","1",Ca.1.4175,"4175"))
C25.df<-rbind(C25.df,list("Ca","2",Ca.2.4175,"4175"))
C25.df<-rbind(C25.df,list("Re","1",Re.1.4175,"4175"))
C25.df<-rbind(C25.df,list("Re","2",Re.2.4175,"4175"))
C25.df<-rbind(C25.df,list("Co","1",Co.1.4175,"4175"))
C25.df<-rbind(C25.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
C25.df<-na.omit(C25.df)

#run ANOVA on C enzymes
C25<-aov(C25~dist+canopy+dist:canopy,data=C25.df)
summary.aov(C25)

#create a data frame
N25.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    N25=NA,
                    campsite=NA)


# calculate means of replicates for N25 data
Ca.1.485 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean((mydata$LAP_25+mydata$NAG_25)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
N25.df<-rbind(N25.df,list("Ca","1",Ca.1.485,"485"))
N25.df<-rbind(N25.df,list("Ca","2",Ca.2.485,"485"))
N25.df<-rbind(N25.df,list("Re","1",Re.1.485,"485"))
N25.df<-rbind(N25.df,list("Re","2",Re.2.485,"485"))
N25.df<-rbind(N25.df,list("Co","1",Co.1.485,"485"))
N25.df<-rbind(N25.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
N25.df<-rbind(N25.df,list("Ca","1",Ca.1.413,"413"))
N25.df<-rbind(N25.df,list("Ca","2",Ca.2.413,"413"))
N25.df<-rbind(N25.df,list("Re","1",Re.1.413,"413"))
N25.df<-rbind(N25.df,list("Re","2",Re.2.413,"413"))
N25.df<-rbind(N25.df,list("Co","1",Co.1.413,"413"))
N25.df<-rbind(N25.df,list("Co","2",Co.2.413,"413"))

N25.df<-rbind(N25.df,list("Ca","1",Ca.1.4174,"4174"))
N25.df<-rbind(N25.df,list("Ca","2",Ca.2.4174,"4174"))
N25.df<-rbind(N25.df,list("Re","1",Re.1.4174,"4174"))
N25.df<-rbind(N25.df,list("Re","2",Re.2.4174,"4174"))
N25.df<-rbind(N25.df,list("Co","1",Co.1.4174,"4174"))
N25.df<-rbind(N25.df,list("Co","2",Co.2.4174,"4174"))

N25.df<-rbind(N25.df,list("Ca","1",Ca.1.4175,"4175"))
N25.df<-rbind(N25.df,list("Ca","2",Ca.2.4175,"4175"))
N25.df<-rbind(N25.df,list("Re","1",Re.1.4175,"4175"))
N25.df<-rbind(N25.df,list("Re","2",Re.2.4175,"4175"))
N25.df<-rbind(N25.df,list("Co","1",Co.1.4175,"4175"))
N25.df<-rbind(N25.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
N25.df<-na.omit(N25.df)

#run ANOVA on C enzymes
N25<-aov(N25~dist+canopy+dist:canopy,data=N25.df)
summary.aov(N25)

#create a data frame
P25.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    P25=NA,
                    campsite=NA)


# calculate means of replicates for P25 data
Ca.1.485 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$PHOS_25[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$PHOS_25[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$PHOS_25[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
P25.df<-rbind(P25.df,list("Ca","1",Ca.1.485,"485"))
P25.df<-rbind(P25.df,list("Ca","2",Ca.2.485,"485"))
P25.df<-rbind(P25.df,list("Re","1",Re.1.485,"485"))
P25.df<-rbind(P25.df,list("Re","2",Re.2.485,"485"))
P25.df<-rbind(P25.df,list("Co","1",Co.1.485,"485"))
P25.df<-rbind(P25.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
P25.df<-rbind(P25.df,list("Ca","1",Ca.1.413,"413"))
P25.df<-rbind(P25.df,list("Ca","2",Ca.2.413,"413"))
P25.df<-rbind(P25.df,list("Re","1",Re.1.413,"413"))
P25.df<-rbind(P25.df,list("Re","2",Re.2.413,"413"))
P25.df<-rbind(P25.df,list("Co","1",Co.1.413,"413"))
P25.df<-rbind(P25.df,list("Co","2",Co.2.413,"413"))

P25.df<-rbind(P25.df,list("Ca","1",Ca.1.4174,"4174"))
P25.df<-rbind(P25.df,list("Ca","2",Ca.2.4174,"4174"))
P25.df<-rbind(P25.df,list("Re","1",Re.1.4174,"4174"))
P25.df<-rbind(P25.df,list("Re","2",Re.2.4174,"4174"))
P25.df<-rbind(P25.df,list("Co","1",Co.1.4174,"4174"))
P25.df<-rbind(P25.df,list("Co","2",Co.2.4174,"4174"))

P25.df<-rbind(P25.df,list("Ca","1",Ca.1.4175,"4175"))
P25.df<-rbind(P25.df,list("Ca","2",Ca.2.4175,"4175"))
P25.df<-rbind(P25.df,list("Re","1",Re.1.4175,"4175"))
P25.df<-rbind(P25.df,list("Re","2",Re.2.4175,"4175"))
P25.df<-rbind(P25.df,list("Co","1",Co.1.4175,"4175"))
P25.df<-rbind(P25.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
P25.df<-na.omit(P25.df)


#run ANOVA on C enzymes
P25<-aov(P25~dist+canopy+dist:canopy,data=P25.df)
summary.aov(P25)

# create a dataframe for enzymes assayed at 25 degrees C
EEA25.df <-data.frame(dist=factor(mydata$dist,levels=c("Ca","Re","Co")),
                    canopy=mydata$canopy,
                    EEA25=(C25.df$C25+N25.df$N25+P25.df$P25))

# calculate % difference in significantly different means
mean(EEA25.df$EEA25[mydata$canopy=="1"],na.rm=T)
mean(EEA25.df$EEA25[mydata$canopy=="2"],na.rm=T)

#create a data frame
C35.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    C35=NA,
                    campsite=NA)

# calculate means of replicates for C35 data
Ca.1.485 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean((mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
C35.df<-rbind(C35.df,list("Ca","1",Ca.1.485,"485"))
C35.df<-rbind(C35.df,list("Ca","2",Ca.2.485,"485"))
C35.df<-rbind(C35.df,list("Re","1",Re.1.485,"485"))
C35.df<-rbind(C35.df,list("Re","2",Re.2.485,"485"))
C35.df<-rbind(C35.df,list("Co","1",Co.1.485,"485"))
C35.df<-rbind(C35.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
C35.df<-rbind(C35.df,list("Ca","1",Ca.1.413,"413"))
C35.df<-rbind(C35.df,list("Ca","2",Ca.2.413,"413"))
C35.df<-rbind(C35.df,list("Re","1",Re.1.413,"413"))
C35.df<-rbind(C35.df,list("Re","2",Re.2.413,"413"))
C35.df<-rbind(C35.df,list("Co","1",Co.1.413,"413"))
C35.df<-rbind(C35.df,list("Co","2",Co.2.413,"413"))

C35.df<-rbind(C35.df,list("Ca","1",Ca.1.4174,"4174"))
C35.df<-rbind(C35.df,list("Ca","2",Ca.2.4174,"4174"))
C35.df<-rbind(C35.df,list("Re","1",Re.1.4174,"4174"))
C35.df<-rbind(C35.df,list("Re","2",Re.2.4174,"4174"))
C35.df<-rbind(C35.df,list("Co","1",Co.1.4174,"4174"))
C35.df<-rbind(C35.df,list("Co","2",Co.2.4174,"4174"))

C35.df<-rbind(C35.df,list("Ca","1",Ca.1.4175,"4175"))
C35.df<-rbind(C35.df,list("Ca","2",Ca.2.4175,"4175"))
C35.df<-rbind(C35.df,list("Re","1",Re.1.4175,"4175"))
C35.df<-rbind(C35.df,list("Re","2",Re.2.4175,"4175"))
C35.df<-rbind(C35.df,list("Co","1",Co.1.4175,"4175"))
C35.df<-rbind(C35.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
C35.df<-na.omit(C35.df)

#run ANOVA on C enzymes
C35<-aov(C35~dist+canopy+dist:canopy,data=C35.df)
summary.aov(C35)

#create a data frame
N35.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    N35=NA,
                    campsite=NA)

# calculate means of replicates for N35 data
Ca.1.485 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean((mydata$LAP_35+mydata$NAG_35)[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
N35.df<-rbind(N35.df,list("Ca","1",Ca.1.485,"485"))
N35.df<-rbind(N35.df,list("Ca","2",Ca.2.485,"485"))
N35.df<-rbind(N35.df,list("Re","1",Re.1.485,"485"))
N35.df<-rbind(N35.df,list("Re","2",Re.2.485,"485"))
N35.df<-rbind(N35.df,list("Co","1",Co.1.485,"485"))
N35.df<-rbind(N35.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
N35.df<-rbind(N35.df,list("Ca","1",Ca.1.413,"413"))
N35.df<-rbind(N35.df,list("Ca","2",Ca.2.413,"413"))
N35.df<-rbind(N35.df,list("Re","1",Re.1.413,"413"))
N35.df<-rbind(N35.df,list("Re","2",Re.2.413,"413"))
N35.df<-rbind(N35.df,list("Co","1",Co.1.413,"413"))
N35.df<-rbind(N35.df,list("Co","2",Co.2.413,"413"))

N35.df<-rbind(N35.df,list("Ca","1",Ca.1.4174,"4174"))
N35.df<-rbind(N35.df,list("Ca","2",Ca.2.4174,"4174"))
N35.df<-rbind(N35.df,list("Re","1",Re.1.4174,"4174"))
N35.df<-rbind(N35.df,list("Re","2",Re.2.4174,"4174"))
N35.df<-rbind(N35.df,list("Co","1",Co.1.4174,"4174"))
N35.df<-rbind(N35.df,list("Co","2",Co.2.4174,"4174"))

N35.df<-rbind(N35.df,list("Ca","1",Ca.1.4175,"4175"))
N35.df<-rbind(N35.df,list("Ca","2",Ca.2.4175,"4175"))
N35.df<-rbind(N35.df,list("Re","1",Re.1.4175,"4175"))
N35.df<-rbind(N35.df,list("Re","2",Re.2.4175,"4175"))
N35.df<-rbind(N35.df,list("Co","1",Co.1.4175,"4175"))
N35.df<-rbind(N35.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
N35.df<-na.omit(N35.df)


#run ANOVA on C enzymes
N35<-aov(N35~dist+canopy+dist:canopy,data=N35.df)
summary.aov(N35)

#create a data frame
P35.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    P35=NA,
                    campsite=NA)


# calculate means of replicates for P35 data
Ca.1.485 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$PHOS_35[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$PHOS_35[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$PHOS_35[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
P35.df<-rbind(P35.df,list("Ca","1",Ca.1.485,"485"))
P35.df<-rbind(P35.df,list("Ca","2",Ca.2.485,"485"))
P35.df<-rbind(P35.df,list("Re","1",Re.1.485,"485"))
P35.df<-rbind(P35.df,list("Re","2",Re.2.485,"485"))
P35.df<-rbind(P35.df,list("Co","1",Co.1.485,"485"))
P35.df<-rbind(P35.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
P35.df<-rbind(P35.df,list("Ca","1",Ca.1.413,"413"))
P35.df<-rbind(P35.df,list("Ca","2",Ca.2.413,"413"))
P35.df<-rbind(P35.df,list("Re","1",Re.1.413,"413"))
P35.df<-rbind(P35.df,list("Re","2",Re.2.413,"413"))
P35.df<-rbind(P35.df,list("Co","1",Co.1.413,"413"))
P35.df<-rbind(P35.df,list("Co","2",Co.2.413,"413"))

P35.df<-rbind(P35.df,list("Ca","1",Ca.1.4174,"4174"))
P35.df<-rbind(P35.df,list("Ca","2",Ca.2.4174,"4174"))
P35.df<-rbind(P35.df,list("Re","1",Re.1.4174,"4174"))
P35.df<-rbind(P35.df,list("Re","2",Re.2.4174,"4174"))
P35.df<-rbind(P35.df,list("Co","1",Co.1.4174,"4174"))
P35.df<-rbind(P35.df,list("Co","2",Co.2.4174,"4174"))

P35.df<-rbind(P35.df,list("Ca","1",Ca.1.4175,"4175"))
P35.df<-rbind(P35.df,list("Ca","2",Ca.2.4175,"4175"))
P35.df<-rbind(P35.df,list("Re","1",Re.1.4175,"4175"))
P35.df<-rbind(P35.df,list("Re","2",Re.2.4175,"4175"))
P35.df<-rbind(P35.df,list("Co","1",Co.1.4175,"4175"))
P35.df<-rbind(P35.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
P35.df<-na.omit(P35.df)

#run ANOVA on C enzymes
P35<-aov(P35~dist+canopy+dist:canopy,data=P35.df)
summary.aov(P35)

# create a dataframe for enzymes assayed at 35 degrees C
EEA35.df <-data.frame(dist=factor(mydata$dist,levels=c("Ca","Re","Co")),
                      canopy=mydata$canopy,
                      EEA35=(C35.df$C35+N35.df$N35+P35.df$P35))

# calculate % difference in significantly different means
mean(EEA35.df$EEA35[mydata$canopy=="1"],na.rm=T)
mean(EEA35.df$EEA35[mydata$canopy=="2"],na.rm=T)


#create a data frame
SAC.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                         canopy=NA,
                         SAC=NA,
                         campsite=NA)


# calculate means of replicates for microbial biomass C data
Ca.1.485 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$SAC[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$SAC[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$SAC[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
SAC.df<-rbind(SAC.df,list("Ca","1",Ca.1.485,"485"))
SAC.df<-rbind(SAC.df,list("Ca","2",Ca.2.485,"485"))
SAC.df<-rbind(SAC.df,list("Re","1",Re.1.485,"485"))
SAC.df<-rbind(SAC.df,list("Re","2",Re.2.485,"485"))
SAC.df<-rbind(SAC.df,list("Co","1",Co.1.485,"485"))
SAC.df<-rbind(SAC.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
SAC.df<-rbind(SAC.df,list("Ca","1",Ca.1.413,"413"))
SAC.df<-rbind(SAC.df,list("Ca","2",Ca.2.413,"413"))
SAC.df<-rbind(SAC.df,list("Re","1",Re.1.413,"413"))
SAC.df<-rbind(SAC.df,list("Re","2",Re.2.413,"413"))
SAC.df<-rbind(SAC.df,list("Co","1",Co.1.413,"413"))
SAC.df<-rbind(SAC.df,list("Co","2",Co.2.413,"413"))

SAC.df<-rbind(SAC.df,list("Ca","1",Ca.1.4174,"4174"))
SAC.df<-rbind(SAC.df,list("Ca","2",Ca.2.4174,"4174"))
SAC.df<-rbind(SAC.df,list("Re","1",Re.1.4174,"4174"))
SAC.df<-rbind(SAC.df,list("Re","2",Re.2.4174,"4174"))
SAC.df<-rbind(SAC.df,list("Co","1",Co.1.4174,"4174"))
SAC.df<-rbind(SAC.df,list("Co","2",Co.2.4174,"4174"))

SAC.df<-rbind(SAC.df,list("Ca","1",Ca.1.4175,"4175"))
SAC.df<-rbind(SAC.df,list("Ca","2",Ca.2.4175,"4175"))
SAC.df<-rbind(SAC.df,list("Re","1",Re.1.4175,"4175"))
SAC.df<-rbind(SAC.df,list("Re","2",Re.2.4175,"4175"))
SAC.df<-rbind(SAC.df,list("Co","1",Co.1.4175,"4175"))
SAC.df<-rbind(SAC.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
SAC.df<-na.omit(SAC.df)


#run ANOVA on SAC
SAC<-aov(SAC~dist+canopy+dist:canopy,data=SAC.df)
summary.aov(SAC)

# calculate % difference in significantly different means
mean(SAC.df$SAC[mydata$canopy=="1"],na.rm=T)
mean(SAC.df$SAC[mydata$canopy=="2"],na.rm=T)
mean(SAC.df$SAC[mydata$dist=="Ca"],na.rm=T)
mean(SAC.df$SAC[mydata$dist=="Co"],na.rm=T)

#create a data frame
SAN.df <-data.frame(dist=factor(NA,levels=c("Ca","Re","Co")),
                    canopy=NA,
                    SAN=NA,
                    campsite=NA)


# calculate means of replicates for microbial biomass N data
Ca.1.485 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Ca.2.485 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Re.1.485 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Re.2.485 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)
Co.1.485 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="485"],na.rm=T)
Co.2.485 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="485"],na.rm=T)

# repeat for each campsite
Ca.1.413 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Ca.2.413 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Re.1.413 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Re.2.413 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)
Co.1.413 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="413"],na.rm=T)
Co.2.413 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="413"],na.rm=T)

Ca.1.4174 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Ca.2.4174 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Re.1.4174 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Re.2.4174 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)
Co.1.4174 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4174"],na.rm=T)
Co.2.4174 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4174"],na.rm=T)

Ca.1.4175 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Ca.2.4175 <- mean(mydata$SAN[mydata$dist=="Ca" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Re.1.4175 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Re.2.4175 <- mean(mydata$SAN[mydata$dist=="Re" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)
Co.1.4175 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="1" & mydata$campsite=="4175"],na.rm=T)
Co.2.4175 <- mean(mydata$SAN[mydata$dist=="Co" & mydata$canopy=="2" & mydata$campsite=="4175"],na.rm=T)


# add values to dataframe
SAN.df<-rbind(SAN.df,list("Ca","1",Ca.1.485,"485"))
SAN.df<-rbind(SAN.df,list("Ca","2",Ca.2.485,"485"))
SAN.df<-rbind(SAN.df,list("Re","1",Re.1.485,"485"))
SAN.df<-rbind(SAN.df,list("Re","2",Re.2.485,"485"))
SAN.df<-rbind(SAN.df,list("Co","1",Co.1.485,"485"))
SAN.df<-rbind(SAN.df,list("Co","2",Co.2.485,"485"))

#repeat for each campsite
SAN.df<-rbind(SAN.df,list("Ca","1",Ca.1.413,"413"))
SAN.df<-rbind(SAN.df,list("Ca","2",Ca.2.413,"413"))
SAN.df<-rbind(SAN.df,list("Re","1",Re.1.413,"413"))
SAN.df<-rbind(SAN.df,list("Re","2",Re.2.413,"413"))
SAN.df<-rbind(SAN.df,list("Co","1",Co.1.413,"413"))
SAN.df<-rbind(SAN.df,list("Co","2",Co.2.413,"413"))

SAN.df<-rbind(SAN.df,list("Ca","1",Ca.1.4174,"4174"))
SAN.df<-rbind(SAN.df,list("Ca","2",Ca.2.4174,"4174"))
SAN.df<-rbind(SAN.df,list("Re","1",Re.1.4174,"4174"))
SAN.df<-rbind(SAN.df,list("Re","2",Re.2.4174,"4174"))
SAN.df<-rbind(SAN.df,list("Co","1",Co.1.4174,"4174"))
SAN.df<-rbind(SAN.df,list("Co","2",Co.2.4174,"4174"))

SAN.df<-rbind(SAN.df,list("Ca","1",Ca.1.4175,"4175"))
SAN.df<-rbind(SAN.df,list("Ca","2",Ca.2.4175,"4175"))
SAN.df<-rbind(SAN.df,list("Re","1",Re.1.4175,"4175"))
SAN.df<-rbind(SAN.df,list("Re","2",Re.2.4175,"4175"))
SAN.df<-rbind(SAN.df,list("Co","1",Co.1.4175,"4175"))
SAN.df<-rbind(SAN.df,list("Co","2",Co.2.4175,"4175"))

# remove rows within dataframe which have NA values
SAN.df<-na.omit(SAN.df)

#run ANOVA on SAN
SAN<-aov(SAN~dist+canopy+dist:canopy,data=SAN.df)
summary.aov(SAN)

# calculate % difference in significantly different means
mean(SAN.df$SAN[mydata$canopy=="1"],na.rm=T)
mean(SAN.df$SAN[mydata$canopy=="2"],na.rm=T)
mean(SAN.df$SAN[mydata$dist=="Ca"],na.rm=T)
mean(SAN.df$SAN[mydata$dist=="Co"],na.rm=T)