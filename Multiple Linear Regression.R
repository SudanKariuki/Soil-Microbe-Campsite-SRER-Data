# Sudan Kariuki. MS Thesis
# Conduct multiple linear regressions on soil microbial activity measures

# clear the enviroment
rm(list=ls(all=TRUE))

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

# response variable: microbial biomass C
# create a dataframe
micc.df <-data.frame(micc=mydata$micC,
                     litter=mydata$litter,
                     GWC=mydata$GWC,
                     cover=mydata$basal_percent,
                     CN=mydata$C_N,
                     canopy=mydata$canopy)

na.rm=T

# Conduct a multiple linear regression analysis
micc <- lm(micc~litter+GWC+cover+canopy+CN, data=micc.df)
summary(micc)
AIC(micc)

# use AIC to select best fit model
micc_selected<-MASS::stepAIC(micc, direction="both")
AIC(micc_selected)
summary(micc_selected)

# if >1 term is significant in selected model, try interaction
# where 3 or more variables are significant, try interactions in pairs 
miccI <- lm(micc~GWC * canopy,data=na.omit(micc.df))
summary(miccI)
#summary shows significant interaction
AIC(miccI)

micc_selected<-lm(micc~GWC * canopy,data=na.omit(micc.df))

# You can also define your highest order and lowest order models and use them in stepAIC

# quine.hi <- aov(log(Days + 2.5) ~ (Eth+Sex+Age+Lrn)^4, quine)
# quine.stp <- stepAIC(quine.hi,
                    # scope = list(lower = ~1),
                    # trace = FALSE)

# REPEAT FOR MICN
# response variable: microbial biomass N
# create a dataframe
micn.df <-data.frame(micn=mydata$micN,
                     litter=mydata$litter,
                     GWC=mydata$GWC,
                     cover=mydata$basal_percent,
                     CN=mydata$C_N,
                     canopy=mydata$canopy)

# Conduct a multiple linear regression analysis
micn <- lm(micn~litter+GWC+cover+canopy+CN,data=micn.df)
summary(micn)

# use AIC to select best fit model
micn_selected<-MASS::stepAIC(micn, direction="both")
AIC(micn_selected)
summary(micn_selected)


# enzyme activity at 25 degrees
# response variable: carbon EEA at 25 degrees
# create a dataframe
C25.df <-data.frame(C25=(mydata$BG_25+mydata$CB_25+mydata$AG_25+mydata$XYL_25),
                    litter=mydata$litter,
                    CN=mydata$C_N,
                    GWC=mydata$GWC,
                    cover=mydata$basal_percent,
                    canopy=mydata$canopy)

# remove missing values from C25.df 
na.rm=T

# Conduct a multiple linear regression analysis
C25 <- lm(C25~litter+GWC+cover+canopy+CN,data=na.omit(C25.df))
summary(C25)

# use AIC to select best fit model
C25_selected<-MASS::stepAIC(C25, direction="both")
AIC(C25_selected)
summary(C25_selected)

# response variable: nitrogen EEA at 25 degrees
# create a dataframe
N25.df <-data.frame(N25=(mydata$LAP_25+mydata$NAG_25),
                    litter=mydata$litter,
                    GWC=mydata$GWC,
                    cover=mydata$basal_percent,
                    canopy=mydata$canopy,
                    CN=mydata$C_N)


#Conduct a multiple linear regression analysis
N25 <- lm(N25~litter+GWC+cover+canopy+CN,data=na.omit(N25.df))
summary(N25)

# use AIC to select best fit model
N25_selected<-MASS::stepAIC(N25, direction="both")
AIC(N25_selected)
summary(N25_selected)

# if >1 term is significant in selected model, try interaction
# where 3 or more variables are significant, try interactions in pairs 
N25 <- lm(N25~litter * GWC,data=na.omit(N25.df))
summary(N25)

# R can generate a multiple regression table for you
library(emmeans)
emmeans(N25, specs=~litter * GWC, at=list(litter=c(3,4,5),GWC=c(0,5,10)))

emmip(N25, formula=litter ~ GWC, at=list(litter=c(3,4,5),GWC=c(0,5,10)), CIs=T)

emmip(N25, formula= GWC~litter, at=list(litter=c(3,4,5),GWC=c(0,5,10)), CIs=T)


# response variable: phos EEA at 25 degrees
# create a dataframe
P25.df <-data.frame(P25=mydata$PHOS_25,
                    litter=mydata$litter,
                    GWC=mydata$GWC,
                    cover=mydata$basal_percent,
                    canopy=mydata$canopy,
                    CN=mydata$C_N)

#Conduct a multiple linear regression analysis
P25 <- lm(P25~litter+GWC+cover+canopy+CN,data=na.omit(P25.df))
summary(P25)

# use AIC to select best fit model
P25_selected<-MASS::stepAIC(P25, direction="both")
AIC(P25_selected)
summary(P25_selected)

#stargazer package can be used to generate and format regression tables in R
#library(stargazer)
#stargazer(P25,N25,type="text" )
#?stargazer

# REPEAT for enzyme activity at 35 degrees
# response variable: carbon EEA at 35 degrees
# create a dataframe
C35.df <-data.frame(C35=(mydata$BG_35+mydata$CB_35+mydata$AG_35+mydata$XYL_35),
                    litter=mydata$litter,
                    GWC=mydata$GWC,
                    cover=mydata$basal_percent,
                    canopy=mydata$canopy,
                    CN=mydata$C_N)

# Conduct a multiple linear regression analysis
C35 <- lm(C35~litter+GWC+cover+canopy+CN,data=na.omit(C35.df))
summary(C35)

# use AIC to select best fit model
C35_selected<-MASS::stepAIC(C35, direction="both")
AIC(C35_selected)
summary(C35_selected)

# response variable: nitrogen EEA at 35 degrees
# create a dataframe
N35.df <-data.frame(N35=(mydata$LAP_35+mydata$NAG_35),
                    litter=mydata$litter,
                    GWC=mydata$GWC,
                    cover=mydata$basal_percent,
                    canopy=mydata$canopy,
                    CN=mydata$C_N)

#Conduct a multiple linear regression analysis
N35 <- lm(N35~litter+GWC+cover+canopy+CN,data=na.omit(N35.df))
summary(N35)

# use AIC to select best fit model
N35_selected<-MASS::stepAIC(N35, direction="both")
AIC(N35_selected)
summary(N35_selected)

# if >1 term is significant in selected model, try interaction
# where 3 or more variables are significant, try interactions in pairs 
N35 <- lm(N35~canopy * GWC,data=na.omit(N35.df))
summary(N35)
# summary shows that interaction is significant
AIC(N35)




# response variable: phos EEA at 35 degrees
# create a dataframe
P35.df <-data.frame(P35=mydata$PHOS_35,
                    litter=mydata$litter,
                    GWC=mydata$GWC,
                    cover=mydata$basal_percent,
                    CN=mydata$C_N,
                    canopy=mydata$canopy)

#Conduct a multiple linear regression analysis
P35 <- lm(P35~litter+GWC+cover+canopy+CN,data=na.omit(P35.df))
summary(P35)

# use AIC to select best fit model
P35_selected<-MASS::stepAIC(P35, direction="both")
AIC(P35_selected)
summary(P35_selected)

# REPEAT FOR SPECIFIC ACTIVITY
# response variable: carbon EEA at 35 degrees
# create a dataframe
sac.df <-data.frame(sac=mydata$SAC,
                     litter=mydata$litter,
                     GWC=mydata$GWC,
                     cover=mydata$basal_percent,
                     CN=mydata$C_N,
                     canopy=mydata$canopy)

na.rm=T

