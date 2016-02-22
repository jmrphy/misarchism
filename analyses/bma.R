### BMA 

require(BMA)
require(compare)
require(foreign)
require(Matching)
require(ggplot2)

setwd("~/Dropbox/gh_projects/misarchism")
source("functions/Altered_BMA_Functions.R")



# Define data matrices

factor.vars$MoralStatismXGovernment<-factor.vars$MoralStatism*factor.vars$Government
attach(factor.vars)
bma.vars<- as.data.frame(cbind(tea_supp, gender_respondent_x, inc_incgroup_pre, dem_age_r_x, white, dem_edu, Obama,
                           Auth, bornagain, church, republican, fox, libcpre_self, MoralStatism, Government, MoralStatismXGovernment))
detach(factor.vars)

y <- bma.vars$tea_supp

bma.vars$tea_supp <- NULL

set.seed(666)

bm <- bic.glmMN(bma.vars, y, glm.family="binomial", strict=FALSE, factor.type=TRUE, occam.window=FALSE, 
                 all.none.list=list(c(13,14,15)), OR.fix=200, nbest=100000)

# summary(bm)

bmastats<-as.data.frame(bm$probne0)
bmastats$Variables<-bm$namesx
names(bmastats)<-c("Probability", "Variables")
bmastats$Variables<-as.factor(bmastats$Variables)
bmastats$Variables<-c("Gender (Male)", "Income", "Age", "Race (White)",
"Education", "Obama", "Authoritarianism", "BornAgain", "Religion",
"PartyID (Republican)", "FoxNews", "Conservatism", "MoralStatism", "Government",
"MoralStatism*Government")
bmastats$Variables<-reorder(bmastats$Variables, bmastats$Probability)

bma.plot<-ggplot(bmastats, aes(x=Variables, y=Probability)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="Probability of Inclusion")

bmastats$EV<-bm$postmean[2:16]
bmastats$SD<-bm$postsd[2:16]

limits <- aes(ymax = bmastats$EV + bmastats$SD, ymin=bmastats$EV - bmastats$SD)

bma.plot2<-ggplot(bmastats, aes(x=Variables)) +
  geom_bar(aes(y=EV), stat="identity") +
  geom_errorbar(limits, width=0.25, colour="grey") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="Expected Values")


# imageplot.bma(bm, order="probne0")


