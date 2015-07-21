setwd("~/Dropbox/gh_projects/misarchism")
require(arm)
require(ggplot2)

df<-read.csv("data/misarchism_data.csv")

########### Run this analysis first


factor.vars<-subset(df)
factor.vars<-factor.vars[c("caseid", "tea_supp", "gender_respondent_x", "fox", "bornagain", "church",
                           "white", "Auth", "dem_edu", "dem_age_r_x", "Obama", 
                            "republican",
                           "inc_incgroup_pre", "libcpre_self",
                           "trad_famval", "gun_control",
                           "trad_tolerant", "trad_adjust", "wiretap",
                           "defsppr_self", "spsrvpr_ssself",
                           "immig_checks", "guarpr_self")]

# Rescale variables for FA
factor.vars[8:length(factor.vars)]<-as.data.frame(sapply(factor.vars[8:length(factor.vars)], function(x) rescale(x)))


detach("package:arm", unload=TRUE)
require(psych)
set.seed(666)

# fad<-factor.vars[10:length(factor.vars)]
# 
# ev <- eigen(cor(fad)) # get eigenvalues
# ap <- parallel(subject=nrow(fad),var=ncol(fad),
#                rep=100,cent=.05)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# scree.plot<-plotnScree(nS)


# Factor analysis on same variables as PCA, minus pcomp1
fit <- fa(factor.vars[14:length(factor.vars)], nfactors=2,
          rotate = "oblimin", fm = "ml", warnings=FALSE,
          missing=FALSE)
# summary(fit)
# print(fit, sort=TRUE)
# fit

#fit <- factanal(factor.vars[7:length(factor.vars)], 3, rotation="varimax", scores="regression")
# print(fit, digits=2, cutoff=.3, sort=TRUE)
# summary(fit)

load <- as.data.frame(fit$loadings[,1:2])
set.seed(123)
rownames(load)<-c("Conservatism", "Family", "Guns", "Intolerant", "Morals", "Wiretapping", "Defense",
                  "Services", "Immigration", "Jobs")
factor.plot<-ggplot(load, aes(x=ML2, y=ML1, label=rownames(load))) +
  geom_text(size=4, position = position_jitter(w = 0.03, h = 0.03)) +
  theme_bw() +
  labs(x="Factor 1 (Moral Statism)", y="Factor 2 (Government)")

# plot factor 1 by factor 2 
detach("package:psych", unload=TRUE)
require(arm)
# factor.vars$factor1 <- rescale(max(fit$scores[,1])-fit$scores[,1]) # reverse to make it "conservatism"
factor.vars$MoralStatism <- rescale(fit$scores[,1])
factor.vars$Government <- rescale(fit$scores[,2])
detach("package:arm", unload=TRUE)

# Keep version with missing for imputation in 'missing.R'
factor.vars.miss<-factor.vars

# Remove missing for main regression models
factor.vars<-factor.vars[complete.cases(factor.vars),]
