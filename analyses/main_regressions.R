require(Zelig)

# source("analyses/princ_comp.R")

factor.vars$bornagain<-relevel(factor.vars$bornagain, ref="2. no")
factor.vars$church<-relevel(factor.vars$church, ref="2. no")
factor.vars$gender_respondent_x<-relevel(factor.vars$gender_respondent_x, ref="2. female")

model1<-glm(tea_supp ~ gender_respondent_x + inc_incgroup_pre + libcpre_self +
              dem_age_r_x + white + dem_edu + Obama + Auth + bornagain +
              church + republican + fox,
              family=binomial,
              data=factor.vars)

#  summary(model1)

# model1classif<-table(model1$fitted.values>.5, model1$y)
# model1correct<-(model1classif[1,1] + model1classif[2,2])
# (model1correct/nrow(factor.vars))*100


# misarchism.vars<-subset(df, select=c("tea_supp", "gender_respondent_x", "libcpre_self", "dem_age_r_x", "white", "dem_edu",
#                                      "trad_adjust", "trad_lifestyle", "defsppr_self", "spsrvpr_ssself","aidblack_self",
#                                       "immig_checks", "wiretap"))
# misarchism.vars<-misarchism.vars[complete.cases(misarchism.vars),]

factor.vars$Gov<-factor.vars$Government
factor.vars$Support<-factor.vars$tea_supp
model2<-glm(Support ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
              white + dem_edu + Obama + Auth + bornagain +
              church + republican + fox + MoralStatism + Gov + MoralStatism:Gov,
              family=binomial,
              data=factor.vars)

# summary(model2)
# require(car)
# vif(model2)

# model2classif<-table(model2$fitted.values>.5, model2$y)
# model2correct<-(model2classif[1,1] + model2classif[2,2])
#  (model2correct/nrow(factor.vars))*100


model2.z<-zelig(tea_supp ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
                  white + dem_edu + Obama + Auth + bornagain + 
                  church + republican + fox + MoralStatism + Government + MoralStatism:Government,
                     model="logit",
                     robust=FALSE,
                     data=factor.vars,
                     cite=F)
# 
# summary(model2.z)

model2.pid<-glm(tea_supp ~ gender_respondent_x + inc_incgroup_pre + libcpre_self + dem_age_r_x +
                            white + dem_edu + Obama + Auth + bornagain +
                            church + republican + fox + MoralStatism + Government + MoralStatism:Government,
                          family=binomial,
                          data=factor.vars)
# summary(model2.pid)
require(car)
# vif(model2.pid)[13]

set.seed(666)

# Moral Statism
ms.low<-setx(model2.z, MoralStatism=quantile(factor.vars$MoralStatism, .1))
ms.hi<-setx(model2.z, MoralStatism=quantile(factor.vars$MoralStatism, .9))
# 
ms.out <- sim(model2.z, x = ms.low, x1 = ms.hi)
# summary(ms.out)

# Governmentalism
g.low<-setx(model2.z, Government=quantile(factor.vars$Government, .1))
g.hi<-setx(model2.z, Government=quantile(factor.vars$Government, .9))
# 
g.out <- sim(model2.z, x = g.hi, x1 = g.low)
# summary(g.out)


# Obama
o.low<-setx(model2.z, Obama=quantile(factor.vars$Obama, .1))
o.hi<-setx(model2.z, Obama=quantile(factor.vars$Obama, .9))
# 
o.out <- sim(model2.z, x = o.hi, x1 = o.low)
# summary(o.out)


# Auth
a.low<-setx(model2.z, Auth=0)
a.hi<-setx(model2.z, Auth=1)
# 
a.out <- sim(model2.z, x = a.low, x1 = a.hi)
# summary(a.out)


# MS * G

msg.low<-setx(model2.z, MoralStatism=seq(min(factor.vars$MoralStatism),max(factor.vars$MoralStatism), by=.075),
              Government=quantile(factor.vars$Government, .9))
msg.hi<-setx(model2.z, MoralStatism=seq(min(factor.vars$MoralStatism),max(factor.vars$MoralStatism), by=.075),
             Government=quantile(factor.vars$Government, .1))
# 
msg.out <- sim(model2.z, x = msg.low, x1 = msg.hi)
# # plot(msg.out)
# # summary(msg.out)


# Most likely model according to BMA
model2.z.reduced<-zelig(tea_supp ~ Obama + fox + MoralStatism + Government + MoralStatism:Government,
                model="logit",
                robust=FALSE,
                data=factor.vars,
                cite=F)

msg.low<-setx(model2.z.reduced, MoralStatism=seq(min(factor.vars$MoralStatism),max(factor.vars$MoralStatism), by=.075),
              Government=quantile(factor.vars$Government, .9))
msg.hi<-setx(model2.z.reduced, MoralStatism=seq(min(factor.vars$MoralStatism),max(factor.vars$MoralStatism), by=.075),
             Government=quantile(factor.vars$Government, .1))
# 
msg.out.reduced <- sim(model2.z.reduced, x = msg.low, x1 = msg.hi)
# # plot(msg.out.reduced)
# # summary(msg.out.reduced)


factor.vars$trad_avg<-(factor.vars$trad_famval+factor.vars$trad_adjust+factor.vars$trad_tolerant)/3

model.allvars<-glm(tea_supp ~ gender_respondent_x + inc_incgroup_pre + libcpre_self +
                       dem_age_r_x + white + dem_edu + Obama + Auth + bornagain +
                       church + republican + fox +
                trad_famval + trad_adjust + trad_tolerant + guarpr_self + defsppr_self + spsrvpr_ssself + gun_control +
                immig_checks + wiretap,
              family="binomial",
              data=factor.vars)
# 
# summary(model.allvars)
# vif(model.allvars)


model.republican<-lm(republican ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
              white + dem_edu + Obama + Auth + bornagain +
              church + fox + MoralStatism + Government + MoralStatism:Government,
            data=factor.vars)
summary(model.republican)

model.conservative<-lm(libcpre_self ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
                       white + dem_edu + Obama + Auth + bornagain +
                       church + republican + fox + MoralStatism + Government + MoralStatism:Government,
                     data=factor.vars)
summary(model.conservative)


