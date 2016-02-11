

#require(Zelig)
require(car)

factor.vars$bornagain<-relevel(factor.vars$bornagain, ref="2. no")
factor.vars$church<-relevel(factor.vars$church, ref="2. no")
factor.vars$gender_respondent_x<-relevel(factor.vars$gender_respondent_x, ref="2. female")
factor.vars$Support<-factor.vars$tea_supp

model1<-glm(Support ~ gender_respondent_x + inc_incgroup_pre +
              dem_age_r_x + white + dem_edu + Obama + Auth + bornagain +
              church + republican + fox + libcpre_self,
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
model2<-glm(Support ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
              white + dem_edu + Obama + Auth + bornagain +
              church + republican + fox + libcpre_self + MoralStatism + Gov + MoralStatism:Gov,
              family=binomial,
              data=factor.vars)

model2.cons<-glm(Support ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
              white + dem_edu + Obama + Auth + bornagain +
              church + republican + fox + libcpre_self + MoralStatism + Gov + MoralStatism:Gov,
            family=binomial,
            data=subset(factor.vars, libcpre_self>mean(libcpre_self)))

# summary(model2)
# require(car)
# vif(model2)

# model2classif<-table(model2$fitted.values>.5, model2$y)
# model2correct<-(model2classif[1,1] + model2classif[2,2])
#  (model2correct/nrow(factor.vars))*100


model2.z<-zelig(Support ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
                  white + dem_edu + Obama + Auth + bornagain + 
                  church + republican + fox + libcpre_self + MoralStatism + Gov + MoralStatism:Gov,
                     model="logit",
                     data=factor.vars,
                     cite=F)
# 
# summary(model2.z)

set.seed(666)

# Moral Statism
ms.low<-setx(model2.z, MoralStatism=quantile(factor.vars$MoralStatism, .1))
ms.out.low <- sim(model2.z, x = ms.low)

ms.hi<-setx(model2.z, MoralStatism=quantile(factor.vars$MoralStatism, .9))
ms.out.hi <- sim(model2.z, x = ms.hi)
# 
ms.out <- sim(model2.z, x = ms.low, x1 = ms.hi)
# summary(ms.out)

# Governmentalism
g.low<-setx(model2.z, Gov=quantile(factor.vars$Gov, .1))
g.out.low <- sim(model2.z, x = g.low)

g.hi<-setx(model2.z, Gov=quantile(factor.vars$Gov, .9))
g.out.hi <- sim(model2.z, x = g.hi)

# 
g.out <- sim(model2.z, x = g.hi, x1 = g.low)
# summary(g.out)


# Obama
o.low<-setx(model2.z, Obama=quantile(factor.vars$Obama, .1))
o.out.low <- sim(model2.z, x = o.low)

o.hi<-setx(model2.z, Obama=quantile(factor.vars$Obama, .9))
o.out.hi <- sim(model2.z, x = o.hi)
# 
o.out <- sim(model2.z, x = o.hi, x1 = o.low)
# summary(o.out)


# Auth
a.low<-setx(model2.z, Auth=0)
a.hi<-setx(model2.z, Auth=1)
# 
a.out <- sim(model2.z, x = a.low, x1 = a.hi)
# summary(a.out)


# MS * G Plot

msg.low<-setx(model2.z, MoralStatism=seq(min(factor.vars$MoralStatism),max(factor.vars$MoralStatism), by=.075),
              Gov=quantile(factor.vars$Gov, .9))
msg.hi<-setx(model2.z, MoralStatism=seq(min(factor.vars$MoralStatism),max(factor.vars$MoralStatism), by=.075),
             Gov=quantile(factor.vars$Gov, .1))
# 
msg.out <- sim(model2.z, x = msg.low, x1 = msg.hi)
# # plot(msg.out)
# # summary(msg.out)

# MS*G Effect calculation

# MS shift when people like government
ms.low.g.hi<-setx(model2.z, Gov=quantile(factor.vars$Gov, .9), MoralStatism=quantile(factor.vars$MoralStatism, .1))
ms.low.g.hi.out <- sim(model2.z, x = ms.low.g.hi)

ms.hi.g.hi<-setx(model2.z, Gov=quantile(factor.vars$Gov, .9), MoralStatism=quantile(factor.vars$MoralStatism, .9))
ms.hi.g.hi.out <- sim(model2.z, x = ms.hi.g.hi)

msgout1 <- sim(model2.z, x = ms.low.g.hi, x1 = ms.hi.g.hi)

# MS shift when people don't like government
ms.low.g.low<-setx(model2.z, Gov=quantile(factor.vars$Gov, .1), MoralStatism=quantile(factor.vars$MoralStatism, .1))
ms.low.g.low.out <- sim(model2.z, x = ms.low.g.low)

ms.hi.g.low<-setx(model2.z, Gov=quantile(factor.vars$Gov, .1), MoralStatism=quantile(factor.vars$MoralStatism, .9))
ms.hi.g.low.out <- sim(model2.z, x = ms.hi.g.low)

msgout2 <- sim(model2.z, x = ms.low.g.low, x1 = ms.hi.g.low)



# Most likely model according to BMA?
model2.z.reduced<-zelig(tea_supp ~ Obama + fox +inc_incgroup_pre + dem_age_r_x +
                          white + libcpre_self + MoralStatism + Government + MoralStatism:Government,
                model="logit",
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

model.allvars<-glm(tea_supp ~ gender_respondent_x + inc_incgroup_pre +
                       dem_age_r_x + white + dem_edu + Obama + Auth + bornagain +
                       church + republican + fox +libcpre_self +
                trad_famval + trad_adjust + trad_tolerant + guarpr_self + defsppr_self + spsrvpr_ssself + gun_control +
                immig_checks + wiretap,
              family="binomial",
              data=factor.vars)
# 
# summary(model.allvars)
# vif(model.allvars)


model.republican<-lm(republican ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
              white + dem_edu + Obama + Auth + bornagain +
              church + fox + libcpre_self + MoralStatism + Government + MoralStatism:Government,
            data=factor.vars)
summary(model.republican)

model.conservative<-lm(libcpre_self ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
                       white + dem_edu + Obama + Auth + bornagain +
                       church + republican + fox + MoralStatism + Government + MoralStatism:Government,
                     data=factor.vars)
summary(model.conservative)


####################################
## With ideology in factor model ###
####################################

model2.noideology<-glm(tea_supp ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
              white + dem_edu + Obama + Auth + bornagain +
              church + republican + fox  + MoralStatism2 + Government2 + MoralStatism2:Government,
            family=binomial,
            data=factor.vars)

