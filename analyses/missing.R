require(Amelia)
require(Zelig)



factor.vars.miss<-factor.vars.miss[c("white", "gender_respondent_x", "tea_supp", "bornagain", "church",
                                     "republican", "fox", "dem_edu",
                                     "libcpre_self", "inc_incgroup_pre", "dem_age_r_x", "Obama", "Auth",
                                     "MoralStatism", "Government")]
factor.vars.miss$caseid<-row.names(factor.vars.miss)
amelia.out <- amelia(factor.vars.miss, m=10, idvars=c("caseid"),
                     noms=c("white", "gender_respondent_x", "tea_supp", "bornagain", "church", "fox"),
                     p2s=0)

# missmap(amelia.out)
#compare.density(amelia.out, var = c("MoralStatism"))
#compare.density(amelia.out, var = c("Government"))
#overimpute(amelia.out, var = "MoralStatism")
#overimpute(amelia.out, var = "Government")
#disperse(amelia.out, dims = 1, m = 5)

model2.z.miss<-zelig(tea_supp ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
                  white + dem_edu + Obama + Auth + bornagain + church + republican + fox + libcpre_self +
                    MoralStatism + Government + MoralStatism:Government,
                model="logit",
                robust=FALSE,
                data=amelia.out$imputations,
                cite=F)

# summary(model2.z.miss)

# xtable(summary(model2.z.miss)$coefficients)
