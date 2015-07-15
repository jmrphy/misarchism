setwd("~/Dropbox/side_projects/misarchism/data")
require(foreign)
require(memisc)
library(psych)
require(reshape2)
require(ggplot2)



df<-read.dta("anes_timeseries_2012_stata12.dta")

df$medsrc_tvprog_36[df$medsrc_tvprog_36=="-1. inapplicable"]<-NA #oreilly
df$medsrc_tvprog_37[df$medsrc_tvprog_37=="-1. inapplicable"]<-NA  #greta van susteren
df$medsrc_tvprog_41[df$medsrc_tvprog_41=="-1. inapplicable"]<-NA  # Bret Baier
df$medsrc_tvprog_25[df$medsrc_tvprog_25=="-1. inapplicable"]<-NA   # hannity
df$medsrc_tvprog_22[df$medsrc_tvprog_22=="-1. inapplicable"]<-NA  ##fox report
df$medsrc_tvprog_21[df$medsrc_tvprog_21=="-1. inapplicable"]<-NA  ##the five
df$medsrc_tvprog_07[df$medsrc_tvprog_07=="-1. inapplicable"]<-NA  #America's newsroom

df$fox<-as.factor(ifelse(df$medsrc_tvprog_36=="1. selected by r" |
                 df$medsrc_tvprog_37=="1. selected by r" |
                 df$medsrc_tvprog_41=="1. selected by r" |
                 df$medsrc_tvprog_25=="1. selected by r" |
                 df$medsrc_tvprog_22=="1. selected by r" |
                 df$medsrc_tvprog_21=="1. selected by r" |
                 df$medsrc_tvprog_07=="1. selected by r",
               "Watches Fox", "Does not watch Fox"))


summary(df$relig_bornagn)
df$relig_bornagn[df$relig_bornagn=="-9. refused"]<-NA
df$relig_bornagn[df$relig_bornagn=="-8. don''t know"]<-NA
df$relig_bornagn[df$relig_bornagn=="-1. inapplicable"]<-NA
df$bornagain<-factor(df$relig_bornagn)
df$bornagain<-relevel(df$bornagain, ref="2. no")
summary(df$bornagain)

summary(df$relig_church)
df$relig_church[df$relig_church=="-9. refused"]<-NA
df$relig_church[df$relig_church=="-8. don''t know"]<-NA
df$church<-factor(df$relig_church)
df$church<-relevel(df$church, ref="2. no")
summary(df$church)

summary(df$pid_x)
df$pid_x[df$pid_x=="-2. missing"]<-NA
df$republican<-as.numeric(factor(df$pid_x))
summary(df$republican)

summary(df$inc_incgroup_pre)
df$inc_incgroup_pre[df$inc_incgroup_pre=="-9. refused"]<-NA
df$inc_incgroup_pre[df$inc_incgroup_pre=="-8. don''t know"]<-NA
df$inc_incgroup_pre[df$inc_incgroup_pre=="-2. missing; iwr mistakenly entered ''2'' in place of dk code for total income"]<-NA
df$inc_incgroup_pre<-factor(df$inc_incgroup_pre)
df$inc_incgroup_pre<-as.numeric(df$inc_incgroup_pre)

summary(df$aidblack_self) # aid to blacks
df$aidblack_self[df$aidblack_self<1]<-NA
df$aidblack_self <- 8-df$aidblack_self

summary(df$guarpr_self)   # guaranteed jobs
df$guarpr_self[df$guarpr_self<0]<-NA
df$guarpr_self <- 8-df$guarpr_self

summary(df$spsrvpr_ssself)   # spending on services
df$spsrvpr_ssself[df$spsrvpr_ssself<1]<-NA

summary(df$defsppr_self)   # defense spending
df$defsppr_self[df$defsppr_self<0]<-NA

summary(df$inspre_self)  # govt-private medical insur scale: self-plmt
df$inspre_self[df$inspre_self<1]<-NA
# df$inspre_self <- 8-df$inspre_self

df$gun_control[df$gun_control=="-9. refused"]<-NA
df$gun_control[df$gun_control=="-8. don''t know"]<-NA

df$gun_control<-recode(df$gun_control,
                       3<-"1. more difficult",
                       2<-"3. keep these rules about the same",
                       1<-"2. easier",
                       otherwise="copy")
df$gun_control<-as.numeric(levels(df$gun_control))[df$gun_control]

summary(df$trad_adjust)
df$trad_adjust[df$trad_adjust=="-9. refused"]<-NA
df$trad_adjust[df$trad_adjust=="-8. don''t know"]<-NA
df$trad_adjust[df$trad_adjust=="-7. deleted due to partial (post-election) interview"]<-NA
df$trad_adjust[df$trad_adjust=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA

df$trad_adjust<-recode(df$trad_adjust,
                       1<-"1. agree strongly",
                       2<-"2. agree somewhat",
                       3<-"3. neither agree nor disagree",
                       4<-"4. disagree somewhat",
                       5<-"5. disagree strongly",
                       otherwise="copy")
df$trad_adjust<-as.numeric(levels(df$trad_adjust))[df$trad_adjust]

df$trad_lifestyle[df$trad_lifestyle=="-9. refused"]<-NA
df$trad_lifestyle[df$trad_lifestyle=="-8. don''t know"]<-NA
df$trad_lifestyle[df$trad_lifestyle=="-7. deleted due to partial (post-election) interview"]<-NA
df$trad_lifestyle[df$trad_lifestyle=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA

df$trad_lifestyle<-recode(df$trad_lifestyle,
                          5<-"1. agree strongly",
                          4<-"2. agree somewhat",
                          3<-"3. neither agree nor disagree",
                          2<-"4. disagree somewhat",
                          1<-"5. disagree strongly",
                          otherwise="copy")
df$trad_lifestyle<-as.numeric(levels(df$trad_lifestyle))[df$trad_lifestyle]


summary(df$trad_tolerant)
df$trad_tolerant[df$trad_tolerant=="-9. refused"]<-NA
df$trad_tolerant[df$trad_tolerant=="-8. don''t know"]<-NA
df$trad_tolerant[df$trad_tolerant=="-7. deleted due to partial (post-election) interview"]<-NA
df$trad_tolerant[df$trad_tolerant=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA

df$trad_tolerant<-recode(df$trad_tolerant,
                          1<-"1. agree strongly",
                          2<-"2. agree somewhat",
                          3<-"3. neither agree nor disagree",
                          4<-"4. disagree somewhat",
                          5<-"5. disagree strongly",
                          otherwise="copy")
df$trad_tolerant<-as.numeric(levels(df$trad_tolerant))[df$trad_tolerant]


summary(df$trad_famval)
df$trad_famval[df$trad_famval=="-9. refused"]<-NA
df$trad_famval[df$trad_famval=="-8. don''t know"]<-NA
df$trad_famval[df$trad_famval=="-7. deleted due to partial (post-election) interview"]<-NA
df$trad_famval[df$trad_famval=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA

df$trad_famval<-recode(df$trad_famval,
                         5<-"1. agree strongly",
                         4<-"2. agree somewhat",
                         3<-"3. neither agree nor disagree",
                         2<-"4. disagree somewhat",
                         1<-"5. disagree strongly",
                         otherwise="copy")
df$trad_famval<-as.numeric(levels(df$trad_famval))[df$trad_famval]

df$libcpre_self[df$libcpre_self=="-9. refused"]<-NA
df$libcpre_self[df$libcpre_self=="-8. don''t know"]<-NA
df$libcpre_self[df$libcpre_self=="-2. haven''t thought much about this"]<-NA

df$libcpre_self<-recode(df$libcpre_self,
                        1<-"1. extremely liberal",
                        2<-"2. liberal",
                        3<-"3. slightly liberal",
                        4<-"4. moderate; middle of the road",
                        5<-"5. slightly conservative",
                        6<-"6. conservative",
                        7<-"7. extremely conservative",
                        otherwise="copy")
df$libcpre_self<-as.numeric(levels(df$libcpre_self))[df$libcpre_self]


summary(df$tea_supp)

df$tea_supp[df$tea_supp=="-9. refused"]<-NA
df$tea_supp[df$tea_supp=="-8. don''t know"]<-NA

df$tea_supp<-recode(df$tea_supp,
                    1<-"1. support",
                    0<-"3. neither support nor oppose",
                    0<-"2. oppose",
                    otherwise="copy")
df$tea_supp<-as.numeric(levels(df$tea_supp))[df$tea_supp]

df$dem_age_r_x[df$dem_age_r_x<18]<-NA

summary(df$dem_edu)
df$dem_edu[df$dem_edu=="-9. refused"]<-NA
df$dem_edu[df$dem_edu=="-8. don''t know"]<-NA
df$dem_edu[df$dem_edu=="95. other {specify}"]<-NA
df$dem_edu<-as.numeric(factor(df$dem_edu))

summary(df$immig_checks)
df$immig_checks[df$immig_checks=="-9. refused"]<-NA
df$immig_checks[df$immig_checks=="-8. don''t know"]<-NA

df$immig_checks<-recode(df$immig_checks,
                        3<-"1. favor",
                        2<-"3. neither favor or oppose",
                        1<-"2. oppose",
                        otherwise="copy")
df$immig_checks<-as.numeric(levels(df$immig_checks))[df$immig_checks]

summary(df$wiretappo_toofar)
df$wiretappo_toofar[df$wiretappo_toofar=="-9. refused"]<-NA
df$wiretappo_toofar[df$wiretappo_toofar=="-8. don''t know"]<-NA
df$wiretappo_toofar[df$wiretappo_toofar=="-7. deleted due to partial (post-election) interview"]<-NA
df$wiretappo_toofar[df$wiretappo_toofar=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA

df$wiretap<-recode(df$wiretappo_toofar,
                   3<-"3. do not go far enough",
                   2<-"2. are just about right",
                   1<-"1. have gone too far",
                   otherwise="copy")
df$wiretap<-as.numeric(levels(df$wiretap))[df$wiretap]

summary(df$dem_raceeth_x)
df$dem_raceeth_x[df$dem_raceeth_x=="-9. refused"]<-NA
df$dem_raceeth_x[df$dem_raceeth_x=="-8. don''t know"]<-NA
df$white<-factor(recode(df$dem_raceeth_x,
                        "White"<-"1. white non-hispanic",
                        "Non-white"<-"2. black non-hispanic",
                        "Non-white"<-"3. hispanic",
                        "Non-white"<-"4. other non-hispanic",
                        otherwise="copy"))
summary(df$white)


summary(df$ft_dpc)
df$ft_dpc[df$ft_dpc<0]<-NA
df$Obama<-df$ft_dpc

summary(df$auth_obed)
df$auth_obed[df$auth_obed=="-9. refused"]<-NA
df$auth_obed[df$auth_obed=="-8. don''t know"]<-NA
df$auth_obed[df$auth_obed=="-7. deleted due to partial (post-election) interview"]<-NA
df$auth_obed[df$auth_obed=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA
df$auth_obed<-recode(df$auth_obed,
                    1<-"1. obedience",
                    0<-"2. self-reliance",
                    otherwise=NA)
df$auth_obed<-as.numeric(levels(df$auth_obed))[df$auth_obed]

summary(df$auth_ind)
df$auth_ind[df$auth_ind=="-9. refused"]<-NA
df$auth_ind[df$auth_ind=="-8. don''t know"]<-NA
df$auth_ind[df$auth_ind=="-7. deleted due to partial (post-election) interview"]<-NA
df$auth_ind[df$auth_ind=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA
df$auth_ind<-recode(df$auth_ind,
                     1<-"2. respect for elders",
                     0<-"1. independence",
                     otherwise=NA)
df$auth_ind<-as.numeric(levels(df$auth_ind))[df$auth_ind]

summary(df$auth_cur)
df$auth_cur[df$auth_cur=="-9. refused"]<-NA
df$auth_cur[df$auth_cur=="-8. don''t know"]<-NA
df$auth_cur[df$auth_cur=="-7. deleted due to partial (post-election) interview"]<-NA
df$auth_cur[df$auth_cur=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA
df$auth_cur<-recode(df$auth_cur,
                    1<-"2. good manners",
                    0<-"1. curiosity",
                    otherwise=NA)
df$auth_cur<-as.numeric(levels(df$auth_cur))[df$auth_cur]

summary(df$auth_consid)
df$auth_consid[df$auth_consid=="-9. refused"]<-NA
df$auth_consid[df$auth_consid=="-8. don''t know"]<-NA
df$auth_consid[df$auth_consid=="-7. deleted due to partial (post-election) interview"]<-NA
df$auth_consid[df$auth_consid=="-6. not asked, unit nonresponse (no post-election interview)"]<-NA
df$auth_consid<-recode(df$auth_consid,
                    1<-"2. well behaved",
                    0<-"1. being considerate",
                    otherwise=NA)
df$auth_consid<-as.numeric(levels(df$auth_consid))[df$auth_consid]




# df$Auth<-rowMeans(cbind(df$auth_obed, df$auth_ind, df$auth_cur, df$auth_consid), na.rm=T)
df$Auth<-(df$auth_obed+df$auth_ind+df$auth_cur+df$auth_consid)/4



control.vars<-c("libcpre_self", "white","dem_edu", "tea_supp", "fox",
                "gender_respondent_x", "dem_age_r_x", "inc_incgroup_pre", "Obama", "Auth",
                "bornagain", "church", "republican")

gov.vars<-c("defsppr_self","spsrvpr_ssself","guarpr_self",
            "immig_checks", "wiretap", "gun_control", "aidblack_self")

moral.vars<-c("trad_adjust", "trad_lifestyle", "trad_tolerant", "trad_famval")



df<-subset(df, select=c("caseid", "tea_supp", gov.vars, control.vars, moral.vars))

write.csv(df, file="misarchism_data.csv")
