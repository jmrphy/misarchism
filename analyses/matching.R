setwd("~/Dropbox/gh_projects/misarchism")
require(Hmisc)
require(compare)
require(foreign)
require(Matching)
require(rbounds)

# # must be run after bma.R

factor.vars$MS<-ifelse(factor.vars$MoralStatism>mean(factor.vars$MoralStatism), TRUE, FALSE)
factor.vars$G<-ifelse(factor.vars$Government>mean(factor.vars$Government), TRUE, FALSE)
factor.vars$MSG<-ifelse(factor.vars$MoralStatismXGovernment>mean(factor.vars$MoralStatismXGovernment), TRUE, FALSE)

# Moral Statism

# attach(factor.vars)
# matdf<- as.data.frame(cbind(tea_supp, gender_respondent_x, inc_incgroup_pre, dem_age_r_x, white, dem_edu, Obama,
#                                Auth, bornagain, church, republican, fox, MS, G, MSG))
# detach(factor.vars)
# 
# ps  <- glm(MS ~ gender_respondent_x + inc_incgroup_pre + dem_age_r_x +
#              white + dem_edu + Obama + Auth + bornagain + church + republican + fox,
#            family = binomial, data = matdf)
# 
# X <- cbind(ps$fitted, matdf$gender_respondent_x, matdf$inc_incgroup_pre, matdf$dem_age_r_x,
#            matdf$white, matdf$dem_edu, matdf$Obama, matdf$Auth, matdf$bornagain, matdf$church,
#            matdf$republican, matdf$fox, matdf$G, matdf$MSG)
# 
# bal.mat <- cbind(ps$fitted, matdf$gender_respondent_x, matdf$inc_incgroup_pre, matdf$dem_age_r_x,
#                  matdf$white, matdf$dem_edu, matdf$Obama, matdf$Auth, matdf$bornagain, matdf$church,
#                  matdf$republican, matdf$fox)
# 
# treatment <- matdf$MS
# 
# Y <- matdf$tea_supp
# 
# set.seed(666)
# 
# genout <- GenMatch(Tr=treatment, X=X, BalanceMatrix=bal.mat, estimand="ATT", 
#                    M=1, pop.size=1000, max.generations=100, wait.generations=2)
# 
# save(genout, file="data/genout_MS.RData")
# load("data/genout_MS.RData")
# 
# match_ms <- Match(Y=Y, Tr=treatment, X=X, estimand="ATT", 
#                    M=1, BiasAdjust=FALSE, Weight.matrix=genout)

# summary(match_ms)

# mb_ms  <- MatchBalance(MS ~ gender_respondent_x + inc_incgroup_pre + inc_incgroup_pre +
#                          dem_age_r_x + white + dem_edu + Obama + Auth + bornagain +
#                          church + republican + fox,
#                        data=matdf, match.out=match_ms, nboots=500, print.level=2)

# binarysens(match_ms, Gamma = 3, GammaInc = 0.01)

### Governmentalism

attach(factor.vars)
matdf<- as.data.frame(cbind(tea_supp, dem_edu, Obama, bornagain,
                            libcpre_self, fox, MS, G, MSG))
detach(factor.vars)

ps  <- glm(G ~ Obama + bornagain + fox + libcpre_self,
           family = binomial, data = matdf)

X <- cbind(ps$fitted, matdf$Obama, matdf$bornagain, matdf$fox, matdf$libcpre_self, matdf$MS, matdf$MSG)

bal.mat <- cbind(ps$fitted, matdf$Obama, matdf$bornagain, matdf$fox, matdf$libcpre_self)

treatment <- matdf$G

Y <- matdf$tea_supp

set.seed(666)
# 
# genout2 <- GenMatch(Tr=treatment, X=X, BalanceMatrix=bal.mat, estimand="ATT", 
#                    M=1, pop.size=1000, max.generations=100, wait.generations=2)
# 
# save(genout2, file="data/genout_G.RData")

load("data/genout_G.RData")

match_g <- Match(Y=Y, Tr=treatment, X=X, estimand="ATT", 
                  M=1, BiasAdjust=FALSE, Weight.matrix=genout2)

# summary(match_g)

# mb_g  <- MatchBalance(G ~ gender_respondent_x + inc_incgroup_pre + inc_incgroup_pre +
#                          dem_age_r_x + white + dem_edu + Obama + Auth + bornagain +
#                          church + republican + fox,
#                        data=matdf, match.out=match_g, nboots=500, print.level=2)

# match_g$est #estimate

# plot(genout2)


### MS * G

attach(factor.vars)
matdf<- as.data.frame(cbind(tea_supp, dem_edu, Obama, bornagain,
                            libcpre_self, fox, MS, G, MSG))
detach(factor.vars)

ps  <- glm(MSG ~ Obama + bornagain + fox + libcpre_self,
           family = binomial, data = matdf)

X <- cbind(ps$fitted, matdf$Obama, matdf$bornagain, matdf$fox, matdf$libcpre_self, matdf$MS, matdf$G)

bal.mat <- cbind(ps$fitted, matdf$Obama, matdf$bornagain, matdf$fox, matdf$libcpre_self)

treatment <- matdf$MSG

Y <- matdf$tea_supp

set.seed(666)
# 
# genout3 <- GenMatch(Tr=treatment, X=X, BalanceMatrix=bal.mat, estimand="ATT", 
#                     M=1, pop.size=1000, max.generations=100, wait.generations=2)
# 
# save(genout3, file="data/genout_MSG.RData")
load("data/genout_MSG.RData")

match_msg <- Match(Y=Y, Tr=treatment, X=X, estimand="ATT", 
                 M=1, BiasAdjust=FALSE, Weight.matrix=genout3)

# summary(match_msg)
# 
# mb_msg  <- MatchBalance(MSG ~ gender_respondent_x + inc_incgroup_pre + inc_incgroup_pre +
# dem_age_r_x + white + dem_edu + Obama + Auth + bornagain +
#   church + republican + fox,
# data=matdf, match.out=match_msg, nboots=500, print.level=2)

# binarysens(match_msg, Gamma = 3, GammaInc = 0.01)

