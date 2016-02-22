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
                  M=1, BiasAdjust=FALSE, Weight.matrix=genout2, exact=T)

# summary(match_g)

# binarysens(match_g, Gamma = 3, GammaInc = 0.01)

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

set.seed(999)
# 
# genout3 <- GenMatch(Tr=treatment, X=X, BalanceMatrix=bal.mat, estimand="ATT", 
#                     M=1, pop.size=1000, max.generations=100, wait.generations=2)
 
# save(genout3, file="data/genout_MSG.RData")

load("data/genout_MSG.RData")

match_msg <- Match(Y=Y, Tr=treatment, X=X, estimand="ATT", 
                 M=1, BiasAdjust=FALSE, Weight.matrix=genout3, exact=T)

# summary(match_msg)
# 
# mb_msg  <- MatchBalance(MSG ~ white + Obama + bornagain + fox + libcpre_self,
# data=matdf, match.out=match_msg, nboots=500, print.level=2)

# binarysens(match_msg, Gamma = 3, GammaInc = 0.01)

