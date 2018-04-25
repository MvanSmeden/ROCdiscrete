#############################################
# Illustration of discrete scoring          #
# Simulation                                #
# Author: M van Smeden                      #
#############################################

# dependencies #
require(pROC)
require(ggplot2)
require(plotROC)
# ---------- #

# simulation parameters
n <- 50000
beta <- 3

# draw data
X <- rnorm(n)
p <- 1/(1+exp(-beta*X))
Y <- rbinom(n,1,p)
p_discrete <- as.numeric(cut(p,c(0,.25,.5,.75,1)))

# calculate ROC
c_cont <- roc(Y,p)
c_discr <- roc(Y,p_discrete)

# plot it all
ggplot(data.frame(Y=Y,p=p),aes(d = Y, m = p)) + geom_roc(labels = F) + 
  geom_roc(data=data.frame(Y=Y,pd=p_discrete),aes(d=Y,m=pd),col="red",labels=F) + theme_light() + 
  annotate("text", label = paste("discrete AUC:",round(as.numeric(c_discr$auc),3)), x = .65, y = .5, size = 5, colour = "red")+
  annotate("text", label = paste("continuous AUC:",round(as.numeric(c_cont$auc),3)), x = .65, y = .56, size = 5, colour = "black")+
  ylab("sensitivity") + xlab("1-specificity")


