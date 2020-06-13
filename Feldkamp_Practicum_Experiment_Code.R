
####Cara Feldkamp
#Experimental Research Practicum Experiment
#Soaking popcorn kernels v. just cooking
#Estimator: difference-in-means

###########################################

set.seed(999)
library(ri2)
library(ri)

#I coded the outcomes as 0=unpopped, 1=partially popped, and 2=fully popped

popcorn <- read.csv("/Users/carafeldkamp/Documents/Popcorn_Data.csv")
#View(popcorn)

popcorn <- na.omit(popcorn)
#Removed two lost kernels


#ri2
declaration <- declare_ra(N = 629, m = 320)
ri2_out <- conduct_ri(Y ~ Z, 
                      declaration = declaration, 
                      assignment = "Z", 
                      sharp_hypothesis = 0,
                      data = popcorn,
                      sims = 1000)
summary(ri2_out)
plot(ri2_out)


#also tried it with ri
Z <- popcorn$Z
Y <- popcorn$Y

probs <- genprobexact(Z)
ate <- estate(Y,Z,prob=probs)

#Confidence Interval
perms <- genperms(Z=Z,maxiter=1000)

Ys <- genouts(Y,Z,ate)
distout <- gendist(Ys = Ys,perms = perms,prob=probs)
dispdist(distout=distout,ate=ate)
mean(distout)
ate

###Sharp Null
Ys.est <- genouts(Y,Z,ate=0)
estdist <- gendist(Ys=Ys.est,perms=perms,prob=probs)
dispdist(distout=estdist,ate=ate)
mean(estdist)

#regression to double-check
b <- lm(Y ~ D, data=popcorn)
summary(b)
