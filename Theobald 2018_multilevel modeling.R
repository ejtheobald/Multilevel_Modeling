##############################
# The value of using multilevel models
# Example: Repeated Measures and Clustering in sections
# Author: Elli Theobald: ellij@uw.edu
# Full citation: Students are rarely independent: When, why, and how to use random effects in discipline-based education research
##############################

# load libraries
  # lme4 contains the function (lmer) to fit multilevel models
library(lme4)

# set working directory
setwd("C:/Users/ElliJTheobald/Dropbox/elli.jenkins@gmail/R/lmerPaper/DataFinal")

# read in the data
mydata <- read.csv("RepeatedStudents.csv",head=T)
head(mydata)

# data description:
  # 2 sections (A and B), 2 treatments (treatment=1 and control=0)
  # each section got 3 doses: either 2 treatments and 1 control (B) or 2 controls and 1 treatment (A)
table(RepeatMeas$Section,RepeatMeas$Treatment,RepeatMeas$Topic)


############################################################
#### Model selection as recommended by Zuur et al. 2009 ####
############################################################


##############
### Step 1 ###
##############
   # Fit the most complex fixed-effects only model that explicitly tests the hypothesis
# Model 1: tests the hypothesis that treatment impacts postscore
  # note that this is fit in R's base package with the function lm (not lmer in the lme4 package)
mod1 <- lm(PostScore ~ PreScore + Treatment + Topic, 
             data=mydata)
summary(mod1)
AIC(mod1)

##############
### Step 2 ###
##############
   # Test all combinations of random effect structures
# Model 2: student random effect (repeated measures) and section random effect (clustering)
mod2 <- lmer(PostScore ~ PreScore + Treatment + Topic + (1|StudentID) + (1|Section), 
             data=mydata)
summary(mod2)
AIC(mod2)

# Model 3: student random effect only (repeated measures)
mod3 <- lmer(PostScore ~ PreScore + Treatment + Topic + (1|StudentID), 
             data=mydata)
summary(mod3)
AIC(mod3)

# Model 4: section random effect only (clustering)
mod4 <- lmer(PostScore ~ PreScore + Treatment + Topic + (1|Section), 
             data=mydata)
summary(mod4)
AIC(mod4)

##############
### Step 3 ###
##############
   # compare all models from steps 1 and 2 using AIC 
AIC(mod1, mod2, mod3, mod4)


######################
### Interpretation ###
######################
# here, model 3, with student as a random effect, accounting for the repeated measures,
  # has the lowest AIC value so should be selected as best fit and interpreted.


