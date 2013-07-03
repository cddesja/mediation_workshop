# --------------------------- #
# MEDIATION & MODERATION IN R #
# --------------------------- #

# Christopher David Desjardins
# Licensed under the GNU GPL v3
# http://www.gnu.org/licenses/gpl.html

## Preamble, run only once
install.packages("mediation")
install.packages("MBESS")


## Load required libraries
source("workshop_functions.R")
library(mediation)
library(ggplot2)

## Jobs data set 
data(jobs) # To learn about the dataset type: ?jobs

# The response will be depress2
# econ_hard will be our predictor
# job_seek will be the mediator and moderator

summary(jobs)

################
## MODERATION ##
################

# What is the distribution of the moderator?
ggplot(data=jobs,aes(x=jobs$job_seek)) + geom_histogram(fill="white",color="black") + theme_bw()

mod.model = lm(depress2 ~ econ_hard*job_seek, jobs)
summary(mod.model)

## Investigate assumptions
plot(mod.model)
jobs$mod.model.resids = resid(mod.model)

## Plot residuals
ggplot(jobs, aes(x = mod.model.resids)) + geom_histogram(aes(y = ..density..),fill="white",color="black") + geom_density(linetype=2,color="red") + theme_bw() + xlab("Residuals") + ylab("Density")

## Probe the interaction
jnr.mod <- johnson.neyman.reg(dependent=jobs$depress2,predictor=jobs$econ_hard,moderator=jobs$job_seek)

names(jnr.mod)

## At what levels of the moderator is the interaction significant?
jnr.mod[jnr.mod$Significant=="Yes",]

## Specifying a specific value for the moderator
Z <- mean(jobs$job_seek) + sd(jobs$job_seek)
region.significance(dependent=jobs$depress2,predictor=jobs$econ_hard,moderator=jobs$job_seek,z=Z)

region.significance(dependent=jobs$depress2,predictor=jobs$econ_hard,moderator=jobs$job_seek,z=1)

## Recreate Figure 2 in Preacher et al. (2006)
ggplot(data = jnr.mod, aes(x = Z, y = Est)) + geom_line() + geom_ribbon(data=jnr.mod ,aes(ymin=LB95,ymax=UB95),alpha=0.1) + geom_hline(yintercept=0, lty=1) + geom_vline(xintercept=jnr.mod[jnr.mod$Significant=="Yes",2], lty = 2) + theme_bw() + coord_cartesian(xlim = c(.9, 5.1)) + xlab("Job Seek") + ylab("Simple Slope")

# OR # 

ggplot(data = jnr.mod, aes(x = Z, y = Est)) + geom_line() + geom_line(data=jnr.mod ,aes(y=LB95,x=Z)) + geom_line(data=jnr.mod ,aes(y=UB95,x=Z)) + geom_hline(yintercept=0, lty=1) + geom_vline(xintercept=jnr.mod[jnr.mod$Significant=="Yes",2], lty = 2) + theme_bw() + coord_cartesian(xlim = c(.9, 5.1)) + xlab("Job Seek") + ylab("Simple Slope")

###############
## MEDIATION ##
###############

econ_hard*job_seek

# Step 1: Is predictor related to response?
step1 <- lm(depress2 ~ econ_hard, data = jobs)
summary(step1)
plot(step1)

# Step 2: Is predictor related to mediator?
step2 <- lm(job_seek ~ econ_hard, data = jobs)
summary(step2)
plot(step2)

# Step 3: Is mediator a significant predictor of response?
step3 <- lm(depress2 ~ econ_hard + job_seek, data = jobs)
summary(step3)
plot(step3)

# Automating it with mediation
set.seed(5452)
mediation(x = jobs$econ_hard, mediator = jobs$job_seek, dv = jobs$depress2, bootstrap = T)
