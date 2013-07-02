# --------------------------- #
# MEDIATION & MODERATION IN R #
# --------------------------- #

# Christopher David Desjardins
# Licensed under the GNU GPL v3
# http://www.gnu.org/licenses/gpl.html

## Preamble, run only once
install.packages("mediation")


## Load required libraries
source("medatioRfunctions.R")
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