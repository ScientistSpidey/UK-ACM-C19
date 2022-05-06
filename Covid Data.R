

# clear the R workspace
rm(list = ls())

library(ggplot2)
library(BSDA)


#Don't forget to set the working directory

my.data <- read.csv("acm_rate.csv", header = T) #import the data
View(my.data)


attach(my.data)

alpha = 0.05 #Used for the Confidence Intervals

#Calculate the standard deviations
sd1 <- sd(ACM.death.rate)
sd2 <- sd(Non.COVID.ACM.rate)
sd3 <- sd(death.rate.for.covid.19)

#Calculate the standard errors
se1 <- sd(ACM.death.rate) / sqrt(length(ACM.death.rate))
se2 <- sd(ACM.death.rate) / sqrt(length(Non.COVID.ACM.rate))
se3 <- sd(ACM.death.rate) / sqrt(length(death.rate.for.covid.19))

#Calculate the confidence intervals
t1 <- qt((1-alpha)/2 + .5, length(ACM.death.rate)-1)   # tend to 1.96 if sample size is big enough
CI1 <- t1*se1

t2 <- qt((1-alpha)/2 + .5, length(Non.COVID.ACM.rate)-1)   # tend to 1.96 if sample size is big enough
CI2 <- t2*se2

t3 <- qt((1-alpha)/2 + .5, length(death.rate.for.covid.19)-1)   # tend to 1.96 if sample size is big enough
CI3 <- t3*se3

#####====== acm death rate =====#####

#Now lets make some plots
ggplot(my.data, aes(x = Vaccination.status, y = ACM.death.rate, fill = ï..Age.group)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=ACM.death.rate-CI1, ymax=ACM.death.rate+CI1), width=.2,
                position=position_dodge(.9)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab ("Vaccine Status") + 
  ylab ("All Cause Death Rate")


ggplot(my.data, aes(x = Vaccination.status, y = Non.COVID.ACM.rate, fill = ï..Age.group)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=Non.COVID.ACM.rate-CI2, ymax=Non.COVID.ACM.rate+CI2), width=.2,
                position=position_dodge(.9)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab ("Vaccine Status") + 
  ylab ("All Cause Death Rate (Not covid related)")


#generate a linear model to get p-value and effect size (Adjusted R-squared)
lm.rate <- lm(ACM.death.rate~Vaccination.status + ï..Age.group, data = my.data)
summary(lm.rate)
#Shows that unvaccinated older are more likely to die

#####====== covid death rate =====#####


#Another Plot
ggplot(my.data, aes(x = Vaccination.status, y = death.rate.for.covid.19, fill = ï..Age.group)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=death.rate.for.covid.19-CI3, ymax=death.rate.for.covid.19+CI3), width=.2,
                position=position_dodge(.9)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab ("Vaccine Status") + 
  ylab ("Covid-19 Death Rate")



#generate a linear model to get p-value and effect size (Adjusted R-squared)
lm.rate2 <- lm(death.rate.for.covid.19~Vaccination.status + ï..Age.group, data = my.data)
summary(lm.rate2)
#shows that the vaccine lowers death from covid


detach(my.data)

#####====== Now combine the sum of vaccinated to see if that changes anything =====#####

my.data2 <- read.csv("acm_rate_total.csv", header = T) #import the data
View(my.data2)


attach(my.data2)

#Calculate the standard deviations
sd4 <- sd(ACM.death.rate)
sd5 <- sd(Non.COVID.ACM.rate)
sd6 <- sd(Covid.19.death.rate)

#Calculate the standard errors
se4 <- sd(ACM.death.rate) / sqrt(length(ACM.death.rate))
se5 <- sd(ACM.death.rate) / sqrt(length(Non.COVID.ACM.rate))
se6 <- sd(ACM.death.rate) / sqrt(length(Covid.19.death.rate))

#Calculate the confidence intervals
t4 <- qt((1-alpha)/2 + .5, length(ACM.death.rate)-1)   # tend to 1.96 if sample size is big enough
CI4 <- t4*se4

t5 <- qt((1-alpha)/2 + .5, length(Non.COVID.ACM.rate)-1)   # tend to 1.96 if sample size is big enough
CI5 <- t5*se5

t6 <- qt((1-alpha)/2 + .5, length(Covid.19.death.rate)-1)   # tend to 1.96 if sample size is big enough
CI6 <- t6*se6


#####====== acm death rate =====#####

#More plots!
ggplot(my.data2, aes(x = Vaccination.status, y = ACM.death.rate, fill = ï..Age.group)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=ACM.death.rate-CI4, ymax=ACM.death.rate+CI4), width=.2,
                position=position_dodge(.9)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab ("Vaccine Status") + 
  ylab ("All Cause Death Rate")


ggplot(my.data2, aes(x = Vaccination.status, y = Non.COVID.ACM.rate, fill = ï..Age.group)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=Non.COVID.ACM.rate-CI5, ymax=Non.COVID.ACM.rate+CI5), width=.2,
                position=position_dodge(.9)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab ("Vaccine Status") + 
  ylab ("All Cause Death Rate (Not covid related)")


#generate a linear model to get p-value and effect size (Adjusted R-squared)
lm.rate3 <- lm(ACM.death.rate~Vaccination.status + ï..Age.group, data = my.data2)
summary(lm.rate3)


#####====== covid death rate =====#####



ggplot(my.data2, aes(x = Vaccination.status, y = Covid.19.death.rate, fill = ï..Age.group)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=Covid.19.death.rate-CI6, ymax=Covid.19.death.rate+CI6), width=.2,
                position=position_dodge(.9)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab ("Vaccine Status") + 
  ylab ("Covid-19 Death Rate")

#generate a linear model to get p-value and effect size (Adjusted R-squared)
lm.rate4 <- lm(Covid.19.death.rate~Vaccination.status + ï..Age.group, data = my.data2)
summary(lm.rate4)


detach(my.data)



