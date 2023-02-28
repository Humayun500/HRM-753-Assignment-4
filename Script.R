setwd("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 4")

save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 4/data.RData")

library (tidyverse)

names (data)

data$sexascategorical
typeof (data$sexascategorical)

data$sexascategorical= as.factor (data$sexascategorical)
data$sexascategorical

data$InfluenzaInfectionPositive
typeof (data$InfluenzaInfectionPositive)
data$InfluenzaInfectionPositive= as.factor (data$InfluenzaInfectionPositive)

#question 1.a
glm.1=glm (InfluenzaInfectionPositive~sexascategorical, family = binomial, data=data)
glm.1
summary (glm.1)

or.glm.1=exp(cbind(coef(glm.1), confint(glm.1, level=0.95)))
or.glm.1

#OR for female is 0.7647059
1/1.3076923
# log OR for female is -0.268264
log (0.7647059)


prob1.glm.1 <- 1.3076923/ (1 + 1.3076923)
prob1.glm.1 #for male

prob2.glm.1 <- 0.7647059/ (1 + 0.7647059)
prob2.glm.1 #for female

#question 1.b
1/1.3076923

#question 1.c
glm.2=glm (InfluenzaInfectionPositive~age, family = binomial, data=data)
glm.2
summary (glm.2)
or.glm.2=exp(cbind(coef(glm.2), confint(glm.2, level=0.95)))
or.glm.2

data$age.5=as.integer(data$age/5) 
glm.3=glm (InfluenzaInfectionPositive~age.5, family = binomial, data=data)
glm.3
summary (glm.3)
or.glm.3=exp(cbind(coef(glm.3), confint(glm.3, level=0.95)))
or.glm.3

#Question 2
predict.glm.1.f=predict (glm.1, data=df.female, type = "response")
predict.glm.1.f
#predict model
df.female <- data.frame(sex=c("Female"))

predict(logistic_model1, new_data, type = "response")


## model summary
summary(myprobit)


#Question 3
glm.4=glm (InfluenzaInfectionPositive ~seroprotectedsH1, family = binomial, data=data)
glm.4
summary (glm.4)

or.glm.4=exp(cbind(coef(glm.4), confint(glm.4, level=0.95)))
or.glm.4







