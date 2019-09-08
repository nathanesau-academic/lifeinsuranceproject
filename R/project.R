rm(list=ls())
cat("\014")
graphics.off()
options(warn=-1)
par(mfrow=c(3,3))

setwd("~/Dropbox/SFU/Past/1141/Acma320/Project/R/")
source("methods.R")
source("ageDistribution.R")
rm(group1, group2, group3, group)
markup = 1.00
premiumFrequency = 12
benefitFrequency = 12
guaranteePeriod = 10
annualBenefit = 50000
i = 0.05

premiums <- numeric(0)
for(x in 30:60) {
  u=65-x
  premiums = c(premiums, markup*getPremium())
}
PremiumTable <- data.frame((seq(30:60)+29),premiums, premiums*count)
#print(EP_Premiums)

#### LOSS FUNCTION VALUES and PROBABILTIES ####
x=30
u=(65-x)
premium = getPremium()*markup

#### COMBINE GUARANTEED PROBABILITIES ####
probabilities <- numeric(0)
losses <- numeric(0)
tvalues <- numeric(0)

lossatguarantee=0
if(guaranteePeriod>0) { lossatguarantee = lossFunction(u,premium) }

for(t in 0:(u*premiumFrequency - 1)) {
    tvalues = c(tvalues, t/premiumFrequency)
    losses = c(losses, lossFunction(t/premiumFrequency, premium))
    probabilities = c(probabilities, udeferredtqselectx(floor(t/premiumFrequency),1,x)/premiumFrequency)
}

probabilityatguarantee=0
found=FALSE

for(t in 0:((w-x-u)*benefitFrequency)) {
  if(t<guaranteePeriod*benefitFrequency) {
    probabilityatguarantee = probabilityatguarantee + udeferredtqselectx(floor(t/benefitFrequency + u),1,x)/benefitFrequency
  }
  
  tvalues = c(tvalues, t/benefitFrequency + u)
  losses = c(losses, lossFunction(t/benefitFrequency+u, premium))
  
  if(t>=guaranteePeriod*benefitFrequency) {
    
    if(found==FALSE) {
      for(t in 1:(guaranteePeriod*benefitFrequency) -1) {
        probabilities = c(probabilities, probabilityatguarantee)
      }
    } 
    found=TRUE
    probabilities = c(probabilities, udeferredtqselectx(floor((t/benefitFrequency) + u), 1,x)/benefitFrequency) 
  }
}

print(length(losses))
print(length(probabilities))
if(guaranteePeriod>0) {
  plot(losses,probabilities, main="Pdf of Loss Function", xlab="Loss", ylab="Probability")
}

#### DO NOT COMBINE GUARANTEED PROBABILITIES ####
probabilities <- numeric(0)
losses <- numeric(0)
tvalues <- numeric(0)

for(t in 0:(u*premiumFrequency - 1)) {
  tvalues = c(tvalues, t/premiumFrequency)
  losses = c(losses, lossFunction(t/premiumFrequency, premium))
  probabilities = c(probabilities, udeferredtqselectx(floor(t/premiumFrequency),1,x)/premiumFrequency)
}
for(t in 0:((w-x-u)*benefitFrequency)) {  
  tvalues = c(tvalues, t/benefitFrequency + u)
  losses = c(losses, lossFunction(t/benefitFrequency+u, premium))
  probabilities = c(probabilities, udeferredtqselectx(floor((t/benefitFrequency) + u), 1,x)/benefitFrequency) 
}

LossesTable <- data.frame(tvalues,losses, probabilities)

#### EXPECTED VALUES and VARIANCE VALUES ####
expectedValues <- numeric(0)
varianceValues <- numeric(0)

j= 1
for(x in 30:60) {
  premium = premiums[j]
  j = j +1
  u = 65-x
  expectedValues = c(expectedValues, 
        expectedValueLossFunction(premium,x,u))
  varianceValues = c(varianceValues,
        varianceLossFunction(premium,x,u))
}
expectedValues = round(expectedValues,5)

#### PLOTS ####
x = 30; u = 65-x
plot(tvalues, losses, main="Loss Function vs Time", ylab="Loss", xlab="K(x)")
plot(losses,probabilities, main="Pdf of Loss Function", xlab="Loss", ylab="Probability")
orderedLossesTable = LossesTable[with(LossesTable,order(losses)),]
row.names(orderedLossesTable)<-NULL

orderedProbs = orderedLossesTable[,3]
orderedLosses = orderedLossesTable[,2]
cdf <- numeric(0)

last = 0
for(k in 1:length(orderedProbs) ){
  last = last + orderedProbs[k]
  cdf = c(cdf, last)
}

ages= seq(30:60)+29
plot(orderedLosses, cdf, main="Cdf of Loss Functon", xlab="Loss", ylab="P(Loss<Value)")
plot(ages, varianceValues, main="Variance Values", ylab="Variance", xlab="Age")
plot(ages, expectedValues, main="Expected Values", ylab="Expected Value", xlab="Age")

ages = seq(30:59)+29
plot(ages,count, main="Age Distribution", xlab="Age", ylab = "Number PH")

#### TOTALS ####
ProspectiveTable <- data.frame(seq(30:60)+29, expectedValues, varianceValues)
names(ProspectiveTable) = c("x", "E(Loss)", "V(Loss)")

totalSD = sqrt(sum(varianceValues*count))
totalExpectedValue = (sum(expectedValues*count))
Zvalue = ((0-totalExpectedValue)/totalSD)
Ploss = (1-pnorm(Zvalue))
cat("markup: ", markup)
cat("\ntotal SD:", totalSD)
cat("\ntotal Expected Value:", totalExpectedValue)
cat("\nprobability of loss:", Ploss)
cat("\n\n")
names(PremiumTable) = c("age", "premium", "total")

rm(last,ages, j, k)