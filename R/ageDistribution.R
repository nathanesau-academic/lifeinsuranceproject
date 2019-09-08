setwd("~/Dropbox/SFU/Past/1141/Acma320/Project/R/")

# We know that there are 10,000 policyholders
#   20% are between ages 30-40
#   30% are between ages 40-50
#   50% are between ages 50-60
#
# Assume that as age increases, number of policyholders at that age increases
# So the number of PHs is a function of age, and weighted by 1000

set.seed(5)
group1 <- floor(sort(runif(2000,30,40)))
group2 <- floor(sort(runif(3000,40,50)))
group3 <- floor(sort(runif(5000,50,60)))

group <- c(group1,group2,group3)
distribution <- as.data.frame(table(group))

# export a column of values
count <- distribution[,2]
write.csv(count, "ageAmounts.csv",row.names=FALSE)