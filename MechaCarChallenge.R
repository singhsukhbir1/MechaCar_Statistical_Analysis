#Deliverable 1--------------------------------
# import the library(dpylr)
library(dplyr)

#import the csv file
mechaCar <- read.csv(file = "MechaCar_mpg.csv",check.names = F,stringsAsFactors = F)

#perform linear regression
lm(mpg ~ vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD ,data=mechaCar)
summary(lm(mpg ~ vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD ,data=mechaCar))

#Deliverable 2-----------------------------------
#import the csv file
suspensionCoil <- read.csv(file="Suspension_Coil.csv",check.names = F,stringsAsFactors = F)

#create total summary
total_summary <- suspensionCoil %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = "keep")
#create lot summary grouped bu manufacturing lot
lot_summary <- suspensionCoil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = "keep")

#Deliverable 3------------------------------
?t.test()
t.test(log10(lot_summary$Mean),mu=mean(log10(suspensionCoil$PSI)))

lot1 <- suspensionCoil %>% filter(Manufacturing_Lot=="Lot1")
lot2 <- suspensionCoil %>% filter(Manufacturing_Lot=="Lot2")
lot3 <- suspensionCoil %>% filter(Manufacturing_Lot=="Lot3")

#3 t.tests()
t.test(log10(lot1$PSI),mu=mean(log10(suspensionCoil$PSI)))
t.test(log10(lot2$PSI),mu=mean(log10(suspensionCoil$PSI)))
t.test(log10(lot3$PSI),mu=mean(log10(suspensionCoil$PSI)))

       