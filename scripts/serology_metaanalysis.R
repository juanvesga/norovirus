library(data.table)
library(meta)


# Aggregate data on prevalence of neutralising ABs against different strains of 
# Norovirus by year of age in children. Uses metanalysis to aggregate by age 

serodata <- data.table(Year = c(2008:2012),
                   Age_group = c("a1_2", "a1_2", "a1_2", "a1_2", "a1_2",
                                 "a2_3", "a2_3", "a2_3", "a2_3", "a2_3",
                                 "a3_4", "a3_4", "a3_4", "a3_4", "a3_4",
                                 "a4_5", "a4_5", "a4_5", "a4_5", "a4_5",
                                 "a5_6", "a5_6", "a5_6", "a5_6", "a5_6",
                                 "a6_7", "a6_7", "a6_7", "a6_7", "a6_7"), 
                   sample    = c(23,24,25,24,7,
                                 25,24,23,23,12,
                                 25,25,24,24,23,
                                 25,25,25,25,24,
                                 25,25,25,23,24,
                                 24,24,25,25,11),
                   positive = c(11,11,6,12,2,
                                11,9,15,15,6,
                                13,17,15,14,15,
                                16,15,18,14,16,
                                18,19,19,17,18,
                                19,12,21,21,8) )

serodata$Year <- as.factor(serodata$Year)
serodata$Age_group <- as.factor(serodata$Age_group)

analysis <- metaprop(data = serodata, 
                     event = positive,
                     n = sample,
                     studlab = Year,
                     byvar = serodata$Age_group)



summary(analysis)

windows()
forest(analysis)


fixed<- cbind(
  c(analysis$TE.fixed.w),
  c(analysis$lower.fixed.w),
  c(analysis$upper.fixed.w))

tab_fixed<-meta:::backtransf(fixed, sm=analysis$sm)

random<- cbind(
  c(analysis$TE.random.w),
  c(analysis$lower.random.w),
  c(analysis$upper.random.w))

tab_random<-meta:::backtransf(random, sm=analysis$sm)
write.csv(tab_random,here("data","serology_prev.csv"))

plot(tab_random[,1],ylim=c(0,1))
