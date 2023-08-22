
#############
library("matrixStats")
library("here")
library("ggplot2")
# Load 
load(here("output","baseline.RData"))
load(here("output","sche_1yr.RData"))
load(here("output","sche_65yr.RData"))
load(here("output","sche_1_65yr.RData"))
load(here("output","camp_5yr.RData"))
load(here("output","camp_65yr.RData"))
load(here("output","camp_5_65yr.RData"))
##
range<-c(0.25,0.5,0.75)


tmp<-t(base$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumbase <- as.data.frame(
  rowQuantiles(x, probs = range ))
#
tmp<-t(sche_1yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumsche_1yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(sche_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumsche_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(sche_1_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumsche_1_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(camp_5yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumcamp_5yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(camp_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumcamp_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(camp_5_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumcamp_5_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#########
# Cases preventes
#########

#
tmp<-t(base$all_inf_day - sche_1yr$all_inf_day  )
x<-apply(tmp,2,FUN=cumsum)

cumred_sche_1yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(base$all_inf_day -sche_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumred_sche_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(base$all_inf_day - sche_1_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumred_sche_1_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(base$all_inf_day -camp_5yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumred_camp_5yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(base$all_inf_day -camp_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumred_camp_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(base$all_inf_day -camp_5_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

cumred_camp_5_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))


###############################
## vaccine doses
##############################
tmp<-t(base$vaccines_day)
x<-apply(tmp,2,FUN=cumsum)

cumVbase <- as.data.frame(
  rowQuantiles(x, probs = range ))
#
tmp<-t(sche_1yr$vaccines_day)
x<-apply(tmp,2,FUN=cumsum)

cumVsche_1yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(sche_65yr$vaccines_day)
x<-apply(tmp,2,FUN=cumsum)

cumVsche_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(sche_1_65yr$vaccines_day)
x<-apply(tmp,2,FUN=cumsum)

cumVsche_1_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(camp_5yr$vaccines_day)
x<-apply(tmp,2,FUN=cumsum)

cumVcamp_5yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(camp_65yr$vaccines_day)
x<-apply(tmp,2,FUN=cumsum)

cumVcamp_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))

#
tmp<-t(camp_5_65yr$vaccines_day)
x<-apply(tmp,2,FUN=cumsum)

cumVcamp_5_65yr <- as.data.frame(
  rowQuantiles(x, probs = range ))



###############################
## Doses per case averteed
##############################
#
tmp<-t(sche_1yr$vaccines_day)
d<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$all_inf_day - sche_1yr$all_inf_day  )
x<-apply(tmp,2,FUN=cumsum)

dosecase_sche_1yr <- as.data.frame(
  rowQuantiles(d/x, probs = range ))

#
tmp<-t(sche_65yr$vaccines_day)
d<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$all_inf_day -sche_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

dosecase_sche_65yr <- as.data.frame(
  rowQuantiles(d/x, probs = range ))

#
tmp<-t(sche_1_65yr$vaccines_day)
d<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$all_inf_day - sche_1_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

dosecase_sche_1_65yr <- as.data.frame(
  rowQuantiles(d/x, probs = range ))

#
tmp<-t(camp_5yr$vaccines_day)
d<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$all_inf_day -camp_5yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

dosecase_camp_5yr <- as.data.frame(
  rowQuantiles(d/x, probs = range ))

#
tmp<-t(camp_65yr$vaccines_day)
d<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$all_inf_day -camp_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

dosecase_camp_65yr <- as.data.frame(
  rowQuantiles(d/x, probs = range ))

#
tmp<-t(camp_5_65yr$vaccines_day)
d<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$all_inf_day -camp_5_65yr$all_inf_day)
x<-apply(tmp,2,FUN=cumsum)

dosecase_camp_5_65yr <- as.data.frame(
  rowQuantiles(d/x, probs = range ))

##

n_times<- (10 * 365)  
tt<-seq(1,n_times,1)

# Vaccine sche
d_case<-rbind(
dosecase_sche_1yr[365*5,],
dosecase_sche_65yr[365*5,],
dosecase_sche_1_65yr[365*5,])


df <- data.frame(scen=c("1yr","65+","1 & 65+"),
                 dose=d_case[,2])
df$scen<-factor(df$scen,levels=c("1yr","65+","1 & 65+"))


p1<-ggplot(data=df, aes(x=scen, y=dose)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(x="target scenario", y="Dose per case averted", 
       title = "Vaccine schedule: 5 year impact")
p1

# Campaign
d_case<-rbind(
dosecase_camp_5yr[365*5,],
dosecase_camp_65yr[365*5,],
dosecase_camp_5_65yr[365*5,])


df <- data.frame(scen=c("5yr","65+","5 & 65+"),
                 dose=d_case[,2])
df$scen<-factor(df$scen,levels=c("5yr","65+","5 & 65+"))
# Basic barplot
p2<-ggplot(data=df, aes(x=scen, y=dose)) +
  geom_bar(stat="identity", fill="orange")+
  theme_minimal()+
  labs(x="target scenario", y="Dose per case averted", 
       title = "Vaccine campaign: 5 year impact")
p2

gridExtra::grid.arrange(p1,p2)


# Reduction Vaccine sche
d_case<-rbind(
  cumred_sche_1yr[365*5,],
  cumred_sche_65yr[365*5,],
  cumred_sche_1_65yr[365*5,])


df <- data.frame(scen=c("1yr","65+","1 & 65+"),
                 dose=d_case[,2])
df$scen<-factor(df$scen,levels=c("1yr","65+","1 & 65+"))


p3<-ggplot(data=df, aes(x=scen, y=dose)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(x="target scenario", y="Cases averted", 
       title = "Vaccine schedule: 5 year impact")
p3

# Campaign
d_case<-rbind(
  cumred_camp_5yr[365*5,],
  cumred_camp_65yr[365*5,],
  cumred_camp_5_65yr[365*5,])


df <- data.frame(scen=c("5yr","65+","5 & 65+"),
                 dose=d_case[,2])
df$scen<-factor(df$scen,levels=c("5yr","65+","5 & 65+"))
# Basic barplot
p4<-ggplot(data=df, aes(x=scen, y=dose)) +
  geom_bar(stat="identity", fill="orange")+
  theme_minimal()+
  labs(x="target scenario", y="Cases averted", 
       title = "Vaccine campaign: 5 year impact")
p4

gridExtra::grid.arrange(p3,p4)

## stacked
tmp<-t(base$all_inf_day)
d<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$gi3_inf_day)
n1<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$gi_inf_day)
n2<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$gii4_inf_day)
n3<-apply(tmp,2,FUN=cumsum)

tmp<-t(base$gii_inf_day)
n4<-apply(tmp,2,FUN=cumsum)


gi3 <- as.data.frame(
  rowQuantiles(n1/d, probs = range ))
gi3$gen<-"GI3"
gi3$day<-tt
  
gi <- as.data.frame(
  rowQuantiles(n2/d, probs = range ))
gi$gen<-"OtherGI"
gi$day<-tt

gii4 <- as.data.frame(
  rowQuantiles(n3/d, probs = range ))
gii4$gen<-"GII4"
gii4$day<-tt

gii <- as.data.frame(
  rowQuantiles(n4/d, probs = range ))
gii$gen<-"OtherGII"
gii$day<-tt

df<-rbind(gi3,gi,gii4,gii)
df$day<-factor(df$day)

id1<- which(df$day==365)
id2<- which(df$day==365*2)
id3<- which(df$day==365*3)
id4<- which(df$day==365*4)
id5<- which(df$day==365*5)
id6<- which(df$day==365*6)
id7<- which(df$day==365*7)
id8<- which(df$day==365*8)
id9<- which(df$day==365*9)
id10<- which(df$day==365*10)

df<-df[c(id1,id2,id3,id4,
       id5,id6,id7,id8,id9,id10),]
df$gen<-factor(df$gen, levels = c("GI3","GI","GII4","GII"))

ggplot(data=df, aes(x=day, y="50%", fill=gen)) +
  geom_bar(position="stack",stat="identity")
  

ggplot(df, aes(day, fill = gen)) +
  geom_histogram(binwidth = 100, position = "fill")


ggplot(df, aes(day, "50%")) +
  geom_area(aes(fill = gen))



##

redu <- ggplot(data = cumred_sche_1yr, aes(x = tt)) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "firebrick", 
              alpha = 0.2) +
  geom_line(aes(y = `50%`), col = "firebrick", lwd = 1) +
  geom_ribbon(data = cumred_sche_65yr, 
              aes(x=tt,ymin = `25%`, ymax = `75%`), 
              fill = "orange", alpha = 0.2) +
  geom_line(data = cumred_sche_65yr,aes(x=tt,y = `50%`), 
            col = "orange", lwd = 1) +
  geom_ribbon(data = cumred_sche_1_65yr, 
              aes(x=tt,ymin = `25%`, ymax = `75%`), 
              fill = "steelblue", alpha = 0.2) +
  geom_line(data = cumred_sche_1_65yr,aes(x=tt,y = `50%`), 
            col = "steelblue", lwd = 1) +
  ylim(0,1e7)+
  labs(title = "Vaccine schedule impact", x = "days", y = "cases averted") +
  theme_minimal() +
  #ylim(0,1.5e5)+
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
redu



reduC <- ggplot(data = cumred_camp_5yr, aes(x = tt)) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "firebrick", 
              alpha = 0.2) +
  geom_line(aes(y = `50%`), col = "firebrick", lwd = 1) +
  geom_ribbon(data = cumred_camp_65yr, 
              aes(x=tt,ymin = `25%`, ymax = `75%`), 
              fill = "orange", alpha = 0.2) +
  geom_line(data = cumred_camp_65yr,aes(x=tt,y = `50%`), 
            col = "orange", lwd = 1) +
  geom_ribbon(data = cumred_camp_5_65yr, 
              aes(x=tt,ymin = `25%`, ymax = `75%`), 
              fill = "steelblue", alpha = 0.2) +
  geom_line(data = cumred_camp_5_65yr,aes(x=tt,y = `50%`), 
            col = "steelblue", lwd = 1) +
  ylim(0,2.5e6)+
  labs(title = "Vaccine campign impact", x = "days", y = "cases averted") +
  theme_minimal() +
  #ylim(0,1.5e5)+
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
reduC



