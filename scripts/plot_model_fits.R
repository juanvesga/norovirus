library(matrixStats)
## Plot model fits
save_plot=0
fake=0
nsim <- 500
ids<- which(processed_chains$probabilities[, "log_posterior"] > -9500) 
posteriors<-processed_chains$trajectories$state[,ids,]
# Sample
sims <- posteriors[,sample(ncol(posteriors), nsim), ]

data<-data_all

######## IID2 Community incidence
x_d <- c(1, 3, 5, 7, 9) # bin x axis positions

df_d <- data.frame(
  x = x_d,
  inc = data_iid2.c4$per1000personyears,
  low = data_iid2.c4$CI_lower,
  up = data_iid2.c4$CI_upper
)


t<-which(sims["t",1,]%in%
           data$time_end[which(!is.na(data$cases_a1))])
irates <-1000*(cbind(
  (sims['inc_year_gii4_1',,t]+
       sims['inc_year_gii_1',,t]+
       sims['inc_year_gi3_1',,t]+
       sims['inc_year_gi_1',,t])/(sims['person_year1',,t]/365),
  (sims['inc_year_gii4_2',,t]+
      sims['inc_year_gii4_2',,t]+
      sims['inc_year_gi3_2',,t]+
      sims['inc_year_gi_2',,t])/(sims['person_year2',,t]/365),
  (sims['inc_year_gii4_3',,t]+
     sims['inc_year_gii4_3',,t]+
     sims['inc_year_gi3_3',,t]+
     sims['inc_year_gi_3',,t])/(sims['person_year3',,t]/365) ,
  (sims['inc_year_gii4_4',,t]+
     sims['inc_year_gii4_4',,t]+
     sims['inc_year_gi3_4',,t]+
     sims['inc_year_gi_4',,t])/(sims['person_year4',,t]/365) ,
  (sims['inc_year_gii4_5',,t]+
     sims['inc_year_gii4_5',,t]+
     sims['inc_year_gi3_5',,t]+
     sims['inc_year_gi_5',,t])/(sims['person_year5',,t]/365)))


df_qtls <- as.data.frame(rowQuantiles(t(irates),
                                      probs = c(0.025, 0.5, 0.975)))

df1 <- data.frame(irates) 
colnames(df1) <- paste(c("0_1","1_4","5_14","15_64","65+"))
df_m <- reshape2::melt(df1)
df_m$variable <- as.factor(df_m$variable)
df_d$x <- factor(c("0_1","1_4","5_14","15_64","65+"))
df_qtls$x <- factor(c("0_1","1_4","5_14","15_64","65+"))


viol_col <- "#ff4d4d"# "chartreuse3" # "yellow3"
err_col <- "black"
data_col <- "black"

fits_iid2 <- ggplot() +
  geom_violin(
    data = df_m,
    aes(x = variable, y = value, fill = "Posterior Density"),
    draw_quantiles = c(0.5),
    width = 0.8,
    linetype = 1,
    trim = FALSE,
    color = "white",
    alpha = 0.5
  ) +
  geom_point(data = df_d, mapping = aes(x = x, y = inc, color = "Data (95% CI)"), size = 2, shape = 15) +
  geom_errorbar(
    mapping = aes(x = x, ymin = low, ymax = up), data = df_d,
    width = .2, position = position_dodge(.9)
  ) +
  labs(title = "Model vs IID2 community incidence", x = "Age group (years)", y = "Incidence per 1000 \n person-year") +
  theme_minimal() +
  ylim(0, 500) +
  scale_fill_manual(name = "", values = c("Posterior Density" = viol_col)) +
  scale_color_manual(name = "", values = c("Data (95% CI)" = data_col)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )


if (save_plot==1){
  save(fits_iid2, file = here("output","fits_iid2.rdata"))
}

######## SGSS cases reported 

id<-which(sims["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported))])
reported<-sims["reported_wk",,id]
reported_obs<-(data$reported[which(!is.na(data$reported))])


mo <- as.Date(seq(as.Date(sgss_start),
                  by = "week",
                  length.out = dim(reported)[2]
))

df_s <- as.data.frame(
  rowQuantiles(t(reported),
               probs = c(0.025, 0.5, 0.975)
  )
)
df_s$x <- mo

df_d <- data.frame(
  x = mo,
  cases = reported_obs
)

df_sim <- data.frame(
  x = mo,
  t(reported)
)

dat_sim <- reshape2::melt(df_sim, id = "x")


fits_sgss <- ggplot(data = df_s, aes(x = x)) +
  geom_line(
    data = dat_sim, aes(x = x, y = value, group = variable), col = "grey",
    alpha = 0.2, lwd = 0.4
  ) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "#69b3a2", alpha = 0.2) +
  geom_line(aes(y = `50%`), col = "#69b3a2", lwd = 1) +
  geom_point(data = df_d, aes(x = x, y = cases)) +
  labs(title = "Model vs SGSS reported cases", x = " ", y = "Cases reported\n (per week)") +
  theme_minimal() +
  scale_x_date(date_breaks = "4 month", date_labels = "%b-%Y") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )


if (save_plot==1){
  save(fits_sgss, file = here("output", "fits_sgss.rdata"))
}

######## Lindesmith seroprevallence children 
x_d <- c(1, 3, 5, 7, 9, 11) # bin x axis positions

id<-which(!is.na(data$sero1))

sero_obs<-c(data$sero1[id], data$sero2[id], data$sero3[id], data$sero4[id],
            data$sero5[id], data$sero6[id])*100

df_d <- data.frame(
  x = x_d,
  sero = sero_obs,
  low = sero_obs,
  up = sero_obs
)


id<-which(sims["t",1,]%in%
            data$time_end[which(!is.na(data$sero1))])

sero_model<-rbind(
  sims['seroprev_num1.2',,id]/sims['seroprev_den1.2',,id] ,
  sims['seroprev_num2.3',,id]/sims['seroprev_den2.3',,id] ,
  sims['seroprev_num3.4',,id]/sims['seroprev_den3.4',,id] ,
  sims['seroprev_num4.5',,id]/sims['seroprev_den4.5',,id] ,
  sims['seroprev_num5.6',,id]/sims['seroprev_den5.6',,id] ,
  sims['seroprev_num6.7',,id]/sims['seroprev_den6.7',,id] )*100



df_qtls <- as.data.frame(rowQuantiles((sero_model),
                                      probs = c(0.025, 0.5, 0.975)))

df1 <- data.frame(t(sero_model)) 
colnames(df1) <- paste(c("0_1","1_2","2_3","3_4","5_6","6_7"))
df_m <- reshape2::melt(df1)
df_m$variable <- as.factor(df_m$variable)
df_d$x <- factor(c("0_1","1_2","2_3","3_4","5_6","6_7"))
df_qtls$x <- factor(c("0_1","1_2","2_3","3_4","5_6","6_7"))


viol_col <-  "yellow3"
err_col <- "black"
data_col <- "black"

fits_sero <- ggplot() +
  geom_violin(
    data = df_m,
    aes(x = variable, y = value, fill = "Posterior Density"),
    draw_quantiles = c(0.5),
    width = 0.8,
    linetype = 1,
    trim = FALSE,
    color = viol_col,
    alpha = 0.5
  ) +
  geom_point(data = df_d, mapping = aes(x = x, y = sero, 
                                        color = "Data (95% CI)"), 
             size = 2, shape = 15) +
  geom_errorbar(
    mapping = aes(x = x, ymin = low, ymax = up), data = df_d,
    width = .2, position = position_dodge(.9)
  ) +
  labs(title = "Model vs Seroprevalence of GII.4", x = "Age group (years)", y = "Seroprevalence (%)") +
  theme_minimal() +
  ylim(0, 100) +
  scale_fill_manual(name = "", values = c("Posterior Density" = viol_col)) +
  scale_color_manual(name = "", values = c("Data (95% CI)" = data_col)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )


if (save_plot==1){
  save(fits_sero, file = here("output","fits_sero.rdata"))
}



######## harris strain type  


id<-which(sims["t",1,]%in%
            data$time_end[which(!is.na(data$gi_prev))])

gi3_model<-((sims['inc_year_gi3_1',,id]+
               sims['inc_year_gi3_2',,id]+
               sims['inc_year_gi3_3',,id]+
               sims['inc_year_gi3_4',,id]+
               sims['inc_year_gi3_5',,id]) /
              (  sims['inc_year_gii4_1',,id]+
                   sims['inc_year_gii4_1',,id]+
                   sims['inc_year_gii4_2',,id]+
                   sims['inc_year_gii4_3',,id]+
                   sims['inc_year_gii4_4',,id]+
                   sims['inc_year_gii4_5',,id]+
                   sims['inc_year_gii_1',,id]+
                   sims['inc_year_gii_1',,id]+
                   sims['inc_year_gii_2',,id]+
                   sims['inc_year_gii_3',,id]+
                   sims['inc_year_gii_4',,id]+
                   sims['inc_year_gii_5',,id]+
                   sims['inc_year_gi3_1',,id]+
                   sims['inc_year_gi3_1',,id]+
                   sims['inc_year_gi3_2',,id]+
                   sims['inc_year_gi3_3',,id]+
                   sims['inc_year_gi3_4',,id]+
                   sims['inc_year_gi3_5',,id]+
                   sims['inc_year_gi_1',,id]+
                   sims['inc_year_gi_1',,id]+
                   sims['inc_year_gi_2',,id]+
                   sims['inc_year_gi_3',,id]+
                   sims['inc_year_gi_4',,id]+
                   sims['inc_year_gi_5',,id]))

gi_model<-(
        sims['inc_year_gi_1',,id]+
        sims['inc_year_gi_1',,id]+
        sims['inc_year_gi_2',,id]+
        sims['inc_year_gi_3',,id]+
        sims['inc_year_gi_4',,id]+
        sims['inc_year_gi_5',,id]) /
       (  sims['inc_year_gii4_1',,id]+
            sims['inc_year_gii4_1',,id]+
            sims['inc_year_gii4_2',,id]+
            sims['inc_year_gii4_3',,id]+
            sims['inc_year_gii4_4',,id]+
            sims['inc_year_gii4_5',,id]+
            sims['inc_year_gii_1',,id]+
            sims['inc_year_gii_1',,id]+
            sims['inc_year_gii_2',,id]+
            sims['inc_year_gii_3',,id]+
            sims['inc_year_gii_4',,id]+
            sims['inc_year_gii_5',,id]+
            sims['inc_year_gi3_1',,id]+
            sims['inc_year_gi3_1',,id]+
            sims['inc_year_gi3_2',,id]+
            sims['inc_year_gi3_3',,id]+
            sims['inc_year_gi3_4',,id]+
            sims['inc_year_gi3_5',,id]+
            sims['inc_year_gi_1',,id]+
            sims['inc_year_gi_1',,id]+
            sims['inc_year_gi_2',,id]+
            sims['inc_year_gi_3',,id]+
            sims['inc_year_gi_4',,id]+
            sims['inc_year_gi_5',,id])

gii4_model<-((sims['inc_year_gii4_1',,id]+
                sims['inc_year_gii4_2',,id]+
                sims['inc_year_gii4_3',,id]+
                sims['inc_year_gii4_4',,id]+
                sims['inc_year_gii4_5',,id]) /
               (  sims['inc_year_gii4_1',,id]+
                    sims['inc_year_gii4_1',,id]+
                    sims['inc_year_gii4_2',,id]+
                    sims['inc_year_gii4_3',,id]+
                    sims['inc_year_gii4_4',,id]+
                    sims['inc_year_gii4_5',,id]+
                    sims['inc_year_gii_1',,id]+
                    sims['inc_year_gii_1',,id]+
                    sims['inc_year_gii_2',,id]+
                    sims['inc_year_gii_3',,id]+
                    sims['inc_year_gii_4',,id]+
                    sims['inc_year_gii_5',,id]+
                    sims['inc_year_gi3_1',,id]+
                    sims['inc_year_gi3_1',,id]+
                    sims['inc_year_gi3_2',,id]+
                    sims['inc_year_gi3_3',,id]+
                    sims['inc_year_gi3_4',,id]+
                    sims['inc_year_gi3_5',,id]+
                    sims['inc_year_gi_1',,id]+
                    sims['inc_year_gi_1',,id]+
                    sims['inc_year_gi_2',,id]+
                    sims['inc_year_gi_3',,id]+
                    sims['inc_year_gi_4',,id]+
                    sims['inc_year_gi_5',,id]))

gii_model<-((sims['inc_year_gii_1',,id]+
                sims['inc_year_gii_2',,id]+
                sims['inc_year_gii_3',,id]+
                sims['inc_year_gii_4',,id]+
                sims['inc_year_gii_5',,id]) /
               (  sims['inc_year_gii4_1',,id]+
                    sims['inc_year_gii4_1',,id]+
                    sims['inc_year_gii4_2',,id]+
                    sims['inc_year_gii4_3',,id]+
                    sims['inc_year_gii4_4',,id]+
                    sims['inc_year_gii4_5',,id]+
                    sims['inc_year_gii_1',,id]+
                    sims['inc_year_gii_1',,id]+
                    sims['inc_year_gii_2',,id]+
                    sims['inc_year_gii_3',,id]+
                    sims['inc_year_gii_4',,id]+
                    sims['inc_year_gii_5',,id]+
                    sims['inc_year_gi3_1',,id]+
                    sims['inc_year_gi3_1',,id]+
                    sims['inc_year_gi3_2',,id]+
                    sims['inc_year_gi3_3',,id]+
                    sims['inc_year_gi3_4',,id]+
                    sims['inc_year_gi3_5',,id]+
                    sims['inc_year_gi_1',,id]+
                    sims['inc_year_gi_1',,id]+
                    sims['inc_year_gi_2',,id]+
                    sims['inc_year_gi_3',,id]+
                    sims['inc_year_gi_4',,id]+
                    sims['inc_year_gi_5',,id]))






if (fake==1){
  
  g2<-rnorm(n = nsim, mean = 88, sd = 3 )
  g2[g2>100]<-100
  g1<-rnorm(n = nsim, mean = 100-88, sd = 3 )
  g1[g1<0]<-0
  
  strain_model<-cbind(g1, g2)
} else{
  strain_model<-cbind(gi3_model,gi_model, gii4_model,gii_model )*100
}


id<-which(!is.na(data$gi_prev))
strain_obs<-c(data$gi3_prev[id],
              data$gi_prev[id],
              data$gii4_prev[id],
              data$gii_prev[id])*100

x_d <- c(1,3,5,7) # bin x axis positions


df_d <- data.frame(
  x = x_d,
  strain = strain_obs,
  low = strain_obs,
  up = strain_obs
)

df_qtls <- as.data.frame(rowQuantiles(t(strain_model),
                                      probs = c(0.025, 0.5, 0.975)))

df1 <- data.frame((strain_model)) 
colnames(df1) <- paste(c("GI.3","Other GI","GII.4", "Other GII"))
df_m <- reshape2::melt(df1)
df_m$variable <- as.factor(df_m$variable)
df_d$x <- factor(c("GI.3","Other GI","GII.4", "Other GII"))
df_qtls$x <- factor(c("GI.3","Other GI","GII.4", "Other GII"))


viol_col <-  "orange"
err_col <- "black"
data_col <- "black"

fits_strain <- ggplot() +
  geom_violin(
    data = df_m,
    aes(x = variable, y = value, fill = "Posterior Density"),
    draw_quantiles = c(0.5),
    width = 0.8,
    linetype = 1,
    trim = FALSE,
    color = viol_col,
    alpha = 0.5
  ) +
  geom_point(data = df_d, mapping = aes(x = x, y = strain, 
                                        color = "Data (95% CI)"), 
             size = 2, shape = 15) +
  geom_errorbar(
    mapping = aes(x = x, ymin = low, ymax = up), data = df_d,
    width = .2, position = position_dodge(.9)
  ) +
  labs(title = "Model vs Genogroup type", x = "Genogroup", y = "Proportion (%)") +
  theme_minimal() +
  ylim(0, 100) +
  scale_fill_manual(name = "", values = c("Posterior Density" = viol_col)) +
  scale_color_manual(name = "", values = c("Data (95% CI)" = data_col)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )


if (save_plot==1){
  save(fits_strain, file = here("output","fits_strain.rdata"))
}

##
gridExtra::grid.arrange(fits_iid2)
gridExtra::grid.arrange(fits_sero)
gridExtra::grid.arrange(fits_sgss)
gridExtra::grid.arrange(fits_strain)

