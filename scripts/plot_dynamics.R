library(matrixStats)
## Plot model fits
save_plot=0
nsim <- 100
posteriors<-processed_chains$pars
# Sample
pars <- posteriors[sample(nrow(posteriors), nsim), ]

inc_GI<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
inc_GII<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases1_1<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases4_1<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases14_1<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases64_1<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases65p_1<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases1_2<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases4_2<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases14_2<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases64_2<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)
cases65p_2<-matrix(0,ncol = nsim, nrow = length(days_vec)+1)




for (h in 1:nsim){
print(100*(h/nsim))
theta=c(pars[h,]/unlist(params$scaling_fac))
c_mat<-params$transmission
c_mat2<-params$transmission_holi

c_mat[adult_id,adult_id]<-c_mat[adult_id,adult_id] * theta[["aduRR"]]
c_mat2[adult_id,adult_id]<-c_mat2[adult_id,adult_id]*  theta[["aduRR"]]


p<-list(
  beta_j  = theta[['beta_j']],   # transm coefficient
  beta_k  = theta[['beta_k']],   # transm coefficient
  repfac= theta[['repfac']],    # Community/reported ratio
  delta = theta[['delta']],      # maternal AB duration (days)
  tau   = theta[['tau']],       # Overall immunity duration (years)0
  w1 = theta[['w1_j']],
  w1 = theta[['w1_k']],
  rho=  theta[['rho']],
  crossp_j=  theta[['crossp_jk']],
  crossp_k=  theta[['crossp_kj']],
  pop  = pop,
  init  = init,
  mu    = params$mu,
  m     = c_mat,
  m_holi= c_mat2,
  aging_vec =params$aging_vec,
  school_step= as.double(params$school),
  n_school_steps=params$n_school_steps,
  N_age = params$N_age
) # number of age groups



filter2$run(p, save_history = TRUE)
sims<-drop(filter2$history())
state<-filter2$state()

inc_GI[,h]<- (sims['inc_day_k',])
inc_GII[,h]<-(sims['inc_day_j',])
cases1_1[,h]<-sims['cases_day1_str1',]
cases4_1[,h]<-sims['cases_day2_str1',]
cases14_1[,h]<-sims['cases_day3_str1',]
cases64_1[,h]<-sims['cases_day4_str1',]
cases65p_1[,h]<-sims['cases_day5_str1',]
cases1_2[,h]<-sims['cases_day1_str2',]
cases4_2[,h]<-sims['cases_day2_str2',]
cases14_2[,h]<-sims['cases_day3_str2',]
cases64_2[,h]<-sims['cases_day4_str2',]
cases65p_2[,h]<-sims['cases_day5_str2',]


}



########  GI
GIcol<-"#69b3a2"

x <- days_vec
y<-inc_GI
y<- y[-1,]

df_s <- as.data.frame(
  rowQuantiles(y,
               probs = c(0.25, 0.5, 0.75)
  )
)
df_s$x <- x


df_sim <- data.frame(
  x = x,
  y=y
)

dat_sim <- reshape2::melt(df_sim, id = "x")


incGI <- ggplot(data = df_s, aes(x = x)) +
  geom_line(
    data = dat_sim, aes(x = x, y = value, group = variable), col = "grey",
    alpha = 0.2, lwd = 0.4
  ) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = GIcol, alpha = 0.2) +
  geom_line(aes(y = `50%`), col = GIcol, lwd = 1) +
  labs(title = "GI incidence", x = " ", y = "Incidence (symptomatic)\n (per day)") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("1991-01-01"),as.Date("2020-01-01") )) +
  #ylim(0,1.5e5)+
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
windows()
gridExtra::grid.arrange(incGI)
if (save_plot==1){
  save(
    incGI, file = here("output", "inc_GI.rdata"))
}






########  GII

GIIcol <- "#ff4d4d"

x <- days_vec
y<-inc_GII
y<- y[-1,]

df_s <- as.data.frame(
  rowQuantiles(y,
               probs = c(0.25, 0.5, 0.75)
  )
)
df_s$x <- x


df_sim <- data.frame(
  x = x,
  y=y
)

dat_sim <- reshape2::melt(df_sim, id = "x")


incGII <- ggplot(data = df_s, aes(x = x)) +
  geom_line(
    data = dat_sim, aes(x = x, y = value, group = variable), col = "grey",
    alpha = 0.2, lwd = 0.4
  ) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = GIIcol, alpha = 0.2) +
  geom_line(aes(y = `50%`), col = GIIcol, lwd = 1) +
  labs(title = "GII incidence", x = " ", y = "Incidence (symptomatic)\n (per day)") +
  theme_minimal() +
  scale_x_date(date_breaks = "4 month", date_labels = "%b-%Y", 
               limits = c(as.Date("2015-01-01"),as.Date("2020-01-01") )) +
  ylim(0,1.5e5)+
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
windows()
gridExtra::grid.arrange(incGII)
if (save_plot==1){
  save(
    incGII, file = here("output", "inc_GII.rdata"))
}


# Stacked incidence
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
mo <- as.Date(seq(as.Date(sim_startdate),as.Date(sim_enddate),
                  by = "year")) 

id<-which(days_vec %in% mo)
mo<-days_vec

c1<-cases1_1[-1,]
c2<-cases4_1[-1,]
c3<-cases14_1[-1,]
c4<-cases64_1[-1,]
c5<-cases65p_1[-1,]


df_1 <- as.data.frame(rowQuantiles(c1,probs = c(0.025, 0.5, 0.975)))
df_1$x <- days_vec

df_2 <- as.data.frame(rowQuantiles(c2,probs = c(0.025, 0.5, 0.975)))
df_2$x <- days_vec

df_3 <- as.data.frame(rowQuantiles(c3, probs = c(0.025, 0.5, 0.975)))
df_3$x <- days_vec

df_4 <- as.data.frame(rowQuantiles(c4,probs = c(0.025, 0.5, 0.975)))
df_4$x <- days_vec

df_5 <- as.data.frame(rowQuantiles(c5,probs = c(0.025, 0.5, 0.975)))
df_5$x <- days_vec


tmp1 <- data.frame(date_index = mo, count = round(df_1$`50%`), incident = "0_1", id = 1)
tmp2 <- data.frame(date_index = mo, count = round(df_2$`50%`), incident = "1_4", id = 2)
tmp3 <- data.frame(date_index = mo, count = round(df_3$`50%`), incident = "5_14", id = 3)
tmp4 <- data.frame(date_index = mo, count = round(df_4$`50%`), incident = "15_65", id = 4)
tmp5 <- data.frame(date_index = mo, count = round(df_5$`50%`), incident = "65p", id = 5)

linelist <- rbind(tmp1, tmp2, tmp3, tmp4,tmp5)
linelist$Outcome <- factor(linelist$incident, levels = c("0_1", "1_4", "5_14", "15_65", "65p"))
colors <- c("0_1" = "brown1", "1_4" = "gold", "5_14" = "lightseagreen",
            "15_65" = "grey43", "65p"="orange")

linelistg2<-linelist

stack <- ggplot(linelist, aes(
  fill = Outcome,
  y = count, x = date_index
)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Incidence GII", x = " ", y = "Incidence\n (symptomatic) per year") +
  theme_classic() +
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%b-%Y", 
               limits = as.Date(c("2017-06-01", "2018-05-30"))) +
  theme( # legend.position = "bottom",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  guides(col = guide_legend(title = NULL))

windows()
stack
save(stack, file = here("output",  "stack_incidenceGII.rdata"))


# Stacked incidence
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
mo <- as.Date(seq(as.Date(sim_startdate),as.Date(sim_enddate),
                  by = "year")) 

id<-which(days_vec %in% mo)
mo<-days_vec

c1<-cases1_2[-1,]
c2<-cases4_2[-1,]
c3<-cases14_2[-1,]
c4<-cases64_2[-1,]
c5<-cases65p_2[-1,]


df_1 <- as.data.frame(rowQuantiles(c1,probs = c(0.025, 0.5, 0.975)))
df_1$x <- days_vec

df_2 <- as.data.frame(rowQuantiles(c2,probs = c(0.025, 0.5, 0.975)))
df_2$x <- days_vec

df_3 <- as.data.frame(rowQuantiles(c3, probs = c(0.025, 0.5, 0.975)))
df_3$x <- days_vec

df_4 <- as.data.frame(rowQuantiles(c4,probs = c(0.025, 0.5, 0.975)))
df_4$x <- days_vec

df_5 <- as.data.frame(rowQuantiles(c5,probs = c(0.025, 0.5, 0.975)))
df_5$x <- days_vec


tmp1 <- data.frame(date_index = mo, count = round(df_1$`50%`), incident = "0_1", id = 1)
tmp2 <- data.frame(date_index = mo, count = round(df_2$`50%`), incident = "1_4", id = 2)
tmp3 <- data.frame(date_index = mo, count = round(df_3$`50%`), incident = "5_14", id = 3)
tmp4 <- data.frame(date_index = mo, count = round(df_4$`50%`), incident = "15_65", id = 4)
tmp5 <- data.frame(date_index = mo, count = round(df_5$`50%`), incident = "65p", id = 5)

linelist <- rbind(tmp1, tmp2, tmp3, tmp4,tmp5)
linelist$Outcome <- factor(linelist$incident, levels = c("0_1", "1_4", "5_14", "15_65", "65p"))
colors <- c("0_1" = "brown1", "1_4" = "gold", "5_14" = "lightseagreen",
            "15_65" = "grey43", "65p"="orange")

linelistg1<-linelist

stack <- ggplot(linelist, aes(
  fill = Outcome,
  y = count, x = date_index
)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Incidence GI", x = " ", y = "Incidence\n (symptomatic) per year") +
  theme_classic() +
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%b-%Y", 
               limits = as.Date(c("2017-06-01", "2018-05-30"))) +
  theme( # legend.position = "bottom",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  guides(col = guide_legend(title = NULL))

windows()
stack
save(stack, file = here("output",  "stack_incidenceGI.rdata"))



########


incig1<-ggplot(linelistg1, aes(x=date_index, y=count)) +
  geom_line() +
  facet_wrap(~Outcome)+
  labs(title = "Incidence GI", x = " ", y = "Incidence\n (symptomatic) per year") +
  #theme_classic() +
  scale_x_date(date_breaks = "6 month", 
               date_labels = "%b-%Y", 
               limits = as.Date(c("2014-06-01", "2018-05-30"))) +
  ylim(0,10000)+
  theme( # legend.position = "bottom",
    #panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )#, scales="free")
  windows()
gridExtra::grid.arrange(incig1)


incig2<-ggplot(linelistg2, aes(x=date_index, y=count)) +
  geom_line() +
  facet_wrap(~Outcome)+
  labs(title = "Incidence GII", x = " ", y = "Incidence\n (symptomatic) per year") +
  #theme_classic() +
  scale_x_date(date_breaks = "6 month", 
               date_labels = "%b-%Y", 
               limits = as.Date(c("2014-06-01", "2018-05-30"))) +
  ylim(0,10000)+
  theme( # legend.position = "bottom",
    #panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )#, scales="free")
windows()
gridExtra::grid.arrange(incig2)


