source("init.R")
load("rdas/pr-icd.rda")
last_day <- "2017-12-31" # Changing global variable

icds <- c("[I00,I99]", "[E00,E35]", "[G00,G99]", "[J00,J99]", "[A00,A79]", "[V01,Y98]")
res <- lapply(icds, function(i){
  dat <- pr_icd %>%
    filter(ICD == i) %>%
    right_join(pr_population, by = "Date") %>%
    group_by(Date) %>%
    summarize(Deaths = sum(Deaths, na.rm = TRUE), Population = Population[1]) %>%
    mutate(Year = year(Date), Month = month(Date), Day = day(Date),
           yd = yday(Date), 
           t = as.numeric(Date) - min(as.numeric(Date)))

  pr_res <- fit_daily_model(dat, 
                            hurricane_dates = hurricane_dates$PR[3],
                            knots_per_year = 4.5,
                            outlier_years = c(2018),
                            last_day = last_day,
                            extrapolate_last_year = 1) %>%
    mutate(ICD = str_remove(icd_map$Description[icd_map$ICD == i], ",.*"))
  })

res <- do.call(bind_rows, res)
res <- res %>% 
  mutate(ICD = ifelse(ICD=="Bacterial infections","Bacterial Infections",ICD),
         ICD = ifelse(ICD=="Diseases of the circulatory system","Circulatory System",ICD),
         ICD = ifelse(ICD=="Diseases of the nervous system","Nervous System",ICD),
         ICD = ifelse(ICD=="Diseases of the respiratory system","Respiratory System",ICD),
         ICD = ifelse(ICD=="Endocrine diseases","Endocrine Diseases",ICD),
         ICD = ifelse(ICD=="External causes of morbidity and mortality", "External Causes", ICD)) %>%
  mutate(ICD = factor(ICD, levels = c("Circulatory System", "Endocrine Diseases",
                                      "Nervous System", "Respiratory System",
                                      "Bacterial Infections", "External Causes")))
                                      
p <- res %>% filter(Year==2017) %>% 
  ggplot(aes(Date, y = f_hat, ymin = f_hat - 1.96*se, ymax = f_hat + 1.96*se)) +
  geom_hline(yintercept=0,lty=2,col="red") +
  geom_ribbon(alpha = 0.60) + 
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black")) + 
  ylab("Log death rate ratio") +
  facet_wrap( ~ ICD) 
### Saving figure 4
ggsave(paste("figs/figure-4.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
