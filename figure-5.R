### Loading data
source("init.R")
###
### Stratifying by demographic variables & fitting daily model
###
load("rdas/pr-pop-age-gender.rda")
load("rdas/pr-icd.rda")
#groups <- expand.grid(gender = list(c("M","F"), "M", "F"), 
#                      interval = c("0-39", "40-59", "60-80", "80-Inf"), 
#                      stringsAsFactors = FALSE) %>%
#  separate(interval, c("start", "end"), sep="-", convert=TRUE)

groups <- expand.grid(gender = list(c("M","F"), "M", "F"), 
                      interval = c("0-39","40-49","50-59", "0-59",
                                   "60-69",
                                   "70-80","80-Inf"), 
                      stringsAsFactors = FALSE) %>%
  separate(interval, c("start", "end"), sep="-", convert=TRUE)
groups <- groups[-c(1,4,10,11,12),]

### Daily model by sex
res <- lapply(1:nrow(groups), function(i){
  
  dat <- pr_icd %>% filter(Gender %in% groups$gender[[i]] & 
                             Age >= groups$start[i] & 
                             Age <= groups$end[i]) %>%
    right_join(pr_population, by = "Date") %>% 
    group_by(Date) %>%
    summarize(Deaths = sum(Deaths, na.rm = TRUE), Population = Population[1]) %>%
    mutate(Year = year(Date), Month = month(Date), Day = day(Date),
           yd = yday(Date),
           t = as.numeric(Date) - min(as.numeric(Date)))
  
  prop_by_year <- pr_pop_age_gender %>% filter(Year %in% unique(dat$Year)) %>%
    filter(gender %in% groups$gender[[i]] & 
             start >= groups$start[i] & end >= groups$start[i] &
             start <= groups$end[i] & end <= groups$end[i]) %>%
    group_by(Year) %>%
    summarize(prop = sum(prop))
  
  dat %>% left_join(prop_by_year, by = "Year") %>%
    mutate(Population = Population*prop) %>%
    select(-prop)

  gender <- ifelse(length(groups$gender[[i]])==2, "All", groups$gender[[i]])
  
  fit_daily_model(dat,
                  hurricane_dates = hurricane_dates$PR[3],
                  knots_per_year = 4.5,
                  outlier_years = c(2018),
                  last_day = last_day,
                  extrapolate_last_year = 1) %>%
    mutate(age_group = paste0(groups$start[i],"-",groups$end[i]),
           gender = gender)
})
combined_demographic_res <- do.call(bind_rows, res)

## figure 5
p <- combined_demographic_res %>% 
  filter(Year == 2017 & gender=="All") %>%
  ggplot(aes(Date, (exp(f_hat)-1)*100, 
             ymin = (exp(f_hat-1.96*se)-1)*100,
             ymax = (exp(f_hat+1.96*se)-1)*100,
             col = age_group,
             fill = age_group)) +
  geom_ribbon(color = 0, alpha = 0.25) + 
  geom_hline(yintercept=0, lty=2, col="red") +  
  geom_line(size=1) +
  scale_x_date(date_labels = "%b", date_breaks = "1 months") +
  xlab("") +
  ylab("% Change in mortality") +
  scale_color_manual(name="", values=c("#E69F00","#CC79A7","#0571b0","#009E73","#D55E00")) +
  scale_fill_manual(name="", values=c("#E69F00","#CC79A7","#0571b0","#009E73","#D55E00")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold", color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(cbbPalette) <- c("black", "orange", "skyblue","green","yellow","blue","vermillon","reddish purple")
names(cbbPalette) <- c("NA","Irma", "Sandy","Hugo","NA","georges","maria","katrina")
cbbPalette

ggsave(paste("figs/figure-5.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

## supp figure
p <- combined_demographic_res %>% 
  filter(Year == 2017 & gender!="All") %>%
  ggplot(aes(Date, 100*(exp(f_hat)-1), 
             ymin = 100*(exp(f_hat - 2*se)-1), ymax = 100*(exp(f_hat + 2*se)-1), 
             fill = gender, col = gender)) +
  geom_hline(yintercept=0, lty=2, col="red") +  
  geom_ribbon(aes(), alpha = 0.25, color = 0) +
  geom_line(size = .8) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  xlab("") +
  ylab("% Change in mortality") +
  facet_wrap( ~ age_group) +
  scale_color_manual(name="", values=c("#ca0020","#525252"), labels=c("Female", "Male")) + 
  scale_fill_manual(name="", values=c("#ca0020","#525252"), labels=c("Female", "Male")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"),
        legend.position = "bottom")

ggsave(paste("figs/supp-figure-10.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)


### cp icd map file to tables
file.copy("data/ICD10_groups.csv", "figs/supp-table-2.csv", overwrite = TRUE)
