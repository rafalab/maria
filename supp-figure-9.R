#####
#####
#####    Code for supplementary figure 9
#####
#####

### Loading data
source("init.R")
load("rdas/fit-models.rda")

### Only data for Louisiana
dat <- combined_monthly_res %>% 
        filter(state=="LA") 

### Plot
p <- dat %>% filter(Date>="2005-01-01" & Date<="2006-05-01") %>% 
  ggplot(aes(Date, 100*increase)) +geom_hline(yintercept=0,lty=2,col="red") +
  geom_errorbar(aes(ymin = 100 * (increase - 2*se/(s_hat+year_offset)),
                    ymax = 100 * (increase + 2*se/(s_hat+year_offset)))) +
  geom_ribbon(aes(ymin = 100 * (-2*sd/(s_hat+year_offset)),
                  ymax = 100 * (+2*sd/(s_hat+year_offset))), alpha = 0.50) +
  geom_point(size = 0.75) +
  xlab("") +
  ylab("% Change in mortality") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months") +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

### Saving supp figure 9
ggsave(paste("figs/supp-figure-9.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)