######
######     Why we stopped at May 31, 2018
######     Supplemental figure 1
######

### Loading PR data
source("init.R")
load("rdas/pr-daily.rda")

### Code for supplemental figure 1
p <- pr_daily %>% filter(Year >= 2018) %>%
      ggplot(aes(Date, Deaths)) +
      geom_point(alpha=0.75) + 
      xlab("") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months") +
      geom_vline(xintercept = last_day, lty=2, col="red") +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black"))
  
### Saving supp figure 1
ggsave(paste("figs/supp-figure-1.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
