#####
#####
#####    Code for supplemental figure 12
#####
#####

source("init.R")
load("rdas/pr-daily.rda")

p <- pr_daily %>% filter(Year==1998) %>% 
      ggplot(aes(Date, Deaths)) + 
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months") +
      geom_vline(xintercept = make_date(1998,09,20), lty=2, col="red") +
      xlab("") +
      ylab("% Change in mortality") +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black")) + 
      geom_point(alpha=0.75)

### Saving supp fig 12
ggsave(paste("figs/supp-figure-12.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
