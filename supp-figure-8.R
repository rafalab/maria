source("init.R")
load("rdas/pr-icd.rda")

p <- pr_icd %>% 
      group_by(Date) %>% 
      summarize(nas = sum(is.na(ICD))) %>% 
      ggplot(aes(Date, 0, xend=Date, yend=nas)) + 
      geom_segment(alpha=0.75) +
      scale_y_continuous(trans = "sqrt", breaks = c(1, 5, 12, 25, 55)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
      ylab("Death certificates with no cause of death") +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-8.pdf"),
       plot = p,
       width = 6, height = 3,
       units = "in",
       dpi = 300)


  
