######
######
######     Author: Rolando J. Acosta
######     Date  : September 1, 2018
######
######     Yearly offset for PR and FL
######     Supplemental figure 4
######
source("init.R")
load("rdas/fit-models.rda")

### Filtering data
dat <- combined_daily_res %>% select(Year, offset_year, offset_year_se, state) %>% unique()

p <- dat %>% 
      ggplot(aes(Year, exp(offset_year), color=state)) +
      geom_hline(yintercept = 1, lty=2, col="red") +
      geom_pointrange(aes(ymin=exp(offset_year-1.96*offset_year_se), 
                          ymax=exp(offset_year+1.96*offset_year_se))) +
      xlab("") +
      ylab("Death rate ratio") +
      scale_color_manual(name="", values=c("#ca0020","#525252")) +
      scale_x_discrete(limits = seq(1985,2018,by=2)) +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black"))
  

# Saving supp figure 4
ggsave(paste("figs/supp-figure-4.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
