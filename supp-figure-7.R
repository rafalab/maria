######
######     Author: Rolando J. Acosta
######     Date  : September 1, 2018
######
######     Goodness of fit for PR and FL
######     Supplemental figure 7
######
source("init.R")
load("rdas/fit-models.rda")

### Filtering data
dat <- combined_daily_res %>% filter(Year != 2001) %>%
        mutate(pearson_resids = (Deaths-fitted_values)/sqrt(fitted_values))
  
### Supp figure 7
p <- dat %>% ggplot(aes(sample = pearson_resids)) + 
          geom_qq(alpha = 0.5) +
          xlab("") +
          ylab("Density") +
          theme(axis.text.x=element_text(angle=0, hjust=1, face="bold",color="black"),
                axis.text.y=element_text(face="bold",color="black"),
                strip.text = element_text(size = 10,face="bold",color="black"), 
                axis.title = element_text(face="bold", color="black")) + 
  geom_abline(slope = 1, intercept = 0, col = "red") +
  facet_wrap( ~ state)

# Saving supp figure 5A: Puerto Rico
ggsave(paste("figs/supp-figure-7.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
