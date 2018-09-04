######
######
######     Author: Rolando J. Acosta
######     Date  : August 1, 2018
######
######     Seasonal effects for Puerto Rico & Florida
######     Supplemental figure 5
######

### Loading data
source("init.R")
load("rdas/fit-models.rda")

### Filtering data: Taking only data for PR
res <- combined_daily_res %>% filter(state=="PR")

### Seasonal fit for PR
p <- seasonal_fit_viz(res, hurricane_dates$PR) + ggtitle("Seasonal fit for PR") +
      theme(axis.text.x=element_text(angle=0, hjust=1, face="bold",color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black")) 

# Saving supp figure 5A: Puerto Rico
ggsave(paste("figs/supp-figure-5A.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### Filtering data: Taking only data for FL
res <- combined_daily_res %>% filter(state=="FL")

### Seasonal fit for FL
p <- seasonal_fit_viz(res, hurricane_dates$FL) + ggtitle("Seasonal fit for FL") +
  theme(axis.text.x=element_text(angle=0, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))


# Saving supp figure 5B: Florida
ggsave(paste("figs/supp-figure-5B.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
