######
######
######     Author: Rolando J. Acosta
######     Date  : August 29, 2018
######
######     Population estimates for Puerto Rico, Florida, Louisiana, and New Jersey
######     Supplemental figure 2
######

### Loading data
source("init.R")
load("rdas/pr-daily.rda")
load("rdas/florida-daily.rda")
load("rdas/louisiana-monthly.rda")
load("rdas/nj-monthly.rda") 

### Supp figure 2A: PR linear interpolation
dat <- pr_daily %>% filter(Date<"2017-07-01")
p   <- population_viz(dat, month_breaks="24") +
        theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
              axis.text.y=element_text(face="bold",color="black"),
              strip.text = element_text(size = 10,face="bold",color="black"), 
              axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-2A.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### Supp figure 2B: PR Teralytics estimates
dat <- pr_daily %>% filter(Date>="2017-07-01" & Date <= "2018-05-31")
p   <- population_viz(dat, month_breaks="1") +
        theme(axis.text.x=element_text(angle=45, hjust=1, face="bold", size=10,color="black"),
              axis.text.y=element_text(face="bold",color="black"),
              strip.text = element_text(size = 10,face="bold",color="black"), 
              axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-2B.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### Supp figure 2C: PR final pop estimates
p <- population_viz(pr_daily, month_breaks="24") +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="bold", size=10,color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-2C.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### Supp figure 2D: FL pop estimates
p <- population_viz(florida_daily, month_breaks="3") +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold", size=10,color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-2D.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### Supp figure 2E: LA pop estimates
p <- population_viz(louisiana_monthly, month_breaks="6") + 
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold", size=10,color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-2E.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### Supp figure 2F: NJ pop estimates
p <- population_viz(nj_monthly, month_breaks="12") +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold", size=10,color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-2F.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
