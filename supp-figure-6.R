######
######
######     Author: Rolando J. Acosta
######     Date  : September 1, 2018
######
######     Model fit with data points
######     Supplemental figure 6
######

### Loading data
source("init.R")
load("rdas/fit-models.rda")

### Filtering data: Taking only data for PR
res <- combined_daily_res %>% filter(state=="PR")

### f(t) with points for PR
mylist  <- list(1985:1989,1990:1994,1995:1999,2000:2004,2005:2009, 2010:2014, 2015:2018)
counter <- 1

for(year in mylist)
{
  n <- length(year)
  p <- f_viz(res, years=year, month_breaks = "3") + ggtitle(paste("PR", year[1], year[n], sep="-")) +
    theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
          axis.text.y=element_text(face="bold",color="black"),
          strip.text = element_text(size = 10,face="bold",color="black"), 
          axis.title = element_text(face="bold", color="black"))

  name <- paste("figs/supp-figure-6",LETTERS[counter],".pdf",sep="")
  ggsave(name, # saving figure
         plot = p,
         width = 6, height = 4,
         units = "in",
         dpi = 300)
  counter <- counter + 1
}

### Filtering data: Taking only data for PR
res <- combined_daily_res %>% filter(state=="FL")

### f(t) with points for FL
p <- f_viz(res, years=2015:2018, month_breaks = "3",l_lim=-.2,u_lim=.2) + ggtitle("FL-2015-2018") +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

ggsave("figs/supp-figure-6H.pdf",
       plot = p, # saving figure
       width = 6, height = 4,
       units = "in",
       dpi = 300)