### Loading data
source("init.R")
load("rdas/fit-models.rda")

res <- lapply(1:3, function(i){
  combined_daily_res %>% 
    filter(state == "PR" & graph_year == year(hurricane_dates$PR[i]) &
             Date >= hurricane_dates$PR[i]) %>%
    filter(cumsum(f_hat - 1.96*se <= 0) == 0) %>% ## until we no longer outside 95% CI
    mutate(excess = cumsum(Deaths - exp(offset_sample_size+offset_seasonal+offset_year)),
           days = t - min(t) + 1,
           se = sqrt(exp(offset_sample_size+offset_seasonal+offset_year)),
           excess_se=sqrt(cumsum(se^2))) %>%
    select(days, excess, excess_se, hurricane) %>%
    bind_rows(tibble(days=0, excess=0, excess_se=0, hurricane = hurricane_names$PR[i])) %>%
    arrange(days)
})

dat <- combined_monthly_res %>% filter(state == "LA")

res[[4]] <- dat %>% 
              filter(state == "LA" & graph_year == year(hurricane_dates$LA[1]) &
                     Date >= hurricane_dates$LA[1]) %>%
              filter(cumsum(diff - 1.96*se <= 0) == 0) %>% ## until we no longer outside 95 interval
              mutate(excess = cumsum(Deaths - (s_hat + year_offset)*Population*days/365),
                     se = sqrt((s_hat + year_offset)*Population*days/365),
                     excess_se=sqrt(cumsum(se^2)),
                     days = cumsum(days)) %>%
              select(days, excess, excess_se, hurricane) %>%
              bind_rows(tibble(days=0, excess=0, hurricane = "LA: Katrina")) %>%
              arrange(days)
res <- do.call(bind_rows, res)

cols <- c("#CC79A7","#0072B2", "#009E73", "#D55E00")
breaks <- c("LA: Katrina", "PR: Maria", "PR: Georges", "PR: Hugo")
p <- res %>% 
  ggplot(aes(days, excess, color = hurricane)) + geom_line() + 
  geom_ribbon(aes(x=days, 
                  ymin=excess-2*excess_se, 
                  ymax=excess+2*excess_se, 
                  fill=hurricane), 
              color = 0,
              alpha=0.75) +
  scale_color_manual(name="", values=cols, breaks = breaks) +
  scale_fill_manual(name="", values=cols, breaks= breaks) +
  ylab("Excess deaths") + xlab("Days after the hurricane") + 
  theme(axis.text.x=element_text(angle=0, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"),
        legend.position = "none") +
  annotate("text", x=200, y=3000,  
             label='bold("Maria")', 
             size=5, color="#D55E00", parse=TRUE) +
  annotate("text", x=145, y=1800,
           label='bold("Katrina")', 
           size=5, color="#CC79A7", parse=TRUE) +
  annotate("text", x=125, y=1200,
           label='bold("Georges")', 
           size=5, color="#0072B2", parse=TRUE) +
  annotate("text", x=30, y=100,
           label='bold("Hugo")', 
           size=5, color="#009E73", parse=TRUE)
  

### Figure 2
ggsave(paste("figs/figure-2.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)
