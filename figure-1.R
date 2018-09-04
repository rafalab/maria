### Loading data
source("init.R")
load("rdas/fit-models.rda")

### Compute SDs
sds <- combined_daily_res %>% filter(state == "PR") %>%
  group_by(day) %>%
  summarize(sd = mad(f_hat)) 

### Code for figure 1
p <- ggplot() +
  geom_ribbon(aes(day, ymin = 100*(exp(2*sd)-1), ymax = 100*(exp(-2*sd)-1)), alpha = 0.1, data = sds) +
  geom_line(aes(day, 100*(exp(f_hat)-1),
                group = paste(state, graph_year), lty = state), color = "grey",
                data = filter(combined_daily_res, hurricane == "None")) +
  
  geom_line(aes(day, 100*(exp(f_hat)-1), col = hurricane), size=1,
            data = filter(combined_daily_res, hurricane != "None")) +
  
  geom_point(aes(day, increase*100, color = hurricane), alpha = 0.75,
             data = filter(combined_monthly_res, hurricane != "None" & increase < 1)) +
  geom_point(aes(day, increase*100), pch = 1,
             data = filter(combined_monthly_res, hurricane != "None" & increase < 1)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 months") +
  xlab("") +
  ylab("% Change in mortality") +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold", color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black")) + 
  scale_linetype(name="") +
  scale_color_manual(name="", 
                     breaks = c("PR: Maria", "PR: Georges", "PR: Hugo", "LA: Katrina", "NJ: Sandy", "FL: Irma"),
                     values=c("#E69F00","#CC79A7","#56B4E9","#0571b0","#009E73","#D55E00","gray","gray")) +
  scale_y_continuous(limits=c(-15, 80), breaks=seq(-15, 75, by=15))
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(cbbPalette) <- c("black", "orange", "skyblue","green","yellow","blue","vermillon","reddish purple")
names(cbbPalette) <- c("NA","Irma", "Sandy","Hugo","NA","georges","maria","katrina")
cbbPalette

### Saving figure 1
ggsave(paste("figs/figure-1.pdf"),
       plot = p,
       width = 7, height = 4,
       units = "in",
       dpi = 300)
