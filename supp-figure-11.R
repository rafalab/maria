source("init.R")
load("rdas/fit-models.rda")

ndays <- 200

dat <- combined_daily_res %>%
  filter(Year <= 2017) %>%
  filter(state == "PR" & !(Year %in% year(hurricane_dates$PR))) %>%
  mutate(group = ifelse(Year == 2014, "2014", "Other years"),
         day = make_date(2017, Month, Day))

sds <- dat %>%
  group_by(day) %>%
  summarize(sd = mad(f_hat))

p <- ggplot() +
  geom_ribbon(aes(day, ymin = (exp(-1.96*sd)-1)*100, ymax = (exp(1.96*sd)-1)*100), 
              data = sds, alpha = 0.5) +
  geom_line(aes(day, (exp(f_hat)-1)*100, group = Year, lty = group), 
            data = dat) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 months") +
  xlab("") +
  ylab("Percent increase in death rate") +
  scale_linetype(name="") +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-11A.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)


#### Second plot
dat <- combined_daily_res %>%
  filter(Year <= 2017) %>%
  filter(state == "PR")

years <- dat %>% filter(! graph_year %in% year(hurricane_dates$PR)) %>%
  .$graph_year %>% unique()

res <- lapply(years, function(i, ndays = 200){
  start <- make_date(i,9,20)
  end <- start + ndays
  tmp <-  dat %>% 
    filter(Date >= start & Date <= end) %>%
    mutate(excess = cumsum(Deaths - exp(offset_sample_size+offset_seasonal+offset_year)),
           days = t - min(t) + 1,
           Year = i) %>%
    select(days, excess, Year) %>%
    bind_rows(tibble(days=0, excess=0, Year = i)) %>%
    arrange(days)
})
res <- do.call(bind_rows, res)

res <- mutate(res, group = ifelse(Year == 2014, "2014", "Other years"))


sds <- res %>%
  group_by(days) %>%
  summarize(sd = mad(excess))


p <- ggplot() +
  geom_ribbon(aes(days, ymin = -1.96*sd, ymax = 1.96*sd), 
              data = sds, alpha = 0.5) +
  geom_line(aes(days, excess, group = Year, lty = group), 
            data = res) + 
  xlab("Days since September 20") +
  ylab("Excess") +
  scale_linetype(name="") + 
  theme(axis.text.x=element_text(angle=0, hjust=1, face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

ggsave(paste("figs/supp-figure-11B.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)