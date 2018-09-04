######
######
######     Author: Rolando J. Acosta
######     Date  : September 1, 2018
######
######     Vital Statistics & CDC WONDER data mismatch for Louisiana
######     Supplemental figure 3
######
source("init.R")

### Loading daily data from Vital Statistics System
vital <- readxl::read_excel("data/louisiana/D0306 date.xlsx", skip = 1, n_max = 1469) %>% as.data.frame()
vital <- vital %>%
  mutate(Year = fill_years(YEAR)) %>%
  select(-YEAR) %>%
  rename(Deaths = NUMBER) %>%
  extract(`DOD: MMDD`, c("Month", "Day"), "(\\d+)(\\d{2})$") %>% 
  filter(!is.na(Month)) %>%
  mutate(Deaths = as.numeric(Deaths)) %>%
  mutate(Date = make_date(Year, Month, Day))

### Computing monthly death counts
vital <- vital %>% group_by(Year, Month) %>%
          summarize(Deaths_Vital = sum(Deaths)) %>% 
          ungroup() %>%
          mutate(Month=as.numeric(Month),
                 Date = ymd(paste(Year,Month,01,sep="-"))) %>%
          arrange(Year, Month)
  

### Loading monthly data from CDC WONDER
cdc <- read_csv("data/louisiana/Louisiana_mort_2000-2008.csv")

### Supp figure 3A
p <- vital %>% left_join(cdc, by="Date") %>% 
      select(Date, Deaths_Vital, Deaths) %>% 
      gather("Group", "Deaths", -Date) %>% 
      mutate(Group=ifelse(Group=="Deaths_Vital", "Vital", "CDC")) %>%
      ggplot(aes(Date, Deaths, color=Group)) + 
      geom_line(alpha=0.50) +
      geom_point(size=2) +
      xlab("") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
      scale_color_manual(name="", values=c("#ca0020","#525252")) +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black"))
  
### Supp figure 3: LA pop estimates
ggsave(paste("figs/supp-figure-3A.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### Supp figure 3B
p <- vital %>% left_join(cdc, by="Date") %>% 
      select(Date, Deaths_Vital, Deaths) %>% 
      gather("Group", "Deaths", -Date) %>% 
      mutate(Group=ifelse(Group=="Deaths_Vital", "Vital", "CDC")) %>%
      spread(Group, Deaths) %>%
      mutate(Difference = CDC - Vital) %>%
      ggplot(aes(Date, Difference)) + 
      geom_hline(yintercept = 0, lty = 2, col = "red") +
      geom_point(size=2,alpha=0.75) +
      xlab("") +
      ylab("CDC WONDER - Vital Statistics")   +
      scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
      scale_y_continuous(limits = c(-325,325)) +
      scale_color_manual(name="", values=c("#ca0020","#525252")) +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="bold",color="black"),
            axis.text.y=element_text(face="bold",color="black"),
            strip.text = element_text(size = 10,face="bold",color="black"), 
            axis.title = element_text(face="bold", color="black"))

### Supp figure 3: LA pop estimates
ggsave(paste("figs/supp-figure-3B.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)