## The following data sources were used to produce the results:
##  - Aggregated and anonymized insights provided by Teralytics Inc. (www.teralytics.net).
source("init.R")

### Loading data
sheets <- readxl::excel_sheets("data/florida/Florida_mortality_data.xlsx")
dat <- lapply(seq_along(sheets), function(i){
  readxl::read_excel("data/florida/Florida_mortality_data.xlsx", sheet = i) %>%
    setNames(c("Month", "Day", "Year", "Deaths")) %>%
    mutate(Year = as.numeric(Year),
           Month = as.numeric(Month),
           Day = as.numeric(Day)) %>%
    mutate(Date = make_date(Year, Month, Day), Deaths = as.numeric(Deaths)) %>% 
    filter(!(Month == 2 & Day == 29))
})
dat <- do.call(bind_rows, dat) %>% arrange(Date)

### For the population estimate we also use teralytics data
pop <- read_csv("data/florida/PEP_2017_PEPANNRES_with_ann.csv") %>%
  slice(2) %>%  
  select(starts_with("respop")) %>%
  gather(Year, Population) %>%
  mutate(Year = str_remove(Year, "respop7")) %>%
  mutate_all(as.integer)
    
## extrapolate for 2018
pop <- pop %>% 
bind_rows(
  data.frame(Year = 2018, 
             Population = predict(lm(Population ~ Year, data = pop), newdata = data.frame(Year = 2018))))

##load teralytics
teralytics <- read_csv("data/teralytics/OD_PuertoRicoAftermath_SepFeb_CountyLvlTop10States.csv") %>%
  mutate(Date = as.Date(paste(move_month, "01", sep = "-"))) 

### People moving out of Florida
out_fl <- teralytics %>%
  filter(origin_state_abbr == "FL") %>%
  group_by(Date) %>%
  summarize(Out = sum(count))

### People moving into Florida
net <- teralytics %>%
  filter(destination_state_abbr=="FL") %>%
  group_by(Date) %>%
  summarize(In = sum(count)) %>%
  left_join(out_fl, by = "Date") %>%
  mutate(net = In - Out, Month = month(Date), Year = year(Date)) %>%
  select(Month, Year, net) %>%
  mutate(total = cumsum(net)) %>%
  bind_rows(data.frame(Month = 3, Year = 2018, net = NA, total = 0)) ## due to lack of data assume no increase

## interpolate population and add the net.
## note that the increase in population is cumulative
predicted_pop <-data.frame(
  Date = dat$Date, 
  Population = approx(make_date(pop$Year, 7, 1), pop$Population, dat$Date, rule = 2)$y) %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  left_join(net, by = c("Month", "Year")) %>%
  mutate(total = replace_na(total, 0)) %>%
  mutate(Population = Population + total)

dat <- dat %>%
  mutate(Population = predicted_pop$Population) %>%
  mutate(yd = my_yday(Date),
         t = as.numeric(Date) - min(as.numeric(Date))) %>%
  select(Date, Deaths, Population, Year, Month, Day, yd, t)


florida_daily <- dat
save(florida_daily, file = "rdas/florida-daily.rda", compress = "xz")

