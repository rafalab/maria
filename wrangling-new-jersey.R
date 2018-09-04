source("init.R")

hurricane_dates <- as.Date(c("2012-10-29"))
hurricane_years <- c(year(hurricane_dates))

## we need to change to raw data
pop <- read_csv("data/new-jersey/NJPOP.csv") %>% 
  mutate(DATE = make_date(year(DATE), 7, 1))

dat <- read_csv("data/new-jersey/NJ_monthly_mort_2000-2016.csv") %>% 
  extract(`Month Code`, "Month", "\\d{4}/(\\d{2})") %>% 
  mutate(Month = as.integer(Month)) %>%
  mutate(Date = make_date(Year, Month, 1)) %>%
  filter(!is.na(Date)) %>%
  mutate(Population = approx(pop$DATE, pop$NJPOP, Date)$y*1000,
              days = as.numeric(diff(c(Date, make_date(2017,1,1)))),
              rate = Deaths/Population * 365/days * 1000) 

nj_monthly <- dat %>% 
  select(Date, Deaths, Population, Year, Month, days) 

save(nj_monthly, file="rdas/nj-monthly.rda", compress = "xz")
