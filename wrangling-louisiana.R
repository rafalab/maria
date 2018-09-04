source("init.R")

hurricane_dates <- as.Date(c("2005-08-29"))
hurricane_years <- c(year(hurricane_dates))
daily <- readxl::read_excel("data/louisiana/D0306 date.xlsx", skip = 1, n_max = 1469) %>% as.data.frame()
daily <- daily %>%
  mutate(Year = fill_years(YEAR)) %>%
  select(-YEAR) %>%
  rename(Deaths = NUMBER) %>%
  extract(`DOD: MMDD`, c("Month", "Day"), "(\\d+)(\\d{2})$") %>% 
  filter(!is.na(Month)) %>%
  mutate(Deaths = as.numeric(Deaths)) %>%
  mutate(Date = make_date(Year, Month, Day)) 

dat <- read_csv("data/louisiana/Louisiana_mort_2000-2008.csv")
##if we compare these two...
# dat %>% 
#   filter(year(Date)==hurricane_years & month(Date) == month(hurricane_dates)) %>% 
#   .$Deaths
# 
# daily %>% filter(year(Date)==hurricane_years & month(Date) == month(hurricane_dates)) %>%
#   summarize(sum(Deaths))
###  they are similar so we use the daily data for August 2009
hurricane_month <- daily %>% filter(year(Date)==hurricane_years &
                   month(Date) == month(hurricane_dates)) %>%
  mutate(hurricane = case_when(Date < hurricane_dates ~ -1,
                               Date == hurricane_dates ~ 0,
                               TRUE ~ 1)) %>%
   group_by(hurricane) %>%
   summarize(Deaths = sum(Deaths), days = n())

## create temporary table like data holding the splitted 8/05 data
august_2005 <- data.frame(Date = c(make_date(hurricane_years, 8, 1),
                           hurricane_dates,
                           hurricane_dates+1),
                  Deaths = hurricane_month$Deaths, 
                  Population = NA) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))
##take out august and put in the one obtained with daily data
dat <- dat %>% filter(!(year(Date)==hurricane_years &
                        month(Date) == month(hurricane_dates))) %>%
  bind_rows(august_2005) %>% 
  arrange(Date) %>%
  mutate(days = as.numeric(diff(c(Date, make_date(2009, 1, 1))))) ## we need days in segment to compute a per year rate

## Now read in the population
pop <- read_csv("data/louisiana/Louisiana_pop_2000-2009.csv") 
dat <- dat %>% select(-Population) %>% left_join(pop, by ="Year")
index <- pop$Year <= hurricane_years
y <- pop$Population[index]
x <- make_date(pop$Year[index], 07, 01)
## extrapolate a line to figure out population one day before hurricane 
population <- c(y, predict(lm(y ~ x), newdata = data.frame(x=hurricane_dates-1)))
date <- c(x, hurricane_dates-1)      
## now we interpolate from this to the beginning of 2016
index <- which(pop$Year > hurricane_years)
## assume the drop happened right after hurricane
## and add them to current values
population <- c(population, pop$Population[index][1], pop$Population[index])
date <- c(date, hurricane_dates, make_date(pop$Year[index], 07, 01)) 
### now interpolate and 
dat$Population <- approx(as.numeric(date), population, as.numeric(dat$Date), rule = 2)$y 
## and compute a rate

louisiana_monthly <- dat %>% 
  select(Date, Deaths, Population, Year, Month, days)

save(louisiana_monthly, file="rdas/louisiana-monthly.rda", compress = "xz")

