## The following data sources were used to produce the results:
##  - Aggregated and anonymized insights provided by Teralytics Inc. (www.teralytics.net).
source("init.R")

max_deaths <- 150

sheets <- excel_sheets("data/puerto-rico/Harvard 1985-2014.xlsx")
sheets <- sheets[sheets!="Sheet3"]

dat <- lapply(seq_along(sheets), function(i){
  read_excel("data/puerto-rico/Harvard 1985-2014.xlsx", sheet = i, skip=1) %>% 
    setNames(c("Year", "Day", 1:12, "Total")) %>%
    select(-Total) %>%
    filter((!str_detect(Year, "[a-z]") | is.na(Year)) & Day!="Total") %>%
    mutate(Year = as.numeric(rep(unique(na.omit(Year)), each = 31))) %>%
    gather(Month, Deaths, -c(Year, Day), convert = TRUE) %>% 
    mutate(Date = make_date(Year, Month, Day)) %>%
    filter(!is.na(Date))  %>% 
    mutate(Year = as.numeric(Year), Deaths = as.numeric(Deaths), Day = as.numeric(Day)) %>%
    arrange(Date)
})

### data from 2015-2018 comes from individual level data
dat_2015_2018 <- read_excel("data/puerto-rico/PRRD_noidMOR 015-18_Harvard.xlsx") %>%
  rename(Date = DeathDate) %>%
  mutate(Age = na_if(Age, 999)) %>%
  filter(!is.na(Date)) %>% 
  mutate(Date = ymd(Date)) %>%
  filter(!(month(Date)==2 & day(Date)==29)) 

## add another entry with the 2015-2018 data to the dat list before we bind
dat[[length(dat) + 1]] <- dat_2015_2018 %>% 
  group_by(Date) %>%
  summarize(Deaths = n()) %>%
  mutate(Year = year(Date), Day = day(Date), Month = month(Date))

dat <- do.call(bind_rows, dat) %>%
  filter(!(Month == 2 & Day == 29) & Deaths <= max_deaths) %>%
  mutate(yd = my_yday(Date),
         t = as.numeric(Date) - min(as.numeric(Date)))

### now to estimate the population
pop <- read_csv("data/puerto-rico/estimados_anuales_poblacionales_por_municipio_y_puerto_rico_annual_population_estimates_for_puer.csv",
                locale = locale(encoding = 'ISO-8859-1')) %>%
  #select("A??o", "Puerto Rico") %>%
  select(1, 80) %>%
  setNames(c("Year", "Population")) %>%
  mutate(Date = make_date(Year, 7, 1)) %>%
  arrange(Date)

### Getting Puerto Rico's population in 2017
last_day_pop <- pop %>% filter(Date == max(Date)) %>% .$Population

### Loading Teralytics data
teralytics <- read_delim("data/teralytics/teralytics_DailyPopulationTrend_PuertoRico.csv", 
                         delim ="|") %>%
  filter(day > max(pop$Date)) %>%
  mutate(Date = day, Population = factorOfMax*last_day_pop) %>%
  select(Date, Population)

## combine the two
pr_pop <- dat %>% filter(Date <= max(pop$Date)) %>%
  mutate(Population = approx(pop$Date, pop$Population, Date)$y) %>% 
  select(Date, Population) %>%
  bind_rows(teralytics) %>% 
  filter(!is.na(Population))

## now interpolate and extrapolate with constante after last day
dat <- dat %>% 
  mutate(Population = approx(pr_pop$Date, pr_pop$Population, Date, rule = 2)$y)
  
pr_daily <- dat %>% 
  select(Date, Deaths, Population, Year, Month, Day, yd, t)

save(pr_daily, file = "rdas/pr-daily.rda", compress = "xz")

######################################################
###
##   Wrangle the individual data with the IDC codes 
##   for the cause of death analysis
###
######################################################

## We already read the data in above
dat <- dat_2015_2018 

##funcion to convert ICDs to numbers
icd_to_number <- function(letter, number) 
  (sapply(letter, utf8ToInt) - utf8ToInt("A"))*100 + as.numeric(number)

regex <- "\\[([A-Z])(\\d{2}),([A-Z])(\\d{2})\\]"

icd_map <- read_csv("data/ICD10_groups.csv") %>%
  extract(ICD, c("letter_1", "number_1", "letter_2", "number_2"), 
          regex = regex, remove = FALSE) %>%
  mutate(as_number_1 = icd_to_number(letter_1, number_1),
         as_number_2 = icd_to_number(letter_2, number_2))

## convert icd codes to letter/number for 1st and 2nd cause of death
## replace 1st cause with 2nd cuase if "Sequelae, and diseases classified elsewhere or B90-B99
dat <- dat %>% 
  extract(`DeathCause_I (ID)`, 
          into = c("letter", "number"), 
          regex = "([A-Z])(\\d{2})\\d*", 
          remove = FALSE) %>%
  mutate(icd_number = icd_to_number(letter, number)) %>% 
  extract(`DeathCause_II (ID)`, 
          into = c("letter_2", "number_2"), 
          regex = "([A-Z])(\\d{2})\\d*",
          remove = FALSE) %>%
  mutate(icd_number_2 = icd_to_number(letter_2, number_2)) %>%
  mutate(icd_number = ifelse((letter == "B" & number>="90" & number<="99") | is.na(`DeathCause_I (ID)`), 
                             icd_number_2, icd_number)) %>%
  select(-icd_number_2, -letter_2, -number_2)

## for speed we convert ICD code into a indicator matrix for which group
## we then pick the corresponding group

icd_indicator <- apply(select(icd_map, contains("as_number")), 1, function(x)
  replace_na(between(dat$icd_number, x[1], x[2]), FALSE))
colnames(icd_indicator) <- icd_map$ICD

dat$ICD <- apply(icd_indicator, 1, function(x) ifelse(any(x), colnames(icd_indicator)[which(x)], NA))
## now the terms  
terms <- c("suicide", "homicide", "storm", "Leptospirosis")
terms_regex <- c("[s|S]uicide", "[h|H]omicide", "[s|S]torm", "[|L]eptospirosis")

term_indicator <- sapply(terms_regex, function(term){
  tmp <- apply(select(dat, contains("Desription")), 2, function(x) 
    replace_na(str_detect(x, term), FALSE))
  apply(tmp, 1, any)
})
colnames(term_indicator) <- terms

dat <- dat %>%  select(DeathNumber, Gender, Age, DeathPlace, Date, DeathDate_Year, ICD)

dat <- cbind(dat,  term_indicator) %>%
  rename(ID = DeathNumber, Place = DeathPlace, Year = DeathDate_Year) %>%
  mutate(Month = month(Date), Day = day(Date), Deaths = 1) %>%
  select(Date, Deaths, ICD, Year, Month, Day, Place, Age, Gender, suicide, homicide, storm, Leptospirosis)

cause_of_death_terms <- terms

pr_icd <- dat 

pr_population <- pr_daily %>% select(Date, Population) %>%
  filter(Date >= min(pr_icd$Date) & Date <= max(pr_icd$Date))

icd_map <- icd_map %>% select(ICD, Description)
save(pr_icd, cause_of_death_terms, icd_map, pr_population, file = "rdas/pr-icd.rda", compress = "xz")         

######################################################
###
### Getting demographic proportion by year
###
######################################################

### Loading raw pop data stratified by gender
dat <- read.csv("data/puerto-rico/PEP_2017_PEPAGESEX_with_ann.csv") %>% 
  gather(key, Population, -(1:3)) %>%
  extract(key, c("type", "Year", "gender", "group"), regex = "([a-z]{3}\\d)(20\\d{2})(sex\\d)_age(.*)$") %>%
  mutate(group = ifelse(group=="85plus", "85to89", group), ##89 place holder to filter by 4 year groups
         Year = as.numeric(Year)) %>%
  filter(type == "est7" & !str_detect(group, "r$|plus") & group != 999) %>%
  filter(gender %in% c("sex1", "sex2")) %>%
  mutate(gender = ifelse(gender == "sex1", "M", "F")) %>%
  select(Year, gender, group, Population) %>%
  extract(group, c("start", "end"), "(\\d+)to(\\d+)", remove=FALSE, convert = TRUE) %>% 
  select(Year, gender, group, start, end, Population) %>%
  filter(end - start == 4) %>%
  group_by(Year) %>%
  mutate(prop = Population/sum(Population)) %>%
  ungroup()

## Assuming same proportions in 2018 as in 2017
dat_2018 <- dat %>% filter(Year == 2017) %>%
  mutate(Year = 2018)

dat <- bind_rows(dat, dat_2018) %>%
  mutate(end = ifelse(end == 89, Inf, end),
         group = ifelse(group=="85to89", "85plus", group))

pr_pop_age_gender <- dat
save(pr_pop_age_gender, file = "rdas/pr-pop-age-gender.rda", compress = "xz")         

