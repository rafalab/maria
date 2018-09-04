## Make csv with population data from puerto rico and florida since we can't make teralytics data public
source("init.R")
load("rdas/pr-daily.rda")

pr_daily %>% select(Date, Population) %>% write_csv(path="data/puerto-rico/pr-population-estimate.csv")

load("rdas/florida-daily.rda")

florida_daily %>% select(Date, Population) %>% write_csv(path="data/florida/florida-population-estimate.csv")
