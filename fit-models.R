source("init.R")

### Loading data
load("rdas/pr-daily.rda")
load("rdas/florida-daily.rda")
load("rdas/louisiana-monthly.rda")
load("rdas/nj-monthly.rda")

### Daily model results for PR
pr_res <- fit_daily_model(pr_daily, 
                          hurricane_dates = hurricane_dates$PR,
                          knots_per_year = nknots,
                          outlier_years = c(2001, 2014, 2018),
                          last_day = last_day,
                          extrapolate_last_year = 2) %>%
  mutate(state = "PR")

### Daily model results for FL
fl_res <- fit_daily_model(florida_daily,
                          hurricane_dates = hurricane_dates$FL,
                          knots_per_year = nknots,
                          outlier_years = c(),
                          last_day = last_day,
                          extrapolate_last_year = 1,
                          verbose = TRUE) %>%
  mutate(state = "FL")

### Combine and define the graph year
combined_daily_res <- bind_rows(pr_res, fl_res) %>%
  mutate(graph_year = Year - (my_yday(Date) <= my_yday(last_day)),
         day = last_day + as.numeric(Date - make_date(graph_year, month(last_day), day(last_day))),
         hurricane = case_when(graph_year == year(hurricane_dates$PR)[1] & state == "PR" ~ hurricane_names$PR[1],
                               graph_year == year(hurricane_dates$PR)[2] & state == "PR"~ hurricane_names$PR[2],
                               graph_year == year(hurricane_dates$PR)[3] & state == "PR"~ hurricane_names$PR[3],
                               graph_year == year(hurricane_dates$FL)[1] & state == "FL" ~ hurricane_names$FL[1],
                               TRUE ~ "None"))

### Monthly model results for LA
la_res <- fit_monthly_model(louisiana_monthly,
                            hurricane_dates =  hurricane_dates$LA) %>%
  mutate(state = "LA")

### Monthly model results for NJ
nj_res <- fit_monthly_model(nj_monthly,
                            hurricane_dates = hurricane_dates$NJ) %>%
  mutate(state = "NJ")

### Combine and define the graph year
combined_monthly_res <- bind_rows(la_res, nj_res) %>%  
  mutate(graph_year = Year - (my_yday(Date) <= my_yday(last_day)),
         day = last_day + as.numeric(Date - make_date(graph_year, month(last_day), day(last_day))),
         hurricane = case_when(graph_year == year(hurricane_dates$LA)[1] & state == "LA" ~ hurricane_names$LA[1],
                               graph_year == year(hurricane_dates$NJ)[1] & state == "NJ" ~ hurricane_names$NJ[1],
                               TRUE ~ "None"))

### Saving newly created data
save(combined_daily_res, combined_monthly_res,
     file = "rdas/fit-models.rda")
