## day of the year skipping 2/29
my_yday <- function(x) ifelse(year(x)%%4 == 0 & month(x)>2, yday(x)-1, yday(x))

## function to create harmonic model
fourier_trend <- function(x, k=3){
  H <- lapply(1:k, function(k){ 
    cbind(sin(2*pi*k/365*x), cos(2*pi*k/365*x)) 
  })
  return(do.call(cbind, H))
}

## for vectors like this 2000 NA NA NA NA NA ... NA 2001 NA NA... fill in the NA
## works for non years too
fill_years <- function(x){
    y <- c(NA, head(x, -1))
    x <- ifelse(is.na(x), y, x)
    if (any(is.na(x))) Recall(x) else x 
}


fit_daily_model <- function(dat, 
                          hurricane_dates,
                          knots_per_year = 4,
                          harmonics = 3,
                          outlier_years = c(),
                          no_effect_season = c(61, 245), ## this is 3/1 to 9/1
                          last_day = max(dat$Date),
                          extrapolate_last_year = 2,
                          verbose=TRUE){ #0 is nothing, 1 is constant, 2 is line

  if(verbose) message("Computing offsets.")
  
  dat <- filter(dat, Date <= last_day)
    
  hurricane_years <- year(hurricane_dates)

  dat <- dat %>% mutate(offset_sample_size = log(Population))
  
  ### find the overall mean ans seasonel
  s <- cbind(1, fourier_trend(dat$yd, k = harmonics))

  ##excluding the hurrican years and outlier years given that they are not normal
  #index <- which(!year(dat$Date) %in% c(hurricane_years, outlier_years))
  index <- which(!year(dat$Date) %in% hurricane_years)
  
  ## offset for year effect
  fit <- glm(dat$Deaths[index] ~ s[index,] - 1,
             offset = dat$offset_sample_size[index],
             family = "poisson")
  
  dat <- dat %>% 
    mutate(offset_seasonal = as.vector(s%*%fit$coefficients),
           offset_seasonal_se = sqrt(colSums(t(s) * vcov(fit) %*% t(s))))
  
  ## find the yearly offset (deviation from the seasonal effect by year)
  ## but using only parts of the year without hurricane effects
  offset_year <- dat %>%
    filter(yd >= no_effect_season[1] & yd <= no_effect_season[2]) %>%
    mutate(Year = as.factor(Year)) %>%
    glm(Deaths ~ Year - 1, data = ., family = "poisson",
        offset = offset_seasonal + offset_sample_size) %>%
    tidy() %>%
    mutate(term = as.numeric(gsub("Year", "", term))) %>%
    select(term, estimate, std.error) %>%
    setNames(c("Year", "offset_year", "offset_year_se"))
  
  ## extrapolate last year
  last_year <- max(year(dat$Date))
  if(extrapolate_last_year == 2){ 
    offset_year$offset_year[offset_year$Year == last_year] <-
      predict(lm(offset_year ~ ns(Year, knots = Year[-c(1,length(Year))]),
                 data = filter(offset_year, Year < last_year)),
                 newdata = filter(offset_year, Year == last_year))
    offset_year$offset_year_se[offset_year$Year == last_year] <- offset_year$offset_year_se[offset_year$Year == last_year-1]
  }  else{
    if(extrapolate_last_year == 1){ 
      offset_year$offset_year[offset_year$Year == last_year] <- offset_year$offset_year[offset_year$Year == last_year - 1]
      offset_year$offset_year_se[offset_year$Year == last_year] <- offset_year$offset_year_se[offset_year$Year == last_year - 1]
    }
  }
  
  ## join it with dat so we can use it in the glm
  if(verbose) message("Preparing design matrices.")
  
  dat <- left_join(dat, offset_year, by="Year")

  ### smooth function with jumps at hurricanes
  hurricane_knots <- filter(dat, Date %in% hurricane_dates) %>% .$t
  nknots <- round(as.numeric(diff(range(dat$Date)))/365 * knots_per_year)
  if(verbose) message(paste("Using", nknots,"knots."))
  knots <- quantile(dat$t, seq(0, 1, length.out = nknots))
  knots <- knots[-c(1, length(knots))]
  ## find the index of closest knot to hurricane
  index_hurricane <- sapply(hurricane_knots, function(tau)
    which.min(abs(tau-knots)))
  ## use natural cubic splines at knots not the hurricane
  f <- ns(dat$t, knots = knots[-index_hurricane])
  ## permit complete change at the hurricane
  tau <- c(hurricane_knots, max(dat$t)+1)
  h <- lapply(seq_along(hurricane_knots), function(i, x=dat$t){
    ind <- I(x>=tau[i] & x<tau[i+1])
    cbind(ind,  poly((x-tau[i])*ind, 3))
  })
  ## combine the hurricane and non-hurricane knots
  h <- do.call(cbind, h)
  ## index of the columns we will not scale because they represent the jump
  ## and we want to interprete this parameter
  no_scale_index <- ncol(f) + seq(1,ncol(h),4)
  f <- cbind(f,h)
  
  ## make the average day the reference:
  f[ ,-no_scale_index] <- sweep(f[, -no_scale_index], 2, colMeans(f[ ,-no_scale_index]))
  
  ##check for full rank
  if(!identical(qr(f)$rank, ncol(f))) stop("Not full rank")
  
  if(verbose) message("Fitting model.")
  
  ## need to figure out how to deal with year effect
  fit_glm <- with(dat,
                  glm(Deaths ~ f-1,
                      offset = offset_sample_size +
                        offset_seasonal +
                        offset_year,
                      family = "poisson"))
  
  
  beta_f <- fit_glm$coef
  se_f   <- sqrt(colSums(t(f) * vcov(fit_glm) %*% t(f)))
  se_f   <- sqrt(se_f^2 + dat$offset_seasonal_se^2 + dat$offset_year_se^2) ##we need to check this
  
  dat <- dat %>%
          mutate(f_hat = (f %*% beta_f)[,1], se = se_f,
                 fitted_values = fitted.values(fit_glm))

  return(dat)
}

fit_monthly_model <- function(dat,
                              hurricane_dates,
                              no_effect_season = c(2, 7)){## this one in months
  
  hurricane_years <- year(hurricane_dates)
  
  dat <- dat %>% mutate(rate = Deaths / Population * 365 / days)
  
  ### compute the seasonal effect
  s_hat <- dat %>% filter(!Year %in% hurricane_years) %>%
    group_by(Month) %>%
    summarize(s_hat = mean(rate)) 
  dat <- left_join(dat, s_hat, by = "Month")
  ## now compute the yearly offset
  year_offset <- dat %>%
    filter(Month >= no_effect_season[1] & Month <= no_effect_season[2]) %>%
    group_by(Year) %>%
    summarize(year_offset = mean(rate - s_hat))
  dat <- left_join(dat, year_offset, by = "Year") %>%
    mutate(expected = s_hat + year_offset,
           diff = rate - expected,
           increase = diff / expected)
  sds <- dat %>%
    ## now compute the sd and se for expected counts
    filter(!Year %in% hurricane_years) %>%
    group_by(Month) %>%
    summarize(sd = sd(rate - year_offset), se = sd/sqrt(n()))
  dat <- left_join(dat, sds, by = "Month") %>%
    select(Date, Deaths, Population, rate, expected, diff,  sd, se, increase, Year, Month, days, s_hat, year_offset)
      return(dat)
}

### Function to visualize population estimates
population_viz <- function(dat, month_breaks="1", theme="theme_minimal")
{
    dslabs::ds_theme_set(new=theme)
    p <- dat %>% ggplot(aes(Date, Population)) +
    geom_line() + xlab("") +
    scale_x_date(date_labels = "%b %Y", date_breaks = paste(month_breaks,"months")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
}

### Function to visualize seasonal fit
seasonal_fit_viz <- function(dat, hurricane_dates, month_breaks="1", theme="theme_minimal")
{
    dslabs::ds_theme_set(new=theme)

    name <- dat$state[1]
    hurricane_years <- year(hurricane_dates)
    tmp <- dat %>%
    filter(!(Year %in% hurricane_years)) %>%
    group_by(yd) %>%
    summarize(avg = exp(mean(log(Deaths) - log(Population)))*1000*365)
    
    p <- dat %>% filter(Year == min(Year)) %>%
      ggplot(aes(yd, exp(offset_seasonal)*1000*365))+xlab("Days")+ylab("Death rate")+
      geom_point(aes(yd, avg), data = tmp, alpha=0.70, col="#525252") +
      geom_ribbon(aes(x=yd,ymin=exp(offset_seasonal-1.96*offset_seasonal_se)*1000*365,
                      ymax=exp(offset_seasonal+1.96*offset_seasonal_se)*1000*365),fill="#ca0020",alpha=0.75)+
      geom_line(col="#ca0020")+ ggtitle(paste("Seasonal fit for", name)) 
    
    return(p)
}

### Function to visualize f_hat with points
f_viz <- function(dat, years, month_breaks="1", theme="theme_minimal", l_lim=-0.50, u_lim=0.70)
{
    dslabs::ds_theme_set(new=theme)
    
    name <- dat$state[1]
    dat  <- dat %>% 
              mutate(points = log(Deaths)-offset_sample_size-offset_seasonal-offset_year)
    
    p <- dat %>% filter(Year %in% years) %>% ggplot() +
      geom_point(aes(Date, points), alpha=0.50,col="#525252") +
      geom_ribbon(aes(x=Date, ymin=f_hat-1.96*se, ymax=f_hat+1.96*se),fill="#ca0020",alpha=0.5) +
      xlab("") + ylab("log Death rate ratio") + ggtitle(paste("f hat for", name)) +
      geom_line(aes(Date, f_hat),col="#ca0020") + geom_hline(yintercept=0,lty=2)+
      scale_x_date(date_labels = "%b %Y", date_breaks = paste(month_breaks,"months")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits=c(l_lim,u_lim),
                         breaks=c(seq(l_lim,u_lim,by=0.10)))
    return(p)
}