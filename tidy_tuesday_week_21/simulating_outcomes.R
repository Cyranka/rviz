remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_week_21/")
library(tidyverse);library(readxl);library(lubridate)

x <- read_csv("california_fires_1.csv") %>% dplyr::rename(year = year_) %>%
    mutate(month = month(ymd(alarm_date)))
y <- read_excel("avg_temperature.xlsx") %>%
    rename(mean_temperature = Value)
z <- x %>% left_join(y, by = c("year" = "Date", "month" = "Month")) 

k <- z %>% group_by(year, month) %>%
    summarise(total = n(),
              avg_temperature = mean(mean_temperature,na.rm = TRUE))  %>%
    filter(!is.na(month))

k %>%
    ggplot(aes(x = avg_temperature, y = total)) + geom_point() + theme_bw()


##Use Only Natural
n_1 <- z %>% group_by(year, month) %>%
    summarise(total = n(),
              avg_temperature = mean(mean_temperature,na.rm = TRUE))  %>%
    filter(!is.na(month))

n_1 %>%
    ggplot(aes(x = avg_temperature, y = total)) + geom_point() + theme_bw()

##No Outliers and fit model
n_2 <- filter(n_1, total <180)

n_2 %>%
    ggplot(aes(x = avg_temperature, y = total)) + geom_point() + theme_bw()

fit_1 <- glm(total~avg_temperature, data = n_2, family = "poisson")

arm::display(fit_1)

###Goodness of Fit Test: Deviance should be distributed chi2
dev <- deviance(fit_1) ##Get Deviance Statistic
residual_df <- df.residual(fit_1) ##Residual Degrees of Freedom

1 - pchisq(dev, residual_df) ##We reject H0 that the model fits well the data

tibble(statistic = c("Deviance GOF","D","df","p-value"),
       value = c("",as.character(round(dev,2)),"281","0"))

##Calculating Dispersion
round(P__disp(fit_1),2) ##Very dispersed model

###What if we try a model that corrects for overdispersion?
##Standard errors have been scaled to account for overdispersion
fit_2 <- glm(total~avg_temperature, data = n_2, family = "quasipoisson")

##Let's simulate the model and plot estimates
simulations <- sim(fit_2, 1000)

##Simulating outcomes 
pred_1 <- exp(simulations@coef %*% c(1,80)) ##if avg temperature is 80 degrees
pred_2 <- exp(simulations@coef %*% c(1,85)) ##if avg temperature is 90 degrees
pred_3 <- exp(simulations@coef %*% c(1,90)) 

##Draw from a normal distribution with mean equal to fitted value and standard deviation sigma
simulations_1 <- sapply(1:1000, function(i)rpois(n = 1,lambda = pred_1[i]))
simulations_2 <- sapply(1:1000, function(i)rpois(n = 1,lambda = pred_2[i]))
simulations_3 <- sapply(1:1000, function(i)rpois(n = 1,lambda = pred_3[i]))

sims <- tibble(sim_1 = simulations_1, sim_2 = simulations_2,sim_3 = simulations_3) %>%
    gather(simulation, estimate) #%>% filter(estimate <25)


sims %>%
    ggplot(aes(x = estimate, fill = simulation)) + 
    geom_density(adjust = 2, alpha = 0.5, show.legend = TRUE) + theme_bw()

