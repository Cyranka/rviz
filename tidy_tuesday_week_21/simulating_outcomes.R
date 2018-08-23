remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

#setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_week_21/")
library(tidyverse);library(readxl);library(lubridate);library(COUNT);library(arm)

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
display(fit_2, digits = 3) ##Notice how standard errors are now rescaled by the square root of the dispersion parameter


##Let's simulate the model and plot estimates
set.seed(4)
simulations <- sim(fit_2, 1000)

##Create list of predictions
temperature_seq <- seq(75,to = 85, by = 1)

# pred_1 <- exp(simulations@coef %*% c(1,80)) ##if avg temperature is 80 degrees.
# pred_2 <- exp(simulations@coef %*% c(1,85)) ##if avg temperature is 90 degrees. Old code adapted from Gelman
# pred_3 <- exp(simulations@coef %*% c(1,90)) 

linear_estimate <- lapply(1:10, function(i)exp(simulations@coef %*% c(1,temperature_seq[i])))


##Draw from a poisson distribution with mean lambda = prediction
#simulations_1 <- sapply(1:1000, function(i)rpois(n = 1,lambda = pred_1[i])) ##Old code adapted from Gelman
#simulations_2 <- sapply(1:1000, function(i)rpois(n = 1,lambda = pred_2[i]))
#simulations_3 <- sapply(1:1000, function(i)rpois(n = 1,lambda = pred_3[i]))


##Drawing 1,000 a poisson distribution with lambda = prediction
simulations_2 <- lapply(1:10, function(i)sapply(1:1000, function(k)rpois(n = 1,lambda = linear_estimate[[i]][[k]])) %>%
                        as_tibble() %>% mutate(temperature = temperature_seq[i]))

# sims <- tibble(sim_1 = simulations_1, sim_2 = simulations_2,sim_3 = simulations_3) %>%
#     gather(simulation, estimate) #%>% filter(estimate <25)


sim_df <- bind_rows(simulations_2)



##
library(ggridges);library(viridis)
sim_df %>% ggplot(aes(x = value, y = temperature,group = temperature, fill = ..x..)) + 
  geom_density_ridges_gradient(color = "black", show.legend = FALSE, alpha = 0.5) + 
  labs(x = "Distribution of predicted number of wildfires", y = "Temperature",
       title = "Model based prediction of the number of wildfires in California",
       subtitle = "Estimates obtained using a quasi poission regression model",
       caption = "Source: Cal Fire\nTidy Tuesday Week 21") + 
  scale_y_reverse(breaks = c(75:84)) + 
  scale_fill_viridis(alpha = 0.3, option = "C") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black", size = 0.3),
        plot.background = element_rect(fill = "gainsboro"),
        text = element_text(color = "black"),
        axis.text = element_text(colour = "black"),
        strip.text = element_text(colour = "black",size = 12),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 13))

##Now add regression uncertainty to the basic plot

##Subset simulations
set.seed(5)
subset_sim <- simulations@coef[sample(1:1000,100),1:2]

##Build Matrices
vavg_temp <- z$mean_temperature[!is.na(z$mean_temperature)]
intercepts <- rep(1, length(vavg_temp))
X_matrix <- matrix(c(intercepts, vavg_temp), ncol = 2)

list_of_fitted_values <- lapply(1:100, function(i)exp(X_matrix %*% subset_sim[i,]) %>% as_tibble() %>% mutate(sim_number = paste0("simulation_",i),
                                                                                                              temperature = X_matrix[,2]))
df_fitted_values <- bind_rows(list_of_fitted_values)
original_model <- tibble(temperature = fit_2$model$avg_temperature, fitted_values = fit_2$fitted.values)


###
graph_2 <- n_2 %>%
  ggplot(aes(x = avg_temperature, y = total)) + geom_point() + theme_bw() + 
  geom_line(data = df_fitted_values, aes(x = temperature, y = V1, group = sim_number), color = "grey", size = 2,alpha = 0.5) + 
  geom_line(data = original_model, aes(x = temperature, y = fitted_values), color = "red", size = 1) + 
  labs(x = "Average monthly temperature", y = "Total wildfires",
       caption = "Source: Cal Fire\nTidy Tuesday Week 21",
       title = "Fitted quasi poisson regression of total wildfires on average monthly temperature",
       subtitle = "Red line represents estimated model\nGrey area shows variance of the estimated model")


graph_2 + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black", size = 0.1),
        #plot.background = element_rect(fill = "gainsboro"),
        text = element_text(color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        strip.text = element_text(colour = "black",size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 11))


