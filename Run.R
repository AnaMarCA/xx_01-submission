library(data.table)
library(dplyr)
library(tidyr)
library(tsibble)
library(tscount)
library(broom)
library(rlang)
library(GGally)
library(forecast)
library(HIDDA.forecasting)
library(openxlsx)

# Norway locations
Norg <- readRDS("norwayLocations.RDS") 
Norg <- Norg %>%  
  select(municip, municipName) %>% 
  mutate(location = municip) %>% 
  select(-municip)

# Read CreateFakeData() created data
d_A <- readRDS("individual_level_data.RDS")

## 6. Aggregate number of sick people per day per municipality ####
d1 <- d_A %>%
  group_by(location,date) %>% 
  summarise(n_sick = n()) %>%  
  left_join(Norg)  %>% 
  ungroup() %>% 
  select(-location) %>% 
  mutate(location = municipName) %>% 
  select(-municipName)

## 7. Ensure that your aggregated dataset includes rows/days with zero sick people  ####
d2 <- d1 %>%
  distinct(location) %>% 
  group_by(location) %>% 
  mutate(date = list(seq(as.Date("2000-01-01"), as.Date("2010-12-31"), by="1 day"))) %>% 
  unnest(date) %>% 
  left_join(d1) %>% 
  mutate(n_sick = ifelse(is.na(n_sick),0,n_sick)) %>% 
  ungroup()

## 8. Collapse your data down to iso-year/iso-weeks for each municipality ####
data_iso <- as.data.frame(d2) %>% 
  mutate(isoYear = as.Date(yearweek(date)) ) %>% 
  group_by(location,isoYear) %>% 
  mutate(values = sum(n_sick))

## 9. Split the data into training data (2000-01 to 2009-53) and production data (2010-01 to 2010-52) ####
training_data <- data_iso %>% 
  group_by(location) %>% 
  dplyr::filter(isoYear >= "2000-01-03", isoYear <= "2009-12-28") %>%  # same as isoYear >= "2000 W01", isoYear <= "2009 W53"
  select(-date) %>% 
  ungroup() %>% 
  group_by(location,isoYear) %>% 
  distinct(isoYear,values) %>% 
  ungroup() %>% 
  group_by(location) %>% 
  mutate(id = row_number()) %>% 
  spread(location,values) %>% 
  select(-id)

production_data <- data_iso %>% 
  group_by(location) %>% 
  dplyr::filter(isoYear >= "2010-01-04", isoYear <= "2010-12-27") %>% # ame as isoYear >= "2010 W01", isoYear <= "2010 W52"
  select(-date) %>% 
  ungroup() %>% 
  group_by(location,isoYear) %>% 
  distinct(isoYear,values) %>% 
  ungroup() %>% 
  group_by(location) %>% 
  mutate(id = row_number()) %>% 
  spread(location,values) %>% 
  select(-id)

## 10. Use the training data to create a regression model (fit a model) that predicts the expected weekly number of sick people ####

# from the simulated data, we can see that the model will follow a Poisson distribution with a seasonal component;
# testing alternative negative binomial distribution, we found the best fiting to be a binomial, possibly due to variations in the binomial data introduced by the fake data creation process for the mu parameter
# given the previous considerations, we fit the training data for a negative binomial distribution and add a seasonal component expressed through the yearly frequency parameter

alarm_data_pj <- list()
for (i in 1:416) {
  td_for_glm <- as.vector(as.data.frame(training_data[,i+1]))
  ts_td_for_glm <- ts(td_for_glm,frequency=52, start=c(2000,1), end= c(2009,54))
  my_model <- tsglm(ts_td_for_glm, model = list(past_obs = 1, past_mean = 52), distr = "nbinom",link="identity") #  link="identity"
  
  ## 11.  For the training data, create a 2 standard deviation prediction interval. ####
  
  fortify_data <- tidy(ts_td_for_glm)
  fortify_data$tsglmfitted <- fitted(my_model)
  fortify_data <- cbind(fortify_data ,
                        sapply(c(tsglmlower=0.025, tsglmupper=0.975), function (p)
                          qnbinom(p, mu = fitted(my_model), size = my_model$distrcoefs)))
  
  
  ## 12. 13.    Identify the potential outbreaks in the training data (i.e. number of sick people > prediction interval). ####
  # Exclude the potential outbreaks from the training data
  alarm_data <- fortify_data %>% 
    mutate(date = seq(as.Date("2000-01-03"), as.Date("2009-12-28"), by="1 week"),  # increased to 28
           process_mean = sum(value)/n(),
           threshold = tsglmupper,
           alarm= ifelse(value >= tsglmupper,"1","0"),
           new_values= ifelse(alarm=="1",process_mean,value)) 
  
  
  ## 14. Refit the model using the new training data (without any outbreaks in it) ####
  training_data2 <- alarm_data %>% 
    select(new_values) 
  td_for_glm2 <- as.data.frame(training_data2)
  ts_td_for_glm2 <- ts(td_for_glm2, frequency=52, start=c(2000,1), end= c(2009,54))
  
  my_model2 <- tsglm(ts_td_for_glm2, model = list(past_obs = 1, past_mean = 52), distr = "nbinom",link="identity") 
  
  
  fortify_data2 <- tidy(ts_td_for_glm2)
  fortify_data2$tsglmfitted <- fitted(my_model2)
  if(is.null(my_model2$distrcoefs)) {
    fortify_data2 <- cbind(fortify_data2 ,
                           sapply(c(tsglmlower=0.025, tsglmupper=0.975), function (p)
                             qnbinom(p, mu = fitted(my_model2), size =  0.0001)))
  }else{
    fortify_data2 <- cbind(fortify_data2 ,
                           sapply(c(tsglmlower=0.025, tsglmupper=0.975), function (p)
                             qnbinom(p, mu = fitted(my_model2), size = my_model2$distrcoefs)))
  }
  
  alarm_data2 <- fortify_data2 %>% 
    mutate(date = seq(as.Date("2000-01-03"), as.Date("2009-12-28"), by="1 week")) %>% 
    select(date, value, tsglmupper, tsglmlower)
  
  ## 15. Create a 2 standard deviation prediction interval for the production data ####
  # PREDICTED production data with forcasting on corrected model
  tsglmfit_t <- update(my_model2 , ts = my_model2$ts[1:52],
                       xreg = my_model2 $xreg[1:52,,drop=FALSE])
  predictions <- fitted(tsglmfit_t)
  
  # model for real production data and CI for real production data
  ptd_for_glm <- as.vector(as.data.frame(production_data[,i+1]))
  pts_td_for_glm <- ts(ptd_for_glm,frequency=52, start=c(2010,1), end= c(2010,52))
  my_model3_t <- tsglm(pts_td_for_glm, model = list(past_obs = 1, past_mean = 52), distr = "nbinom",link="identity") 
  
  fortify_data2f <- tidy(pts_td_for_glm)
  fortify_data2f$tsglmfitted <- fitted(my_model3_t )
  if(is.null(my_model3_t$distrcoefs)) {
    fortify_data2f <- cbind(fortify_data2f ,
                            sapply(c(tsglmlower=0.025, tsglmupper=0.975), function (p)
                              qnbinom(p, mu = fitted(my_model3_t), size =  0.0001)))
  }else{
    fortify_data2f <- cbind(fortify_data2f ,
                            sapply(c(tsglmlower=0.025, tsglmupper=0.975), function (p)
                              qnbinom(p, mu = fitted(my_model3_t), size = my_model3_t$distrcoefs)))
  }
  
  ## 16. Identify the potential outbreaks in the production data ####
  
  # and get the outbreaks for production data
  alarm_data_p <- fortify_data2f  %>% 
    mutate(date = seq(as.Date("2010-01-04"), as.Date("2010-12-27"), by="1 week"),  # increased to 28
           process_mean = sum(value)/n(),
           predictions=predictions,
           alarm= ifelse(value >= tsglmupper,"1","0"),
           new_values= ifelse(alarm=="1",process_mean,value)) %>% 
    select(date, value, tsglmupper, tsglmlower, predictions) # , series
  
  alarm_data_pj[[i+1]] <- full_join(alarm_data2, alarm_data_p) %>% mutate(potential_outbreak = ifelse(value >= tsglmupper & predictions >=0,1,0))
  
}


## 17. create xls files ####
alarm_data_pj10 <-  alarm_data_pj 
alarm_data_pj10 <-  do.call("rbind", alarm_data_pj10)
names <- as.vector(rep(names(training_data)[-1], each=574))
names_csv <- as.data.frame(names(training_data)[-1]) 
names_csv<- write.csv(names, file = "names_csv.csv")
alarm_data_pj10 <- alarm_data_pj10 %>% 
  mutate(Municipality = names)


Municipalities <- unique(alarm_data_pj10$Municipality)
for (i in 1:length(Municipalities)) {
  alarm_data_iter <- 
    alarm_data_pj10 %>% 
    filter(Municipality == paste(Municipalities[i])) 
  write.xlsx(alarm_data_iter, file=paste("outbreaks_Municip",Municipalities[i],".xlsx", sep=""), sheetName="Sheet1", 
             col.names=TRUE, row.names=F, append=FALSE, showNA=TRUE, password=NULL)
}


## 18. create jpeg of outbreaks ####
for (i in 1:length(Municipalities)) {
  alarm_data_iter <- 
    alarm_data_pj10 %>% 
    filter(Municipality == paste(Municipalities[i])) 
  
  jpeg(filename = paste("overview_Municip", Municipalities[i],".jpeg", sep=""), width=8, height=6, units = "in", res = 100)
  
  print( ggplot() +  
           geom_rect(data= alarm_data_iter, aes(xmin=as.Date("2000-01-03"), xmax=as.Date("2009-12-28"), ymin=0, ymax= Inf), alpha = 0.75, fill = "gray1") +
           geom_ribbon(data= alarm_data_iter, aes(ymin= tsglmupper, ymax= Inf , x= date, fill = "band" ), alpha = 0.8, fill = "salmon1") +
           geom_ribbon(data= alarm_data_iter, aes(ymin= 0, ymax= tsglmupper , x= date, fill = "band" ), alpha = 0.8, fill = "skyblue") +
           geom_ribbon(data= alarm_data_iter, aes(ymin= 0, ymax= tsglmlower , x= date, fill = "band" ), alpha = 0.8, fill = "seashell3") +
           geom_line(data= alarm_data_iter, aes(x=date, y= value), colour= "black") +
           geom_line(data= alarm_data_iter, aes(x=date, y= predictions), colour= "dimgrey") + 
           geom_point(data= alarm_data_iter, aes(x=date, y= predictions), colour= "dimgrey") + 
           geom_point(data= alarm_data_iter[alarm_data_iter$potential_outbreak == 1,], aes(x=date, y= value), colour= "black",size=3, shape= 23, fill="red") + # real values
           scale_x_date(date_breaks = "4 months", aes(date, origin = "2000-01-03"), expand = c(0, 0)) +
           scale_y_continuous( expand = c(0, 0)) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.9)),plot.title = element_text(hjust = 0.5, size=20,lineheight=0.10),plot.background=element_rect(fill="azure2")) +
           ggtitle(paste0("", Municipalities[i])) +
           xlab("Date") + ylab("Number of cases of disease X "))
  
  dev.off()
  
}

