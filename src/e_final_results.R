### Portfolio Optimization
### Lasso Results 
### Sergio Parra 
### sparrap@itam.mx

# Library ----

paquetes<-c("tidyverse", "lubridate","stringr","scales","gsheet","RColorBrewer",
            "openxlsx","ggrepel" ,"readxl","janitor","tidyselect","tidyquant")

sapply(paquetes,
       function(x) {
         if(!x %in% rownames(installed.packages()))
           install.packages(x)
         require(x, character.only=T)
       })
rm(list=setdiff(ls(),c("")))

setwd("~/Documents/ITAM/Tesis/")

### Data 

stocks_bmv <- read_csv("03_Output/FinalDataSet.csv")

results_lasso_all <- read_csv("03_Output/df_coeff_lasso_v2.csv") 
results_lasso_mom <- read_csv("03_Output/df_coeff_lasso_monthly.csv")|> select(- 1)
results_lasso_cm <- read_csv("03_Output/df_coeff_lasso_complete_monthly.csv")|> select(- 1)
results_lasso_tm <- read_csv("03_Output/df_coeff_lasso_threemonth.csv")|> select(- 1)


# Create tables of the monthly portfolios to include in the appendix

tabla_lasso_all <- results_lasso_all |> 
  arrange(desc(weight)) |>
  select(2:4) |>
  slice(1:16) |>
  rename(Stock = 1,
         Coefficient = 2,
         Weight = 3)
sum(tabla_lasso_all$Weight)

tabla_lasso_mom <- results_lasso_mom |>
  pivot_longer(cols = 1:96,names_to = "stock",values_to = "weight") |>
  drop_na()

tabla_lasso_cm <- results_lasso_cm |>
  pivot_longer(cols = 1:96,names_to = "stock",values_to = "weight") |>
  drop_na()

tabla_lasso_tm <- results_lasso_tm |>
  pivot_longer(cols = 1:96,names_to = "stock",values_to = "weight") |>
  drop_na()

### cumulative sum of selected stocks
  
df_test_returns_all <- read_csv("03_Output/df_test_returns.csv")

df_test_all <- df_test_returns_all |>
  group_by(symbol) |>
  mutate(cum_ret_model = cumsum(test_returns)) |>
  select(2:4) |>
  filter(symbol %in% tabla_lasso_all$Stock) |>
  left_join(tabla_lasso_all |>
              select(Stock, Weight) |>
              rename(symbol=Stock) |>
              mutate(weight=Weight/100), by = "symbol")

df_test_all <- df_test_all |>
  mutate(weighted_return=cum_ret_model*weight)

results_all <- df_test_all |>
  dplyr::select(symbol,weighted_return) |>
  group_by(symbol) |>
  mutate(ind = 1:max(length(symbol))) |>
  pivot_wider(names_from = "symbol",values_from = "weighted_return") |>
  ungroup() |>
  mutate(total_returns = dplyr::select(., 2:17) |> rowSums(na.rm = TRUE))


### Monthly training strategy: Month over month 


df_test_returns_mom <- read_csv("03_Output/df_test_returns_monthly.csv")

# Transform data to long format, then left join by the training month 
# and evaluate returns with new weights

df_test_mom <- df_test_returns_mom |>
  group_by(symbol,test_month) |>
  mutate(cum_ret_model = cumsum(test_returns),
         ind=1:n()) |>
  select(ind,symbol,train_month,test_month,test_returns,cum_ret_model) |> 
  left_join(tabla_lasso_mom,by=c("train_month"="training_month","symbol"="stock")) |> 
  drop_na(weight) # Remove those months that are not considered in the lasso model 

# Keep only returns of the next 30 trading days, warning!!! what if a stock is repeated??!!

# What we can do is to get all the trading days a stock was available in the market and then consider those 
# days as trading days and remove the date 

# Hacer un loop para que para cada mes me de los días de ese y el siguiente mes y me lo guarde en una data frame 
# Luegoi elimina ceros y eliminar días repetidos 

meses <- unique(str_sub(stocks_bmv |> drop_na(date) |> select(date) |> pull(),1,7)) 

aux_meses <- stocks_bmv |>
  mutate(mes = str_sub(stocks_bmv$date,1,7)) |> 
  select(date,mes,symbol)

df_aux <- data.frame()

for (i in 1:46) {
  
  train_month = meses[i]
  test_month = meses[i+1]
  next_two_month = meses[i+2]
  
  df_aux_count <- aux_meses |> 
    filter(mes %in% c(test_month,next_two_month)) |> 
    group_by(symbol) |> 
    mutate(ind = 1:n()) |> 
    mutate(train_month = train_month,
           test_month = test_month)
    
    df_aux <- bind_rows(df_aux,df_aux_count)
  
}

aux <- left_join(df_test_mom,df_aux |> select(date,ind,symbol,train_month,test_month),
                 by = c("ind","symbol","train_month","test_month"))

# quitar ceros de los días que no tradeo porque se entrena el modelo
# hay un problema con los nuevos pesos cuando incia el mes
# no le puedo dar distinct al día debido a que quiero que se entrene con la data mañas reciente 
# tengo que pensar si acomodando la data de mayor fecha am  menor y así le doy distinct is me quedo con la data que quiero 

aux <- aux |> 
  filter(ind>=9) |> 
  arrange(desc(date)) |> 
  distinct(date,symbol,.keep_all = T) |> 
  arrange(date)

final_results_mom <- aux |> 
  mutate(daily_return_per_stock = test_returns * weight) |> 
  group_by(date) |> 
  summarise(daily_returns = sum(daily_return_per_stock)) |> 
  mutate(cum_returns = cumsum(daily_returns))|> 
  mutate(tipo = "OML") |> 
  select(- daily_returns)
plot(final_results_mom$cum_returns)


### Monthly training strategy: Cumulative Months Strategy 


df_test_returns_cm <- read_csv("03_Output/df_test_returns_complete_monthly.csv")

# Transform data to long format, then left join by the training month 
# and evaluate returns with new weights

df_test_cm <- df_test_returns_cm |>
  group_by(symbol,test_month) |>
  mutate(cum_ret_model = cumsum(test_returns),
         ind=1:n()) |>
  select(ind,symbol,end_month,test_month,test_returns,cum_ret_model) |> 
  left_join(tabla_lasso_cm,by=c("end_month"="training_month","symbol"="stock")) |> 
  drop_na(weight) # Remove those months that are not considered in the lasso model 


aux <- left_join(df_test_cm,df_aux |> select(date,ind,symbol,train_month,test_month),
                 by = c("ind","symbol","end_month"="train_month","test_month"))

aux <- aux |> 
  filter(ind>=9) |> 
  arrange(desc(date)) |> 
  distinct(date,symbol,.keep_all = T) |> 
  arrange(date)

final_results_cm <- aux |> 
  mutate(daily_return_per_stock = test_returns * weight) |> 
  group_by(date) |> 
  summarise(daily_returns = sum(daily_return_per_stock)) |> 
  mutate(cum_returns = cumsum(daily_returns))|> 
  mutate(tipo = "CM") |> 
  select(- daily_returns)
plot(final_results_cm$cum_returns)


### Monthly training strategy: Three Month Strategy 


df_test_returns_tm <- read_csv("03_Output/df_test_returns_threemonth.csv")

# Transform data to long format, then left join by the training month 
# and evaluate returns with new weights

df_test_tm <- df_test_returns_tm |>
  group_by(symbol,test_month) |>
  mutate(cum_ret_model = cumsum(test_returns),
         ind=1:n()) |>
  select(ind,symbol,end_month,test_month,test_returns,cum_ret_model) |> 
  left_join(tabla_lasso_tm,by=c("end_month"="training_month","symbol"="stock")) |> 
  drop_na(weight) # Remove those months that are not considered in the lasso model 


aux <- left_join(df_test_tm,df_aux |> select(date,ind,symbol,train_month,test_month),
                 by = c("ind","symbol","end_month"="train_month","test_month"))

aux <- aux |> 
  filter(ind>=9) |> 
  arrange(desc(date)) |> 
  distinct(date,symbol,.keep_all = T) |> 
  arrange(date)

final_results_tm <- aux |> 
  mutate(daily_return_per_stock = test_returns * weight) |> 
  group_by(date) |> 
  summarise(daily_returns = sum(daily_return_per_stock,na.rm = T)) |> 
  mutate(cum_returns = cumsum(daily_returns))|> 
  mutate(tipo = "TML") |> 
  select(- daily_returns)
plot(final_results_tm$cum_returns)

# See how the market performed for the same period of time 

data_ipc <- read_csv("02_Input/IPC_MXX.csv")

data_ipc <- data_ipc |>  
  mutate(returns = (Close-lag(Close))/lag(Close)*100) |>  
  drop_na() |>  
  mutate(cum_returns=cumsum(returns))  |>  
  dplyr::select(Date,cum_returns) |>  
  rename(date=Date) |> 
  mutate(tipo = "IPC")

final_results <- bind_rows(data_ipc,final_results_mom,final_results_tm,final_results_cm)

ggplot(final_results, aes(x = date, y = cum_returns,group=tipo,color=tipo)) +
  geom_line(size=1.5) +
  ggtitle("Results: Portfolio vs Market Returns") +
  labs(y = "Cumulative Returns", x = "Trading Day") +
  #coord_cartesian(ylim=c(0,0.55)) +
  #scale_fill_brewer(palette="PuOr") +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  scale_y_continuous(labels = function(x) paste0(scales::comma(x),"%")) +
  #scale_y_continuous(labels = scales::percent)+
  #scale_x_continuous(labels = comma)+
  scale_x_date(date_breaks = "3 month")+
  theme_bw() +
  theme(text =element_text(family="sans"),
        legend.title = element_blank(),
        legend.position = "bottom",   
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold",angle = 90,vjust = .5,hjust = 1),#angle = 90,vjust = .5,hjust = 1
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold",size = 15),
        axis.title.y = element_text(face = "bold",size = 15),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = .5, face ="bold",size = 15),
        plot.subtitle = element_text(hjust = .5,size = 13),
        plot.caption = element_text(size=11))
ggsave(filename = paste0("03_Output/Plots/FinalResultsStrategies.png"),width=8,height=6)


### How diversified is my portfolio? How many stocks I have per month? 


total_stocks_mom <- tabla_lasso_mom |> 
  group_by(training_month) |> 
  distinct(stock) |> 
  mutate(ind=1) |> 
  summarise(total_stocks=sum(ind)) |> 
  mutate(tipo="OML")

total_stocks_tm <- tabla_lasso_tm|> 
  group_by(training_month) |> 
  distinct(stock) |> 
  mutate(ind=1) |> 
  summarise(total_stocks=sum(ind))|> 
  mutate(tipo="TML")

total_stocks_cm <- tabla_lasso_cm|> 
  group_by(training_month) |> 
  distinct(stock) |> 
  mutate(ind=1) |> 
  summarise(total_stocks=sum(ind))|> 
  mutate(tipo="CM")

total_stocks <- bind_rows(total_stocks_mom,
                          total_stocks_tm,
                          total_stocks_cm)
total_stocks$training_month <- factor(total_stocks$training_month,
                                      levels = unique(total_stocks$training_month))


ggplot(total_stocks, aes(x = training_month, y = total_stocks,group=tipo,color=tipo)) +
  geom_line(size=1.5) +
  ggtitle("Stocks per Month and Strategy") +
  labs(y = "Number of Stocks", x = "Training Month") +
  #coord_cartesian(ylim=c(0,0.55)) +
  #scale_fill_brewer(palette="PuOr") +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  #scale_y_continuous(labels = scales::percent)+
  #scale_x_continuous(labels = comma)+
  #scale_x_date(date_breaks = "3 month")+
  theme_bw() +
  theme(text =element_text(family="sans"),
        legend.title = element_blank(),
        legend.position = "bottom",   
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold",angle = 90,vjust = .5,hjust = 1),#angle = 90,vjust = .5,hjust = 1
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold",size = 15),
        axis.title.y = element_text(face = "bold",size = 15),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = .5, face ="bold",size = 15),
        plot.subtitle = element_text(hjust = .5,size = 13),
        plot.caption = element_text(size=11))
ggsave(filename = paste0("03_Output/Plots/StocksPerMonthStrategies.png"),width=8,height=6)

















