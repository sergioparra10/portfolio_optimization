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
# tengo que pensar si acomodando la data de mayor fecha am  menor y así le doy sistintct is me quedo con la data que quiero 

aux <- aux |> 
  filter(ind>=9) |> 
  arrange(desc(date)) |> 
  distinct(date,symbol,.keep_all = T) |> 
  arrange(date)

final_results_mom <- aux |> 
  mutate(daily_return_per_stock = test_returns * weight) |> 
  group_by(date) |> 
  summarise(daily_returns = sum(daily_return_per_stock)) |> 
  mutate(cum_returns = cumsum(daily_returns))
plot(final_results_mom$cum_returns)








