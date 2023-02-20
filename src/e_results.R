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

#results_lasso <- read_csv("03_Output/df_coeff_lasso.csv")
results_lasso <- read_csv("03_Output/df_coeff_lasso_v2.csv")

tabla_lasso <- results_lasso %>% 
  arrange(desc(weight)) %>% 
  select(2:4) %>% 
  slice(1:11) %>% 
  rename(Stock = 1,
         Coefficient = 2,
         Weight = 3)

xtable::xtable(tabla_lasso)

df_test_returns <- read_csv("03_Output/df_test_returns.csv")

df_test <- df_test_returns %>% 
  group_by(symbol) %>% 
  mutate(cum_ret_model = cumsum(test_returns)) %>% 
  select(2:4) %>% 
  filter(symbol %in% tabla_lasso$Stock) %>% 
  left_join(tabla_lasso %>% 
              select(Stock, Weight) %>% 
              rename(symbol=Stock) %>% 
              mutate(weight=Weight/100), by = "symbol")

df_test <- df_test %>% 
  mutate(weighted_return=cum_ret_model*weight)

results <- df_test %>% 
  dplyr::select(symbol,weighted_return) %>% 
  group_by(symbol) %>% 
  mutate(ind = 1:max(length(symbol))) %>% 
  pivot_wider(names_from = "symbol",values_from = "weighted_return") %>% 
  ungroup() %>% 
  mutate(total_returns = select(., 2:12) %>% rowSums(na.rm = TRUE))

# See how the market performed for the same period of time 


data_ipc <- read_csv("02_Input/IPC_^MXX.csv")
# get last 200 trading days 
data_ipc_200 <- data_ipc %>% 
  slice(473:673) %>% 
  mutate(returns = (Close-lag(Close))/lag(Close)*100) %>% 
  drop_na() %>% 
  mutate(cum_returns=cumsum(returns)) %>% 
  dplyr::select(Date,cum_returns) %>% 
  rename(IPC = cum_returns) %>% 
  mutate(ind=1:200)

final_results <- left_join(data_ipc_200,results, by = "ind") %>% 
  dplyr::select(1,2,15) %>% 
  rename(`Trading Strategy`=total_returns) %>% 
  pivot_longer(cols = 2:3,names_to = "strategy",values_to = "returns")


ggplot(final_results, aes(x = Date, y = returns,group=strategy,color=strategy)) +
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
  scale_x_date(date_breaks = "1 month")+
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
ggsave(filename = paste0("03_Output/Plots/FinalResults.png"),width=8,height=6)




