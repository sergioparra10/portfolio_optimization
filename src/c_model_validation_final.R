### Portfolio Optimization
### Model Validation, month over month & monthly historic 
### Sergio Parra 
### sparrap@itam.mx

# Library ----

paquetes<-c("tidyverse", "lubridate","stringr","scales","gsheet","RColorBrewer",
            "openxlsx","ggrepel" ,"readxl","janitor","xtable")

sapply(paquetes,
       function(x) {
         if(!x %in% rownames(installed.packages()))
           install.packages(x)
         require(x, character.only=T)
       })
rm(list=setdiff(ls(),c("")))

setwd("~/Documents/ITAM/Tesis/")

### Data 

load(file="03_Output/data_bmv.rda")
df_train_returns <- read_csv("03_Output/df_train_returns.csv") %>% 
  rename(ind=1)
df_train_returns_mom <- read_csv("03_Output/df_train_returns_monthly.csv") %>% 
  rename(ind=1)
df_train_returns_cm <- read_csv("03_Output/df_train_hold_complete_monthly.csv") %>% 
  rename(ind=1)
df_train_returns_tm <- read_csv("03_Output/df_train_hold_threemonth.csv") %>% 
  rename(ind=1)

df_train_hold <- read_csv("03_Output/df_train_hold.csv")%>% 
  rename(ind=1)
df_train_hold_mom <- read_csv("03_Output/df_train_hold_monthly.csv")%>% 
  rename(ind=1)
df_train_hold_cm <- read_csv("03_Output/df_train_hold_complete_monthly.csv")%>% 
  rename(ind=1)
df_train_hold_tm <- read_csv("03_Output/df_train_hold_threemonth.csv")%>% 
  rename(ind=1)

df_test_returns <- read_csv("03_Output/df_test_returns.csv")%>% 
  rename(ind=1)
df_test_returns_mom <- read_csv("03_Output/df_test_returns_monthly.csv")%>% 
  rename(ind=1)
df_test_returns_cm <- read_csv("03_Output/df_test_returns_complete_monthly.csv")%>% 
  rename(ind=1)
df_test_returns_tm <- read_csv("03_Output/df_test_returns_threemonth.csv")%>% 
  rename(ind=1)

df_test_hold <- read_csv("03_Output/df_test_hold.csv")%>% 
  rename(ind=1)
df_test_hold_mom <- read_csv("03_Output/df_test_hold_monthly.csv")%>% 
  rename(ind=1)
df_test_hold_cm <- read_csv("03_Output/df_test_hold_complete_monthly.csv")%>% 
  rename(ind=1)
df_test_hold_tm <- read_csv("03_Output/df_test_hold_threemonth.csv")%>% 
  rename(ind=1)

df_sharpe <- read_csv("03_Output/df_sharpes.csv")%>% 
  rename(ind=1)
df_sharpe_mom <- read_csv("03_Output/df_sharpes_monthly.csv")%>% 
  rename(ind=1)
df_sharpe_cm <- read_csv("03_Output/df_sharpes_complete_monthly.csv")%>% 
  rename(ind=1)
df_sharpe_tm <- read_csv("03_Output/df_sharpes_threemonth.csv")%>% 
  rename(ind=1)

df_thetas <- read_csv("03_Output/df_thetas.csv")%>% 
  rename(ind=1)
df_thetas_mom <- read_csv("03_Output/df_thetas_monthly.csv")%>% 
  rename(ind=1)
df_thetas_cm <- read_csv("03_Output/df_thetas_complete_monthly.csv")%>% 
  rename(ind=1)
df_thetas_tm <- read_csv("03_Output/df_thetas_threemonth.csv")%>% 
  rename(ind=1)

# Returns are different as in one data base are standardized and in the other 
# one are just log returns 

### Sharpe Ratio 
# Get the example of one of the best performing models 

max_sr <- df_sharpe %>% 
  group_by(symbol) %>% 
  summarise(maximo=max(sharpe)) %>% 
  arrange(- maximo)

# This is not a correct way of comparing the sharpe ratios, it must be in the same month 

max_sr_mom <- df_sharpe_mom %>% 
  group_by(symbol) %>% 
  summarise(maximo=max(sharpe)) %>% 
  arrange(- maximo)

max_sr_cm <- df_sharpe_cm %>% 
  group_by(symbol) %>% 
  summarise(maximo=max(sharpe)) %>% 
  arrange(- maximo)

max_sr_tm <- df_sharpe_tm %>% 
  group_by(symbol) %>% 
  summarise(maximo=max(sharpe)) %>% 
  arrange(- maximo)

# Lets join the tables mom, cm, tm to evaluate which strategy yields better results on average 
# Compare the sharpe ratios distribution. However, it should be within the same stocks. 

df_sharpes_mix <- bind_rows(
  df_sharpe_cm |> dplyr::select(sharpe,symbol,test_month) |> mutate(strategy="cm"),
  df_sharpe_mom |> dplyr::select(sharpe,symbol,test_month) |> mutate(strategy="mom"),
  df_sharpe_tm |> dplyr::select(sharpe,symbol,test_month) |> mutate(strategy="tm"))

max_month_strategy <- df_sharpes_mix |> 
  group_by(symbol,test_month,strategy) |> 
  summarise(max_sr=max(sharpe)) |> 
  pivot_wider(names_from = "strategy",values_from = "max_sr")

# aux <- max_month_strategy |>
#   ungroup() |>
#   filter(symbol=="AC.MX") |>
#   drop_na()
# 
# t_tests <- t.test(aux$cm,aux$mom)
# t_tests$p.value
# t_tests$statistic[1]
# t_tests$conf.int[1] # Lower
# t_tests$conf.int[2] # Upper
# t_tests$estimate[1]
# t_tests$estimate[2]

plot_sr_mix <- df_sharpes_mix |> 
  group_by(symbol,test_month,strategy) |> 
  summarise(max_sr=max(sharpe)) |> 
  drop_na()

# Do the same test for all the stocks and store the values in a loop
df_t_test <- data.frame()
for (i in unique(df_sharpes_mix$symbol)) {
  aux <- plot_sr_mix |> 
    filter(symbol == i) |> 
    drop_na()
  
  t_test_1 <- t.test(max_sr ~ strategy, 
                     data = subset(aux, strategy %in% c("cm", "mom")),
                     conf.level = .99)
  t_test_2 <- t.test(max_sr ~ strategy, 
                     data = subset(aux, strategy %in% c("cm", "tm")),
                     conf.level = .99)
  t_test_3 <- t.test(max_sr ~ strategy, 
                     data = subset(aux, strategy %in% c("mom", "tm")),
                     conf.level = .99)

  df_t_test_aux_1 <- data.frame(symbol=i,
                              mean_1  = t_test_1$estimate[1],
                              mean_2 = t_test_1$estimate[2],
                              lower_ci = t_test_1$conf.int[1],
                              upper_ci = t_test_1$conf.int[2],
                              t_statistic = t_test_1$statistic[1],
                              p_value  = t_test_1$p.value,
                              group_1="cm",
                              group_2="mom")
  df_t_test_aux_2 <- data.frame(symbol=i,
                                mean_1  = t_test_2$estimate[1],
                                mean_2 = t_test_2$estimate[2],
                                lower_ci = t_test_2$conf.int[1],
                                upper_ci = t_test_2$conf.int[2],
                                t_statistic = t_test_2$statistic[1],
                                p_value  = t_test_2$p.value,
                                group_1="cm",
                                group_2="tm")
  df_t_test_aux_3 <- data.frame(symbol=i,
                                mean_1 = t_test_3$estimate[1],
                                mean_2 = t_test_3$estimate[2],
                                lower_ci = t_test_3$conf.int[1],
                                upper_ci = t_test_3$conf.int[2],
                                t_statistic = t_test_3$statistic[1],
                                p_value  = t_test_3$p.value,
                                group_1="mom",
                                group_2="tm")
  df_t_test <- bind_rows(df_t_test,
                         df_t_test_aux_1,
                         df_t_test_aux_2,
                         df_t_test_aux_3)
}

df_t_test <- df_t_test |> 
  mutate(difference_means = mean_1 - mean_2) |> 
  arrange(- difference_means) |> 
  relocate(difference_means,.after = mean_2) |> 
  mutate(p_value = round(p_value,4))

plot_sr_mix <- plot_sr_mix |> 
  mutate(training_strategy = case_when(
    strategy == "mom" ~ "One Month Lag",
    strategy == "tm" ~ "Three Month Lag",
    strategy == "cm" ~ "Cumulative Months"))


ggplot(plot_sr_mix |> 
         filter(symbol=="LACOMERUBC.MX"), # LACOMERUBC.MX
       aes(max_sr, fill = training_strategy)) + 
  geom_histogram(alpha = 0.5)+
  ggtitle(paste0("Strategy performance comparison: LACOMERUBC.MX")) +
  labs(y = "Months", x = "Sharpe Ratio",
       subtitle = "Test months and Sharpe Ratio Distribution") +
  #coord_cartesian(ylim=c(0,0.55)) +
  #scale_fill_brewer(palette="PuOr") +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  scale_fill_discrete(breaks=c("Cumulative Months", 
                               "Three Month Lag",
                               "One Month Lag"))+
  #scale_y_continuous(labels = scales::percent)+
  theme_bw() +
  theme(text =element_text(family="sans"),
        legend.title = element_blank(),
        legend.position = "bottom",   
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),#angle = 90,vjust = .5,hjust = 1
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold",size = 15),
        axis.title.y = element_text(face = "bold",size = 15),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = .5, face ="bold",size = 15),
        plot.subtitle = element_text(hjust = .5,size = 13),
        plot.caption = element_text(size=11))
ggsave(filename = paste0("03_Output/Plots/ResultsSharpeRatio_StrategyComparison.png"),
       width=8,height=5)

### Plot training epochs vs sharpe ratio evolution 
# Plot in one single ggplot all the training strategies with a 
# example stock, say LACOMERUBC.MX


plot_sr <- df_sharpe %>% 
  dplyr::filter(symbol == max_sr$symbol[4]) %>% 
  mutate(epochs = 1:1000,
         col="col")

ggplot(plot_sr, aes(x = epochs, y = sharpe,group=col,color=col)) +
  geom_line(size=1.5) +
  ggtitle(paste0("Training Results: ",unique(plot_sr$symbol))) +
  labs(y = "Sharpe Ratio", x = "Epoch Number") +
  #coord_cartesian(ylim=c(0,0.55)) +
  #scale_fill_brewer(palette="PuOr") +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  #scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = comma)+
  theme_bw() +
  theme(text =element_text(family="sans"),
        legend.title = element_blank(),
        legend.position = "none",   
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),#angle = 90,vjust = .5,hjust = 1
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold",size = 15),
        axis.title.y = element_text(face = "bold",size = 15),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = .5, face ="bold",size = 15),
        plot.subtitle = element_text(hjust = .5,size = 13),
        plot.caption = element_text(size=11))
ggsave(filename = paste0("03_Output/Plots/ResultsSharpeRatio.png"),width=8,height=5)

plot_sr_mix<- df_sharpes_mix |> 
  filter(test_month == "2020-01",
         symbol=="BBVA.MX") |> # "LACOMERUBC.MX"      "BBVA.MX" BIMBOA.MX ELEKTRA.MX GCARSOA1.MX
  group_by(strategy) |> 
  mutate(epochs= row_number())
plot_sr_mix$strategy[plot_sr_mix$strategy=="cm"] <- "CM"
plot_sr_mix$strategy[plot_sr_mix$strategy=="tm"] <- "TML"
plot_sr_mix$strategy[plot_sr_mix$strategy=="mom"] <- "OML"


ggplot(plot_sr_mix, aes(x = epochs, y = sharpe,group=strategy,color=strategy)) +
  geom_line(size=1.5) +
  ggtitle(paste0("Training Results per Strategy: ",unique(plot_sr_mix$symbol))) +
  labs(y = "Sharpe Ratio", x = "Epoch Number") +
  #coord_cartesian(ylim=c(0,0.55)) +
  #scale_fill_brewer(palette="PuOr") +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  #scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = comma,breaks = c(0,100,200,300,400,500,600,700,800))+
  theme_bw() +
  theme(text =element_text(family="sans"),
        legend.title = element_blank(),
        legend.position = "bottom",   
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),#angle = 90,vjust = .5,hjust = 1
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold",size = 15),
        axis.title.y = element_text(face = "bold",size = 15),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = .5, face ="bold",size = 15),
        plot.subtitle = element_text(hjust = .5,size = 13),
        plot.caption = element_text(size=11))
ggsave(filename = paste0("03_Output/Plots/ResultsSharpeRatioAllStrategiesBBVA_MX.png"),width=8,height=5)


### Save t test data frame to add to the appendix 
df_t_test_appendix <- df_t_test |> 
  rename(Stock = 1,
         'Mean 1' = 2,
         'Mean 2' = 3,
         'Diff in Means' = 4,
         'Lower CI' = 5,
         'Upper CI' = 6,
         'T Statistic' = 7,
         'P-value' = 8) |> 
  mutate(Strategy = case_when(
    group_1 == "mom" & group_2 == "tm" ~ "OML vs TML",
    group_1 == "cm" & group_2 == "mom" ~ "CM vs OML",
    group_1 == "cm" & group_2 == "tm" ~ "CM vs TML"
  )) |> 
  select(1:8,11)
sum(is.na(df_t_test_appendix$Strategy))

rownames(df_t_test_appendix) <- 1:nrow(df_t_test_appendix)
xtable(df_t_test_appendix |> slice(1:30))
xtable(df_t_test_appendix |> slice(31:60))
xtable(df_t_test_appendix |> slice(61:90))
xtable(df_t_test_appendix |> slice(91:120))

xtable(df_t_test_appendix |> slice(121:150))
xtable(df_t_test_appendix |> slice(151:180))
xtable(df_t_test_appendix |> slice(181:210))
xtable(df_t_test_appendix |> slice(211:240))

xtable(df_t_test_appendix |> slice(241:270))
xtable(df_t_test_appendix |> slice(271:285))


### Model performance: training and testing data frames 
# Keep only complete testing periods: start date: 2019-04-01

df_test <- left_join(df_test_returns,df_test_hold) %>% 
  relocate(symbol,ind) %>% 
  group_by(symbol) %>% 
  mutate(cum_ret_model = cumsum(test_returns),
         cum_ret_hold = cumsum(hold_returns))

df_train<- left_join(df_train_returns,df_train_hold) %>% 
  relocate(symbol,ind) %>% 
  group_by(symbol) %>% 
  mutate(cum_ret_model = cumsum(train_returns),
         cum_ret_hold = cumsum(hold_returns))

max_returns_test <- df_test %>% 
  group_by(symbol) %>% 
  filter(ind == max(ind)) %>% 
  drop_na() %>%
  filter(cum_ret_model>0) %>% 
  mutate(dif_returns = cum_ret_model - cum_ret_hold) %>% 
  arrange(- dif_returns, cum_ret_model)

plot_mod_train <- df_train %>% 
  filter(symbol == max_returns_test$symbol[1]) %>% 
  dplyr::select(symbol,5,6) %>% 
  mutate(trading_day = 1:807) %>% 
  pivot_longer(cols = 2:3,names_to = "strategy",values_to = "cum_returns")
plot_mod_train$strategy[plot_mod_train$strategy=="cum_ret_hold"] <- "Buy and Hold"
plot_mod_train$strategy[plot_mod_train$strategy=="cum_ret_model"] <- "Reinforcement Learning Model"


ggplot(plot_mod_train, aes(x = trading_day, y = cum_returns,group=strategy,color=strategy)) +
  geom_line(size=1.5) +
  ggtitle(paste0("Training Data: ",unique(plot_mod_train$symbol))) +
  labs(y = "Cumulative Returns", x = "Trading Day") +
  #coord_cartesian(ylim=c(0,0.55)) +
  #scale_fill_brewer(palette="PuOr") +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  scale_y_continuous(labels = function(x) paste0(scales::comma(x),"%")) +
  #scale_y_continuous(labels = scales::percent)+
  #scale_x_continuous(labels = comma)+
  theme_bw() +
  theme(text =element_text(family="sans"),
        legend.title = element_blank(),
        legend.position = "bottom",   
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),#angle = 90,vjust = .5,hjust = 1
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold",size = 15),
        axis.title.y = element_text(face = "bold",size = 15),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = .5, face ="bold",size = 15),
        plot.subtitle = element_text(hjust = .5,size = 13),
        plot.caption = element_text(size=11))
ggsave(filename = paste0("03_Output/Plots/ResultsTrainModel.png"),width=8,height=5)



plot_mod_test <- df_test %>% 
  filter(symbol == max_returns_test$symbol[1]) %>% 
  dplyr::select(symbol,5,6) %>% 
  mutate(trading_day = 1:200) %>% 
  pivot_longer(cols = 2:3,names_to = "strategy",values_to = "cum_returns")
plot_mod_test$strategy[plot_mod_test$strategy=="cum_ret_hold"] <- "Buy and Hold"
plot_mod_test$strategy[plot_mod_test$strategy=="cum_ret_model"] <- "Reinforcement Learning Model"


ggplot(plot_mod_test, aes(x = trading_day, y = cum_returns,group=strategy,color=strategy)) +
  geom_line(size=1.5) +
  ggtitle(paste0("Test Data: ",unique(plot_mod_test$symbol))) +
  labs(y = "Cumulative Returns", x = "Trading Day") +
  #coord_cartesian(ylim=c(0,0.55)) +
  #scale_fill_brewer(palette="PuOr") +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  scale_y_continuous(labels = function(x) paste0(scales::comma(x),"%")) +
  #scale_y_continuous(labels = scales::percent)+
  #scale_x_continuous(labels = comma)+
  theme_bw() +
  theme(text =element_text(family="sans"),
        legend.title = element_blank(),
        legend.position = "bottom",   
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),#angle = 90,vjust = .5,hjust = 1
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold",size = 15),
        axis.title.y = element_text(face = "bold",size = 15),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = .5, face ="bold",size = 15),
        plot.subtitle = element_text(hjust = .5,size = 13),
        plot.caption = element_text(size=11))
ggsave(filename = paste0("03_Output/Plots/ResultsTestModel.png"),width=8,height=5)







