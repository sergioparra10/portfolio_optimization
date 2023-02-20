### Portfolio Optimization
### Model Validation
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
df_train_hold <- read_csv("03_Output/df_train_hold.csv")%>% 
  rename(ind=1)
df_test_returns <- read_csv("03_Output/df_test_returns.csv")%>% 
  rename(ind=1)
df_test_hold <- read_csv("03_Output/df_test_hold.csv")%>% 
  rename(ind=1)
df_sharpe <- read_csv("03_Output/df_sharpes.csv")%>% 
  rename(ind=1)

# Returns are different as in one data base are standarized and in the other 
# one are just log returns 

### Sharpe Ratio 
# Get the example of one of the best performing models 

max_sr <- df_sharpe %>% 
  group_by(symbol) %>% 
  summarise(maximo=max(sharpe)) %>% 
  arrange(- maximo)

# Plot training epochs vs sharpe ratio evolution 

plot_sr <- df_sharpe %>% 
  dplyr::filter(symbol == max_sr$symbol[2]) %>% 
  mutate(epochs = 1:2000,
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
#ggsave(filename = paste0("03_Output/Plots/ResultsSharpeRatio.png"),width=8,height=5)


### Sharpe Ratios distribution

max_sr <- max_sr %>% 
  drop_na() %>% 
  mutate(col="col") %>%
  mutate(risk_cat=cut(as.numeric(maximo),
                        unique(quantile(as.numeric(maximo),na.rm=T,probs=seq(0,1,by=1/3))),
                                              include.lowest = T,labels=F))

mean_sr <- mean(max_sr$maximo)

ggplot(max_sr, aes(x = maximo)) +
  geom_histogram(bins = 50, fill = "steelblue", colour = "black") +
  ggtitle("Sharpe Ratios Distribution") +
  labs(y = "Number Of Stocks", x = "Sharpe Ratio",
       caption = "The dashed line is the distribution's mean.") +
  geom_vline(xintercept = mean_sr,linetype=2,size=1)+
  #scale_y_continuous(labels = scales::percent)+
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

#ggsave(filename = paste0("03_Output/Plots/DistributionSharpeRatio.png"),width=8,height=5)


# Summary statistics, sharpe ratio 

summary_stats_sr <- summary(max_sr$maximo)

aux <- data_bmv %>% 
  filter(symbol == "AEROMEX.MX")

### Model performance: training and testing data frames 

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
  mutate(trading_day = 1:472) %>% 
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
#ggsave(filename = paste0("03_Output/Plots/ResultsTrainModel.png"),width=8,height=5)



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
#ggsave(filename = paste0("03_Output/Plots/ResultsTestModel.png"),width=8,height=5)

# Summary statistics of cumulative returns 

returns_test <- df_test %>% 
  group_by(symbol) %>% 
  filter(ind == max(ind)) %>% 
  drop_na() %>%
  #filter(cum_ret_model>0) %>% 
  #mutate(dif_returns = cum_ret_model - cum_ret_hold) %>% 
  select(symbol,5,6) %>% 
  pivot_longer(cols = 2:3,names_to = "strategy",values_to = "cum_returns")

returns_test$strategy[returns_test$strategy=="cum_ret_hold"] <- "Buy and Hold"
returns_test$strategy[returns_test$strategy=="cum_ret_model"] <- "Reinforcement Learning Model"

ggplot(returns_test, aes(x=cum_returns, fill=strategy)) + 
  geom_histogram(alpha=0.5, position="identity")+
  ggtitle("Cummulative Returns in Test Data") +
  labs(x = "Cumulative Returns", y = "Number of stocks") +
  xlim(-50,75)+
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
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
#ggsave(filename = paste0("03_Output/Plots/ResultsTestDist.png"),width=8,height=5)

### Prepare data for lasso modeling 

data_lasso_example <- read_csv("03_Output/df_stocks_portfolio_weights.csv")

### ON HOLD 
# Filter those stocks that we are going to select 

data_lasso <- data_bmv %>% 
  drop_na(log_rt_close) %>% 
  dplyr::select(date,symbol,log_rt_close) %>% 
  mutate(log_rt_close=as.numeric(log_rt_close)) %>% 
  pivot_wider(names_from = "symbol",values_from = "log_rt_close") %>% 
  drop_na()
#write_csv(data_lasso,"03_Output/data_lasso_final.csv")

### Summary Statistics: Stock returns 

est_descriptiva <- RCT::summary_statistics(data = data_lasso %>% 
                                             dplyr::select(- 1))
names(est_descriptiva)
est_descriptiva <- est_descriptiva %>% 
  select(1,2,3,4,7,8,9,12)

xtable::xtable(est_descriptiva)

library(glmnet)

X <- data_lasso %>% dplyr::select(2:98)%>% remove_rownames
X <- prcomp(X)
X <- X$x

# # before standardization
# colMeans(X)    # mean
# apply(X,2,sd)  # standard deviation
# 
# # scale : mean = 0, std=1
# X = scale(X)
# 
# # after standardization
# colMeans(X)    # mean
# apply(X,2,sd)  # standard deviation

Y <- rep(1,443)

#——————————————–
# Model
#——————————————–
# in paper alpha=0.0005
lambda <- 0.01

# standard linear regression without intercept(-1)
li.eq <-  lm(Y ~ X-1) 

# lasso
la.eq <- glmnet(X, Y, lambda=lambda, 
                family="gaussian", 
                intercept = F, alpha=1) 
aux <- la.eq$beta

# Ridge
ri.eq <- glmnet(X, Y, lambda=lambda, 
                family="gaussian", 
                intercept = F, alpha=0) 



