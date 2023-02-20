### Portfolio Optimization
### Data extraction
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

stocks_bmv <- read_excel("02_Input/stocks_bmv.xlsx")
rows_originales <- nrow(stocks_bmv)
stocks_bmv <- stocks_bmv %>% 
  drop_na(acronimo_yahoo)
rows_info <- nrow(stocks_bmv)

data_bmv <- list()
for (i in stocks_bmv$acronimo_yahoo) {
  data_aux <- tq_get(i,
                    from = "2019-01-01",
                    to = "2022-12-31",
                    get = "stock.prices")
  data_bmv <- rbind(data_bmv,data_aux)
}

disponibles <- unique(data_bmv$symbol)

### Estimate log returns 

data_bmv <- data_bmv %>% 
  group_by(symbol) %>% 
  mutate(log_rt_close = c(NA,diff(log(close), lag=1)))

save(data_bmv,file = "03_Output/data_bmv.rda")

load(file= "03_Output/data_bmv.rda")
write_csv(data_bmv,"03_Output/FinalDataSet.csv")
df <- read_csv("03_Output/FinalDataSet.csv")





