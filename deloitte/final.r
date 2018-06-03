library(plyr)
library(tidyverse)
library(reshape2)
library(hms)
library(lubridate)


input_data <- read_csv(file = "in/tables/ga_transactions.csv")
input_data$source <- as.factor(input_data$source)
input_data$medium <- as.factor(input_data$medium)
input_data$campaign <- as.factor(input_data$campaign)

shorted <- input_data[,3:9]
################
source_Table <- data.frame(table(shorted$source))
top10 <- source_Table[order(-source_Table$Freq),] 

ggplot(top10[c(1:10),], aes(Var1,Freq)) + geom_point()
###############
shorted$Year <- format(as.Date(shorted$date), "%Y")
shorted$Month <- format(as.Date(shorted$date), "%m")


################
year2017 <- shorted %>% 
  filter(Year == 2017) %>% 
  group_by(Month, source) %>% 
  summarize(sumTransactionRevenue = sum(transactionRevenue)) %>% 
  top_n(n = 12, wt = sumTransactionRevenue)


year2017_1 <- shorted %>% 
  filter(Year == 2017) %>% 
  group_by(Month, source) %>% 
  summarize(sumTransactionRevenue = sum(transactionRevenue)) %>% 
  top_n(n = 12, wt = sumTransactionRevenue) %>% 
  summarise(LargestSumTransactionRevenue = max(sumTransactionRevenue))

year2017_2 <- inner_join(year2017, year2017_1, by = c("sumTransactionRevenue" = "LargestSumTransactionRevenue") )

ggplot(year2017_2, aes(Month.x, sumTransactionRevenue)) + geom_histogram()

####################
write.csv(result, file = "out/tables/output.csv", row.names = FALSE)
##########
ga_profiles <- read_csv("in/tables/ga_profiles.csv")
ga_sessions <- read_csv("in/tables/ga_sessions.csv")
ga_ana_pageviews <- read_csv("in/tables/google_analytics_pageview.csv")

merged_data <- inner_join(ga_profiles, ga_sessions, by=c("id" = "idProfile"))

####################
ggplot(zbozi, aes(impressions, clicks)) + geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  ggtitle("Impressions & Clicks", subtitle = "") + xlab("Impressions") + ylab("clicks")


ggplot(zbozi, aes(cpc, position)) + geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  ggtitle("Cpc & Position", subtitle = "") + xlab("Cpc") + ylab("Position")

ggplot(zbozi, aes(spend, position)) + geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  ggtitle("Spend & Position", subtitle = "") + xlab("Spend") + ylab("Position")

##################### Load Heureka_cz/sk
heureka_cz <- read_csv("in/tables/heureka_cz.csv", 
                                          col_types = cols(cpc = col_double()), 
                                          locale = locale(decimal_mark = ","))
heureka_cz$conversion_rates <- as.double(heureka_cz$conversion_rates) / 100

heureka_sk <- read_csv("in/tables/heureka_sk.csv")
heureka_sk$conversion_rates <- as.double(heureka_sk$conversion_rates) / 10000

heureka_sk_conv <- read_csv("out/tables/heureka_fx_conv.csv", 
                            col_types = cols(cpc = col_double(), 
                                             rate = col_character()), 
                            locale = locale(decimal_mark = ","))

exchange <- read_csv("in/tables/exchange_rates.csv", 
                     col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>% 
  filter(currency_orderby == 1)

heureka_sk <- left_join(heureka_sk, exchange, by = c("date"="date"))
heureka_sk_conv$conversion_rates <- as.double(heureka_sk_conv$conversion_rates) / 10000
heureka_sk_conv$cpc_cz <- heureka_sk_conv$cpc * as.double(heureka_sk_conv$rate)
heureka_sk_conv$spend_cz <- heureka_sk_conv$spend * as.double(heureka_sk_conv$rate)

heureka_sk_sm <- data.frame(heureka_sk_conv$date,
                  heureka_sk_conv$cpc_cz, 
                  heureka_sk_conv$spend_cz, 
                  heureka_sk_conv$conversion_rates) 

heureka_cz_sm <- data.frame(heureka_cz$date,
                  heureka_cz$cpc,
                  heureka_cz$spend, 
                  heureka_cz$conversion_rates)

mergedDF_heureka <- inner_join(heureka_sk_sm, heureka_cz_sm, 
                               by = c("heureka_sk_conv.date"="heureka_cz.date"))

############ Load & Process Zbozi data
zbozi <- read_csv("in/tables/zbozi_cz.csv", 
                  col_types = cols(date = col_date(format = "%d.%m.%Y")))

names(zbozi)[names(zbozi) == 'spend'] <- 'zbozi_spend'
df_zbozi_Spend <- zbozi[,c(2,7)]

df_heureka_Zbozi_joined <- inner_join(mergedDF_heureka, df_zbozi_Spend, 
                                      by = c("heureka_sk_conv.date"="date"))

df_heureka_Zbozi_joined$CZ_SK_Spend <- df_heureka_Zbozi_joined$heureka_cz.spend + 
  df_heureka_Zbozi_joined$heureka_sk_conv.spend_cz

############ Melt
#df_heureka_Zbozi_Spend <- melt(df_heureka_Zbozi_joined[,(c(3,6,8))])
df_heureka_Zbozi_Spend <- melt(df_heureka_Zbozi_joined[,(c(8,10))])

############ Plot it
ggplot(df_heureka_Zbozi_Spend) + 
  geom_col(aes(variable, value), show.legend=F) + 
  ggtitle("Expenditure on Heureka CZ&SK vs. Zbozi", subtitle = "16.Jan 2018 - 3.Mar 2018") + 
  xlab("Marketing Channel") + ylab("Costs") +
  theme(axis.line = element_line(colour = "darkblue", 
                                size = 1, linetype = "solid")) +
  scale_x_discrete(labels=c("Zbozi", "Heureka CZ/SK"))

######################
### For Month/week
colnames(df_heureka_Zbozi_joined)
df_heureka_Zbozi_joined$week <- strftime(df_heureka_Zbozi_joined$heureka_sk_conv.date, 
                                         format = "%V")
df_heureka_Zbozi_joined$heureka_sk_conv.date <- as.Date(df_heureka_Zbozi_joined$heureka_sk_conv.date)

dfs <- df_heureka_Zbozi_joined[,c(1,8,10)]
  
df_s2 <- gather(dfs, key = variable, 
                value = measurement, 
                -heureka_sk_conv.date)

ggplot(df_s2, aes(as.Date(heureka_sk_conv.date, format = "%b-%Y"), measurement, colour = variable)) + 
  geom_line() +
  ggtitle("Expenditure on Heureka CZ & SK vs. Zbozi", subtitle = "16.Jan 2018 - 3.Apr 2018") + 
  xlab("Month") + ylab("Costs in (CZK)") + 
  scale_y_continuous(breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) +
  scale_color_hue(name="Marketing Channel",
                  labels=c("Heureka CZ & SK", "Zbozi.cz"))


##################

dfs <- df_heureka_Zbozi_joined[,c(1,8,10)]

df_s3 <- gather(dfs, key = variable, 
                value = measurement, 
                -heureka_sk_conv.date)


ggplot(df_s3, aes(as.Date(heureka_sk_conv.date, format = "%b-%Y"), measurement, colour = variable)) + 
  geom_line() +
  ggtitle("Expenditure on Heureka CZ & SK vs. Zbozi", subtitle = "16.Jan 2018 - 3.Apr 2018") + 
  xlab("Month") + ylab("Costs in (CZK)") + 
  scale_y_continuous(breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) +
  scale_color_hue(name="Marketing Channel",
                  labels=c("Heureka CZ & SK", "Zbozi.cz"))
