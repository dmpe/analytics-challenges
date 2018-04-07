library(tidyverse)
library(ggthemes)
library(reshape2)
library(hms)


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






