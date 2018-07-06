#Slots Forecast
library(tidyverse)
library(lubridate)
slots.raw <- readxl::read_excel("data/Lucky Duck Entertainment revenue 2013.xls", na = ".")

str(slots.raw)
funModeling::df_status(slots.raw)



VAR_NUMERIC <- c("GrossRevenue", "FcstLower", "FcstUpper", "NoMachines", "Plays", "GrossRevenuePerMachine", "PlaysPerMachine", "Diff_w_Upper", "Diff_w_Lower")

library(corrplot)
corrplot.mixed(cor(slots.raw[,VAR_NUMERIC]))


models.cat <- slots.raw %>% 
  group_by(Month, Casino, Section, MachineType, Denomination, Model) %>% 
  summarise(GrossRevenue = sum(GrossRevenue),
            NoMachines = sum(NoMachines),
            Plays = sum(Plays),
            PlaysPerMachine = Plays / NoMachines,
            RevenuePerPlay = GrossRevenue / Plays) 

models.cat$Denomination <- as.ordered(models.cat$Denomination)
head(models.cat)


#
ggplot(aes(x = Denomination, y = RevenuePerPlay), data = models.cat) +
  geom_boxplot()
#Strange to have much more revenue 
models.cat[which(models.cat$RevenuePerPlay > 100),] #WOW, few plays big lose
slots.raw[which(slots.raw$Plays < 200 & slots.raw$GrossRevenue > 15000),] %>% View() #same slots

models.cat %>% filter(RevenuePerPlay < 100) %>%
  ggplot(aes(x = Denomination, y = log(RevenuePerPlay))) +
  geom_boxplot()
models.cat %>% filter(RevenuePerPlay < 100) %>%
  ggplot() +
  geom_boxplot(aes(x = Casino, y = RevenuePerPlay)) +
  facet_wrap(~Denomination)

#How many unique models exist of each slot?
unique.models <- models.cat %>% group_by(MachineType, Denomination) %>%
  summarise(Unique_models = length(unique(Model)))



#c'e' difference nei ricavi nei vari mesi per modello?
par(mfrow=c(2, 1))
ggplot(models.cat) +
  geom_boxplot(aes(x = Month, y = GrossRevenue/NoMachines, group = Month))

#per quanto riguarda i ricavi totali?
ggplot(models.cat) +
  geom_boxplot(aes(x = Month, y = log(GrossRevenue), group = Month)) +
  ylab("Log of total revenue by model")
par(mfrow=c(1, 1))

corrplot.mixed(cor(models.cat[, c("GrossRevenue","Plays", "NoMachines")]))

idx.train <- which(models.cat$Month < ymd("2012-07-01"))
train <- models.cat[idx.train, ]
test <- models.cat[-idx.train, ]


