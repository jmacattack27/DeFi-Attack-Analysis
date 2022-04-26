#### Preamble ####
# Purpose: Clean the data downloaded from DEFIYIELD's REKT database
# Author: Jack McKay
# Data: April 25 2022
# Contact: jack.mckay@utoronto.ca
# License: MIT


#### Workspace setup ####
# Use R Projects, not setwd().
library(tidyverse)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyselect)
library(reshape2)
library("viridis")
library(kableExtra)


# Read in the raw data. 

raw_data <- read.csv (here::here("inputs/data/DeFiAttackData.csv"))

# Basic Data Cleaning

raw_data <- subset(raw_data, select = -c(Title:Date)) 
raw_data <- subset(raw_data, select = -c(Image1, Label, column))

raw_data <-
  raw_data %>% 
  rename(project_url = Name_URL,
         project = Name,
         chain = Info2,
         type_of_issue = Inf1,
         funds_lost = Price1,
         date = Date1)

cleaned_data <-
  raw_data %>% 
  slice_head(n = 200)

cleaned_data[1, 3] = 'RONIN'
cleaned_data$funds_lost = as.numeric(gsub("[\\$,]", "", cleaned_data$funds_lost))



# First creating a new data frame recording how many times each chain was exploited. 

chain_prop <- as.data.frame(table(cleaned_data$chain))[-c(1, 4, 7, 8), ]
chain_prop <- rename(chain_prop, Chain = Var1, Number.of.Exploits = Freq)

# Note that some of these exploits occurred on multiple chains. These entries are manually accounted for below

chain_prop[3, 2] = 57 + 4
chain_prop[5, 2] = 96 + 5
chain_prop[11, 2] = 7 + 3

chain_prop <- add_row(chain_prop, Chain = 'CRONOS', Number.of.Exploits = 1)

# Adding a column for the total number of recorded exploits on each chain

chain_prop$total_exploits <- c(1, 6, 1668, 2, 944, 16, 1, 3, 1, 1, 94, 1, 8, 1, 3)



# Now we can visualize the data

chain_prop %>% 
  ggplot(aes(x=Chain, y=Number.of.Exploits)) +
  geom_bar(stat='identity', fill='darkblue') +
  labs(x = 'Chain',
       y = 'Number of Exploits',
       title = "Number of Exploits per Chain") +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(plot.title = element_text(hjust = 0.5))



# Creating a new dataframe in a format such that we can make a stacked barchart

new_prop2 <- data.frame(chain_prop$Number.of.Exploits, c('ALGO', 'AVAX', 'BSC', 'EOS', 'ETH', 'FANTOM', 'HARMONY', 'HECO', 'MOVR', 'POLKADOT', 'POLYGON', 'RONIN', 'SOLANA', 'TRON', 'CRONOS'))
new_prop2$ExploitCategory <- rep('Top 200 Costliest Attacks', 15)

reprows <- new_prop2[1:15,]
new_prop2 <- rbind(new_prop2, reprows)
new_prop2[16:30, 3] <- rep('All Other Attacks')
new_prop2[16:30, 1] <- c(0, 2, 1607, 0, 843, 11, 0, 1, 0, 0, 84, 0, 2, 0, 2)
colnames(new_prop2) <- c('Number of Attacks', 'Chain', 'Exploit Category')



# Visualizing the data
ggplot(new_prop2, aes(fill=`Exploit Category`, y=`Number of Attacks`, x=Chain)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Total Attack Distribution Across Chains, Categorized by Funds Lost") +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(plot.title = element_text(hjust = 0.5))



# Adding a new column to our chain_prop data set containing the total value locked in each chain

chain_prop$TVL <- c(0.173, 10.57, 13.42, 0.255, 115.45, 5.21, 0.602, 0.343, 0.261, 0.004, 4.03, 0.218, 6.5, 4.2, 3.84)
chain_prop <- mutate(chain_prop, adjusted.exploits = Number.of.Exploits / TVL)

# We create a new data frame without data on Polkadot, to make the results more interpretable
chain_prop1 <- chain_prop[-c(10),]

# Our first graph, including data on Polkadot
x <- ggplot(data=chain_prop, aes(x=Chain, y=adjusted.exploits)) +
  geom_bar(stat='identity', fill='darkblue') +
  labs(x = 'Chain',
       y = 'Ratio of Exploits to TVL',
       title = "Successful Attacks Adjusted for TVL (Including Polkadot)") +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(plot.title = element_text(hjust = 0.5))

# Our scond graph, excluding data on Polkadot
y <- ggplot(data=chain_prop1, aes(x=Chain, y=adjusted.exploits)) +
  geom_bar(stat='identity', fill='darkblue') +
  labs(x = 'Chain',
       y = 'Ratio of Exploits to TVL',
       title = "Successful Attacks Adjusted for TVL (Excluding Polkadot)") +
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(x, y, nrow = 2)



# Creating a new data set containing the number of attacks of each type and the net funds lost via each attack

attack_prop <- as.data.frame(table(cleaned_data$type_of_issue))

# Summing the total funds lost via each type of attack
exploit_funds <- sum(cleaned_data[cleaned_data$type_of_issue=="Exploit",]$funds_lost)
abandoned_funds <- sum(cleaned_data[cleaned_data$type_of_issue=="Abandoned",]$funds_lost)
exit_scam_funds <- sum(cleaned_data[cleaned_data$type_of_issue=="Exit Scam",]$funds_lost)
flash_loan_funds <- sum(cleaned_data[cleaned_data$type_of_issue=="Flash loan",]$funds_lost)
access_control_funds <- sum(cleaned_data[cleaned_data$type_of_issue=="Access control",]$funds_lost)
honeypot_funds <- sum(cleaned_data[cleaned_data$type_of_issue=="Honeypot",]$funds_lost)

attack_prop$total_funds_lost <- c(abandoned_funds, access_control_funds, exit_scam_funds, exploit_funds, flash_loan_funds, honeypot_funds)
attack_prop <- mutate(attack_prop, avg_funds_lost = total_funds_lost / Freq)



atk_freq <- ggplot(data=attack_prop, aes(x=Var1, y=Freq)) +
  geom_bar(stat='identity', fill='darkgreen') +
  labs(x = 'Type of Attack',
       y = 'Number of Attacks',
       title = "Number of Attacks") +
  theme(plot.title = element_text(hjust = 0.5))

atk_net_dmg <- ggplot(data=attack_prop, aes(x=Var1, y=total_funds_lost)) +
  geom_bar(stat='identity', fill='darkred') +
  labs(x = 'Type of Attack',
       y = 'Total Funds Lost',
       title = "Total Funds Lost by Attack") +
  theme(plot.title = element_text(hjust = 0.5))

atk_avg_dmg <- ggplot(data=attack_prop, aes(x=Var1, y=avg_funds_lost)) +
  geom_bar(stat='identity', fill='purple') +
  labs(x = 'Type of Attack',
       y = 'Avg Funds Lost per Attack',
       title = "Average Funds Lost per Attack") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(atk_freq, atk_net_dmg, atk_avg_dmg, ncol = 1)



# Adding a column recording the total number of recorded attacks of each type
attack_prop$all_attacks<- c(14, 97, 623, 180, 42, 1907)

# Graphing this distribution
net_atk_freq <- ggplot(data=attack_prop, aes(x=Var1, y=all_attacks)) +
  geom_bar(stat='identity', fill='maroon2') +
  labs(x = 'Type of Attack',
       y = 'Number of Recorded Attacks',
       title = "Total Number of Recorded Attacks of Each Type") +
  theme(plot.title = element_text(hjust = 0.5))

net_atk_freq



# Adding a column recording the likelihood that a given attack will be one of the 200 costliest
attack_prop <- mutate(attack_prop, likelihood = 100 * round(Freq / all_attacks, digits=3))
attack_prop$likelihood <- paste0(attack_prop$likelihood, '%')


attack_prop %>% 
  select(Var1, Freq, total_funds_lost, avg_funds_lost, all_attacks, likelihood) %>% 
  kable(
    caption = "Key Data on Attacks",
    col.names = c("Type of Attack", "Attacks (Top 200)", "Net Funds Lost\n(Top 200)", "Average Funds Lost (Top 200)", "Total Recorded Attacks", "Likelihood"),
    digits = 2,
    booktabs = TRUE,
    linesep = ""
  ) %>% 
  column_spec(3:5, width = "3cm")



# Data wrangling in order to get a dataset containing the number of attacks of each time per year since 2016
attack_time <- cleaned_data[, c("type_of_issue", "date")]

attacks2022 <- attack_time[grep("2022$", attack_time$date), ]
attacks2021 <- attack_time[grep("2021$", attack_time$date), ]
attacks2020 <- attack_time[grep("2020$", attack_time$date), ]
attacks2019 <- attack_time[grep("2019$", attack_time$date), ]
attacks2018 <- attack_time[grep("2018$", attack_time$date), ]
attacks2017 <- attack_time[grep("2017$", attack_time$date), ]
attacks2016 <- attack_time[grep("2016$", attack_time$date), ]
attacks2015 <- attack_time[grep("2015$", attack_time$date), ]
attacks2014 <- attack_time[grep("2014$", attack_time$date), ]
attacks2013 <- attack_time[grep("2013$", attack_time$date), ]


attacktype2022 <- as.data.frame(table(attacks2022$type_of_issue))
attacktype2021 <- as.data.frame(table(attacks2021$type_of_issue))
attacktype2020 <- as.data.frame(table(attacks2020$type_of_issue))
attacktype2019 <- as.data.frame(table(attacks2019$type_of_issue))
attacktype2018 <- as.data.frame(table(attacks2018$type_of_issue))
attacktype2017 <- as.data.frame(table(attacks2017$type_of_issue))
attacktype2016 <- as.data.frame(table(attacks2016$type_of_issue))
attacktype2015 <- as.data.frame(table(attacks2015$type_of_issue))
attacktype2014 <- as.data.frame(table(attacks2014$type_of_issue))
attacktype2013 <- as.data.frame(table(attacks2013$type_of_issue))


attackyr <- attacktype2020
attackyr <- add_row(attackyr, Var1 = "Abandoned", Freq = 0, .before = 0)
attackyr <- add_row(attackyr, Var1 = "Honeypot", Freq = 0)
attackyr$`2021` <- attacktype2021$Freq
attackyr$`2022` <- c(0, 6, 11, 18, 4, 0)
attackyr <- rename(attackyr, type_of_attack = Var1,
                   `2020` = Freq)
attackyr$`2016` <- c(0, 0, 0, 1, 0, 0)
attackyr$`2017` <- c(0, 0, 0, 0, 0, 0)
attackyr$`2018` <- c(0, 0, 0, 1, 0, 0)
attackyr$`2019` <- c(0, 1, 0, 0, 0, 0)
attackyr <- attackyr[, c(1, 5, 6, 7, 8, 2, 3, 4)]


thing <- attackyr[, -c(1)]
thing2 <- as.data.frame(t(thing))
thing2 <- rename(thing2,
                 Abandoned = V1,
                 `Access control` = V2,
                 `Exit Scam` = V3,
                 Exploit = V4,
                 `Flash loan` = V5,
                 Honeypot = V6)
thing2$Date <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022)
df <- melt(thing2 ,  id.vars = 'Date', variable.name = 'Type of Attack')



ggplot(df, aes(Date, value)) +
  geom_line(aes(colour = `Type of Attack`)) +
  labs(x = 'Year',
       y = 'Number of Attacks',
       title = 'Types of Attacks by Year') +
  geom_point(aes(colour = `Type of Attack`)) +
  theme(plot.title = element_text(hjust = 0.5))



eth_data <- cleaned_data[grep("ETH", cleaned_data$chain), ]
bsc_data <- cleaned_data[grep("BSC", cleaned_data$chain), ]
poly_data <- cleaned_data[grep("POLYGON", cleaned_data$chain), ]
sol_data <- cleaned_data[grep("SOLANA", cleaned_data$chain), ]


eth_prop <- as.data.frame(table(eth_data$type_of_issue))
bsc_prop <- as.data.frame(table(bsc_data$type_of_issue))
poly_prop <- as.data.frame(table(poly_data$type_of_issue))
sol_prop <- as.data.frame(table(sol_data$type_of_issue))

bsc_prop <- add_row(bsc_prop, Var1 = "Abandoned", Freq = 0, .before = 0)
bsc_prop <- add_row(bsc_prop, Var1 = "Honeypot", Freq = 0)

poly_prop <- add_row(poly_prop, Var1 = "Access control", Freq = 0, .before = 0)
poly_prop <- add_row(poly_prop, Var1 = "Abandoned", Freq = 0, .before = 0)
poly_prop <- add_row(poly_prop, Var1 = "Honeypot", Freq = 0)

sol_prop <- add_row(sol_prop, Var1 = "Access control", Freq = 0, .before = 0)
sol_prop <- add_row(sol_prop, Var1 = "Abandoned", Freq = 0, .before = 0)
sol_prop <- add_row(sol_prop, Var1 = "Flash loan", Freq = 0)
sol_prop <- add_row(sol_prop, Var1 = "Honeypot", Freq = 0)

chainattack <- data.frame(eth_prop$Freq, bsc_prop$Freq, poly_prop$Freq, sol_prop$Freq)
colnames(chainattack) <- c("ETH", "BSC", "Polygon", "Solana")
rownames(chainattack) <- c("Abandoned", "Access control", "Exit Scam", "Exploit", "Flash loan", "Honeypot")



# Creating a stacked bar chart breaking the destribution of attacks on each chain down by type of attack
barplot(as.matrix(chainattack), 
        col=viridis(6), 
        border="white", 
        space=0.04, 
        font.axis=2, 
        xlab="Chain",
        ylab="Type of Attack",
        main="Proportion of Attacks Across Chains",
        legend=TRUE)























barplot(t(as.matrix(new_prop)),
        col=viridis(2),
        border="white",
        space=0.04,
        font.axis=2,
        xlab="Chain",
        ylab="Number of Attacks",
        main="Distribution of Attacks by Chain",
        legend=TRUE)