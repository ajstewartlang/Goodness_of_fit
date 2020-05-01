library(tidyverse)
library(psych)
library(rstudioapi)

##########################
## EXPLANATORY GOODNESS ##
##########################

data <- read_csv("Explanation_QUALITY.csv")
data <- data[3:nrow(data),]
nrow(data) # 157

dat0 <- filter(data, Finished == 1)
nrow(dat0) # 142
dat <- filter(dat0, check == "7" & serious == "1" & UserLanguage == "EN")
nrow(dat) # 134

# time (in secs) spent on survey
describe(as.numeric(as.data.frame(dat)[,6]))

# gender
table(dat$Gender)

# age
describe(as.numeric(dat$Age))

# mean responses
res <- sapply(23:58, function(x) mean(as.numeric(as.data.frame(dat)[,x]), na.rm = T))

# in the survey, we interchanged strong and weak explanations in the two groups
strong <- res[c(seq(1, 17, 2), seq(20, 36, 2))]
weak <- res[c(seq(2, 18, 2), seq(19, 35, 2))]

results <- c(strong, weak)

###########
## TRUTH ##
###########

dataT <- read_csv("Explanation_TRUTH.csv")
dataT <- dataT[3:nrow(dataT),]
nrow(dataT) # 158

dat0T <- filter(dataT, Finished == 1)
nrow(dat0T) # 144
datT <- filter(dat0T, check == "7" & serious == "1" & UserLanguage == "EN")
nrow(datT) # 138

# time (in secs) spent on survey
describe(as.numeric(as.data.frame(datT)[,6]))

# gender
table(datT$Gender)

# age
describe(as.numeric(datT$Age))

# mean responses
resT <- sapply(23:58, function(x) mean(as.numeric(as.data.frame(datT)[,x]), na.rm = T))

# in the survey, we interchanged strong and weak explanations in the two groups
strongT <- resT[c(seq(1, 17, 2), seq(20, 36, 2))]
weakT <- resT[c(seq(2, 18, 2), seq(19, 35, 2))]

resultsT <- c(strongT, weakT)

#################
## CORRELATION ##
#################

cor.test(results, resultsT)

# Andrew's code below ####
# Explanatory goodness

summary <- dat %>% 
  pivot_longer(cols = colnames(dat)[23:58], names_to = "Item", values_to = "Score") %>%
  select(Item, Score) %>%
  mutate(Item = factor(Item),
         Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  group_by(Item) %>%
  summarise(mean_score = mean(Score)) 

summary$Item <- str_replace(summary$Item, "A_", "_A-")
summary$Item <- str_replace(summary$Item, "B_", "_B-")
  
summary <- summary %>%
  separate(col = Item, into = c("Item", "Condition"), sep = "_") 

summary$Condition <- str_replace(summary$Condition, "-acceptance", "")

# Read in and combine with eye-tracking data
  
#Truth


