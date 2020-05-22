library(tidyverse)
library(psych)
library(lme4)
library(performance)

##########################
## EXPLANATORY GOODNESS ##
##########################

dat <- read_csv("questionnaire_data/Explanation_QUALITY.csv")
dat <- dat[3:nrow(dat),]
nrow(dat) # 157

dat0 <- filter(dat, Finished == 1)
nrow(dat0) # 142
dat <- filter(dat0, check == "7" & serious == "1" & UserLanguage == "EN" & (check_7_TEXT == "I read the instructions" | check_7_TEXT == "\"I read the instructions\""))
nrow(dat) # 126

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

#################
## PROBABILITY ##
#################

dataP <- read_csv("questionnaire_data/Explanation_PROB.csv")
dataP <- dataP[3:nrow(dataP),]
nrow(dataP) # 157

dat0P <- filter(dataP, Finished == 1)
nrow(dat0P) # 143
datP <- filter(dat0P, check == "7" & serious == "1" & UserLanguage == "EN" & (check_7_TEXT == "I read the instructions" | check_7_TEXT == "\"I read the instructions\""))
nrow(datP) # 130

# time (in secs) spent on survey
describe(as.numeric(as.data.frame(datP)[,6]))

# gender
table(datP$Gender)

# age
describe(as.numeric(datP$Age))

# mean responses
resP <- sapply(23:58, function(x) mean(as.numeric(as.data.frame(datP)[,x]), na.rm = T))

# in the survey, we interchanged strong and weak explanations in the two groups
strongP <- resP[c(seq(1, 17, 2), seq(20, 36, 2))]
weakP <- resP[c(seq(2, 18, 2), seq(19, 35, 2))]

resultsP <- c(strongP, weakP)

#################
## CORRELATION ##
#################

cor.test(results, resultsP)

# Andrew's script below
# Andrew's code below
# Explanation quality ####

summary <- dat %>% 
  pivot_longer(cols = colnames(dat)[23:58], names_to = "Item", values_to = "Score") %>%
  select(Item, Score) %>%
  mutate(Item = factor(Item),
         Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  group_by(Item) %>%
  summarise(mean_quality = mean(Score)) 

summary$Item <- str_replace(summary$Item, "A_", "_A-")
summary$Item <- str_replace(summary$Item, "B_", "_B-")

summary <- summary %>%
  separate(col = Item, into = c("Item", "Condition"), sep = "_") 

summary$Condition <- str_replace(summary$Condition, "A-acceptance", "High")
summary$Condition <- str_replace(summary$Condition, "B-acceptance", "Low")

ready_to_combine <- summary %>%
  unite(col = "Item_Condition", c(Item, Condition), sep="_")

# Read in and combine with eye-tracking data

# First pass
first_pass <- read_csv("eye_data/FPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_fp <- left_join(first_pass, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_quality + (1 | P.s), data = joined_data_fp)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_quality + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_quality + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_spillover)
#check_model(model_spillover)

# Regression path
regression_path <- read_csv("eye_data/RPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_rp <- left_join(regression_path , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_quality + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_quality + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_quality + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_spillover)
#check_model(model_spillover)

# Total time
total_time <- read_csv("eye_data/TTs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_tt <- left_join(total_time , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_quality + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_quality + (1 | P.s) , data = joined_data_tt)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_quality + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_spillover)
#check_model(model_spillover)

# Regressions out
regressions_out <- read_csv("eye_data/ROs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_ro <- left_join(regressions_out, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- glmer(Antecedent ~ mean_quality + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_antecedent)

model_consequent <- glmer(Consequent ~ mean_quality + (1 | P.s), family = binomial, data = joined_data_ro)
summary(model_consequent)

model_spillover <- glmer(spillover ~ mean_quality + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_spillover)

joined_data_ro %>%
  ggplot(aes(x = spillover, y = mean_quality))+
  geom_smooth(method = "lm")

#Explanation Probability Main ####

summary <- dataP %>% 
  pivot_longer(cols = colnames(dataP)[23:58], names_to = "Item", values_to = "Score") %>%
  select(Item, Score) %>%
  mutate(Item = factor(Item),
         Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  group_by(Item) %>%
  summarise(mean_probability = mean(Score)) 

summary$Item <- str_replace(summary$Item, "A_", "_A-")
summary$Item <- str_replace(summary$Item, "B_", "_B-")

summary <- summary %>%
  separate(col = Item, into = c("Item", "Condition"), sep = "_") 

summary$Condition <- str_replace(summary$Condition, "A-acceptance", "High")
summary$Condition <- str_replace(summary$Condition, "B-acceptance", "Low")

ready_to_combine <- summary %>%
  unite(col = "Item_Condition", c(Item, Condition), sep="_")

# Read in and combine with eye-tracking data

# First pass
first_pass <- read_csv("eye_data/FPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_fp <- left_join(first_pass, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_spillover)
#check_model(model_spillover)

# Regression path
regression_path <- read_csv("eye_data/RPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_rp <- left_join(regression_path , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_spillover)
#check_model(model_spillover)

# Total time
total_time <- read_csv("eye_data/TTs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_tt <- left_join(total_time , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_probability + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_spillover)
#check_model(model_spillover)

# Regressions out
regressions_out <- read_csv("eye_data/ROs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_ro <- left_join(regressions_out, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- glmer(Antecedent ~ mean_probability + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_antecedent)

model_consequent <- glmer(Consequent ~ mean_probability + (1 | P.s), family = binomial, data = joined_data_ro)
summary(model_consequent)

joined_data_ro %>%
  ggplot(aes(x = Consequent, y = mean_probability)) +
  geom_smooth(method = "lm")

model_spillover <- glmer(spillover ~ mean_probability + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_spillover)

# Combine the questionnaire data ####

# Explanation quality
summary <- dat %>% 
  pivot_longer(cols = colnames(dat)[23:58], names_to = "Item", values_to = "Score") %>%
  select(Item, Score) %>%
  mutate(Item = factor(Item),
         Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  group_by(Item) %>%
  summarise(mean_quality = mean(Score)) 

summary$Item <- str_replace(summary$Item, "A_", "_A-")
summary$Item <- str_replace(summary$Item, "B_", "_B-")

summary <- summary %>%
  separate(col = Item, into = c("Item", "Condition"), sep = "_") 

summary$Condition <- str_replace(summary$Condition, "A-acceptance", "High")
summary$Condition <- str_replace(summary$Condition, "B-acceptance", "Low")

ready_to_combine_quality <- summary %>%
  unite(col = "Item_Condition", c(Item, Condition), sep="_")

# Probability

summary <- dataP %>% 
  pivot_longer(cols = colnames(dataP)[23:58], names_to = "Item", values_to = "Score") %>%
  select(Item, Score) %>%
  mutate(Item = factor(Item),
         Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  group_by(Item) %>%
  summarise(mean_probability = mean(Score)) 

summary$Item <- str_replace(summary$Item, "A_", "_A-")
summary$Item <- str_replace(summary$Item, "B_", "_B-")

summary <- summary %>%
  separate(col = Item, into = c("Item", "Condition"), sep = "_") 

summary$Condition <- str_replace(summary$Condition, "A-acceptance", "High")
summary$Condition <- str_replace(summary$Condition, "B-acceptance", "Low")

ready_to_combine_probability <- summary %>%
  unite(col = "Item_Condition", c(Item, Condition), sep="_")

ready_to_combine <- left_join(ready_to_combine_quality, ready_to_combine_probability, by = "Item_Condition")

# Read in and combine with eye-tracking data - both predictors####

# First pass
first_pass <- read_csv("eye_data/FPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_fp <- left_join(first_pass, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_spillover)
#check_model(model_spillover)

# Regression path
regression_path <- read_csv("eye_data/RPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_rp <- left_join(regression_path , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_spillover)
#check_model(model_spillover)

# Total time
total_time <- read_csv("eye_data/TTs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_tt <- left_join(total_time , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_antecedent)
#check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_consequent)
#check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_spillover)
#check_model(model_spillover)

# Regressions out
regressions_out <- read_csv("eye_data/ROs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_ro <- left_join(regressions_out, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- glmer(Antecedent ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_antecedent)

model_consequent <- glmer(Consequent ~ mean_quality + mean_probability + (1 | P.s), family = binomial, data = joined_data_ro)
summary(model_consequent)

model_spillover <- glmer(spillover ~ mean_quality + mean_probability + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_spillover)
