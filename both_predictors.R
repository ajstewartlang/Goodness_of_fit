library(tidyverse)
library(psych)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

# Explanatory Goodness Start####

my_data <- read_csv("questionnaire_data/Explanation_QUALITY.csv")
my_data <- my_data[3:nrow(my_data),]
nrow(my_data) # 157

dat0 <- filter(my_data, Finished == 1)
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

# Andrew's code below
# Explanatory Goodness Main####

summary <- dat %>% 
  pivot_longer(cols = colnames(dat)[23:58], names_to = "Item", values_to = "Score") %>%
  select(Item, Score) %>%
  mutate(Item = factor(Item),
         Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  group_by(Item) %>%
  summarise(mean_goodness = mean(Score)) 

summary$Item <- str_replace(summary$Item, "A_", "_A-")
summary$Item <- str_replace(summary$Item, "B_", "_B-")

summary <- summary %>%
  separate(col = Item, into = c("Item", "Condition"), sep = "_") 

summary$Condition <- str_replace(summary$Condition, "A-acceptance", "High")
summary$Condition <- str_replace(summary$Condition, "B-acceptance", "Low")

ready_to_combine_goodness <- summary %>%
  unite(col = "Item_Condition", c(Item, Condition), sep="_")

# Truth Start ####

dataT <- read_csv("questionnaire_data/Explanation_TRUTH.csv")
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

#Truth Main ####

summary <- datT %>% 
  pivot_longer(cols = colnames(datT)[23:58], names_to = "Item", values_to = "Score") %>%
  select(Item, Score) %>%
  mutate(Item = factor(Item),
         Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  group_by(Item) %>%
  summarise(mean_truth = mean(Score)) 

summary$Item <- str_replace(summary$Item, "A_", "_A-")
summary$Item <- str_replace(summary$Item, "B_", "_B-")

summary <- summary %>%
  separate(col = Item, into = c("Item", "Condition"), sep = "_") 

summary$Condition <- str_replace(summary$Condition, "A-acceptance", "High")
summary$Condition <- str_replace(summary$Condition, "B-acceptance", "Low")

ready_to_combine_truth <- summary %>%
  unite(col = "Item_Condition", c(Item, Condition), sep="_")

ready_to_combine <- left_join(ready_to_combine_goodness, ready_to_combine_truth, by = "Item_Condition")

# Read in and combine with eye-tracking data

# First pass
first_pass <- read_csv("eye_data/FPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_fp <- left_join(first_pass, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_spillover)
check_model(model_spillover)

# Regression path
regression_path <- read_csv("eye_data/RPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_rp <- left_join(regression_path , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_spillover)
check_model(model_spillover)

# Total time
total_time <- read_csv("eye_data/TTs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_tt <- left_join(total_time , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_truth + mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_spillover)
check_model(model_spillover)

# Regressions out
regressions_out <- read_csv("eye_data/ROs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_ro <- left_join(regressions_out, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- glmer(Antecedent ~ mean_truth + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_antecedent)

model_consequent <- glmer(Consequent ~ mean_truth + (1 | P.s), family = binomial, data = joined_data_ro)
summary(model_consequent)

joined_data_ro %>%
  ggplot(aes(x = Consequent, y = mean_truth)) +
  geom_smooth(method = "lm")

model_spillover <- glmer(spillover ~ mean_truth + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_spillover)

# Aggregating data as suggested by Igor

first_pass <- read_csv("eye_data/FPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_") %>%
  rename(Antecedent_FP = Antecedent,
         Consequent_FP = Consequent,
         Spillover_FP = spillover) %>%
  group_by(Item_Condition) %>%
  summarise(Antecedent_FP_mean = mean(Antecedent_FP, na.rm = TRUE), 
            Consequent_FP_mean = mean(Consequent_FP, na.rm = TRUE),
            Spillover_FP_mean = mean(Spillover_FP, na.rm = TRUE))

regression_path <- read_csv("eye_data/RPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_") %>%
  rename(Antecedent_RP = Antecedent,
         Consequent_RP = Consequent,
         Spillover_RP = spillover) %>%
  group_by(Item_Condition) %>%
  summarise(Antecedent_RP_mean = mean(Antecedent_RP, na.rm = TRUE),
            Consequent_RP_mean = mean(Consequent_RP, na.rm = TRUE),
            Spillover_RP_mean = mean(Spillover_RP, na.rm = TRUE))

total_time <- read_csv("eye_data/TTs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_") %>%
  rename(Antecedent_TT = Antecedent,
         Consequent_TT = Consequent,
         Spillover_TT = spillover) %>%
  group_by(Item_Condition) %>%
  summarise(Antecedent_TT_mean = mean(Antecedent_TT, na.rm = TRUE),
            Consequent_TT_mean = mean(Consequent_TT, na.rm = TRUE),
            Spillover_TT_mean = mean(Spillover_TT, na.rm = TRUE))

regressions_out <- read_csv("eye_data/ROs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_") %>%
  rename(Antecedent_RO = Antecedent,
         Consequent_RO = Consequent,
         Spillover_RO = spillover) %>%
  group_by(Item_Condition) %>%
  summarise(Antecedent_RO_mean = mean(Antecedent_RO, na.rm = TRUE),
            Consequent_RO_mean = mean(Consequent_RO, na.rm = TRUE),
            Spillover_RO_mean = mean(Spillover_RO, na.rm = TRUE))

all_rt_data <- left_join(first_pass, regression_path, by = "Item_Condition") %>%
  left_join(total_time, by = "Item_Condition") %>%
  left_join(ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = c("Item", "Condition"), sep = "_")

# write_csv(all_rt_data, "old/aggregated.csv")

all_rt_data

model_lm_ant_tt <- lm(Antecedent_TT_mean ~ scale(mean_goodness) + scale(mean_truth), 
                       data = all_rt_data)
summary(model_lm_ant_tt)

model_lm_cons_fp <- lm(Consequent_FP_mean ~ scale(mean_goodness) + scale(mean_truth), 
                       data = all_rt_data)
summary(model_lm_cons_fp)

model_lm_cons_rp <- lm(Consequent_RP_mean ~ scale(mean_goodness) + scale(mean_truth), 
                       data = all_rt_data)
summary(model_lm_cons_rp)

model_lm_cons_tt <- lm(Consequent_TT_mean ~ scale(mean_goodness) + scale(mean_truth), 
                       data = all_rt_data)
summary(model_lm_cons_tt)

model_lm_spillover_fp <- lm(Spillover_FP_mean ~ scale(mean_goodness) + scale(mean_truth), 
                       data = all_rt_data)
summary(model_lm_spillover_fp)

model_lm_spillover_rp <- lm(Spillover_RP_mean ~ scale(mean_goodness) + scale(mean_truth), 
                       data = all_rt_data)
summary(model_lm_spillover_rp)

model_lm_spillover_tt <- lm(Spillover_TT_mean ~ scale(mean_goodness) + scale(mean_truth), 
                       data = all_rt_data)
summary(model_lm_spillover_tt)
