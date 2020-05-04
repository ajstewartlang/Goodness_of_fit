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

model_antecedent <- lmer(Antecedent ~ mean_goodness + (1 | P.s), data = joined_data_fp)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_spillover)
check_model(model_spillover)

# Regression path
regression_path <- read_csv("eye_data/RPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_rp <- left_join(regression_path , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_spillover)
check_model(model_spillover)

# Total time
total_time <- read_csv("eye_data/TTs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_tt <- left_join(total_time , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_goodness + (1 | P.s) , data = joined_data_tt)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_goodness + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_spillover)
check_model(model_spillover)

# Regressions out
regressions_out <- read_csv("eye_data/ROs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_ro <- left_join(regressions_out, ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- glmer(Antecedent ~ mean_goodness + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_antecedent)

model_consequent <- glmer(Consequent ~ mean_goodness + (1 | P.s), family = binomial, data = joined_data_ro)
summary(model_consequent)

model_spillover <- glmer(spillover ~ mean_goodness + (1 | P.s) + (1 | Item), family = binomial, data = joined_data_ro)
summary(model_spillover)

joined_data_ro %>%
  ggplot(aes(x = spillover, y = mean_goodness))+
  geom_smooth(method = "lm")

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

model_antecedent <- lmer(Antecedent ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_fp)
summary(model_spillover)
check_model(model_spillover)

# Regression path
regression_path <- read_csv("eye_data/RPs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_rp <- left_join(regression_path , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_rp)
summary(model_spillover)
check_model(model_spillover)

# Total time
total_time <- read_csv("eye_data/TTs.csv") %>%
  unite(col = "Item_Condition", c(Item, Fit), sep="_")

# Join the rating and eye-tracking data
joined_data_tt <- left_join(total_time , ready_to_combine, by = "Item_Condition") %>%
  separate(col = "Item_Condition", into = (c("Item", "Condition")), sep = "_") %>%
  mutate(Condition = factor(Condition))

model_antecedent <- lmer(Antecedent ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_antecedent)
check_model(model_antecedent)

model_consequent<- lmer(Consequent ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_tt)
summary(model_consequent)
check_model(model_consequent)

model_spillover <- lmer(spillover ~ mean_truth + (1 | P.s) + (1 | Item), data = joined_data_tt)
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


