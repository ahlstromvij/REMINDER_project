set.seed(1000)

# load packages
library(ggplot2)
library(mirt)
library(polycor)
library(texreg)
library(tidyverse)
library(emmeans)
library(ggpubr)
library(ltm)

# read in data
model_data <- read.csv("data/model_data.csv")

# let's start by building two IRT models

# general knowledge scale
# here  we need to use all 3 items; if not, we won't have enough degrees of freedom
# number of unique response patterns: 4 ([0,0],[1,0],[0,1],[1,1])
# so df = 4 - 1 = 3
# parameters to estimate for two items in a 2PL IRT: 2 intercepts * 2 slopes = 4
# good thing that using all 3 is defensible given the good fit of mod_2f_6_items.fit

# save general knowledge items in separate df
know_items_gen <- data.frame(model_data$gen_know_ep,
                             model_data$gen_know_party,
                             model_data$gen_know_switzerland)

# fit 2PL (two parameter) model
know_scale_gen <- mirt(data=know_items_gen,
                       model=1,
                       itemtype = "2PL")

# have a look at how well the items load (we want > 0.3 in F1 column)
summary(know_scale_gen)

# plot individual item trace lines (steepness shows how well it discriminates)
plot(know_scale_gen, type="trace")

# plot test information (shows where on the scale it discriminates well, with theta = 0 representing mean ability)
plot(know_scale_gen, type="info")

# look at how discriminating the individual items are (ideally want discrimination [i.e., a] values > 1)
coef(know_scale_gen, IRTpars=T)

# save scores to df
model_data$know_score_general <- fscores(know_scale_gen)[,1]

# look at quick summary of scores (e.g., min, max, and mean)
summary(model_data$know_score_general) # mean of 0
sd(model_data$know_score_general) # sd of 0.74

# unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(know_items_gen, cor="tet") # unidimensional

# Q3 for local independence (ideally no higher than +/-0.2, but short scales tend to give higher values)
Q3resid <- data.frame(residuals(know_scale_gen, type="Q3")) # max is -0.411

# evaluate model fit visually
itemfit(know_scale_gen, empirical.plot = 1)
itemfit(know_scale_gen, empirical.plot = 2)
itemfit(know_scale_gen, empirical.plot = 3)

# guessing parameter
know_scale_gen_ltm2 <- ltm(know_items_gen ~ z1, IRT.param = TRUE)
coef(know_scale_gen_ltm2)

know_scale_gen_ltm3 <- tpm(know_items_gen, type="latent.trait", IRT.param = TRUE)
coef(know_scale_gen_ltm3) # Want guessing parameter to be low
max(coef(know_scale_gen_ltm3)[1])

# immigration knowledge scale
know_items_imm <- data.frame(model_data$mig_know_free_move,
                             model_data$mig_know_schengen,
                             model_data$mig_know_syrians)

know_scale_imm <- mirt(data=know_items_imm,
                       model=1,
                       itemtype = "2PL")

summary(know_scale_imm)
plot(know_scale_imm, type="trace")
plot(know_scale_imm, type="info")
coef(know_scale_imm, IRTpars=T)
model_data$know_score_imm <- fscores(know_scale_imm)[,1]
summary(model_data$know_score_imm) # mean of 0
sd(model_data$know_score_imm) # sd of 0.7

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_imm, cor="tet") # unidimensional

Q3resid <- data.frame(residuals(know_scale_imm, type="Q3"))

itemfit(know_scale_imm, empirical.plot = 1)
itemfit(know_scale_imm, empirical.plot = 2)
itemfit(know_scale_imm, empirical.plot = 3)

# guessing parameter
know_scale_imm_ltm2 <- ltm(know_items_imm ~ z1, IRT.param = TRUE)
coef(know_scale_imm_ltm2)

know_scale_imm_ltm3 <- tpm(know_items_imm, type="latent.trait", IRT.param = TRUE)
coef(know_scale_imm_ltm3) # Want guessing parameter to be low
max(coef(know_scale_imm_ltm3)[1])

# construct validity
m_gen <- lm(know_score_general ~
              gender + # men know more
              age + # older know more
              education_ISCED, # more educated know more
            data = model_data)
summary(m_gen)

m_imm <- lm(know_score_imm ~
              gender + # men know more
              age + # older know more
              education_ISCED, # more educated know more
            data = model_data)
summary(m_imm)

# print regression tables
texreg::wordreg(list(m_gen,m_imm),
                file="tables/construct_validity_table.docx",
                single.row = TRUE, 
                caption = "Main effects (OLS)",
                custom.model.names=c("General knowledge",
                                     "Immigration knowledge"),
                custom.coef.names = c("(Intercept)", "Male", "Age", "Education"),
                caption.above = TRUE,
                float.pos = "h!",
                custom.note="%stars.",
                stars = c(0.001, 0.01, 0.05),
                digits=3)

# differences in mean by type of knowledge by demographics
model_data %>% 
  group_by(gender) %>% 
  summarise(general = mean(know_score_general),
            immigration = mean(know_score_imm))

model_data %>% 
  group_by(education_ISCED) %>% 
  summarise(general = mean(know_score_general),
            immigration = mean(know_score_imm))

model_data %>% 
  mutate(age_cat = cut(age, breaks=c(17, 24, 34, 44, 54, 64, Inf), 
                       labels=c("18-24","25-34","35-45","45-54","55-64","65plus"),
                       ordered_result = TRUE)) %>% 
  group_by(age_cat) %>% 
  summarise(general = mean(know_score_general),
            immigration = mean(know_score_imm))

# pivot longer
long_df <- model_data %>% 
  dplyr::select(gender, age, education_ISCED, know_score_imm, know_score_general) %>% 
  mutate(age_cat = cut(age, breaks=c(17, 24, 34, 44, 54, 64, Inf), 
                       labels=c("18-24","25-34","35-45","45-54","55-64","65 plus"),
                       ordered_result = TRUE),
         education_ISCED = factor(education_ISCED),
         gender = str_to_title(gender))  %>% 
  dplyr::select(-age) %>% 
  pivot_longer(cols = -c(gender, age_cat, education_ISCED),
               names_to = "Knowledge",
               values_to = "value") %>% 
  mutate(Knowledge = case_when(Knowledge == "know_score_general" ~ "General",
                               Knowledge == "know_score_imm" ~ "Immigration"))

# plot pooled distributions by demographics
long_df %>% 
  ggplot() +
  aes(x = gender, y = value, fill = gender) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE)

long_df %>% 
  ggplot() +
  aes(x = age_cat, y = value, fill = age_cat) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE)

long_df %>% 
  ggplot() +
  aes(x = education_ISCED, y = value, fill = education_ISCED) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE)

# so, there are differences by demographics
# to what extent might they be explainable by the two types of knowledge?
dist_gender <- long_df %>% 
  ggplot() +
  aes(x = gender, y = value, fill = Knowledge) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE) +
  xlab("Gender") +
  ylab("Knowledge score") + 
  scale_fill_grey()

dist_age <- long_df %>% 
  ggplot() +
  aes(x = age_cat, y = value, fill = Knowledge) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE) +
  xlab("Age") +
  ylab("Knowledge score") + 
  scale_fill_grey()

dist_educ <- long_df %>% 
  ggplot() +
  aes(x = education_ISCED, y = value, fill = Knowledge) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE) + 
  xlab("Education (ISCED)") +
  ylab("Knowledge score") + 
  scale_fill_grey()

png(file="plots/know_means_demographics.png", width = 20, height = 7, units = 'in', res = 300)
ggarrange(dist_gender, dist_age, dist_educ, nrow=1, common.legend = TRUE, legend = "bottom")
dev.off()

# differences between types of knowledge withing demographic categories are small
# let's test it statistically: once we factor in any differences in levels of knowledge
# owing to demographic factors, are any remaining difference due to knowledge type
# statistically significant? no.
longer_df <- model_data %>% 
  dplyr::select(gender, age, education_ISCED, know_score_imm, know_score_general) %>% 
  pivot_longer(cols = -c(gender, age, education_ISCED),
               names_to = "Knowledge",
               values_to = "value") %>% 
  mutate(Knowledge = case_when(Knowledge == "know_score_general" ~ "General",
                               Knowledge == "know_score_imm" ~ "Immigration"))

m_knowledge <- longer_df %>% 
  lm(value ~ Knowledge +
       gender +
       age +
       education_ISCED,
     data = .)
summary(m_knowledge)

texreg::wordreg(list(m_knowledge),
                file="tables/knowledge_type_effect_table.docx",
                single.row = TRUE, 
                caption = "Main effect (OLS)",
                custom.coef.names = c("(Intercept)", "Immigration knowledge", "Male", "Age", "Education"),
                caption.above = TRUE,
                float.pos = "h!",
                custom.note="%stars.",
                stars = c(0.001, 0.01, 0.05),
                digits=3)

# plot(m_knowledge)

# function to calculate and plot marginal means
emmeans_function <- function(model, covariates, xlabs) {
  plot_list <- list()
  for (i in 1:length(covariates)) {
    emmeans_object <- summary(emmeans(model, specs = covariates[i]))
    p <- ggplot(emmeans_object) +
      aes_string(x = covariates[i], y = "emmean") +
      geom_line(group=1, color = "#F8766D") +
      geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL), color = "#F8766D") +
      geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
      xlab(xlabs[i]) +
      ylab("Estimated marginal mean")
    plot_list[[i]] <- p
  }
  return(ggarrange(plotlist = plot_list, nrow=1))
}

# benchmark data 1: bes wave 17
bes_data <- read_csv("data/bes_data.csv")
bes_data <- bes_data %>% 
  mutate(education = recode(education,
                            `0` = "No qual",
                            `1` = "Below GCSE",
                            `2` = "GCSE",
                            `3` = "A-level",
                            `4` = "Undergrad",
                            `5` = "Postgrad"),
         education = factor(education,
                            levels = c("No qual",
                                       "Below GCSE",
                                       "GCSE",
                                       "A-level",
                                       "Undergrad",
                                       "Postgrad"),
                            ordered = T),
         income_quartile = recode(income,
                                  `1` = "Q1",
                                  `2` = "Q1",
                                  `3` = "Q1",
                                  `4` = "Q1",
                                  `5` = "Q1",
                                  `6` = "Q2",
                                  `7` = "Q2",
                                  `8` = "Q3",
                                  `9` = "Q3",
                                  `10` = "Q3",
                                  `11` = "Q3",
                                  `12` = "Q3",
                                  `13` = "Q4",
                                  `14` = "Q4",
                                  `15` = "Q4"),
         age = recode(age,
                      `1` = "18-24",
                      `2` = "25-34",
                      `3` = "35-44",
                      `4` = "45-54",
                      `5` = "55-64",
                      `6` = "65plus",
                      `7` = "65plus",
                      `8` = "65plus"),
         gender = recode(gender,
                         `1` = "male",
                         `2` = "female"))

m_bes <- lm(ability ~
              age +
              gender +
              education,
            data = bes_data)

png(file="plots/bes_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_bes,
                 c("education","gender","age"),
                 c("Highest level of education","Gender"))
dev.off()

# benchmark data 2: anes 2019
anes_data <- read_csv("data/anes_data.csv")
anes_data <- anes_data %>% 
  mutate(educ = factor(educ,
                       levels = c("no_hs",
                                  "high_school",
                                  "some_college",
                                  "two_yr_college",
                                  "four_yr_college",
                                  "post_grad"),
                       ordered = T),
         gender = recode(gender,
                         `1` = "male",
                         `0` = "female"),
         age_cat = cut(age, breaks=c(18, 24, 34, 44, 54, 64, Inf), 
                       labels=c("18-24","25-34","35-45","45-54","55-64","65plus"),
                       ordered_result = TRUE),
         # https://dqydj.com/average-median-top-household-income-percentiles/
         income_quartile = recode(faminc_new,
                                  `1` = "Q1",
                                  `2` = "Q1",
                                  `3` = "Q1",
                                  `4` = "Q2",
                                  `5` = "Q2",
                                  `6` = "Q2",
                                  `7` = "Q2",
                                  `8` = "Q3",
                                  `9` = "Q3",
                                  `10` = "Q3",
                                  `11` = "Q4",
                                  `12` = "Q4",
                                  `13` = "Q4",
                                  `14` = "Q4",
                                  `15` = "Q4",
                                  `16` = "Q4"))

m_anes <- lm(ability ~
               age_cat +
               gender +
               educ,
             data = anes_data)

png(file="plots/anes_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_anes,
                 c("educ","gender","age_cat"),
                 c("Highest level of education","Gender"))
dev.off()

# benchmark data 3: ces 2020
ces_data <- read_csv("data/CES20_Common_OUTPUT_vv.csv")
# please note: this CSV is too large to store in the repo
# it can be downloaded from https://dataverse.harvard.edu/dataverse/cces
ces_data <- ces_data %>% 
  dplyr::select(gender, educ, birthyr, CC20_311a, CurrentGovParty, CC20_311b, CurrentSen1Party, 
                CC20_311c, CurrentSen2Party, CC20_311d, CurrentHouseParty) %>% 
  filter(!is.na(CurrentGovParty), !is.na(CurrentSen1Party), !is.na(CurrentSen2Party), !is.na(CurrentHouseParty)) %>% # removes 765 observations
  na.omit %>% # removes an additional 97 observations (0.2%), for a total of 1.4%
  mutate(gender = recode(gender,
                         `1` = "male",
                         `2` = "female"),
         educ = recode(educ,
                       `1` = "no_hs",
                       `2` = "high_school",
                       `3` = "some_college",
                       `4` = "two_yr_college",
                       `5` = "four_yr_college",
                       `6` = "post_grad"),
         educ = factor(educ,
                       levels = c("no_hs",
                                  "high_school",
                                  "some_college",
                                  "two_yr_college",
                                  "four_yr_college",
                                  "post_grad"),
                       ordered = T),
         age = 2020 - birthyr,
         age_cat = cut(age, breaks=c(17, 24, 34, 44, 54, 64, Inf), 
                       labels=c("18-24","25-34","35-45","45-54","55-64","65plus"),
                       ordered_result = TRUE),
         CurrentGovParty = recode(CurrentGovParty,
                                  "Democratic" = 3,
                                  "Republican" = 2),
         CurrentSen1Party = recode(CurrentSen1Party,
                                   "Democratic" = 3,
                                   "Republican" = 2),
         CurrentSen2Party = recode(CurrentSen2Party,
                                   "Democratic" = 3,
                                   "Republican" = 2,
                                   "Independent" = 4),
         CurrentHouseParty = recode(CurrentHouseParty,
                                    "Democratic" = 3,
                                    "Republican" = 2,
                                    "Libertarian" = 4),
         know_gov_name = case_when(CC20_311a == CurrentGovParty ~ 1,
                                   TRUE ~ 0),
         know_sen1_name = case_when(CC20_311b == CurrentSen1Party ~ 1,
                                    TRUE ~ 0),
         know_sen2_name = case_when(CC20_311c == CurrentSen2Party ~ 1,
                                    TRUE ~ 0),
         know_house_name = case_when(CC20_311d == CurrentHouseParty ~ 1,
                                     TRUE ~ 0)) %>% 
  dplyr::select(gender, age, age_cat, educ, know_gov_name, know_sen1_name, know_sen2_name, know_house_name)

# create knowledge scale
ces_know_items <- data.frame(ces_data$know_gov_name,
                             ces_data$know_sen1_name,
                             ces_data$know_sen2_name,
                             ces_data$know_house_name)

library(mirt)
ces_know_irt <- mirt(data=ces_know_items,
                     model=1,
                     itemtype = "2PL",
                     verbose=FALSE)
summary(ces_know_irt)

plot(ces_know_irt, type="trace")
plot(ces_know_irt, type="info")
coef(ces_know_irt, IRTpars=T)

ces_data$knowledge <- fscores(ces_know_irt)[,1] # each person's expected score

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(ces_know_items, cor="tet")
# Suggests one factor (unidimensional)

# Q3 for local independence
Q3resid <- data.frame(residuals(ces_know_irt, type="Q3")) # max = 0.335

itemfit(ces_know_irt, empirical.plot = 1)
itemfit(ces_know_irt, empirical.plot = 2)
itemfit(ces_know_irt, empirical.plot = 3)
itemfit(ces_know_irt, empirical.plot = 4)

range(ces_data$knowledge)

m_ces <- lm(knowledge ~
              age_cat +
              gender +
              educ,
            data = ces_data)

png(file="plots/ces_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_ces,
                 c("educ","gender","age_cat"),
                 c("Highest level of education","Gender","Age bracket"))
dev.off()

# reminder data - all nationalities
table(model_data$nationality)
model_data <- model_data %>% 
  mutate(education_ISCED = factor(education_ISCED,
                                  ordered = T),
         age_cat = cut(age, breaks=c(18, 24, 34, 44, 54, 64, Inf), 
                       labels=c("18-24","25-34","35-45","45-54","55-64","65plus"),
                       ordered_result = TRUE))

m_reminder_general <- lm(know_score_general ~
                           age_cat +
                           gender +
                           education_ISCED,
                         data = model_data)

png(file="plots/reminder_gen_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_reminder_general,
                 c("education_ISCED","gender","age_cat"),
                 c("Highest level of education","Gender","Age bracket"))
dev.off()

m_reminder_imm <- lm(know_score_imm ~
                       age_cat +
                       gender +
                       education_ISCED,
                     data = model_data)

png(file="plots/reminder_imm_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_reminder_imm,
                 c("education_ISCED","gender","age_cat"),
                 c("Highest level of education","Gender","Age bracket"))
dev.off()

emmmeans_gen_gender <- summary(emmeans(m_reminder_general, specs = c("gender"))) %>% 
  mutate(knowledge_type = "general")

emmmeans_imm_gender <- summary(emmeans(m_reminder_imm, specs = c("gender"))) %>% 
  mutate(knowledge_type = "immigration")

emmeans_gender_combined <- rbind(emmmeans_gen_gender, emmmeans_imm_gender)

emmeans_gender <- emmeans_gender_combined %>% 
  ggplot() +
  aes(x = gender, y = emmean, linetype = knowledge_type, shape = knowledge_type, group = knowledge_type) +
  geom_line() +
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)) +
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
  xlab("Gender") +
  ylab("Estimated marginal mean")
emmeans_gender

emmmeans_gen_age_cat <- summary(emmeans(m_reminder_general, specs = c("age_cat"))) %>% 
  mutate(knowledge_type = "general")

emmmeans_imm_age_cat <- summary(emmeans(m_reminder_imm, specs = c("age_cat"))) %>% 
  mutate(knowledge_type = "immigration")

emmeans_age_cat_combined <- rbind(emmmeans_gen_age_cat, emmmeans_imm_age_cat)

emmeans_age_cat <- emmeans_age_cat_combined %>% 
  ggplot() +
  aes(x = age_cat, y = emmean, linetype = knowledge_type, shape = knowledge_type, group = knowledge_type) +
  geom_line() +
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)) +
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
  xlab("Age") +
  ylab("Estimated marginal mean")
emmeans_age_cat

emmmeans_gen_education_ISCED <- summary(emmeans(m_reminder_general, specs = c("education_ISCED"))) %>% 
  mutate(knowledge_type = "general")

emmmeans_imm_education_ISCED <- summary(emmeans(m_reminder_imm, specs = c("education_ISCED"))) %>% 
  mutate(knowledge_type = "immigration")

emmeans_education_ISCED_combined <- rbind(emmmeans_gen_education_ISCED, emmmeans_imm_education_ISCED)

emmeans_education_ISCED <- emmeans_education_ISCED_combined %>% 
  ggplot() +
  aes(x = education_ISCED, y = emmean, linetype = knowledge_type, shape = knowledge_type, group = knowledge_type) +
  geom_line() +
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)) +
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
  xlab("Education") +
  ylab("Estimated marginal mean")
emmeans_education_ISCED

png(file="plots/reminder_emmeans_combined.png", width = 20, height = 6, units = 'in', res = 300)
ggarrange(emmeans_education_ISCED, emmeans_gender, emmeans_age_cat, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

# reminder data - by nationality
means_by_nationality <- function(nationalities, knowledge_gen, knowledge_imm) {
  plot_list <- list()
  for (i in 1:length(nationalities)) {
    m1 <- lm(glue::glue("{knowledge_gen} ~ age_cat + gender + education_ISCED"),
             data = subset(model_data, nationality==nationalities[i]))
    m2 <- lm(glue::glue("{knowledge_imm} ~ age_cat + gender + education_ISCED"),
             data = subset(model_data, nationality==nationalities[i]))
    
    emmmeans_gen_gender <- summary(emmeans(m1, specs = c("gender"))) %>% 
      mutate(knowledge_type = "general")
    
    emmmeans_imm_gender <- summary(emmeans(m2, specs = c("gender"))) %>% 
      mutate(knowledge_type = "immigration")
    
    emmeans_gender_combined <- rbind(emmmeans_gen_gender, emmmeans_imm_gender)
    
    emmeans_gender <- emmeans_gender_combined %>% 
      ggplot() +
      aes(x = gender, y = emmean, linetype = knowledge_type, shape = knowledge_type, group = knowledge_type) +
      geom_line() +
      geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)) +
      geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
      xlab("Gender") +
      ylab("Estimated marginal mean")
    
    emmmeans_gen_age_cat <- summary(emmeans(m1, specs = c("age_cat"))) %>% 
      mutate(knowledge_type = "general")
    
    emmmeans_imm_age_cat <- summary(emmeans(m2, specs = c("age_cat"))) %>% 
      mutate(knowledge_type = "immigration")
    
    emmeans_age_cat_combined <- rbind(emmmeans_gen_age_cat, emmmeans_imm_age_cat)
    
    emmeans_age_cat <- emmeans_age_cat_combined %>% 
      ggplot() +
      aes(x = age_cat, y = emmean, linetype = knowledge_type, shape = knowledge_type, group = knowledge_type) +
      geom_line() +
      geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)) +
      geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
      xlab("Age") +
      ylab("Estimated marginal mean")
    
    emmmeans_gen_education_ISCED <- summary(emmeans(m1, specs = c("education_ISCED"))) %>% 
      mutate(knowledge_type = "general")
    
    emmmeans_imm_education_ISCED <- summary(emmeans(m2, specs = c("education_ISCED"))) %>% 
      mutate(knowledge_type = "immigration")
    
    emmeans_education_ISCED_combined <- rbind(emmmeans_gen_education_ISCED, emmmeans_imm_education_ISCED)
    
    emmeans_education_ISCED <- emmeans_education_ISCED_combined %>% 
      ggplot() +
      aes(x = education_ISCED, y = emmean, linetype = knowledge_type, shape = knowledge_type, group = knowledge_type) +
      geom_line() +
      geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)) +
      geom_hline(yintercept=0, linetype="dashed", alpha=0.5) +
      xlab("Age") +
      ylab("Estimated marginal mean")
    
    plot <- ggarrange(emmeans_education_ISCED, emmeans_gender, emmeans_age_cat, nrow = 1, common.legend = TRUE, legend = "bottom")
    
    plot_list[[i]] <- annotate_figure(plot, top = text_grob(str_to_upper(nationalities[i], locale = "en"), color = "black", size = 12))
  }
  return(ggarrange(plotlist = plot_list, nrow=7))
}

png(file="plots/reminder_gen_nat.png", width = 20, height = 50, units = 'in', res = 300)
means_by_nationality(c("germany","hungary","poland","romania","spain","sweden","uk"), "know_score_general", "know_score_imm")
dev.off()
