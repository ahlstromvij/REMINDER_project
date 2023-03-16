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
                caption = "Main effects (OLS with robust standard errors)",
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
  mutate(age_cat = case_when(age >= quantile(age)[1] & age <= quantile(age)[2] ~ "18_37",
                             age > quantile(age)[2] & age <= quantile(age)[3] ~ "38_50",
                             age > quantile(age)[3] & age <= quantile(age)[4] ~ "51_62",
                             age > quantile(age)[4] ~ "62_up")) %>% 
  group_by(age_cat) %>% 
  summarise(general = mean(know_score_general),
            immigration = mean(know_score_imm))

long_df <- model_data %>% 
  dplyr::select(gender, age, education_ISCED, know_score_imm, know_score_general) %>% 
  mutate(age_cat = case_when(age >= quantile(age)[1] & age <= quantile(age)[2] ~ "18_37",
                             age > quantile(age)[2] & age <= quantile(age)[3] ~ "38_50",
                             age > quantile(age)[3] & age <= quantile(age)[4] ~ "51_62",
                             age > quantile(age)[4] ~ "62_up"),
         education_ISCED = factor(education_ISCED))  %>% 
  dplyr::select(-age) %>% 
  pivot_longer(cols = -c(gender, age_cat, education_ISCED),
               names_to = "knowledge_type",
               values_to = "value")

long_df %>% 
  ggplot() +
  aes(x = gender, y = value, color = knowledge_type) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE)

long_df %>% 
  ggplot() +
  aes(x = age_cat, y = value, color = knowledge_type) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE)

long_df %>% 
  ggplot() +
  aes(x = education_ISCED, y = value, color = knowledge_type) +
  geom_violin() +
  stat_summary(fun.y="mean",geom="crossbar", mapping=aes(ymin=after_stat(y), ymax=after_stat(y)), 
               width=1, position=position_dodge(),show.legend = FALSE)

long_df %>% 
  lm(value ~ knowledge_type +
       gender +
       age_cat +
       education_ISCED,
     data = .) %>% 
  summary()

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

# reminder data - by nationality
means_by_nationality <- function(nationalities, knowledge_var) {
  plot_list <- list()
  for (i in 1:length(nationalities)) {
    m <- lm(glue::glue("{knowledge_var} ~ age_cat + gender + education_ISCED"),
            data = subset(model_data, nationality==nationalities[i]))
    plot <- emmeans_function(m,
                             c("education_ISCED","gender","age_cat"),
                             c("Highest level of education","Gender","Age bracket"))
    plot_list[[i]] <- annotate_figure(plot, top = text_grob(str_to_upper(nationalities[i], locale = "en"), color = "black", size = 12))
  }
  return(ggarrange(plotlist = plot_list, nrow=7))
}

png(file="plots/reminder_gen_emmeans_nat.png", width = 20, height = 50, units = 'in', res = 300)
means_by_nationality(c("germany","hungary","poland","romania","spain","sweden","uk"), "know_score_general")
dev.off()

png(file="plots/reminder_imm_emmeans_nat.png", width = 20, height = 50, units = 'in', res = 300)
means_by_nationality(c("germany","hungary","poland","romania","spain","sweden","uk"), "know_score_imm")
dev.off()
