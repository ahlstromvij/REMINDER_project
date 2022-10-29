set.seed(100)
library(tidyverse)
library(emmeans)
library(ggpubr)

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
bes_data
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

table(bes_data$education)
table(bes_data$income_quartile)
table(bes_data$age)
table(bes_data$gender)

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
table(anes_data$gender)
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
psych::fa.parallel(ces_know_items, cor="poly", fa="fa")
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
reminder_data <- read_csv("data/model_data_IRT.csv")
table(reminder_data$nationality)
reminder_data <- reminder_data %>% 
  mutate(education_ISCED = factor(education_ISCED,
                                  ordered = T),
         age_cat = cut(age, breaks=c(18, 24, 34, 44, 54, 64, Inf), 
                       labels=c("18-24","25-34","35-45","45-54","55-64","65plus"),
                       ordered_result = TRUE))

m_reminder_general <- lm(know_score_general ~
                           age_cat +
                           gender +
                           education_ISCED,
                         data = reminder_data)

png(file="plots/reminder_gen_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_reminder_general,
                 c("education_ISCED","gender","age_cat"),
                 c("Highest level of education","Gender","Age bracket"))
dev.off()

m_reminder_imm <- lm(know_score_imm ~
                       age_cat +
                       gender +
                       education_ISCED,
                     data = reminder_data)

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
            data = subset(reminder_data, nationality==nationalities[i]))
    plot <- emmeans_function(m,
                             c("education_ISCED","gender","age_cat"),
                             c("Highest level of education","Gender","Age bracket"))
    plot_list[[i]] <- annotate_figure(plot, top = text_grob(str_to_upper(nationalities[i], locale = "en"), color = "black", size = 12))
  }
  return(ggarrange(plotlist = plot_list, nrow=7))
}

means_by_nationality(c("germany","hungary","poland","romania","spain","sweden","uk"), "know_score_general")
means_by_nationality(c("germany","hungary","poland","romania","spain","sweden","uk"), "know_score_imm")

#epcc data
epcc_data <- read_csv("data/epcc_data.csv")
epcc_data <- epcc_data %>% 
  filter(education != "other" & education != "student") %>% 
  mutate(education = factor(education, 
                            levels=c("no_qual","gcse","alevel","degree"),
                            ordered = T),
         education = recode(education,
                            "no_qual" = "None",
                            "gcse" = "GCSE",
                            "alevel" = "A-level",
                            "degree" = "Degree"),
         gender = recode(gender,
                         "male" = "Male",
                         "female" = "Female"),
         age = factor(age,
                      levels = c("15-24",
                                 "25-34",
                                 "35-44",
                                 "45-54",
                                 "55-64",
                                 "65+"),
                      ordered = T))

m_epcc <- lm(knowledge ~
               age +
               gender +
               education,
             data = epcc_data)

png(file="plots/epcc_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_epcc,
                 c("education","gender","age"),
                 c("Highest level of education","Gender","Age bracket"))
dev.off()

# covid data
covid_data <- read_csv("data/covid_data.csv")
covid_data <- covid_data %>% 
  mutate(education = factor(education, 
                            levels=c("No formal education",
                                     "GCSE (or equivalent)",
                                     "A-level (or equivalent)",
                                     "Undergraduate degree (e.g., BA)",
                                     "Postgraduate degree (e.g., MA, MSc, PhD)"),
                            ordered = T),
         education = recode(education,
                            "No formal education" = "No education",
                            "GCSE (or equivalent)" = "GCSE",
                            "A-level (or equivalent)" = "A-level",
                            "Undergraduate degree (e.g., BA)" = "Undergrad",
                            "Postgraduate degree (e.g., MA, MSc, PhD)" = "Postgrad"),
         age_cat = cut(age, breaks=c(18, 24, 34, 44, 54, 64, Inf), 
                       labels=c("18-24","25-34","35-45","45-54","55-64","65plus"),
                       ordered_result = TRUE),
         income_quartiles = recode(income,
                                   `1` <- "Q1",
                                   `2` <- "Q1",
                                   `3` <- "Q2",
                                   `4` <- "Q3",
                                   `5` <- "Q3",
                                   `6` <- "Q3",
                                   `7` <- "Q4",
                                   `8` <- "Q4",
                                   `9` <- "Q4",
                                   `10` <- "Q4",
                                   `11` <- "Q4",
                                   `12` <- "Q4"))

m_covid <- lm(know ~
                age_cat +
                gender +
                education,
              data = covid_data)

png(file="plots/covid_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_covid,
                 c("education","gender","age_cat"),
                 c("Education","Gender","Age bracket"))
dev.off()
