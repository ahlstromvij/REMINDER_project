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
bes_data <- read_csv("data/bes_data_preprocessed-2.csv")
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
              education +
              income_quartile,
            data = bes_data)

png(file="plots/bes_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_bes,
                 c("education","gender","age","income_quartile"),
                 c("Highest level of education","Gender","Age bracket","Income quartile"))
dev.off()

# benchmark data 2: anes 2019
anes_data <- read_csv("data/anes_data_processed-2019-imputed.csv")
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
               educ +
               income_quartile,
             data = anes_data)

png(file="plots/anes_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_anes,
                 c("educ","gender","age_cat","income_quartile"),
                 c("Highest level of education","Gender","Age bracket","Income quartile"))
dev.off()

# reminder data - all nationalities
reminder_data <- read_csv("data/model_data_IRT_propscores.csv")
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
    plot_list[[i]] <- emmeans_function(m,
                                       c("education_ISCED","gender","age_cat"),
                                       c("Highest level of education","Gender","Age bracket"))
  }
  return(ggarrange(plotlist = plot_list, nrow=7))
}

means_by_nationality(c("germany","hungary","poland","romania","spain","sweden","uk"), "know_score_general")
means_by_nationality(c("germany","hungary","poland","romania","spain","sweden","uk"), "know_score_imm")

#epcc data
epcc_data <- read_csv("data/epcc_data_processed.csv")
epcc_data <- epcc_data %>% 
  filter(education != "other" & education != "student") %>% 
  mutate(education = factor(education, 
                            levels=c("no_qual","gcse","alevel","degree"),
                            ordered = T),
         age = factor(age,
                      levels = c("15-24",
                                 "25-34",
                                 "35-44",
                                 "45-54",
                                 "55-64",
                                 "65plus"),
                      ordered = T),
         income = factor(income,
                         levels = c("up_to_9499",
                                    "9500_17499",
                                    "17500_24999",
                                    "25000plus")))

m_epcc <- lm(knowledge ~
               age +
               gender +
               income +
               education,
             data = epcc_data)

png(file="plots/epcc_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_epcc,
                 c("education","gender","age","income"),
                 c("Highest level of education","Gender","Age bracket","Income"))
dev.off()

# covid data
covid_data <- read_csv("data/covid_processed_all_mirt.csv")
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
                education +
                income_quartiles,
              data = covid_data)

png(file="plots/covid_emmeans.png", width = 20, height = 6, units = 'in', res = 300)
emmeans_function(m_covid,
                 c("education","gender","age_cat","income_quartiles"),
                 c("Education","Gender","Age bracket","Income"))
dev.off()
