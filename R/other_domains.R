set.seed(1000)
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
