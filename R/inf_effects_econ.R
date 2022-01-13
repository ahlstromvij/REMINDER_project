set.seed(100)

# load packages
library(lmtest)
library(sandwich)
library(ggplot2)

# read in data
model_data <- read.csv("data/model_data_IRT_propscores.csv")

# make sure each variable is of the correct class
class(model_data$gender) # factor
class(model_data$age) # integer
class(model_data$nationality) # factor
model_data$education_ISCED <- factor(model_data$education_ISCED, levels=c(0,1,2,3,4,5,6,7,8), ordered=TRUE) # ordered factor
model_data$in_paid_work <- factor(model_data$in_paid_work) # factor
model_data$religion <- factor(model_data$religion) # factor
class(model_data$ideology_left_right) # integer (treating this as continuous)
class(model_data$gen_imm_escape) # integer
class(model_data$gen_imm_work) # integer
class(model_data$gen_imm_study) # integer
class(model_data$gen_imm_family_war) # integer
class(model_data$gen_imm_marriage) # integer
class(model_data$gen_imm_ess_good_bad) # integer
class(model_data$gen_imm_ess_jobs) # integer
class(model_data$gen_imm_ess_welfare) # integer
class(model_data$gen_imm_ess_safety) # integer
class(model_data$imm_anger) # integer
class(model_data$imm_fear) # integer
class(model_data$imm_hope) # integer
class(model_data$imm_sympathy) # integer
class(model_data$imm_size) # integer
class(model_data$eu_nat_identity) # integer
class(model_data$free_move_protect_jobs) # integer
class(model_data$free_move_protect_services) # integer
class(model_data$gen_know_switzerland) # integer
class(model_data$gen_know_ep) # integer
class(model_data$gen_know_party) # integer
class(model_data$mig_know_free_move) # integer
class(model_data$mig_know_schengen) # integer
class(model_data$mig_know_asylum) # integer
class(model_data$mig_know_syrians) # integer

# subset by nationality
table(model_data$nationality)
model_data_germany <- subset(model_data, nationality=="germany")
model_data_hungary <- subset(model_data, nationality=="hungary")
model_data_poland <- subset(model_data, nationality=="poland")
model_data_uk <- subset(model_data, nationality=="uk")
model_data_spain <- subset(model_data, nationality=="spain")
model_data_sweden <- subset(model_data, nationality=="sweden")
model_data_uk <- subset(model_data, nationality=="uk")

# create df to collect knowledge effects and confidence intervals
df_knowledge <- data.frame("nationality" = rep(c("germany","hungary","poland","uk","spain","sweden","uk"),2),
                           "type" = c(rep("general",7),rep("immigration",7)),
                           "effect" = NA,
                           "lwr" = NA,
                           "upr" = NA)

# information effects on perceptions about impacts on economy
# higher = better for economy

# germany
m_econ_gen_germany <- lm(gen_imm_ess_good_bad ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_germany, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_germany)

# robust SEs
m_econ_gen_germany_vcov <- vcovHC(m_econ_gen_germany, type="HC1")
coeftest(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)
df_knowledge$effect[df_knowledge$nationality=="germany" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)[2,1]
coefci(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)
df_knowledge$lwr[df_knowledge$nationality=="germany" & df_knowledge$type=="general"] <- coefci(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="germany" & df_knowledge$type=="general"] <- coefci(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)[2,2]

plot(m_econ_gen_germany, 1)
# Used to check the linear relationship assumptions. A horizontal line, without distinct patterns 
# is an indication for a linear relationship, what is good.
# If the residual plot indicates a non-linear relationship in the data, then a simple approach is 
# to use non-linear transformations of the predictors, such as log(x), sqrt(x) and x^2, in the 
# regression model.

plot(m_econ_gen_germany, 2)
# Used to examine whether the residuals are normally distributed. It’s good if residuals points 
# follow the straight dashed line.

plot(m_econ_gen_germany, 3)
# Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line 
# with equally spread points is a good indication of homoscedasticity.
# A possible solution to reduce the heteroscedasticity problem is to use a log or square root 
# transformation of the outcome variable (y).

plot(m_econ_gen_germany, 5)
# Residuals vs Leverage. Used to identify influential cases, that is extreme values that might 
# influence the regression results when included or excluded from the analysis. 
# Observations whose standardized residuals are greater than 3 in absolute value are possible outliers.
# A data point has high leverage, if it has extreme predictor x values. This can be detected by 
# examining the leverage statistic or the hat-value. A value of this statistic above 2(p + 1)/n 
# indicates an observation with high leverage (P. Bruce and Bruce 2017); where, p is the number of 
# predictors and n is the number of observations.

m_econ_imm_germany <- lm(gen_imm_ess_good_bad ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_germany, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_germany)
m_econ_imm_germany_vcov <- vcovHC(m_econ_imm_germany, type="HC1")
coeftest(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)
df_knowledge$effect[df_knowledge$nationality=="germany" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)[2,1]
coefci(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)
df_knowledge$lwr[df_knowledge$nationality=="germany" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="germany" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_germany)

# hungary
m_econ_gen_hungary <- lm(gen_imm_ess_good_bad ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_hungary, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_hungary)
m_econ_gen_hungary_vcov <- vcovHC(m_econ_gen_hungary, type="HC1")
coeftest(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)
df_knowledge$effect[df_knowledge$nationality=="hungary" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)[2,1]
coefci(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)
df_knowledge$lwr[df_knowledge$nationality=="hungary" & df_knowledge$type=="general"] <- coefci(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="hungary" & df_knowledge$type=="general"] <- coefci(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_hungary)

m_econ_imm_hungary <- lm(gen_imm_ess_good_bad ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_hungary, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_hungary)
m_econ_imm_hungary_vcov <- vcovHC(m_econ_imm_hungary, type="HC1")
coeftest(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)
df_knowledge$effect[df_knowledge$nationality=="hungary" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)[2,1]
coefci(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)
df_knowledge$lwr[df_knowledge$nationality=="hungary" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="hungary" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_hungary)

# poland
m_econ_gen_poland <- lm(gen_imm_ess_good_bad ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_poland, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_poland)
m_econ_gen_poland_vcov <- vcovHC(m_econ_gen_poland, type="HC1")
coeftest(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)
df_knowledge$effect[df_knowledge$nationality=="poland" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)[2,1]
coefci(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)
df_knowledge$lwr[df_knowledge$nationality=="poland" & df_knowledge$type=="general"] <- coefci(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="poland" & df_knowledge$type=="general"] <- coefci(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_poland)

m_econ_imm_poland <- lm(gen_imm_ess_good_bad ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_poland, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_poland)
m_econ_imm_poland_vcov <- vcovHC(m_econ_imm_poland, type="HC1")
coeftest(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)
df_knowledge$effect[df_knowledge$nationality=="poland" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)[2,1]
coefci(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)
df_knowledge$lwr[df_knowledge$nationality=="poland" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="poland" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_poland)

# uk
m_econ_gen_uk <- lm(gen_imm_ess_good_bad ~ know_score_general_binary +
                          gender +
                          age +
                          education_ISCED +
                          in_paid_work +
                          religion +
                          ideology_left_right,
                        data = model_data_uk, 
                        weights = (weight.ATE_general))
summary(m_econ_gen_uk)
m_econ_gen_uk_vcov <- vcovHC(m_econ_gen_uk, type="HC1")
coeftest(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)
df_knowledge$effect[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,1]
coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)
df_knowledge$lwr[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_uk)

m_econ_imm_uk <- lm(gen_imm_ess_good_bad ~ know_score_imm_binary +
                          gender +
                          age +
                          education_ISCED +
                          in_paid_work +
                          religion +
                          ideology_left_right,
                        data = model_data_uk, 
                        weights = (weight.ATE_imm))
summary(m_econ_imm_uk)
m_econ_imm_uk_vcov <- vcovHC(m_econ_imm_uk, type="HC1")
coeftest(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)
df_knowledge$effect[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,1]
coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)
df_knowledge$lwr[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_uk)

# spain
m_econ_gen_spain <- lm(gen_imm_ess_good_bad ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_spain, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_spain)
m_econ_gen_spain_vcov <- vcovHC(m_econ_gen_spain, type="HC1")
coeftest(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)
df_knowledge$effect[df_knowledge$nationality=="spain" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)[2,1]
coefci(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)
df_knowledge$lwr[df_knowledge$nationality=="spain" & df_knowledge$type=="general"] <- coefci(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="spain" & df_knowledge$type=="general"] <- coefci(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_spain)

m_econ_imm_spain <- lm(gen_imm_ess_good_bad ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_spain, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_spain)
m_econ_imm_spain_vcov <- vcovHC(m_econ_imm_spain, type="HC1")
coeftest(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)
df_knowledge$effect[df_knowledge$nationality=="spain" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)[2,1]
coefci(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)
df_knowledge$lwr[df_knowledge$nationality=="spain" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="spain" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_spain)

# sweden
m_econ_gen_sweden <- lm(gen_imm_ess_good_bad ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_sweden, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_sweden)
m_econ_gen_sweden_vcov <- vcovHC(m_econ_gen_sweden, type="HC1")
coeftest(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)
df_knowledge$effect[df_knowledge$nationality=="sweden" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)[2,1]
coefci(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)
df_knowledge$lwr[df_knowledge$nationality=="sweden" & df_knowledge$type=="general"] <- coefci(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="sweden" & df_knowledge$type=="general"] <- coefci(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_sweden)

m_econ_imm_sweden <- lm(gen_imm_ess_good_bad ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_sweden, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_sweden)
m_econ_imm_sweden_vcov <- vcovHC(m_econ_imm_sweden, type="HC1")
coeftest(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)
df_knowledge$effect[df_knowledge$nationality=="sweden" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)[2,1]
coefci(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)
df_knowledge$lwr[df_knowledge$nationality=="sweden" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="sweden" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_sweden)

# uk
m_econ_gen_uk <- lm(gen_imm_ess_good_bad ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_uk, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_uk)
m_econ_gen_uk_vcov <- vcovHC(m_econ_gen_uk, type="HC1")
coeftest(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)
df_knowledge$effect[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,1]
coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)
df_knowledge$lwr[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_uk)

m_econ_imm_uk <- lm(gen_imm_ess_good_bad ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_uk, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_uk)
m_econ_imm_uk_vcov <- vcovHC(m_econ_imm_uk, type="HC1")
coeftest(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)
df_knowledge$effect[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,1]
coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)
df_knowledge$lwr[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_uk)

# plot knowledge effects
ggplot(df_knowledge, aes(nationality, effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.2) +
  facet_grid(. ~ type) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  coord_flip()
