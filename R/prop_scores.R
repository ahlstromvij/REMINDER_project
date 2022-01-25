set.seed(100)

# load packages
library(cobalt)

# read in data
model_data <- read.csv("data/model_data_IRT.csv")

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
model_data$know_score_general_binary <- factor(model_data$know_score_general_binary) # factor
model_data$know_score_imm_binary <- factor(model_data$know_score_imm_binary) # factor
model_data$know_score_combo_binary <- factor(model_data$know_score_combo_binary) # factor

# calculate propensity scores for general knowledge scale
colnames(model_data)
p_scores_general <- glm(know_score_general_binary ~
                        gender +
                        age +
                        education_ISCED +
                        in_paid_work +
                        religion +
                        ideology_left_right,
                     data = model_data, 
                     family = "binomial")
summary(p_scores_general)

model_data$ps_value_general <- predict(p_scores_general, type="response")
head(model_data$ps_value_general)
model_data$weight.ATE_general <- ifelse(model_data$know_score_general_binary == 1, 1/model_data$ps_value_general, 1/(1-model_data$ps_value_general))
head(model_data$weight.ATE_general)
hist(model_data$weight.ATE_general)

# check balance using balance plots
covs_general <- subset(model_data, select = c(gender, age, education_ISCED, in_paid_work, religion, ideology_left_right))
bal.plot(covs_general, treat = model_data$know_score_general_binary, estimand = "ATE", weights = model_data$weight.ATE_general, method = "weighting", var.name = "gender", which = "both")
bal.plot(covs_general, treat = model_data$know_score_general_binary, estimand = "ATE", weights = model_data$weight.ATE_general, method = "weighting", var.name = "age", which = "both")
bal.plot(covs_general, treat = model_data$know_score_general_binary, estimand = "ATE", weights = model_data$weight.ATE_general, method = "weighting", var.name = "education_ISCED", which = "both")
bal.plot(covs_general, treat = model_data$know_score_general_binary, estimand = "ATE", weights = model_data$weight.ATE_general, method = "weighting", var.name = "in_paid_work", which = "both")
bal.plot(covs_general, treat = model_data$know_score_general_binary, estimand = "ATE", weights = model_data$weight.ATE_general, method = "weighting", var.name = "religion", which = "both")
bal.plot(covs_general, treat = model_data$know_score_general_binary, estimand = "ATE", weights = model_data$weight.ATE_general, method = "weighting", var.name = "ideology_left_right", which = "both")

# calculate propensity scores for immigration knowledge scale
p_scores_imm <- glm(know_score_imm_binary ~
                          gender +
                          age +
                          education_ISCED +
                          in_paid_work +
                          religion +
                          ideology_left_right,
                        data = model_data, 
                        family = "binomial")
summary(p_scores_imm)

model_data$ps_value_imm <- predict(p_scores_imm, type="response")
head(model_data$ps_value_imm)
model_data$weight.ATE_imm <- ifelse(model_data$know_score_imm_binary == 1, 1/model_data$ps_value_imm, 1/(1-model_data$ps_value_imm))
head(model_data$weight.ATE_imm)
hist(model_data$weight.ATE_imm)

# check balance using balance plots
bal.plot(covs_general, treat = model_data$know_score_imm_binary, estimand = "ATE", weights = model_data$weight.ATE_imm, method = "weighting", var.name = "gender", which = "both")
bal.plot(covs_general, treat = model_data$know_score_imm_binary, estimand = "ATE", weights = model_data$weight.ATE_imm, method = "weighting", var.name = "age", which = "both")
bal.plot(covs_general, treat = model_data$know_score_imm_binary, estimand = "ATE", weights = model_data$weight.ATE_imm, method = "weighting", var.name = "education_ISCED", which = "both")
bal.plot(covs_general, treat = model_data$know_score_imm_binary, estimand = "ATE", weights = model_data$weight.ATE_imm, method = "weighting", var.name = "in_paid_work", which = "both")
bal.plot(covs_general, treat = model_data$know_score_imm_binary, estimand = "ATE", weights = model_data$weight.ATE_imm, method = "weighting", var.name = "religion", which = "both")
bal.plot(covs_general, treat = model_data$know_score_imm_binary, estimand = "ATE", weights = model_data$weight.ATE_imm, method = "weighting", var.name = "ideology_left_right", which = "both")

# calculate propensity scores for combined knowledge scale
p_scores_combo <- glm(know_score_combo_binary ~
                      gender +
                      age +
                      education_ISCED +
                      in_paid_work +
                      religion +
                      ideology_left_right,
                    data = model_data, 
                    family = "binomial")
summary(p_scores_combo)

model_data$ps_value_combo <- predict(p_scores_combo, type="response")
head(model_data$ps_value_combo)
model_data$weight.ATE_combo <- ifelse(model_data$know_score_combo_binary == 1, 1/model_data$ps_value_imm, 1/(1-model_data$ps_value_imm))
head(model_data$weight.ATE_combo)
hist(model_data$weight.ATE_combo)

# check balance using balance plots
bal.plot(covs_general, treat = model_data$know_score_combo_binary, estimand = "ATE", weights = model_data$weight.ATE_combo, method = "weighting", var.name = "gender", which = "both")
bal.plot(covs_general, treat = model_data$know_score_combo_binary, estimand = "ATE", weights = model_data$weight.ATE_combo, method = "weighting", var.name = "age", which = "both")
bal.plot(covs_general, treat = model_data$know_score_combo_binary, estimand = "ATE", weights = model_data$weight.ATE_combo, method = "weighting", var.name = "education_ISCED", which = "both")
bal.plot(covs_general, treat = model_data$know_score_combo_binary, estimand = "ATE", weights = model_data$weight.ATE_combo, method = "weighting", var.name = "in_paid_work", which = "both")
bal.plot(covs_general, treat = model_data$know_score_combo_binary, estimand = "ATE", weights = model_data$weight.ATE_combo, method = "weighting", var.name = "religion", which = "both")
bal.plot(covs_general, treat = model_data$know_score_combo_binary, estimand = "ATE", weights = model_data$weight.ATE_combo, method = "weighting", var.name = "ideology_left_right", which = "both")

# save df as CSV
write.csv(model_data, "data/model_data_IRT_propscores.csv", row.names = FALSE)
