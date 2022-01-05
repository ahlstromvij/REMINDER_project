# load packages
library(haven)

# read in SPSS data
all_data <- read_spss("10085_da_en_v1_0.zsav")

# add demographic variables
df <- data.frame(all_data$W1_Q1) # 1 = man; 2 = woman
names(df)[ncol(df)] <- "gender"

df <- cbind(df, all_data$W1_Q2A) # continuous version; see Q2C for ordinal
names(df)[ncol(df)] <- "age"

df$nationality <- NA
for(i in 1:length(df$nationality)){
  if(all_data$W1_Q3_1_1[i] == 1){
    df$nationality[i] <- "germany"
  }
  if(all_data$W1_Q3_1_2[i] == 1){
    df$nationality[i] <- "spain"
  }
  if(all_data$W1_Q3_1_3[i] == 1){
    df$nationality[i] <- "sweden"
  }
  if(all_data$W1_Q3_1_4[i] == 1){
    df$nationality[i] <- "uk"
  }
  if(all_data$W1_Q3_1_5[i] == 1){
    df$nationality[i] <- "hungary"
  }
  if(all_data$W1_Q3_1_6[i] == 1){
    df$nationality[i] <- "poland"
  }
  if(all_data$W1_Q3_1_7[i] == 1){
    df$nationality[i] <- "romania"
  }
  if(all_data$W1_Q3_1_8[i] == 1){
    df$nationality[i] <- "other"
  }
}
table(df$nationality)

df <- cbind(df, all_data$W1_Q40) # see https://ec.europa.eu/eurostat/statistics-explained/index.php?title=International_Standard_Classification_of_Education_(ISCED)
names(df)[ncol(df)] <- "education_ISCED"

df <- cbind(df, all_data$W1_Q45) # 1 = in paid work
names(df)[ncol(df)] <- "in_paid_work"

df <- cbind(df, all_data$W2_Q87) 
names(df)[ncol(df)] <- "religion" # lots of NAs

# ideology
df <- cbind(df, all_data$W1_Q8) 
names(df)[ncol(df)] <- "ideology_left_right" # 1 = max left; 10 = max right

# general immigration attitudes
# How do you feel about immigration by the following groups of people to [COUNTRY]?
df <- cbind(df, all_data$W1_Q15_1) 
names(df)[ncol(df)] <- "gen_imm_escape" # Those who come to escape war and oppression
# 1 = very negatively; 5 = very positively

df <- cbind(df, all_data$W1_Q15_2) 
names(df)[ncol(df)] <- "gen_imm_work" # Those who come to work
# 1 = very negatively; 5 = very positively

df <- cbind(df, all_data$W1_Q15_3) 
names(df)[ncol(df)] <- "gen_imm_study" # Those who come to study
# 1 = very negatively; 5 = very positively

df <- cbind(df, all_data$W1_Q15_4) 
names(df)[ncol(df)] <- "gen_imm_family_war" # Those who come to live with their family after separation, due to war and oppression
# 1 = very negatively; 5 = very positively

df <- cbind(df, all_data$W1_Q15_5) 
names(df)[ncol(df)] <- "gen_imm_marriage" # Those who come to live with their family, due to marriage or one spouse getting a job in [COUNTRY]
# 1 = very negatively; 5 = very positively

# immigration items from ESS
df <- cbind(df, all_data$W1_Q16) 
names(df)[ncol(df)] <- "gen_imm_ess_good_bad" # Would you say it is generally bad or good for [country]'s economy that people come to live here from other countries?
# 0 = bad; 10 = good

df <- cbind(df, all_data$W1_Q17) 
names(df)[ncol(df)] <- "gen_imm_ess_jobs" # Would you say that people who come to live here generally take jobs away from workers in [country], or generally help to create new jobs
# 0 = take jobs; 10 = create new jobs

df <- cbind(df, all_data$W1_Q18) 
names(df)[ncol(df)] <- "gen_imm_ess_welfare" # Most people who come to live here work and pay taxes. They also use health and welfare services. On balance, do you think people who come here take out more than they put in or put in more than they take out?
# 0 = take out more; 10 = put in more

df <- cbind(df, all_data$W1_Q19) 
names(df)[ncol(df)] <- "gen_imm_ess_safety" # Is [country]’s safety situation made better or worse by people coming to live here from other countries?
# 0 = made better; 10 = made worse

# emotions about immigration
# 1 = not at all; 7 = a lot
df <- cbind(df, all_data$W1_Q20_1)
names(df)[ncol(df)] <- "imm_anger" 
df <- cbind(df, all_data$W1_Q20_2)
names(df)[ncol(df)] <- "imm_fear" 
df <- cbind(df, all_data$W1_Q20_3)
names(df)[ncol(df)] <- "imm_hope" 
df <- cbind(df, all_data$W1_Q20_4)
names(df)[ncol(df)] <- "imm_sympathy" 

# estimations of size of immigrant group
df <- cbind(df, all_data$W1_Q25_2)
names(df)[ncol(df)] <- "imm_size" # Out of every 100 people in [COUNTRY], how many do you think were born outside [COUNTRY]? (percentage)

# free movement
df <- cbind(df, all_data$W1_Q28_3)
names(df)[ncol(df)] <- "free_move_protect_jobs" # The movement of individuals between EU countries should be restricted to protect native workers.
# 1 = strongly disagree; 5 = strongly agree

df <- cbind(df, all_data$W1_Q28_4)
names(df)[ncol(df)] <- "free_move_protect_services" # The movement of individuals between EU countries should be restricted to avoid exploitation of the public services (e.g. education and health services).
# 1 = strongly disagree; 5 = strongly agree

# general political knowledge
df <- cbind(df, all_data$W2_Q82_1)
names(df)[ncol(df)] <- "gen_know_switzerland" # correct = 2
table(df$gen_know_switzerland)
df$gen_know_switzerland[df$gen_know_switzerland==1] <- 0
df$gen_know_switzerland[df$gen_know_switzerland==2] <- 1

df <- cbind(df, all_data$W2_Q82_2)
names(df)[ncol(df)] <- "gen_know_ep" # correct = 2
table(df$gen_know_ep)
df$gen_know_ep[df$gen_know_ep==1] <- 0
df$gen_know_ep[df$gen_know_ep==2] <- 1

df <- cbind(df, all_data$W2_Q82_3)
names(df)[ncol(df)] <- "gen_know_party" # correct = 1
table(df$gen_know_party)
df$gen_know_party[df$gen_know_party==2] <- 0

# migration political knowledge
df <- cbind(df, all_data$W2_Q83_1)
names(df)[ncol(df)] <- "mig_know_free_move" # correct = 1
table(df$mig_know_free_move)
df$mig_know_free_move[df$mig_know_free_move==2] <- 0

df <- cbind(df, all_data$W2_Q83_2)
names(df)[ncol(df)] <- "mig_know_schenger" # correct = 1
table(df$mig_know_schenger)
df$mig_know_schenger[df$mig_know_schenger==2] <- 0

df <- cbind(df, all_data$W2_Q83_3)
names(df)[ncol(df)] <- "mig_know_asylum" # correct = 2
table(df$mig_know_asylum)
df$mig_know_asylum[df$mig_know_asylum==1] <- 0
df$mig_know_asylum[df$mig_know_asylum==2] <- 1

df <- cbind(df, all_data$W2_Q83_4)
names(df)[ncol(df)] <- "mig_know_syrians" # correct = 1
table(df$mig_know_syrians)
df$mig_know_syrians[df$mig_know_syrians==2] <- 0
