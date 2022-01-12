# load packages
library(haven)

# read in SPSS data
all_data <- read_spss("data/10085_da_en_v1_0.zsav")

# add demographic variables
df <- data.frame(all_data$W1_Q1) # 1 = man; 2 = female
names(df)[ncol(df)] <- "gender"
df$gender <- as.character(df$gender)
df$gender[df$gender==1] <- "male"
df$gender[df$gender==2] <- "female"

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
df$in_paid_work[df$in_paid_work==2] <- 0

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

# eu and national identity
df <- cbind(df, all_data$W2_Q51_2)
names(df)[ncol(df)] <- "eu_nat_identity" # The European Union poses a threat to (NATIONALITY) identity and culture.
# 1 = strongly disagree; 5 = strongly agree

# free movement
df <- cbind(df, all_data$W1_Q28_3)
names(df)[ncol(df)] <- "free_move_protect_jobs" # The movement of individuals between EU countries should be restricted to protect native workers.
# 1 = strongly disagree; 5 = strongly agree

df <- cbind(df, all_data$W1_Q28_4)
names(df)[ncol(df)] <- "free_move_protect_services" # The movement of individuals between EU countries should be restricted to avoid exploitation of the public services (e.g. education and health services).
# 1 = strongly disagree; 5 = strongly agree

# flag for participation in W2
df$part_in_W2 <- NA
for(i in 1:length(df$part_in_W2)) {
  if(!is.na(all_data$W2_DATE[i])) {
    df$part_in_W2[i] <- 1
  }
  else
    df$part_in_W2[i] <- 0
}

# general political knowledge
# coding NAs as 0 (i.e., not knowing)
df <- cbind(df, all_data$W2_Q82_1)
names(df)[ncol(df)] <- "gen_know_switzerland" # correct = 2
table(df$gen_know_switzerland)
df$gen_know_switzerland[df$gen_know_switzerland==1] <- 0
df$gen_know_switzerland[df$gen_know_switzerland==2] <- 1
df$gen_know_switzerland[is.na(df$gen_know_switzerland) & df$part_in_W2==1] <- 0

df <- cbind(df, all_data$W2_Q82_2)
names(df)[ncol(df)] <- "gen_know_ep" # correct = 2
table(df$gen_know_ep)
df$gen_know_ep[df$gen_know_ep==1] <- 0
df$gen_know_ep[df$gen_know_ep==2] <- 1
df$gen_know_ep[is.na(df$gen_know_ep) & df$part_in_W2==1] <- 0

df <- cbind(df, all_data$W2_Q82_3)
names(df)[ncol(df)] <- "gen_know_party" # correct = 1
table(df$gen_know_party)
df$gen_know_party[df$gen_know_party==2] <- 0
df$gen_know_party[is.na(df$gen_know_party) & df$part_in_W2==1] <- 0

# migration political knowledge
df <- cbind(df, all_data$W2_Q83_1)
names(df)[ncol(df)] <- "mig_know_free_move" # correct = 1
table(df$mig_know_free_move)
df$mig_know_free_move[df$mig_know_free_move==2] <- 0
df$mig_know_free_move[is.na(df$mig_know_free_move) & df$part_in_W2==1] <- 0

df <- cbind(df, all_data$W2_Q83_2)
names(df)[ncol(df)] <- "mig_know_schengen" # correct = 1
table(df$mig_know_schengen)
df$mig_know_schengen[df$mig_know_schengen==2] <- 0
df$mig_know_schengen[is.na(df$mig_know_schengen) & df$part_in_W2==1] <- 0

df <- cbind(df, all_data$W2_Q83_3)
names(df)[ncol(df)] <- "mig_know_asylum" # correct = 2
table(df$mig_know_asylum)
df$mig_know_asylum[df$mig_know_asylum==1] <- 0
df$mig_know_asylum[df$mig_know_asylum==2] <- 1
df$mig_know_asylum[is.na(df$mig_know_asylum) & df$part_in_W2==1] <- 0

df <- cbind(df, all_data$W2_Q83_4)
names(df)[ncol(df)] <- "mig_know_syrians" # correct = 1
table(df$mig_know_syrians)
df$mig_know_syrians[df$mig_know_syrians==2] <- 0
df$mig_know_syrians[is.na(df$mig_know_syrians) & df$part_in_W2==1] <- 0

# knowledge sums and proportion correct
df$gen_know_sum <- rowSums(df[,25:27], na.rm = TRUE)
df$mig_know_sum <- rowSums(df[,28:31], na.rm = TRUE)
df$gen_know_prop <- df$gen_know_sum/3
df$mig_know_prop <- df$mig_know_sum/4

# quality control variables
df <- cbind(df, all_data$W1_EX1) # basic
names(df)[ncol(df)] <- "W1_EX1"

df <- cbind(df, all_data$W1_EX2) # advanced
names(df)[ncol(df)] <- "W1_EX2"

df <- cbind(df, all_data$W2_EX1) # basic
names(df)[ncol(df)] <- "W2_EX1"

df <- cbind(df, all_data$W2_EX2) # advanced
names(df)[ncol(df)] <- "W2_EX2"

df <- cbind(df, all_data$W2_CC) # W2 quality control question; 2 = correct
names(df)[ncol(df)] <- "W2_CC"

# weights
df <- cbind(df, all_data$WEIGHT)
names(df)[ncol(df)] <- "WEIGHT"
df$WEIGHT[is.na(df$WEIGHT)] <- 1 # observations with weight NA assumed to be 1

df <- cbind(df, all_data$WEIGHTEX1)
names(df)[ncol(df)] <- "WEIGHTEX1"
df$WEIGHTEX1[is.na(df$WEIGHTEX1)] <- 1 # observations with weight NA assumed to be 1

df <- cbind(df, all_data$WEIGHTEX2)
names(df)[ncol(df)] <- "WEIGHTEX2"
df$WEIGHTEX2[is.na(df$WEIGHTEX2)] <- 1 # observations with weight NA assumed to be 1

# exclude all who failed basic quality control in W2
table(df$W2_EX1) # 63
table(df$W2_EX2) # none
df_quality <- subset(df, is.na(W2_EX1))

# drop "other" category for nationality
df_quality <- subset(df, nationality!="other")

# save df as CSV
write.csv(df, "data/df_preprocessed.csv", row.names = FALSE)
