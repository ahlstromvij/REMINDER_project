# load packages
library(table1)

# read in preprocessed data
df_preprocessed <- read.csv("df_preprocessed.csv")

# subset to only data from both W1 and 2
df_W2 <- subset(df_preprocessed, part_in_W2==1)

# summarize data
# sociodemographics
table1(~ factor(gender) + age + factor(education_ISCED) + factor(in_paid_work) + ideology_left_right | nationality, data=df_W2)

# general immigration attitudes
table1(~ gen_imm_escape + gen_imm_work + gen_imm_study + gen_imm_family_war + gen_imm_marriage + gen_imm_marriage | nationality, data=df_W2)

# ess immigration attitudes
table1(~ gen_imm_ess_good_bad + gen_imm_ess_jobs + gen_imm_ess_welfare + gen_imm_ess_safety | nationality, data=df_W2)

# emotions about immigration
table1(~ imm_anger + imm_fear + imm_hope + imm_sympathy | nationality, data=df_W2)

# immigration size
table1(~ imm_size | nationality, data=df_W2)

# free movement and EU identity
table1(~ free_move_protect_jobs + free_move_protect_services + eu_nat_identity | nationality, data=df_W2)

# general knowledge
table1(~ gen_know_switzerland + gen_know_ep + gen_know_party | nationality, data=df_W2)

# specific knowledge about immigration
table1(~ mig_know_free_move + mig_know_schenger + mig_know_asylum + mig_know_syrians | nationality, data=df_W2)

# knowledge sums and proportions
table1(~ gen_know_sum + mig_know_sum | nationality, data=df_W2)
table1(~ gen_know_prop + mig_know_prop | nationality, data=df_W2)

# correlation between general and specific knowledge
cor(df_W2$gen_know_sum, df_W2$mig_know_sum)

# save df as CSV
write.csv(df_W2, "df_preprocessed_W2.csv", row.names = FALSE)
