set.seed(100)

# load packages
library(naniar)
library(Hmisc)

# read in data
df_W2 <- read.csv("data/df_preprocessed_W2.csv")

# visualize missing
vis_miss(df_W2[,c(1:24,26:32)]) # ignoring quality control, weights, and W2 flag
# 1.3% missing

# make sure each variable is of the correct class
class(df_W2$gender) # factor
class(df_W2$age) # integer
class(df_W2$nationality) # factor
df_W2$education_ISCED <- factor(df_W2$education_ISCED, levels=c(0,1,2,3,4,5,6,7,8), ordered=TRUE) # ordered factor
df_W2$in_paid_work <- factor(df_W2$in_paid_work) # factor
df_W2$religion <- factor(df_W2$religion) # factor
class(df_W2$ideology_left_right) # integer (treating this as continuous)
class(df_W2$gen_imm_escape) # integer
class(df_W2$gen_imm_work) # integer
class(df_W2$gen_imm_study) # integer
class(df_W2$gen_imm_family_war) # integer
class(df_W2$gen_imm_marriage) # integer
class(df_W2$gen_imm_ess_good_bad) # integer
class(df_W2$gen_imm_ess_jobs) # integer
class(df_W2$gen_imm_ess_welfare) # integer
class(df_W2$gen_imm_ess_safety) # integer
class(df_W2$imm_anger) # integer
class(df_W2$imm_fear) # integer
class(df_W2$imm_hope) # integer
class(df_W2$imm_sympathy) # integer
class(df_W2$imm_size) # integer
class(df_W2$eu_nat_identity) # integer
class(df_W2$free_move_protect_jobs) # integer
class(df_W2$free_move_protect_services) # integer
class(df_W2$gen_know_switzerland) # integer
class(df_W2$gen_know_ep) # integer
class(df_W2$gen_know_party) # integer
class(df_W2$mig_know_free_move) # integer
class(df_W2$mig_know_schengen) # integer
class(df_W2$mig_know_asylum) # integer
class(df_W2$mig_know_syrians) # integer

# impute missing
names(df_W2)
impute_arg <- aregImpute(~ gender +
                           age +
                           nationality +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right +
                           gen_imm_escape +
                           gen_imm_work +
                           gen_imm_study +
                           gen_imm_family_war +
                           gen_imm_marriage +
                           gen_imm_ess_good_bad +
                           gen_imm_ess_jobs +
                           gen_imm_ess_welfare +
                           gen_imm_ess_safety +
                           imm_anger +
                           imm_fear +
                           imm_hope +
                           imm_sympathy +
                           imm_size +
                           eu_nat_identity +
                           free_move_protect_jobs +
                           free_move_protect_services +
                           gen_know_switzerland +
                           gen_know_ep +
                           gen_know_party +
                           mig_know_free_move +
                           mig_know_schengen +
                           mig_know_asylum +
                           mig_know_syrians,
                         data = df_W2, n.impute = 10, tlinear = FALSE)
impute_arg
imp_data <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=df_W2, list.out=TRUE, pr=FALSE, check=FALSE)) 
head(imp_data, 20)
model_data <- cbind(imp_data, df_W2[,33:46])

# create table 1
table1 <- model_data %>% 
  group_by(nationality) %>% 
  summarise(mean_age = mean(age, na.rm=T),
            male = sum(gender=="male"),
            female = sum(gender=="female"),
            ISCED_0 = sum(education_ISCED==0),
            ISCED_1 = sum(education_ISCED==1),
            ISCED_2 = sum(education_ISCED==2),
            ISCED_3 = sum(education_ISCED==3),
            ISCED_4 = sum(education_ISCED==4),
            ISCED_5 = sum(education_ISCED==5),
            ISCED_6 = sum(education_ISCED==6),
            ISCED_7 = sum(education_ISCED==7),
            ISCED_8 = sum(education_ISCED==8),
  )

# write table to CSV
write.csv(table1, "tables/table1.csv", row.names = FALSE)

# save df as CSV
write.csv(model_data, "data/model_data.csv", row.names = FALSE)
