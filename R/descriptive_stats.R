# load packages
library(table1)
library(ggplot2)

# read in preprocessed data
df_preprocessed <- read.csv("data/df_preprocessed.csv")

# subset to only data from both W1 and 2 or who failed W2 quality check
df_W2 <- subset(df_preprocessed, part_in_W2==1 & W2_CC==2)

# summarize data
# sociodemographics
df_W2$education_binary <- ifelse(df_W2$education_ISCED > 5, 1, 0)
table1(~ factor(gender) + age + factor(education_ISCED) + education_binary + factor(in_paid_work) + ideology_left_right | nationality, data=df_W2)
# romania most right; spain most left
# romania most educated; sweden least educated

# general immigration attitudes
table1(~ gen_imm_escape + gen_imm_work + gen_imm_study + gen_imm_family_war + gen_imm_marriage | nationality, data=df_W2)

gen_imm_plot <- data.frame("nationality" = (rep(c("germany", "hungary", "poland", "romania", "spain", "sweden", "uk"),5)),
                           "variable" = c(rep("gen_imm_escape",7),rep("gen_imm_work",7),rep("gen_imm_study",7),rep("gen_imm_family_war",7),rep("gen_imm_marriage",7)),
                           "mean" = c(tapply(df_W2$gen_imm_escape,df_W2$nationality,mean,na.rm=TRUE),
                                      tapply(df_W2$gen_imm_work,df_W2$nationality,mean,na.rm=TRUE),
                                      tapply(df_W2$gen_imm_study,df_W2$nationality,mean,na.rm=TRUE),
                                      tapply(df_W2$gen_imm_family_war,df_W2$nationality,mean,na.rm=TRUE),
                                      tapply(df_W2$gen_imm_marriage,df_W2$nationality,mean,na.rm=TRUE)))

gen_imm_plot_overall <- data.frame(variable = c("gen_imm_escape","gen_imm_work", "gen_imm_study", "gen_imm_family_war", "gen_imm_marriage"),
                                   overall_mean = c(mean(df_W2$gen_imm_escape,na.rm=TRUE),
                                                    mean(df_W2$gen_imm_work,na.rm=TRUE),
                                                    mean(df_W2$gen_imm_study,na.rm=TRUE),
                                                    mean(df_W2$gen_imm_family_war,na.rm=TRUE),
                                                    mean(df_W2$gen_imm_marriage,na.rm=TRUE)))

ggplot(gen_imm_plot) +
  geom_bar(aes(x=nationality, y=mean), stat="identity") +
  facet_grid(variable ~ .) +
  geom_hline(data = gen_imm_plot_overall, aes(yintercept = overall_mean), linetype="dashed", color = "red")

# escape: germany, poland, spain, and sweden more positive than average; hungary, romania and UK less
# family: poland, spain and sweden more positive; germany, hungary, romania, and uk less
# marriage: poland, romania, spain more positive; germany, hungary, and uk less
# study: poland romania, spain more positive; germany, hungary, sweden, and uk less
# work: sweden more positive; germany, hungary, poland less

# let's create a simple, additive scale, just to get a sense of who's positive across these categories, and who's not:
df_W2$gen_imm_sum <- rowSums(df_W2[,c(8:12)])
table1(~ gen_imm_sum | nationality, data=df_W2)
# poland most positive; uk least positive

# ess immigration attitudes
table1(~ gen_imm_ess_good_bad + gen_imm_ess_jobs + gen_imm_ess_welfare + gen_imm_ess_safety | nationality, data=df_W2)

gen_imm_ess_plot <- data.frame("nationality" = (rep(c("germany", "hungary", "poland", "romania", "spain", "sweden", "uk"),4)),
                           "variable" = c(rep("gen_imm_ess_good_bad",7),rep("gen_imm_ess_jobs",7),rep("gen_imm_ess_welfare",7),rep("gen_imm_ess_safety",7)),
                           "mean" = c(tapply(df_W2$gen_imm_ess_good_bad,df_W2$nationality,mean,na.rm=TRUE),
                                      tapply(df_W2$gen_imm_ess_jobs,df_W2$nationality,mean,na.rm=TRUE),
                                      tapply(df_W2$gen_imm_ess_welfare,df_W2$nationality,mean,na.rm=TRUE),
                                      tapply(df_W2$gen_imm_ess_safety,df_W2$nationality,mean,na.rm=TRUE)))

gen_imm_ess_plot_overall <- data.frame(variable = c("gen_imm_ess_good_bad","gen_imm_ess_jobs", "gen_imm_ess_welfare", "gen_imm_ess_safety"),
                                   overall_mean = c(mean(df_W2$gen_imm_ess_good_bad,na.rm=TRUE),
                                                    mean(df_W2$gen_imm_ess_jobs,na.rm=TRUE),
                                                    mean(df_W2$gen_imm_ess_welfare,na.rm=TRUE),
                                                    mean(df_W2$gen_imm_ess_safety,na.rm=TRUE)))

ggplot(gen_imm_ess_plot) +
  geom_bar(aes(x=nationality, y=mean), stat="identity") +
  facet_grid(variable ~ .) +
  geom_hline(data = gen_imm_ess_plot_overall, aes(yintercept = overall_mean), linetype="dashed", color = "red")

# good for economy: poland, romania, spain
# bad for economy: germany, hungary, sweden, uk

# take jobs: hungary, uk
# create jobs: poland, romania, sweden

# more safe: poland, romania, spain
# less safe: germany, sweden, uk

# take out more: germany, hungary, spain, sweden, uk
# put in more: poland, romania

# emotions about immigration
table1(~ imm_anger + imm_fear + imm_hope + imm_sympathy | nationality, data=df_W2)

imm_emotions_plot <- data.frame("nationality" = (rep(c("germany", "hungary", "poland", "romania", "spain", "sweden", "uk"),4)),
                               "variable" = c(rep("imm_anger",7),rep("imm_fear",7),rep("imm_hope",7),rep("imm_sympathy",7)),
                               "mean" = c(tapply(df_W2$imm_anger,df_W2$nationality,mean,na.rm=TRUE),
                                          tapply(df_W2$imm_fear,df_W2$nationality,mean,na.rm=TRUE),
                                          tapply(df_W2$imm_hope,df_W2$nationality,mean,na.rm=TRUE),
                                          tapply(df_W2$imm_sympathy,df_W2$nationality,mean,na.rm=TRUE)))

imm_emotions_plot_overall <- data.frame(variable = c("imm_anger","imm_fear", "imm_hope", "imm_sympathy"),
                                       overall_mean = c(mean(df_W2$imm_anger,na.rm=TRUE),
                                                        mean(df_W2$imm_fear,na.rm=TRUE),
                                                        mean(df_W2$imm_hope,na.rm=TRUE),
                                                        mean(df_W2$imm_sympathy,na.rm=TRUE)))

ggplot(imm_emotions_plot) +
  geom_bar(aes(x=nationality, y=mean), stat="identity") +
  facet_grid(variable ~ .) +
  geom_hline(data = imm_emotions_plot_overall, aes(yintercept = overall_mean), linetype="dashed", color = "red")

# a lot of anger: germany, hungary, sweden, uk
# a lot of fear: germany, hungary
# a lot of sympathy: poland, romania, spain, sweden 

# immigration size
table1(~ imm_size | nationality, data=df_W2)

# free movement and EU identity
table1(~ free_move_protect_jobs + free_move_protect_services + eu_nat_identity | nationality, data=df_W2)

free_move_plot <- data.frame("nationality" = (rep(c("germany", "hungary", "poland", "romania", "spain", "sweden", "uk"),3)),
                                "variable" = c(rep("free_move_protect_jobs",7),rep("free_move_protect_services",7),rep("eu_nat_identity",7)),
                                "mean" = c(tapply(df_W2$free_move_protect_jobs,df_W2$nationality,mean,na.rm=TRUE),
                                           tapply(df_W2$free_move_protect_services,df_W2$nationality,mean,na.rm=TRUE),
                                           tapply(df_W2$eu_nat_identity,df_W2$nationality,mean,na.rm=TRUE)))

free_move_plot_overall <- data.frame(variable = c("free_move_protect_jobs","free_move_protect_services", "eu_nat_identity"),
                                        overall_mean = c(mean(df_W2$free_move_protect_jobs,na.rm=TRUE),
                                                         mean(df_W2$free_move_protect_services,na.rm=TRUE),
                                                         mean(df_W2$eu_nat_identity,na.rm=TRUE)))

ggplot(free_move_plot) +
  geom_bar(aes(x=nationality, y=mean), stat="identity") +
  facet_grid(variable ~ .) +
  geom_hline(data = free_move_plot_overall, aes(yintercept = overall_mean), linetype="dashed", color = "red")

# eu threatens national identity: sweden, uk
# restrict free movement to protect jobs: germany, spain, sweden, uk
# restrict free movement to protect public services: germany, spain, sweden, uk 

# general knowledge
table1(~ gen_know_switzerland + gen_know_ep + gen_know_party | nationality, data=df_W2)

# specific knowledge about immigration
table1(~ mig_know_free_move + mig_know_schengen + mig_know_asylum + mig_know_syrians | nationality, data=df_W2)

# knowledge sums and proportions
table1(~ gen_know_sum + mig_know_sum | nationality, data=df_W2)
table1(~ gen_know_prop + mig_know_prop | nationality, data=df_W2)

know_plot <- data.frame("nationality" = (rep(c("germany", "hungary", "poland", "romania", "spain", "sweden", "uk"),2)),
                             "variable" = c(rep("gen_know_prop",7),rep("mig_know_prop",7)),
                             "mean" = c(tapply(df_W2$gen_know_prop,df_W2$nationality,mean,na.rm=TRUE),
                                        tapply(df_W2$mig_know_prop,df_W2$nationality,mean,na.rm=TRUE)))

know_plot_overall <- data.frame(variable = c("gen_know_prop","mig_know_prop"),
                                     overall_mean = c(mean(df_W2$gen_know_prop,na.rm=TRUE),
                                                      mean(df_W2$mig_know_prop,na.rm=TRUE)))

ggplot(know_plot) +
  geom_bar(aes(x=nationality, y=mean), stat="identity") +
  facet_grid(variable ~ .) +
  geom_hline(data = know_plot_overall, aes(yintercept = overall_mean), linetype="dashed", color = "red")

# high general knowledge: germany, hungary, poland
# high specific knowledge: germany, hungary, spain, sweden

# correlation between general and specific knowledge
cor(df_W2$gen_know_sum, df_W2$mig_know_sum) # 0.4283856

# save df as CSV
write.csv(df_W2, "data/df_preprocessed_W2.csv", row.names = FALSE)
