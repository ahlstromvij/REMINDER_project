set.seed(100)

# load packages
library(psych)
library(mirt)
library(ggplot2)

# read in data
model_data <- read.csv("data/model_data.csv")

# general knowledge scale
# save general knowledge items in separate df
know_items_gen <- data.frame(model_data$gen_know_ep,
                             model_data$gen_know_party,
                             model_data$gen_know_switzerland)

# fit 2PL (two parameter) model
know_scale_gen <- mirt(data=know_items_gen,
                        model=1,
                        itemtype = "2PL")

# have a look at how well the items load (we want > 0.4 in F1 column)
summary(know_scale_gen)

# plot individual item trace lines (steepness shows how well it discriminates)
plot(know_scale_gen, type="trace")

# plot test information (shows where on the scale it discriminates well, with theta = 0 representing mean ability)
plot(know_scale_gen, type="info")

# look at how discriminating the individual items are (ideally want discrimination [i.e., a] values > 1)
coef(know_scale_gen, IRTpars=T)

# save scores to df
model_data$know_score_general <- fscores(know_scale_gen)[,1]

# look at quick summary of scores (e.g., min, max, and mean)
summary(model_data$know_score_general)

# unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(know_items_gen, cor="tet") # unidimensional

# Q3 for local independence (ideally no higher than +/-0.2, but short scales tend to give higher values)
Q3resid <- data.frame(residuals(know_scale_gen, type="Q3")) # max is -0.411

# evaluate model fit visually
itemfit(know_scale_gen, empirical.plot = 1)
itemfit(know_scale_gen, empirical.plot = 2)
itemfit(know_scale_gen, empirical.plot = 3)


# immigration knowledge scale
know_items_imm <- data.frame(#model_data$mig_know_asylum,
                             model_data$mig_know_free_move,
                             model_data$mig_know_schengen, # unidimensional if removed
                             model_data$mig_know_syrians)

know_scale_imm <- mirt(data=know_items_imm,
                       model=1,
                       itemtype = "2PL")

summary(know_scale_imm)
plot(know_scale_imm, type="trace")
plot(know_scale_imm, type="info")
coef(know_scale_imm, IRTpars=T)
model_data$know_score_imm <- fscores(know_scale_imm)[,1]
summary(model_data$know_score_imm)

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_imm, cor="tet") # unidimensional

Q3resid <- data.frame(residuals(know_scale_imm, type="Q3")) # max = -0.436

itemfit(know_scale_imm, empirical.plot = 1)
itemfit(know_scale_imm, empirical.plot = 2)
itemfit(know_scale_imm, empirical.plot = 3)

# create binary knowledge variables for 'full information'
quantile(model_data$know_score_general)
table(model_data$know_score_general)
ggplot(model_data, aes(x=know_score_general)) + 
  geom_histogram(color="black", fill="white", binwidth=0.1)
max_know_score_general <- 0.7
model_data$know_score_general_binary <- NA
model_data$know_score_general_binary[model_data$know_score_general > max_know_score_general] <- 1
model_data$know_score_general_binary[model_data$know_score_general <= max_know_score_general] <- 0
table(model_data$know_score_general_binary)

quantile(model_data$know_score_imm)
table(model_data$know_score_imm)
ggplot(model_data, aes(x=know_score_imm)) + 
  geom_histogram(color="black", fill="white", binwidth=0.1)
max_know_score_imm <- 0.7
model_data$know_score_imm_binary <- NA
model_data$know_score_imm_binary[model_data$know_score_imm > max_know_score_imm] <- 1
model_data$know_score_imm_binary[model_data$know_score_imm <= max_know_score_imm] <- 0
table(model_data$know_score_imm_binary)

# save df as CSV
write.csv(model_data, "data/model_data_IRT.csv", row.names = FALSE)
