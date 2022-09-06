set.seed(100)

# load packages
library(psych)
library(ggplot2)
library(lavaan)
library(mirt)
library(polycor)
library(texreg)

# read in data
model_data <- read.csv("data/model_data.csv")

# all items
all_items <- data.frame(model_data$gen_know_ep,
                        model_data$gen_know_party,
                        model_data$gen_know_switzerland,
                        model_data$mig_know_asylum,
                        model_data$mig_know_free_move,
                        model_data$mig_know_schengen,
                        model_data$mig_know_syrians)

# parallel analysis with scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(all_items, cor="tet") # 4 factors

# let's look at these with efa, using polychoric correlations
efa_poly <- fa.poly(all_items, nfactors=4)
print(efa_poly$fa$loadings, cutoff=0.3)
# three of the immigration items load well on one factor; two general on another

# let's try to remove the fourth immigration item (asylum)
all_items <- data.frame(model_data$gen_know_ep,
                        model_data$gen_know_party,
                        model_data$gen_know_switzerland,
                        # model_data$mig_know_asylum,
                        model_data$mig_know_free_move,
                        model_data$mig_know_schengen,
                        model_data$mig_know_syrians)

# parallel analysis with scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(all_items, cor="tet") # 3 factors

# let's look at these with efa
efa_poly <- fa.poly(all_items, nfactors=3)
print(efa_poly$fa$loadings, cutoff = 0.3)
# know_party variable is still the one on the third factor

# let's see what happens if we run a parallel analysis on just the three general items
know_items_gen <- data.frame(model_data$gen_know_ep,
                             model_data$gen_know_party,
                             model_data$gen_know_switzerland)

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_gen, cor="tet") 
# comes out to 1 factor

# let's do the same for the immigration variables
know_items_imm <- data.frame(model_data$mig_know_asylum,
                             model_data$mig_know_free_move,
                             model_data$mig_know_schengen,
                             model_data$mig_know_syrians)

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_imm, cor="tet") 
# comes out to 2 factors

# so let's remove the asylum variable, as earlier
know_items_imm <- data.frame(model_data$mig_know_free_move,
                             model_data$mig_know_schengen,
                             model_data$mig_know_syrians)

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_imm, cor="tet") 
# comes out to 1 factor

# so it seems there's evidence of at least two dimensions: one for general knowledge, and one for immigration knowledge.
# but let's investigate further by looking at how a couple of different unidimensional and two-dimensional models
# fare in terms of fit by using cfa.

# cfa on unidimensional model with six items
# lavaan with dichotomous data: https://lavaan.ugent.be/tutorial/cat.html
mod_1f_6_items <- 'knowledge =~ gen_know_ep + gen_know_party + gen_know_switzerland + mig_know_free_move + mig_know_schengen + mig_know_syrians'
mod_1f_6_items.fit <- cfa(mod_1f_6_items, data=model_data, ordered = TRUE)
summary(mod_1f_6_items.fit, standardized=TRUE)
# the Std.all column gives the type of loadings we would see in efa correlations between items and its latent. we want > 0.3.

# let's create a data-frame where we'll save fit measures for our models
fit_measures <- data.frame("measure" = c("RMSEA","CFI","TLI","AGFI","X2","df"),
                           "1f_6_items" = NA,
                           "1f_5_items" = NA,
                           "2f_6_items" = NA,
                           "2f_5_items" = NA)

fit_measures$X1f_6_items <- c(fitmeasures(mod_1f_6_items.fit)["rmsea"], 
                              fitmeasures(mod_1f_6_items.fit)["cfi"], 
                              fitmeasures(mod_1f_6_items.fit)["tli"], 
                              fitmeasures(mod_1f_6_items.fit)["agfi"], 
                              fitmeasures(mod_1f_6_items.fit)["chisq"], 
                              fitmeasures(mod_1f_6_items.fit)["df"])
fit_measures$X1f_6_items <- round(fit_measures$X1f_6_items,3)

# explanation of measures here: https://stats.oarc.ucla.edu/spss/seminars/introduction-to-factor-analysis/a-practical-introduction-to-factor-analysis-confirmatory-factor-analysis/
# want root mean square error of approximation (RMSEA) <0.06. (Dima 2018)
# RMSEA values less than 0.05 or 0.01 correspond to good and very good fit respectively (Andrews 2021)
# want Comparative Fit Index (CFI) >0.95 (Dima 2018)
# want Tucker-Lewis index (TLI) >0.95 (Dima 2018)
# agfi: adjusted goodness of fit; 0-1, higher better
# chisq: the chi-square statistic we obtain from the maximum likelihood statistic (similar to the EFA)
# df: degrees of freedom

# cfa on unidimensional model with five items
mod_1f_5_items <- 'knowledge =~ gen_know_ep + gen_know_switzerland + mig_know_free_move + mig_know_schengen + mig_know_syrians'
mod_1f_5_items.fit <- cfa(mod_1f_5_items, data=model_data, ordered = TRUE)
summary(mod_1f_5_items.fit, standardized=TRUE)

fit_measures$X1f_5_items <- c(fitmeasures(mod_1f_5_items.fit)["rmsea"], 
                              fitmeasures(mod_1f_5_items.fit)["cfi"], 
                              fitmeasures(mod_1f_5_items.fit)["tli"], 
                              fitmeasures(mod_1f_5_items.fit)["agfi"], 
                              fitmeasures(mod_1f_5_items.fit)["chisq"],
                              fitmeasures(mod_1f_5_items.fit)["df"])
fit_measures$X1f_5_items <- round(fit_measures$X1f_5_items,3)

# cfa on two-dimensional model with six items
mod_2f_6_items <- '
gen_knowledge =~ gen_know_ep + gen_know_party + gen_know_switzerland
mig_knowledge =~ mig_know_free_move + mig_know_schengen + mig_know_syrians'
mod_2f_6_items.fit <- cfa(mod_2f_6_items, data=model_data, ordered = TRUE)
summary(mod_2f_6_items.fit, standardized=TRUE)

fit_measures$X2f_6_items <- c(fitmeasures(mod_2f_6_items.fit)["rmsea"], 
                              fitmeasures(mod_2f_6_items.fit)["cfi"], 
                              fitmeasures(mod_2f_6_items.fit)["tli"], 
                              fitmeasures(mod_2f_6_items.fit)["agfi"], 
                              fitmeasures(mod_2f_6_items.fit)["chisq"], 
                              fitmeasures(mod_2f_6_items.fit)["df"])
fit_measures$X2f_6_items <- round(fit_measures$X2f_6_items,3)

# cfa on two-dimensional model with five items
mod_2f_5_items <- '
gen_knowledge =~ gen_know_ep + gen_know_switzerland
mig_knowledge =~ mig_know_free_move + mig_know_schengen + mig_know_syrians'
mod_2f_5_items.fit <- cfa(mod_2f_5_items, data=model_data, ordered = TRUE)
summary(mod_2f_5_items.fit, standardized=TRUE)

fit_measures$X2f_5_items <- c(fitmeasures(mod_2f_5_items.fit)["rmsea"], 
                              fitmeasures(mod_2f_5_items.fit)["cfi"], 
                              fitmeasures(mod_2f_5_items.fit)["tli"], 
                              fitmeasures(mod_2f_5_items.fit)["agfi"], 
                              fitmeasures(mod_2f_5_items.fit)["chisq"],
                              fitmeasures(mod_2f_5_items.fit)["df"])
fit_measures$X2f_5_items <- round(fit_measures$X2f_5_items,3)

# we see that the 5-item two-dimensional model is the 'best' one in terms of fit; arguably, it has excellent fit.
# but both unidimensional models exhibit very good fit as well, albeit the 5-item one more than the 6-item.

# we can also test whether the improved fit is significant in moving from 1 to 2 dimensions
anova(mod_1f_5_items.fit,
      mod_2f_5_items.fit) # significant
anova(mod_1f_6_items.fit,
      mod_2f_6_items.fit) # significant

# in conclusion: the result of parallel analysis and superior fit of the two-dimensional models offers
# evidence that general knowledge and immigration knowledge constitute separate latent traits.
# at the same time, the really quite good fit of the two unidimensional models means that we should not treat
# the generalist assumption as obviously misguided.

# the way to proceed then is as follows: create two scales, in line with the evidence of two-dimensionality.
# if the generalist assumption holds, the effects of being fully informed on each of these scales should
# be roughly the same, as given by the coefficient sizes (and directions) of the knowledge variables of
# on counterfactual models of such full information.

# so let's start by building two IRT models

# general knowledge scale
# here  we need to use all 3 items; if not, we won't have enough degrees of freedom
# number of unique response patterns: 4 ([0,0],[1,0],[0,1],[1,1])
# so df = 4 - 1 = 3
# parameters to estimate for two items in a 2PL IRT: 2 intercepts * 2 slopes = 4
# good thing that using all 3 is defensible given the good fit of mod_2f_6_items.fit

# save general knowledge items in separate df
know_items_gen <- data.frame(model_data$gen_know_ep,
                             model_data$gen_know_party,
                             model_data$gen_know_switzerland)

# fit 2PL (two parameter) model
know_scale_gen <- mirt(data=know_items_gen,
                        model=1,
                        itemtype = "2PL")

# have a look at how well the items load (we want > 0.3 in F1 column)
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
summary(model_data$know_score_general) # mean of 0
sd(model_data$know_score_general) # sd of 0.74

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
know_items_imm <- data.frame(model_data$mig_know_free_move,
                             model_data$mig_know_schengen,
                             model_data$mig_know_syrians)

know_scale_imm <- mirt(data=know_items_imm,
                       model=1,
                       itemtype = "2PL")

summary(know_scale_imm)
plot(know_scale_imm, type="trace")
plot(know_scale_imm, type="info")
coef(know_scale_imm, IRTpars=T)
model_data$know_score_imm <- fscores(know_scale_imm)[,1]
summary(model_data$know_score_imm) # mean of 0
sd(model_data$know_score_imm) # sd of 0.7

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_imm, cor="tet") # unidimensional

Q3resid <- data.frame(residuals(know_scale_imm, type="Q3"))

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
prop.table(table(model_data$know_score_general_binary))

quantile(model_data$know_score_imm)
table(model_data$know_score_imm)
ggplot(model_data, aes(x=know_score_imm)) + 
  geom_histogram(color="black", fill="white", binwidth=0.1)
max_know_score_imm <- 0.7
model_data$know_score_imm_binary <- NA
model_data$know_score_imm_binary[model_data$know_score_imm > max_know_score_imm] <- 1
model_data$know_score_imm_binary[model_data$know_score_imm <= max_know_score_imm] <- 0
table(model_data$know_score_imm_binary)
prop.table(table(model_data$know_score_imm_binary))

# construct validity
m_gen <- lm(know_score_general ~
              gender + # men know more
              age + # older know more
              education_ISCED, # more educated know more
            data = model_data)
summary(m_gen)

m_imm <- lm(know_score_imm ~
              gender + # men know more
              age + # older know more
              education_ISCED, # more educated know more
            data = model_data)
summary(m_imm)

# print regression tables
texreg::wordreg(list(m_gen,m_imm),
               file="tables/construct_validity_table.docx",
               single.row = TRUE, 
               caption = "Main effects (OLS with robust standard errors)",
               custom.model.names=c("General knowledge",
                                    "Immigration knowledge"),
               custom.coef.names = c("(Intercept)", "Male", "Age", "Education"),
               caption.above = TRUE,
               float.pos = "h!",
               custom.note="%stars.",
               stars = c(0.001, 0.01, 0.05),
               digits=3)

# save df as CSV
write.csv(model_data, "data/model_data_IRT.csv", row.names = FALSE)
