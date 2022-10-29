set.seed(100)

# load packages
library(psych)
library(lavaan)
library(mirt)
library(polycor)

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
psych::fa.parallel(all_items, cor="tet", fa="fa") # 4 factors

# let's look at these with efa, using polychoric correlations
efa_poly <- fa.poly(all_items, 4)
print(efa_poly$fa$loadings)
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
psych::fa.parallel(all_items, cor="tet", fa="fa") # 3 factors

# let's look at these with efa
efa_poly <- fa.poly(all_items, nfactors=3)
print(efa_poly$fa$loadings, cutoff = 0.3)
# know_party variable is still the one on the third factor

# let's see what happens if we run a parallel analysis on just the three general items
know_items_gen <- data.frame(model_data$gen_know_ep,
                             model_data$gen_know_party,
                             model_data$gen_know_switzerland)

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_gen, cor="tet", fa="fa") 
# comes out to 1 factor

# let's do the same for the immigration variables
know_items_imm <- data.frame(model_data$mig_know_asylum,
                             model_data$mig_know_free_move,
                             model_data$mig_know_schengen,
                             model_data$mig_know_syrians)

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_imm, cor="tet", fa="fa") 
# comes out to 2 factors

# so let's remove the asylum variable, as earlier
know_items_imm <- data.frame(model_data$mig_know_free_move,
                             model_data$mig_know_schengen,
                             model_data$mig_know_syrians)

par(mfrow=c(1, 1))
psych::fa.parallel(know_items_imm, cor="tet", fa="fa") 
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
fit_measures <- data.frame("measure" = c("RMSEA","CFI","TLI","AGFI"),
                           "1f_6_items" = NA,
                           "1f_5_items" = NA,
                           "2f_6_items" = NA,
                           "2f_5_items" = NA)

fit_measures$X1f_6_items <- c(fitmeasures(mod_1f_6_items.fit)["rmsea"], 
                              fitmeasures(mod_1f_6_items.fit)["cfi"], 
                              fitmeasures(mod_1f_6_items.fit)["tli"], 
                              fitmeasures(mod_1f_6_items.fit)["agfi"])
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
                              fitmeasures(mod_1f_5_items.fit)["agfi"])
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
                              fitmeasures(mod_2f_6_items.fit)["agfi"])
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
                              fitmeasures(mod_2f_5_items.fit)["agfi"])
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