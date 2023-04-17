# Is general political knowledge indicative of issue-specific knowledge? Stress-testing the generalist assumption

## Abstract

Using general political knowledge questions to explain variation in policy-specific outcomes invokes what we call a *generalist assumption*: possessing general knowledge is indicative of unmeasured, issue-specific knowledge. Yet the empirical evidence on which this assumption rests is limited in terms of sample coverage and recency. Is it warranted? Using convergent evidence from best-available U.S. and European surveys (total N=82,919), we argue that it is. First, factor analysis applied to cross-national European data that, unusually, includes items about general politics and the specific issue of EU immigration reveals plausible unidimensionality. Second, knowledge scales built using Item Response Theory modeling display similar associations with known correlates of political knowledge -- gender, age, education -- that are mirrored in established ANES, CES, and BES general knowledge measures. Moreover, we find that these patterns remain after extending our analysis to a survey testing issue-specific knowledge about COVID-19 in the U.K., although a similar analysis of knowledge about climate change in the U.K., Germany, Norway, and France exhibits different patterns for age and gender in some regions. This highlights the need not to deploy the generalist assumption uncritically. We conclude with practical guidance and offer reproducable code for stress-testing unidimensionality assumptions.

## R Scripts

The scripts in the `R` folder are listed below. Steps 1-3 in the manuscript's empirical analysis (see the `markdown` folder) correspond to scripts 4-6 below.

1. `preprocessing.R` - Reads in the REMINDER project data from `data/10085_da_en_v1_0.zsav` and pre-processes it. Outputs `data/df_preprocessed.csv`. 
2. `descriptive_stats.R` - Reads in pre-processed data from the previous step and subsets it to only include Wave 2 data, and shows some basic tables and plots by way of descriptive statistics in relation to immigration attitudes and knowledge. Outputs `data/df_preprocessed_W2.csv`.
3. `imputation.R` - Reads in the pre-processed data from the previous step and imputes all missing values using multiple imputation, to prepare for analysis. Outputs `data/model_data.csv`. 
4. `factor_analysis.R` - Reads in the data from the previous step and conducts a series of factor analyses to investigate dimensionality of the knowledge items.
5. `construct_validity.R` - Reads in the data from step 3, constructs two IRT knowledge scales (general knowledge and immigration knowledge), and tests for construct validity by investigating associations with known correlates of political knowledge and with well-established ANES, CES, and BES knowledge measures using estimated marginal means.
6. `other_domains.R` - Reads in the data from step 3 and investigates estimated marginal means of knowledge in two, other domains: public health (COVID-19) and climate change.

All plots are saved in the `plots` folder and all tables to the `tables` folder. The accompanying manuscript along with an appendix with supporting information can be found in the `markdown` folder. 

## Data

The primary, REMINDER data set for this project is available at https://data.aussda.at/dataset.xhtml?persistentId=doi:10.11587/LBSMPQ. Any use of that data set is subject to the terms provided there.

## Requirements

The code in this repository was produced using R 4.1.2 on macOS Monterey (12.6.3).
