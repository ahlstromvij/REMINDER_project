# Is possessing general political knowledge diagnostic of holding issue-specific knowledge? Stress-testing the generalist assumption

William L Allen (University of Oxford)  
Kristoffer Ahlstrom-Vij (Birkbeck, University of London)

## Abstract

Using general political knowledge questions to explain variation in policy-specific outcomes invokes a potentially strong generalist assumption: possessing general knowledge is diagnostic of holding unmeasured issue-specific knowledge. Is this warranted? Using convergent evidence drawn from best-available US and European surveys, we argue it is. First, factor analysis applied to a cross-national 2018 European data set that, unusually, includes questions about general politics and the specific issue of EU immigration reveals how the questions produce a plausibly unidimensional set. Second, knowledge scales built using Item Response Theory (IRT) modeling of responses to each set of questions display similar associations with known correlates of political knowledge--gender, age, and education--that are mirrored in established ANES, CES, and BES general knowledge measures. Finally, these patterns remain after extending our analysis to surveys that test knowledge about COVID-19 and climate change. We propose steps for conducting similar stress-tests on unidimensionality assumptions.

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

The code in this repository was produced using R 4.1.2 on macOS Monterey (12.6).
