# Is possessing general political knowledge diagnostic of holding issue-specific knowledge? Stress-testing the generalist assumption

William L Allen (University of Oxford)  
Kristoffer Ahlstrom-Vij (Birkbeck, University of London)

## Abstract

The practice of using general political knowledge questions to explain variation in policy-specific outcomes invokes a potentially strong generalist assumption: possessing general knowledge is diagnostic of holding issue-specific knowledge. Is this warranted? Using convergent evidence drawn from best-available US and European surveys, we argue it is. First, factor analysis applied to a cross-national 2018 European data that, unusually, include questions about general politics and the specific issue of EU immigration reveals how combining the questions produces a plausibly unidimensional set. Second, knowledge scales built using Item Response Theory (IRT) modeling of responses to each set of questions display similar associations with known correlates of political knowledge--gender, age, and education--including by country and with well-established ANES, CES, and BES knowledge measures. Finally, these patterns remain after extending our analysis to surveys that test knowledge about public health (specifically COVID-19) and climate change. We propose steps for conducting similar stress-tests on unidimensionality assumptions.

## Files

The R scripts in the `R` folder are meant to be run in the following order:

1. `preprocessing.R` - Reads in the REMINDER project data from `data/10085_da_en_v1_0.zsav` and pre-processes it. Outputs `data/df_preprocessed.csv`. 
2. `descriptive_stats.R` - Reads in pre-processed data from the previous step and subsets it to only include Wave 2 data, and shows some basic tables and plots by way of descriptive statistics in relation to immigration attitudes and knowledge. Outputs `data/df_preprocessed_W2.csv`.
3. `imputation.R` - Reads in the pre-processed data from the previous step and imputes all missing values using multiple imputation, to prepare for modeling. Outputs a 'table 1' as well as `data/model_data.csv`. 
4. `scales.R` - Reads in the data from the previous step, conducts a series of factor analyses to investigate dimensionality, and builds two scales using IRT modeling, and tests these for construct validity. Outputs `data/model_data_IRT.csv`.
5. `marginal_means.R` - Reads in the data from the previous step along with a number of other data sets, to see whether the estimated mean level of knowledge on two established knowledge scale from BES, ANES, and CES exhibit similar patterns as the mean levels on the above scales and on a number of other issue-specific knowledge scales.

All plots are saved in the `plots` folder and all tables to the `tables` folder. The accompanying manuscript along with an appendix with supporting information can be found in the `markdown` folder. 

## Data

The primary data set for this project is available at https://data.aussda.at/dataset.xhtml?persistentId=doi:10.11587/LBSMPQ. Any use of that data set is subject to the terms provided there.

## Requirements

The code in this repository was produced using R 4.1.2 on macOS Catalina (10.15.7).
