# Revisiting the Measurement and Dimensionality of Political Knowledge: Evidence from Seven European Countries

William L Allen (University of Oxford)  
Kristoffer Ahlstrom-Vij (Birkbeck, University of London)

## Abstract

Knowledge matters for the political preferences people form, and the choices they make. Despite the recent growth of experimental work, however, the bulk of research on the role of knowledge in politics still uses observational data, where researchers typically rely on batteries of general political knowledge questions to distinguish more informed voters from less informed ones. When used to explain outcomes across policy-specific domains, this practice invokes what we call a generalist assumption: possessing general knowledge is diagnostic of holding issue-specific knowledge (and vice versa). As the list of geographical areas and issue domains of interest to political scientists grow, the weight put on this assumption increases significantly; so, is it warranted?  

Using 2018 survey data from seven European countries (Germany, Hungary, Poland, Romania, Spain, Sweden, UK) that, unusually, includes knowledge questions about both general politics as well as about EU immigration (N = 10,749), we combine several approaches to test the core expectations of the generalist assumption: 

- First, exploratory and confirmatory factor analyses suggest that the two sets of questions are plausibly unidimensional. 
- Second, after constructing Item Response Theory (IRT) knowledge scales from the items, we demonstrate how both scales also display similar associations with key respondent features (gender, age, and education) that align with prior expectations about who typically holds more political knowledge. 
- Finally, using two reference sets from BES (Wave 17) and ANES (2019 Pilot study), we show that the estimated marginal mean level of general political knowledge by education, gender, age and (where available) income exhibit the same pattern not only on the two scales above, but also on independent scales measuring knowledge on public health and climate change. 

We conclude that the preponderance of evidence points to the generalist assumption being defensible at least as it relates to the range of areas considered here, i.e., immigration, public health, and climate change. However, we caution against deploying it uncritically and offer our code as a means of enabling others to conduct similar stress-tests.

## Files

The R scripts in the `R` folder are meant to be run in the following order:

1. `preprocessing.R` - Reads in the SPSS data from `data/10085_da_en_v1_0.zsav` and pre-processes it. Outputs `data/df_preprocessed.csv`. 
2. `descriptive_stats.R` - Reads in pre-processed data from the previous step and subsets it to only include Wave 2 data, and shows some basic tables and plots by way of descriptive statistics. Outputs `data/df_preprocessed_W2.csv`.
3. `imputation.R` - Reads in the pre-processed data from the previous step and imputes all missing values using multiple imputation, to prepare for modeling. Outputs `data/model_data.csv`. 
4. `scales.R` - Reads in the data from the previous step, conducts a series of factor analyses to investigate dimensionality, and builds two scales using IRT modeling, and tests these for construct validity. Outputs `data/model_data_IRT.csv`.
5. `marginal_means.R` - Reads in the data from the previous step along with a number of other data sets, to see whether the estimated mean level of knowledge on two established knowledge scale from BES and ANES exhibit similar patterns as the mean levels on the above scales and on a number of other issue-specific knowledge scales.

All plots are saved in the `plots` folder and all tables to the `plots` folder.

## Data

The primary data set for this project is available at https://data.aussda.at/dataset.xhtml?persistentId=doi:10.11587/LBSMPQ. Any use of that data set is subject to the terms provided there.

## Requirements

The code in this repository was produced using R 4.1.2 on macOS Catalina (10.15.7).
