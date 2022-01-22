# Revisiting the Measurement and Consequences of Political Knowledge: Evidence from Seven European Countries on EU Immigration

William L Allen (University of Oxford)  
Kristoffer Ahlstrom-Vij (Birkbeck, University of London)

## Abstract

In most theories of democratic functioning, citizens are assumed to be well-represented when political decisions are made in accordance with their political preferences. However, what if there were substantial gaps between the distribution of preferences as captured in surveys and the distribution that would have likely been observed had citizens possessed more knowledge about the relevant issues? This would raise questions about whether the commitments reported by the public in polls reveal what they truly want—and, in turn, whether policies implemented on the basis of these reports make for good political representation. Such questions are particularly resonant now amid concerns about the political impacts of mis- and disinformation given the long shadow of Trumpism.  

Claiming that citizens’ true political preferences are the ones they would have held had they been more fully-informed is not to resort to elitism. It simply takes the political sentiments of the public seriously enough not to confuse them with whims and personal tastes. Rather than political preferences floating freely from factual beliefs, appeals to facts are integral to how people make sense of their values. All the more concerning, then, that low levels of political knowledge among voters are well-established, and that the tradition of information effects scholarship has uncovered ample evidence of actual distributions of political expression—be it in the form of votes or reported preferences—diverging from what would likely have been seen had people been more informed. 

However, most of this work on information effects relies on batteries of general political knowledge questions to distinguish more informed voters from less informed ones: rarely do large-scale observational surveys directly test respondents’ knowledge about particular topics. This invokes a potentially strong assumption: possessing general knowledge should be diagnostic of holding issue-specific knowledge. We empirically test this assumption using 2018 survey data from seven European countries (Germany, Hungary, Poland, Romania, Spain, Sweden, UK) that, unusually, includes knowledge questions about general politics as well as EU immigration (N=11,429). After building knowledge scales using item response theory, we construct counterfactual models that simulate how becoming more informed about either general politics or EU migration changes respondents’ immigration attitudes and preferences (an “information effect.”) Then, we compare the sizes and directions of these effects across both types of knowledge questions. 

Our study, arising from a longer-standing collaboration between an empirical comparative political scientist (Allen) and a social epistemologist (Ahlstrom-Vij), makes two contributions that connect political theory and empirical enquiry: (1) it provides novel cross-national evidence of how and to what extent EU immigration attitudes and preferences are sensitive to citizens’ levels of different kinds of knowledge, and (2) it tests the validity of using general political knowledge questions as a proxy for respondents’ specific knowledge about issues. This presents practical implications for information effects research that depends on these common survey question types, and invites normative reflection on the state of political representation.

## Files

The R scripts in the `R` folder are meant to be run in the following order:

1. `preprocessing.R` - Reads in the SPSS data from `data/10085_da_en_v1_0.zsav` and pre-processes it. Outputs `data/df_preprocessed.csv`. 
2. `descriptive_stats.R` - Reads in pre-processed data from the previous step and subsets it to only include Wave 2 data, and shows some basic tables and plots by way of descriptive statistics. Outputs `data/df_preprocessed_W2.csv`.
3. `imputation.R` - Reads in the pre-processed data from the previous step and imputes all missing values using multiple imputation, to prepare for modeling. Outputs `data/model_data.csv`. 
4. `scales.R` - Reads in the data from the previous step and builds scales using IRT modeling. Outputs `data/model_data_IRT.csv`.
5. `prop_scores.R` - Reads in the data from the previous step and calculates propensity scores for both knowledge scales. Outputs `data/model_data_IRT_propscores.csv`.

The following scripts calculate information effects on the basis of the data from step 5 for four dependent variables:

- `inf_effects_econ.R` - Calculates the information effect on perceptions of the economic impact of immigration.
- `inf_effects_protect_jobs.R` - Calculates the information effect on desire to restrict free movement to protect jobs.
- `inf_effects_fear.R` - Calculates the information effect on fear of immigration.
- `inf_effects_anger.R` - Calculates the information effect on anger over immigration.

## Data

The data for this project is available at https://data.aussda.at/dataset.xhtml?persistentId=doi:10.11587/LBSMPQ. Any use of that data set is subject to the terms provided there.

## Requirements

The code in this repository was produced using R 4.1.2 on macOS Catalina (10.15.7).
