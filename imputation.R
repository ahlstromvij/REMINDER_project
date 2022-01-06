# read in preprocessed data
df_preprocessed <- read.csv("df_preprocessed.csv")

# visualise missing data
library(naniar)
vis_miss(df_preprocessed[,c(1:30,36:38)]) # ignoring quality control NAs

