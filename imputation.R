# read in preprocessed data
df_preprocessed <- read.csv("df_preprocessed.csv")

# observations by country
table("Country" = df_preprocessed$nationality)
table("Country" = df_preprocessed$nationality,
      "Took part in W2" = df_preprocessed$part_in_W2)

# visualize missing data
library(naniar)
vis_miss(df_preprocessed[,c(1:30)]) # ignoring quality control, weights, and W2 flag

# only those who also took part in W2
df_W2 <- subset(df_preprocessed, part_in_W2==1)
vis_miss(df_W2[,c(1:30)]) # ignoring quality control, weights, and W2 flag

# summarize wave 2 data
library(modelsummary)
datasummary_skim(df_W2) # numerical variables
datasummary_skim(df_W2, type="categorical") # categorical variables
