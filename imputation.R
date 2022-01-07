# load packages
library(naniar)

# read in preprocessed data
df_W2 <- read.csv("df_preprocessed_W2.csv")

# visualize missing
vis_miss(df_W2[,c(1:31)]) # ignoring quality control, weights, and W2 flag
