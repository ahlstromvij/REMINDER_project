# packages
library(haven)

# read in data
all_data <- read_spss("10085_da_en_v1_0.zsav")

# demographic variables
df <- data.frame(all_data$W1_Q1) # 1 = man; 2 = woman
names(df)[1] <- "gender"

df <- cbind(df, all_data$W1_Q2A) # continuous version; see Q2C for ordinal
names(df)[2] <- "age"

df$nationality <- NA
for(i in 1:length(df$nationality)){
  if(all_data$W1_Q3_1_1[i] == 1){
    df$nationality[i] <- "germany"
  }
  if(all_data$W1_Q3_1_2[i] == 1){
    df$nationality[i] <- "spain"
  }
  if(all_data$W1_Q3_1_3[i] == 1){
    df$nationality[i] <- "sweden"
  }
  if(all_data$W1_Q3_1_4[i] == 1){
    df$nationality[i] <- "uk"
  }
  if(all_data$W1_Q3_1_5[i] == 1){
    df$nationality[i] <- "hungary"
  }
  if(all_data$W1_Q3_1_6[i] == 1){
    df$nationality[i] <- "poland"
  }
  if(all_data$W1_Q3_1_7[i] == 1){
    df$nationality[i] <- "romania"
  }
  if(all_data$W1_Q3_1_8[i] == 1){
    df$nationality[i] <- "other"
  }
}
table(df$nationality)

df <- cbind(df, all_data$W1_Q40) # see https://ec.europa.eu/eurostat/statistics-explained/index.php?title=International_Standard_Classification_of_Education_(ISCED)
names(df)[4] <- "education_ISCED"

df <- cbind(df, all_data$W1_Q45) # 1 = in paid work
names(df)[5] <- "in_paid_work"

df <- cbind(df, all_data$W2_Q87) 
names(df)[6] <- "religion" # lots of NAs

# attitudinal variables
df <- cbind(df, all_data$W1_Q8) 
names(df)[7] <- "ideology_left_right" # 1 = max left; 10 = max right
