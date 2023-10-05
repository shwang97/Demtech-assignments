# creating accident data
library(tidyverse)
df <- tibble(
  age = 16:30
)
df <- df %>%
  mutate(q_a = 0.062 - 0.000053 * age^2)

# creating mortality data
library(HMDHFDplus)
us <- "shwang97@wisc.edu"
pw<- "Pkpk0221!!@@"

df2 <- readHMDweb(CNTRY = "USA", item = "bltper_1x1", username = us,
                      password = pw)

df2_re <- df2 %>%
  filter(Year == 2005 & Age >= 16 & Age <= 30) %>%
  select(Age, qx) %>%
  rename(age = Age)

# combining the data sets 
merged <- merge(df, df2_re, by = "age")
merged <- merged %>%
  mutate(lx = if_else(age == 16, 85000, NA_real_))

library(tidyverse)

# Initialize the lx column with the base value for age 16
merged <- merged %>%
  mutate(lx = if_else(age == 16, 85000, NA_real_))

# Compute the life table values using a loop
for (i in 1:(nrow(merged)-1)) {
  merged <- merged %>%
    mutate(da = if_else(row_number() == i, lx[i] * q_a[i], NA_real_),
           dx = if_else(row_number() == i, lx[i] * qx[i], NA_real_),
           lx = if_else(row_number() == (i + 1), lx[i] - da[i] - dx[i], lx))
}

# Compute da and dx for the last age
merged <- merged %>%
  mutate(da = if_else(is.na(da), lx * q_a, da),
         dx = if_else(is.na(dx), lx * qx, dx))

# a
(merged$lx[15] - merged$da[15]- merged$dx[15])/merged$lx[1]

# b 
sum(merged$da[10:15])/merged$lx[10]

# c 
sum(merged$dx[1:15])/merged$lx[1]




