install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)
devtools::install_github("jhudsl/matahari")
library(matahari)
dance_start(value = FALSE, contents = FALSE)
dance_save("/Users/stephenmalam/Desktop/college_major_analysis.rds")

library(dplyr)
library(ggplot2)
# Boxplot of Median Earnings by Major Category
ggplot(college, aes(x = major_category, y = median, fill = major_category)) +
  geom_boxplot() +
  labs(title = "Median Earnings by Major Category",
       x = "Major Category",
       y = "Median Earnings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


college %>%
  group_by(major_category) %>%
  summarise(count = n(), avg_sample_size = mean(sample_size, na.rm = TRUE)) %>%
  arrange(desc(avg_sample_size))

# Inspect the sample_size variable
summary(college$sample_size)

# Fit the weighted linear model
weighted_lm <- lm(median ~ major_category, data = college, weights = sample_size)

# Summary of the weighted model
summary(weighted_lm)

weighted_anova <- anova(weighted_lm)
print(weighted_anova)
# Analysis of Variance Table
# 
# Response: median
# Df     Sum Sq    Mean Sq F value    Pr(>F)    
# major_category  15 2.1938e+12 1.4625e+11  3.3374 7.133e-05 ***
#   Residuals      157 6.8800e+12 4.3821e+10                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Null Hypothesis (H₀):
  # There is no significant association between college major category and median earnings.
#The p-value (7.133e-05) is significantly less than 0.05 (common alpha level), indicating strong evidence against the null hypothesis.
#Conclusion: There is a statistically significant association between college major category and median earnings.

