library(tidyverse)

group1 <- c(5.1, 4.9, 6.3, 5.8, 5.0)
group2 <- c(6.4, 6.7, 7.1, 6.9, 7.3)

# Conduct a two-sample t-test (independent samples)
t_test_result <- t.test(group1, group2, 
                        alternative = "two.sided",  # Can be "two.sided", "less", or "greater"
                        var.equal = TRUE)          # Use TRUE if variances are assumed equal

# Print the result
print(t_test_result)

lm(group1 ~ group2) |> 
  summary()