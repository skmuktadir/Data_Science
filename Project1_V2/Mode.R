x = c("Yes", "No", "Yes", "Yes", "No")
ux = c("Yes", "No")
match(x, ux)
# Returns: c(1, 2, 1, 1, 2)
# Explanation:
# "Yes" is in the 1st position of ux, "No" is in the 2nd position of ux.

tabulate(match(x, ux)) # 3,2

which.max(tabulate(match(x, ux)))# 3 maximum and it's 1
 
 
