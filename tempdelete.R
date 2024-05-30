install.packages("metafor")
library(metafor)

# Example data
dat <- data.frame(
  study = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5", "Study 6", "Study 7"),
  yi = c(0.2, 0.5, -0.3, 0.4, -0.2, 0.1, 0.3),
  vi = c(0.1, 0.2, 0.15, 0.25, 0.1, 0.2, 0.15),
  moderator = factor(c("A", "A", "B", "B", "A", "B", "A"))
)

# Meta-regression model
res <- rma(yi = yi, vi = vi, mods = ~ moderator, data = dat)
summary(res)

# Separate models for each level of the moderator
res_A <- rma(yi = yi, vi = vi, data = dat, subset = (moderator == "A"))
res_B <- rma(yi = yi, vi = vi, data = dat, subset = (moderator == "B"))

# Create forest plot for the main model
forest(res, slab = dat$study, main = "Meta-Regression Forest Plot",
       xlab = "Effect Size", order = order(dat$moderator))


forest(res, slab = dat$study, main = "Meta-Regression Forest Plot",
       xlab = "Effect Size", order = (dat$yi))



# Add studies colored by moderator level
rows_A <- which(dat$moderator == "A")
rows_B <- which(dat$moderator == "B")

# Overlay the same forest plot with colored segments for each moderator level
forest(res, slab = dat$study, col = "blue", rows = rows_A, add = TRUE)
forest(res, slab = dat$study, col = "red", rows = rows_B, add = TRUE)

# Add pooled estimates for each subgroup
addpoly(res_A, row = -1, mlab = "Pooled Effect (A)", col = "blue")
addpoly(res_B, row = -2, mlab = "Pooled Effect (B)", col = "red")

# Add legend to describe the moderator levels
legend("topright", legend = levels(dat$moderator), pch = 15, col = c("blue", "red"), bty = "n")
