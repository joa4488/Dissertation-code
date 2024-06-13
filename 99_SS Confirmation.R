# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024

library("longpower")

# N total sample size
# Ra correlation matrix for group a
# ra retention in group a
# sigmaa standard deviation of observation of interest in group a
# Rb correlation matrix for group a
# rb retention in group b
# sigmab standard deviation of observation of interest in group b. If NULL, sigmab is
# assumed same as sigmaa. If not NULL, sigmaa and sigmab are averaged.
# lambda allocation ratio
# delta effect size
# sig.level type one error
# power power
# alternative one- or two-sided test
# tol numerical tolerance used in root finding.


power.mmrm(
  N = 100,
  Ra = NULL,
  ra = NULL,
  sigmaa = NULL,
  Rb = NULL,
  rb = NULL,
  sigmab = NULL,
  lambda = 1,
  delta = NULL,
  sig.level = 0.05,
  power = NULL,
  alternative = c("two.sided", "one.sided"),
  tol = .Machine$double.eps^2
)
