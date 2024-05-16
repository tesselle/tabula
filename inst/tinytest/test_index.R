# Compare with PAST v4.16c
n <- c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1)
expect_equal(round(index_simpson(n, unbiased = TRUE), 3), 0.115) # 0.1147
expect_equal(round(index_shannon(n, unbiased = TRUE, ACE = FALSE), 3), 2.464) # 2.464
expect_equal(round(index_brillouin(n), 3), 2.231) # 2.231
expect_equal(round(index_menhinick(n), 3), 1.534) # 1.534
expect_equal(round(index_margalef(n), 3), 3.7) # 3.7
expect_equal(round(index_berger(n), 3), 0.206) # 0.2059
expect_equal(round(index_chao1(n, unbiased = TRUE), 3), 21.491) # 21.49
expect_error(index_chao1(n, unbiased = TRUE, improved = TRUE)) # 22.65 (?)
expect_equal(round(index_ace(n), 3), 22.087) # 22.09
expect_equal(round(index_squares(n), 3), 21.924) # 21.92

# Heterogeneity ================================================================
## Berger-Parker dominance
# Magurran 1988, p. 63
n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
       16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
expect_equal(index_berger(n), 0.254) # 1 / 3.49

## Brillouin diversity
# Magurran 1988, p. 150
n <- c(17, 15, 11, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1)
expect_equal(
  round(index_brillouin(n, evenness = FALSE), 3),
  1.876
)
expect_equal(
  round(index_brillouin(n, evenness = TRUE), 3),
  0.828
) # 0.827

## McIntosh dominance
# Magurran 1988, p. 154
n <- c(254, 153, 90, 69, 68, 58, 51, 45, 40, 39, 25, 23, 19, 18, 16, 14, 14,
       11, 11, 11, 11, 10, 6, 6, 6, 6, 5, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1)
expect_equal(
  round(index_mcintosh(n, evenness = FALSE), 3),
  0.707
)
expect_equal(
  round(index_mcintosh(n, evenness = TRUE), 3),
  0.818
)

## Shannon diversity
# Magurran 1988, p. 145
n <- c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1)
expect_equal(
  round(index_shannon(n, unbiased = FALSE), 3),
  2.408
) # 2.404
expect_equal(
  round(index_shannon(n, evenness = TRUE), 3),
  0.804
) # 0.8025
expect_equal(
  round(tabula:::variance_shannon(n), 4),
  0.0054
) # 0.00502

## Simpson dominance
# Magurran 1988, p. 63
n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
       16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
expect_equal(
  round(index_simpson(n, unbiased = TRUE), 3),
  0.118
) # 1 / 8.50

# Similarity ===================================================================
# Data from Magurran 1988, p. 166
data("aves")

## Jaccard index - character
x <- c("horse", "dog", "cat", "cow")
y <- c("sheep", "cat", "bird", "bull")
expect_equal(round(index_jaccard(x, y), 3), 0.143) # 0.14

## Jaccard index - numeric
# Magurran 1988, p. 165
expect_equal(
  round(index_jaccard(as.numeric(aves[1, ]), as.numeric(aves[2, ])), 3),
  0.462
) # 0.46

## Soreson index
# Magurran 1988, p. 165
expect_equal(
  round(index_sorenson(as.numeric(aves[1, ]), as.numeric(aves[2, ])), 3),
  0.632
) # 0.63

## Bray index
# Magurran 1988, p. 165
expect_equal(
  round(index_bray(as.numeric(aves[1, ]), as.numeric(aves[2, ])), 3),
  0.444
) # 0.44

## Morisita-Horn
# Magurran 1988, p. 167
expect_equal(
  round(index_morisita(as.numeric(aves[1, ]), as.numeric(aves[2, ])), 3),
  0.813
) # 0.8133

## Brainerd-Robinson
x <- c(16, 9, 3, 0, 1)
y <- c(13, 3, 2, 0, 0)
expect_equal(round(index_brainerd(x, y)), 164) # 164

## Binomial co-occurrence
x <- c(16, 9, 3, 0, 1)
y <- c(13, 3, 2, 0, 0)
expect_equal(round(index_binomial(x, y), 3), 0.537) # 0.54

# Turnover =====================================================================
data("woodland")

## Whittaker index
# Magurran 1988, p. 162
expect_equal(index_whittaker(as.matrix(woodland)), 1)

## Cody index
# Magurran 1988, p. 162
expect_equal(index_cody(as.matrix(woodland)), 3)

## Routledge 'R' index
# Magurran 1988, p. 163
expect_equal(round(index_routledge1(as.matrix(woodland)), 3), 0.286)

## Routledge 'I' index
# Magurran 1988, p. 163
expect_equal(round(index_routledge2(as.matrix(woodland)), 3), 0.559)

## Routledge 'E' index
# Magurran 1988, p. 164
expect_equal(round(index_routledge3(as.matrix(woodland)), 3), 1.750)

## Wilson index
# Magurran 1988, p. 164
expect_equal(index_wilson(as.matrix(woodland)), 1)

# Rarefaction ==================================================================
## Hurlbert rarefaction
# Magurran 1988, p. 128
n1 <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
n2 <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
expect_equal(round(index_hurlbert(n1, 13), 3), 6.557) # 6.58

## Baxter rarefaction
# Baxter 2001, p. 720
a <- c(2, 12, 7, 1, 0, 3, 12, 15, 0, 3, 1, 1, 12, 7, 3, 11, 3, 1, 7, 2, 4, 3,
       3, 1, 5, 1, 1, 1, 0, 2, 0, 1, 0, 1, 1, 1, 3, 0, 2, 1, 4, 5, 4, 5)
expect_equal(round(index_baxter(a, sample = 5), 3), 4.594) # 4.59

# Richness =====================================================================
## Margalef richness
# Magurran 1988, p. 63
n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
       16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
expect_equal(round(index_margalef(n), 3), 5.471) # 5.47

## Menhinick richness
# Magurran 1988, p. 128
n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
expect_equal(round(index_menhinick(n), 3), 1.877) # 1.88

# Data from Chao & Chiu (2016)
x <- c(1:21, 23, 25, 27, 28, 30, 32, 34:37, 41, 45, 46, 49, 52, 89, 110, 123, 140)
edge <- rep(x = x, times = c(113, 50, 39, 29, 15, 11, 13, 5, 6, 6, 3, 4, 3, 5,
                             2, 5, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1,
                             0, 0, 2, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0))
interior <- rep(x = x, times = c(129, 49, 42, 32, 19, 17, 7, 9, 7, 7, 6, 3, 3, 3,
                                 4, 4, 2, 2, 3, 4, 6, 2, 1, 2, 1, 1, 1,
                                 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1))

## Chao1-type estimators
expect_equal(round(index_chao1(edge, unbiased = FALSE), 3), 461.625) # 461.625
expect_equal(round(index_chao1(edge, unbiased = TRUE), 3), 458.016) # 458.016
expect_equal(round(index_chao1(edge, unbiased = FALSE, improved = TRUE), 3), 488.284) # 488.313

expect_equal(round(index_chao1(interior, unbiased = FALSE), 3), 540.728) # 540.728
expect_equal(round(index_chao1(interior, unbiased = TRUE), 3), 536.044) # 536.044
expect_equal(round(index_chao1(interior, unbiased = FALSE, improved = TRUE), 3), 572.471) # 572.505

## ACE-type estimators
expect_equal(round(index_ace(edge, k = 10), 3), 443.684) # 443.684
expect_equal(round(index_ace(interior, k = 10), 3), 498.834) # 498.834

## Chao2-type estimators
a <- matrix(
  c(0, 1, 1, 1,
    1, 0, 0, 1,
    0, 0, 1, 1),
  nrow = 3,
  ncol = 4,
  byrow = TRUE
)
b <- matrix(
  c(0, 1, 1, 1,
    1, 0, 0, 1,
    0, 0, 0, 1),
  nrow = 3,
  ncol = 4,
  byrow = TRUE
)
c <- matrix(
  c(0, 1, 1, 1,
    1, 0, 0, 1,
    0, 0, 1, 1,
    1, 0, 1, 1),
  nrow = 4,
  ncol = 4,
  byrow = TRUE
)
expect_equal(round(index_chao2(a, unbiased = TRUE), 3), 4.333) # 4.33
expect_equal(round(index_chao2(a, unbiased = FALSE), 3), 5.333) # 5.33
expect_equal(index_chao2(b, unbiased = FALSE), 6) # 6
expect_equal(round(index_chao2(c, unbiased = TRUE, improved = TRUE), 3), 4.052) # 4.05
expect_equal(round(index_chao2(c, unbiased = FALSE, improved = TRUE), 3), 4.427) # 4.43

## ICE-type estimators
a <- matrix(
  c(0, 1, 1, 1,
    1, 0, 0, 1,
    0, 0, 1, 1),
  nrow = 3,
  ncol = 4,
  byrow = TRUE
)
expect_equal(index_ice(a, k = 10), 5.6) # 5.6
