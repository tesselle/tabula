context("Magurran 1988")
library(tabula)
options("verbose" = TRUE)

# birds <- read.table("data-raw/birds.csv", sep = ",", dec= ".",
#                     header = TRUE, row.names = 1, encoding = "UTF-8")

# Alpha diversity ==============================================================
test_that("Rarefaction", {
  # Magurran 1988, p. 128
  n1 <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  n2 <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(hurlbertRarefaction(n1, 13), 2), 6.56) # 6.58
})
# Richness ---------------------------------------------------------------------
test_that("Margalef richness", {
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(round(margalefRichness(n), 2), 5.47)
  # Magurran 1988, p. 128
  n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  expect_equal(round(margalefRichness(n), 2), 2.55)
  n <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(margalefRichness(n), 2), 1.95)
})
test_that("Menhinick richness", {
  # Magurran 1988, p. 128
  n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  expect_equal(round(menhinickRichness(n), 2), 1.88)
  n <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(menhinickRichness(n), 2), 1.66)
})
# Diversity --------------------------------------------------------------------
test_that("Shannon diversity", {
  # Magurran 1988, p. 38
  n <- c(235, 218, 192, 87, 20, 11, 11, 8, 7, 4, 3, 2, 2, 1, 1)
  expect_equal(round(shannonDiversity(n), 2), 1.69)
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(round(shannonDiversity(n), 2), 2.61)
  expect_equal(round(shannonEvenness(n), 2), 0.73) # 0.74
  # Magurran 1988, p. 145
  n1 <- c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1)
  expect_equal(round(shannonDiversity(n1), 3), 2.408) # 2.404
  expect_equal(round(shannonEvenness(n1), 4), 0.8038) # 0.8025
  expect_equal(round(shannonVariance(n1), 5), 0.00540) # 0.00502
  n2 <- c(65, 30, 30, 20, 14, 11, 9, 5, 4, 3, 3, 2, 1, 1)
  expect_equal(round(shannonDiversity(n2), 3), 2.056)
  expect_equal(round(shannonEvenness(n2), 4), 0.7791)
  expect_equal(round(shannonVariance(n2), 5), 0.00452) # 0.00427
})
test_that("Brillouin diversity", {
  # Magurran 1988, p. 38
  n <- c(235, 218, 192, 87, 20, 11, 11, 8, 7, 4, 3, 2, 2, 1, 1)
  expect_equal(round(brillouinDiversity(n), 2), 1.65)
  # Magurran 1988, p. 150
  n <- c(17, 15, 11, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1)
  expect_equal(round(brillouinDiversity(n), 3), 1.876)
  expect_equal(round(brillouinEvenness(n), 3), 0.828) # 0.827
})
# Dominance --------------------------------------------------------------------
test_that("Simpson dominance", {
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(round(simpsonDominance(n), 3), 0.118) # 1 / 8.50
  # Magurran 1988, p. 152
  n <- c(752, 276, 194, 126, 121, 97, 95, 83, 72, 44, 39, 16, 15, 13, 9, 9, 9,
         8, 7, 4, 2, 2, 1, 1, 1)
  expect_equal(round(simpsonDominance(n), 3), 0.187)
})
test_that("McIntosh dominance", {
  # Magurran 1988, p. 154
  n <- c(254, 153, 90, 69, 68, 58, 51, 45, 40, 39, 25, 23, 19, 18, 16, 14, 14,
         11, 11, 11, 11, 10, 6, 6, 6, 6, 5, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1)
  expect_equal(round(mcintoshDominance(n), 4), 0.7066)
  expect_equal(round(mcintoshEvenness(n), 4), 0.8180)
})
test_that("Berger-Parker dominance", {
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(bergerDominance(n), 0.254) # 1 / 3.49
  # Magurran 1988, p. 156
  n <- c(394, 3487, 275, 683, 22, 1, 0, 1, 6, 8, 1, 1, 2)
  expect_equal(round(bergerDominance(n), 3), 0.714)
})

# Beta diversity ===============================================================
# Turnover index ---------------------------------------------------------------
trees <- matrix(
  c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, TRUE, FALSE, TRUE, FALSE,
    FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
    FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
  nrow = 6, ncol = 6, byrow = FALSE,
  dimnames = list(c("1", "2", "3", "4", "5", "6"),
                  c("Birch", "Oak", "Rowan", "Beech", "Hazel", "Holly"))
)
test_that("Whittaker index", {
  # Magurran 1988, p. 162
  expect_equal(whittakerBeta(trees), 1)
})
test_that("Cody index", {
  # Magurran 1988, p. 162
  expect_equal(codyBeta(trees), 3)
})
test_that("Routledge 'R' index", {
  # Magurran 1988, p. 163
  expect_equal(round(routledge1Beta(trees), 4), 0.2857)
})
test_that("Routledge 'I' index", {
  # Magurran 1988, p. 163
  expect_equal(round(routledge2Beta(trees), 4), 0.5595)
})
test_that("Routledge 'E' index", {
  # Magurran 1988, p. 164
  expect_equal(round(routledge3Beta(trees), 3), 1.750)
})
test_that("Wilson index", {
  # Magurran 1988, p. 164
  expect_equal(wilsonBeta(trees), 1)
})
# Similarity index -------------------------------------------------------------
birds_unmanaged <- c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3,
                     13.0, 14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9,
                     4.3, 1.4, 2.9)
birds_managed <- c(0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7,
                   2.9, 0, 0, 2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9)
test_that("Jaccard index", {
  # Magurran 1988, p. 165
  expect_equal(round(jaccardSimilarity(birds_unmanaged, birds_managed), 2), 0.46)
})
test_that("Soreson index", {
  # Magurran 1988, p. 165
  expect_equal(round(sorensonSimilarity(birds_unmanaged, birds_managed), 2), 0.63)
})
test_that("Bray index", {
  # Magurran 1988, p. 165
  expect_equal(round(braySimilarity(birds_unmanaged, birds_managed), 2), 0.44)
})
test_that("Morisita-Horn", {
  # Magurran 1988, p. 167
  expect_equal(round(morisitaSimilarity(birds_unmanaged, birds_managed), 4), 0.8134) # 0.8133
})
test_that("Brainerd-Robinson", {
  x <- c(16, 9, 3, 0, 1)
  y <- c(13, 3, 2, 0, 0)
  expect_equal(round(brainerdSimilarity(x, y), 0), 164)
})
