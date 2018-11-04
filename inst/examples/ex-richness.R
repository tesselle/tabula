# Rarefaction ==================================================================
# Data from Magurran 1988, p. 128-129
trapA <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1), nrow = 1)
# Coerce to count matrix
trapA <- as(trapA, "CountMatrix")

rarefy(trap, sample = 13) # 6.56

# Richness =====================================================================
# Margalef and Menhinick index -------------------------------------------------
# Data from Magurran 1988, p. 128-129
trap <- rbind(A = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1),
              B = c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0))
# Coerce to count matrix
trap <- as(trap, "CountMatrix")

richness(trap, method = c("margalef", "menhinick"), simplify = TRUE)
# A 2.55 1.88
# B 1.95 1.66
