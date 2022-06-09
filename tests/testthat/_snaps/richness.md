# Richness

    Code
      boot
    Output
            Min. 1st Qu. Median     Mean 3rd Qu. Max.
      P610s    4       5      5 5.233333    5.75    7
      P610e    2       3      3 3.233333    4.00    4
      P625     1       2      2 1.833333    2.00    2
      P630     1       1      1 1.000000    1.00    1
      P307     1       1      1 1.000000    1.00    1
      P631     4       5      5 5.300000    6.00    7
      P623     1       2      2 2.266667    3.00    3
      P624     4       5      6 5.966667    7.00    7
      P626s    5       6      6 6.233333    7.00    7
      P626e    8       8      9 8.600000    9.00    9
      P627     1       2      2 2.433333    3.00    3
      P628     4       5      5 5.300000    6.00    7

---

    Code
      jack
    Output
            mean bias    error
      P610s  6.3 -6.3 1.374773
      P610e  3.6 -3.6 1.469694
      P625   1.8 -1.8 1.200000
      P630   0.9 -0.9 0.900000
      P307   0.9 -0.9 0.900000
      P631   6.3 -6.3 1.374773
      P623   2.7 -2.7 1.374773
      P624   6.3 -6.3 1.374773
      P626s  6.3 -6.3 1.374773
      P626e  8.1 -8.1 0.900000
      P627   2.7 -2.7 1.374773
      P628   6.3 -6.3 1.374773

# Margalef richness

    Code
      index_margalef(n)
    Output
      [1] 5.470981

# Menhinick richness

    Code
      index_menhinick(n)
    Output
      [1] 1.87663

# Chao1-type estimators

    Code
      index_chao1(edge, unbiased = FALSE)
    Output
      [1] 461.6254

---

    Code
      index_chao1(edge, unbiased = TRUE)
    Output
      [1] 458.0157

---

    Code
      index_chao1(edge, unbiased = FALSE, improved = TRUE)
    Output
      [1] 488.2843

---

    Code
      index_chao1(interior, unbiased = FALSE)
    Output
      [1] 540.7279

---

    Code
      index_chao1(interior, unbiased = TRUE)
    Output
      [1] 536.0439

---

    Code
      index_chao1(interior, unbiased = FALSE, improved = TRUE)
    Output
      [1] 572.4706

# ACE-type estimators

    Code
      index_ace(edge, k = 10)
    Output
      [1] 445.8224

---

    Code
      index_ace(interior, k = 10)
    Output
      [1] 501.0447

# Chao2-type estimators

    Code
      index_chao2(a, unbiased = TRUE)
    Output
      [1] 4.333333

---

    Code
      index_chao2(a, unbiased = FALSE)
    Output
      [1] 5.333333

---

    Code
      index_chao2(b, unbiased = FALSE)
    Output
      [1] 6

---

    Code
      index_chao2(c, unbiased = TRUE, improved = TRUE)
    Output
      [1] 4.052083

---

    Code
      index_chao2(c, unbiased = FALSE, improved = TRUE)
    Output
      [1] 4.427083

# ICE-type estimators

    Code
      index_ice(a, k = 10)
    Output
      [1] 5.6

