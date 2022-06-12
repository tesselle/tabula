# Richness

    Code
      boot
    Output
            original      mean        bias     error
      P610s        7 7.3000000  0.30000000 1.4419814
      P610e        4 3.2000000 -0.80000000 1.5844068
      P625         2 1.7666667 -0.23333333 0.9714310
      P630         1 1.0333333  0.03333333 0.9994251
      P307         1 0.9666667 -0.03333333 0.8502873
      P631         7 7.1666667  0.16666667 1.4874958
      P623         3 3.1000000  0.10000000 1.4703976
      P624         7 6.6333333 -0.36666667 0.9278575
      P626s        7 6.9666667 -0.03333333 1.4015591
      P626e        9 9.0666667  0.06666667 0.8276820
      P627         3 3.0666667  0.06666667 1.3628908
      P628         7 6.9666667 -0.03333333 1.5421287

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

