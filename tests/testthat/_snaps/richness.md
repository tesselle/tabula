# Richness

    Code
      boot
    Output
                       original     mean       bias    error
      Altamira               38 37.03333 -0.9666667 2.235811
      Cueto de la Mina       27 27.23333  0.2333333 2.896887
      El Juyo                19 20.56667  1.5666667 3.410767
      El Cierro              15 14.50000 -0.5000000 3.598371
      La Paloma              12 11.43333 -0.5666667 2.176415

---

    Code
      jack
    Output
                       original     mean      bias    error
      Altamira               38 37.13636 -37.13636 2.250344
      Cueto de la Mina       27 26.38636 -26.38636 3.192919
      El Juyo                19 18.56818 -18.56818 3.248092
      El Cierro              15 14.65909 -14.65909 3.108323
      La Paloma              12 11.72727 -11.72727 2.920432

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

