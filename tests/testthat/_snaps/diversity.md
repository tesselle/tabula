# Heterogeneity

    Code
      boot
    Output
                       original     mean         bias      error
      Altamira         3.269200 3.246032 -0.023167845 0.09389994
      Cueto de la Mina 2.955298 2.947461 -0.007837264 0.12133334
      El Juyo          2.491683 2.594521  0.102838183 0.20137910
      El Cierro        2.485604 2.433003 -0.052600529 0.29127163
      La Paloma        2.329187 2.277140 -0.052046157 0.19394048

---

    Code
      jack
    Output
                       original     mean       bias      error
      Altamira         3.269200 3.246457 -0.9779158 0.08145529
      Cueto de la Mina 2.955298 2.932856 -0.9649994 0.15907033
      El Juyo          2.491683 2.469588 -0.9500688 0.19670002
      El Cierro        2.485604 2.462723 -0.9838709 0.22195573
      La Paloma        2.329187 2.305876 -1.0023704 0.25266291

# Evenness

    Code
      boot
    Output
                        original      mean          bias      error
      Altamira         0.8987278 0.8990759  0.0003481099 0.01699481
      Cueto de la Mina 0.8966760 0.8934907 -0.0031853362 0.02072053
      El Juyo          0.8462335 0.8614894  0.0152559629 0.03402342
      El Cierro        0.9178574 0.9212744  0.0034169705 0.02322799
      La Paloma        0.9373336 0.9410459  0.0037122290 0.01304888

---

    Code
      jack
    Output
                        original      mean        bias      error
      Altamira         0.8987278 0.8981620 -0.02432905 0.01554736
      Cueto de la Mina 0.8966760 0.8961617 -0.02211797 0.03326155
      El Juyo          0.8462335 0.8454248 -0.03477395 0.03690732
      El Cierro        0.9178574 0.9173676 -0.02105864 0.02905785
      La Paloma        0.9373336 0.9368884 -0.01914568 0.01698814

# Berger-Parker dominance

    Code
      index_berger(n)
    Output
      [1] 0.254

# Brillouin diversity

    Code
      index_brillouin(n, evenness = FALSE)
    Output
      [1] 1.876307

---

    Code
      index_brillouin(n, evenness = TRUE)
    Output
      [1] 0.8275138

# McIntosh dominance

    Code
      index_mcintosh(n, evenness = FALSE)
    Output
      [1] 0.7066343

---

    Code
      index_mcintosh(n, evenness = TRUE)
    Output
      [1] 0.8180305

# Shannon diversity

    Code
      index_shannon(n, evenness = FALSE)
    Output
      [1] 2.407983

---

    Code
      index_shannon(n, evenness = TRUE)
    Output
      [1] 0.8038044

---

    Code
      variance_shannon(n)
    Output
      [1] 0.005400433

# Simpson dominance

    Code
      index_simpson(n)
    Output
      [1] 0.1176834

