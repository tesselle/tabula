# Similiraty measure (count data)

    Code
      similarity(birds, method = method[i])
    Output
              unmanaged
      managed 0.4442754

---

    Code
      similarity(birds, method = method[i])
    Output
              unmanaged
      managed 0.4615385

---

    Code
      similarity(birds, method = method[i])
    Output
              unmanaged
      managed 0.8134497

---

    Code
      similarity(birds, method = method[i])
    Output
              unmanaged
      managed 0.6315789

# Jaccard index - character

    Code
      index_jaccard(x, y)
    Output
      [1] 0.1428571

# Jaccard index - numeric

    Code
      index_jaccard(birds[1, ], birds[2, ])
    Output
      [1] 0.4615385

# Soreson index

    Code
      index_sorenson(birds[1, ], birds[2, ])
    Output
      [1] 0.6315789

# Bray index

    Code
      index_bray(birds[1, ], birds[2, ])
    Output
      [1] 0.4442754

# Morisita-Horn

    Code
      index_morisita(birds[1, ], birds[2, ])
    Output
      [1] 0.8134497

# Brainerd-Robinson

    Code
      index_brainerd(x, y)
    Output
      [1] 164.3678

# Binomial co-occurrence

    Code
      index_binomial(x, y)
    Output
      [1] 0.5370862

