# Data from Huntley 2008
ceramics <- CountMatrix(
  data = c(16, 9, 3, 0, 1,
           13, 3, 2, 0, 0,
           9, 5, 2, 5, 0,
           14, 12, 3, 0, 0,
           0, 26, 4, 0, 0,
           1, 26, 4, 0, 0,
           0, 11, 3, 13, 0,
           0, 0, 17, 0, 16,
           0, 0, 18, 0, 14),
  nrow = 9, byrow = TRUE,
  dimnames = list(c("Atsinna", "Cienega", "Mirabal", "PdMuertos",
                    "Hesh", "LowPesc", "BoxS", "Ojo Bon", "S170"),
                  c("DLH-1", "DLH-2a", "DLH-2b", "DLH-2c", "DLH-4"))
)

# Brainerd-Robinson measure (count data)
C <- similarity(ceramics, "brainerd")
plot_spot(C)

# Data from Magurran 1988, p. 166
birds <- CountMatrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9) * 10,
  nrow = 2, byrow = TRUE, dimnames = list(c("unmanaged", "managed"), NULL)
)

# Jaccard measure (presence/absence data)
similarity(birds, "jaccard") # 0.46

# Sorenson measure (presence/absence data)
similarity(birds, "sorenson") # 0.63

# Jaccard measure (Bray's formula ; count data)
similarity(birds, "bray") # 0.44

# Morisita-Horn measure (count data)
similarity(birds, "morisita") # 0.81
