## Data from Huntley 2008
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
  nrow = 9, byrow = TRUE
)
rownames(ceramics) <- c("Atsinna", "Cienega", "Mirabal", "PdMuertos",
                        "Hesh", "LowPesc", "BoxS", "Ojo Bon", "S170")
colnames(ceramics) <- c("DLH-1", "DLH-2a", "DLH-2b", "DLH-2c", "DLH-4")

## Brainerd-Robinson measure (count data)
C <- similarity(ceramics, "brainerd")
plot_spot(C)

## Data from Magurran 1988, p. 166
data("birds", package = "folio")

## Plot spot diagram
birds <- as_count(birds)

## Jaccard measure (presence/absence data)
similarity(birds, "jaccard") # 0.46

## Sorenson measure (presence/absence data)
similarity(birds, "sorenson") # 0.63

# Jaccard measure (Bray's formula ; count data)
similarity(birds, "bray") # 0.44

# Morisita-Horn measure (count data)
similarity(birds, "morisita") # 0.81
