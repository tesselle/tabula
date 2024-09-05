## Replicate fig. 1 of Tóthmérész 1995
spc <- matrix(
  data = c(33, 29, 28, 5, 5, 0, 0, 42, 30, 10,
           8, 5, 5, 0, 32, 21, 16, 12, 9, 6, 4),
  nrow = 3, byrow = TRUE, dimnames = list(c("Z", "B", "C"), NULL)
)

profiles(spc, color = color("bright"))
