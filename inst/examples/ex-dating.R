zuni <- as(zuni, "CountMatrix")

dates <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

(events <- dateEvent(zuni, dates, axes = 10))

head(events[["rows"]])

head(events[["columns"]])

plotDate(zuni, events, type = "event", select = 2) +
  ggplot2::theme_bw()

plotDate(zuni, events, type = "acc", select = 2) +
  ggplot2::theme_bw()

plotDate(zuni, events, select = 2) +
  ggplot2::theme_bw()

plotBar(events, select = 1:20) +
  ggplot2::theme_bw()
