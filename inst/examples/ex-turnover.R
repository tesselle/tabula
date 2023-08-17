## Data from Magurran 1988, p. 162
data("woodland")

## Whittaker's measure
turnover(woodland, "whittaker") # 1

## Cody's measure
turnover(woodland, "cody") # 3

## Routledge's measures
turnover(woodland, "routledge1") # 0.29
turnover(woodland, "routledge2") # 0.56
turnover(woodland, "routledge3") # 1.75

## Wilson and Shmida's measure
turnover(woodland, "wilson") # 1
