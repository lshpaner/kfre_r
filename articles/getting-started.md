# kfre: Getting Started

## Quick start

``` r

toy <- data.frame(
  age=c(55,72), sex_txt=c("male","female"),
  eGFR=c(45,28), uACR=c(120,800),
  dm=c(1,0), htn=c(1,1),
  albumin=c(4.2,3.4), phosphorous=c(3.3,4.6),
  bicarbonate=c(24,22), calcium=c(9.1,9.8)
)

rp <- kfre:::RiskPredictor$new(
  df = toy,
  columns = list(age="age", sex="sex_txt", eGFR="eGFR", uACR="uACR",
                 dm="dm", htn="htn", albumin="albumin", phosphorous="phosphorous",
                 bicarbonate="bicarbonate", calcium="calcium")
)

rp$predict_kfre(years=2, is_north_american=TRUE, num_vars=4)
#> [1] 0.01247073 0.09997874
```

``` r

toy2 <- kfre::add_kfre_risk_col(toy, "age","sex_txt","eGFR","uACR",
                                dm_col="dm", htn_col="htn",
                                albumin_col="albumin", phosphorous_col="phosphorous",
                                bicarbonate_col="bicarbonate", calcium_col="calcium",
                                num_vars=c(4,6,8), years=c(2,5), is_north_american=TRUE)

head(toy2)
#>   age sex_txt eGFR uACR dm htn albumin phosphorous bicarbonate calcium
#> 1  55    male   45  120  1   1     4.2         3.3          24     9.1
#> 2  72  female   28  800  0   1     3.4         4.6          22     9.8
#>   kfre_4var_2year kfre_4var_5year kfre_6var_2year kfre_6var_5year
#> 1      0.01247073      0.03842137       0.0119651      0.03688339
#> 2      0.09997874      0.28026055       0.1094176      0.30356514
#>   kfre_8var_2year kfre_8var_5year
#> 1      0.01126961      0.03624505
#> 2      0.11930161      0.33888148
```
