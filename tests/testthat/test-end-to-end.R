test_that("basic smoke test", {
  toy <- data.frame(age=c(55,72), sex_txt=c("male","female"), eGFR=c(45,28), uACR=c(120,800), dm=c(1,0), htn=c(1,1), albumin=c(4.2,3.4), phosphorous=c(3.3,4.6), bicarbonate=c(24,22), calcium=c(9.1,9.8))
  rp <- RiskPredictor$new(df=toy, columns=list(age="age",sex="sex_txt",eGFR="eGFR",uACR="uACR",dm="dm",htn="htn",albumin="albumin",phosphorous="phosphorous",bicarbonate="bicarbonate",calcium="calcium"))
  p4 <- rp$predict_kfre(2, TRUE, FALSE, 4, precision=10)
  expect_equal(length(p4), 2)
})
