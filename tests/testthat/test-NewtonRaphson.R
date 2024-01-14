test_that("NewtonRaphson works", {
  heartdata
  heartdata$famhist <- as.integer(heartdata$famhist)-1
  X<-as.matrix(heartdata[,
                         c("sbp","tobacco","ldl","adiposity","famhist","typea","obesity","alcohol","age")])
  y <- heartdata$chd
  check <- logregNewtonRaphson(X,y, max.iter=30)
  expect_equal(length(check$coefficients),ncol(cbind(X,y)))
})
