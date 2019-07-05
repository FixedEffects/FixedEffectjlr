
library(lfe)
library(data.table)
library(glmhdfe)


N = 1e7
K = 1e2
df = data.frame(
  id1 =  as.factor(sample(N/K, N, replace = TRUE)),
  id2 =  as.factor(sample(K, N, replace = TRUE)),
  y =  runif(N),
  x1 =  runif(N),
  x2 =  runif(N)
)

system.time(felm(y ~ x1 + x2|id1, df))              # 18s
system.time(glmhdfe(y ~ x1 + x2|id1, df))           # 49s

system.time(felm(y ~ x1 + x2|id1|0|id1, df))        # 23s
system.time(glmhdfe(y ~ x1 + x2|id1|0|id1, df))     # 41s

system.time(felm(y ~ x1 + x2|(id1 + id2), df))      # 12s
system.time(glmhdfe(y ~ x1 + x2|(id1 + id2), df))   # 49s

system.time(felm(y ~ x1 + x2|(id1 + id2)|0|id1+id2, df))     # 133s
system.time(glmhdfe(y ~ x1 + x2|(id1 + id2)|0|id1+id2, df))  # 60s


library(alpaca)
data <- simGLM(1000L, 200L, 1805L, model = "logit")
mod <- feglm(y ~ x1 + x2 + x3 | i + t, data)
feglm(y ~ x1 + x2 | id1, df)