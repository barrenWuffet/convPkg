
options(scipen = 999)

library(gbm)
library(convPkg5)

df1 <- data.frame(col1 = -100:100)

df1$col2 <- (-4/9) * df1$col1 ^2
dfa <- df1[df1$col1 < 0,];dim(dfa)
dfb <- df1[df1$col1 > 0,];dim(dfb)

head(df1)
plot(df1$col1, df1$col2, cex = .5)
cor(df1$col1, df1$col2)

lm1 <- lm(df1$col2 ~ df1$col1)
summary(lm1)

lma <- lm(dfa$col2 ~ dfa$col1)

df1$pred <- predict(lm1, df1)

lines(df1$col1, df1$pred, col = 'green', lwd = 2)

NTREES = 50
gbm50 <- gbm(df1$col2 ~ df1$col1
            , distribution = 'gaussian'
            , n.trees = NTREES
            )

NTREES = 100
gbm100 <- gbm(df1$col2 ~ df1$col1
            , distribution = 'gaussian'
            , n.trees = NTREES
)
NTREES = 200
gbm200 <- gbm(df1$col2 ~ df1$col1
              , distribution = 'gaussian'
              , n.trees = NTREES
)
NTREES = 500
gbm500 <- gbm(df1$col2 ~ df1$col1
              , distribution = 'gaussian'
              , n.trees = NTREES
)

plot(gbm500)

df1$pred_gbm_50 <- predict(gbm50, df1, type = 'response', n.trees = 50)
df1$pred_gbm_100 <- predict(gbm100, df1, type = 'response', n.trees = 100)
df1$pred_gbm_200 <- predict(gbm200, df1, type = 'response', n.trees = 200)
df1$pred_gbm_500 <- predict(gbm500, df1, type = 'response', n.trees = 500)
#plot(df1$col1, df1$col2)

lines(df1$col1, df1$pred_gbm_50, col = 'red', lwd = 2)
lines(df1$col1, df1$pred_gbm_100, col = 'orange', lwd = 2)
lines(df1$col1, df1$pred_gbm_200, col = 'blue', lwd = 2)
lines(df1$col1, df1$pred_gbm_500, col = 'purple', lwd = 2)
grid()

