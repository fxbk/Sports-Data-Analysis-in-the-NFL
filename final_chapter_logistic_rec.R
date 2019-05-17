library(MASS)

df_train <- read.csv('play-by-play_processed_train.csv', header = 1)
df_test <- read.csv('play-by-play_processed_test.csv', header = 1)


model_BIC_stepwise_full <- stepAIC(glm(complete_pass ~ . - X - yards_gained, data = df_train, family = binomial), direction = "both", k = log(nrow(df)))
x2 <- summary(model_BIC_stepwise_full)

odds_x2 <- exp(cbind(x2$coefficients[,1], x2$coefficients[,1]-1.96*x2$coefficients[,2],x2$coefficients[,1]+1.96*x2$coefficients[,2]))
write.csv(odds_x2, 'odds_final_logistic_rec_BIC.csv')
write.csv(x2$coefficients, 'final_logistic_rec_coefficients_BIC.csv')
write.csv(predict(model_BIC_stepwise_full, df_test, type="response"), 'predictions_log_rec_BIC.csv')

model_AIC_stepwise_full <- stepAIC(glm(complete_pass ~ . - X - yards_gained, data = df_train, family = binomial), direction = "both")
x3 <- summary(model_AIC_stepwise_full)

odds_x3 <- exp(cbind(x3$coefficients[,1], x3$coefficients[,1]-1.96*x3$coefficients[,2],x3$coefficients[,1]+1.96*x3$coefficients[,2]))
write.csv(odds_x3, 'odds_final_logistic_rec_AIC.csv')
write.csv(x3$coefficients, 'final_logistic_rec_coefficients_AIC.csv')
write.csv(predict(model_AIC_stepwise_full, df_test, type="response"), 'predictions_log_rec_AIC.csv')

df_train1 <- read.csv('df_train1.csv', header = 1)
model_BIC_stepwise_full <- stepAIC(glm(complete_pass ~ . - X - yards_gained, data = df_train1, family = binomial), direction = "both", k = log(nrow(df)))
x4 <- summary(model_BIC_stepwise_full)
odds_x4 <- exp(cbind(x4$coefficients[,1], x4$coefficients[,1]-1.96*x4$coefficients[,2],x4$coefficients[,1]+1.96*x4$coefficients[,2]))
write.csv(odds_x4, 'odds_final_logistic_rec_BIC_subsampled.csv')
write.csv(x4$coefficients, 'final_logistic_rec_coefficients_BIC_subsampled.csv')
write.csv(predict(model_BIC_stepwise_full, df_test, type="response"), 'predictions_log_rec_BIC_subsampled.csv')
x