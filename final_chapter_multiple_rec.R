library(MASS)
library(car)
df <- read.csv('play-by-play_processed_complete_pass.csv', header = 1)

model_BIC_stepwise_full_multiple <- stepAIC(lm(yards_gained ~ . - X, data = df), direction = "both", k = log(nrow(df)))
x4<- summary(model_BIC_stepwise_full_multiple)
plot(cooks.distance(model_BIC_stepwise_full_multiple))
influencePlot(model_BIC_stepwise_full_multiple, id.method="identify", main="Influence Plot BIC", sub="Circle size is proportial to Cook's Distance" )

write.csv(x4$coefficients, 'final_multiple_rec_coefficients_BIC.csv')
write.csv(cooks.distance(model_BIC_stepwise_full_multiple), 'final_multiple_rec_cooks_BIC.csv')
write.csv(studres(model_BIC_stepwise_full_multiple), 'final_multiple_rec_sresid_BIC.csv')
write.csv(hatvalues(model_BIC_stepwise_full_multiple), 'final_mulitple_rec_hat_BIC.csv')

model_AIC_stepwise_full_multiple <- stepAIC(lm(yards_gained ~ . - X, data = df), direction = "both")
write.csv(residuals(model_BIC_stepwise_full_multiple), 'residuals_model_BIC_stepwise_full_multiple.csv')
x5 <- summary(model_AIC_stepwise_full_multiple)
plot(cooks.distance(model_BIC_stepwise_full_multiple))

influencePlot(model_AIC_stepwise_full_multiple, id.method="identify", main="Influence Plot AIC", sub="Circle size is proportial to Cook's Distance" )

write.csv(x5$coefficients, 'final_multiple_rec_coefficients_AIC.csv')

#predict(model_BIC_stepwise_full, df, type="response")

df_transformed <- read.csv('play-by-play_processed_complete_pass_transformed.csv', header = 1)

model_BIC_stepwise_full_multiple_transformed <- stepAIC(lm(yards_gained ~ . -X, data = df_transformed), direction = "both", k = log(nrow(df_transformed)))
x6 <- summary(model_BIC_stepwise_full_multiple_transformed)
write.csv(residuals(model_BIC_stepwise_full_multiple_transformed), 'residuals_model_BIC_stepwise_full_multiple_transformed.csv')
write.csv(x6$coefficients, 'final_multiple_rec_coefficients_BIC_transformed.csv')

influencePlot(model_BIC_stepwise_full_multiple_transformed, id.method="identify", main="Influence Plot AIC", sub="Circle size is proportial to Cook's Distance" )

write.csv(cooks.distance(model_BIC_stepwise_full_multiple_transformed), 'final_multiple_rec_cooks_BIC_transformed.csv')
write.csv(studres(model_BIC_stepwise_full_multiple_transformed), 'final_multiple_rec_sresid_BIC_transformed.csv')
write.csv(hatvalues(model_BIC_stepwise_full_multiple_transformed), 'final_mulitple_rec_hat_BIC_transformed.csv')
