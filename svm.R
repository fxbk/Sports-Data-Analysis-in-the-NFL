library(e1071)

df_train_short <- read.csv('play-by-play_processed_train_half.csv', header = 1)
df_test_short <- read.csv('play-by-play_processed_test_half.csv', header = 1)


svmfit <- svm(complete_pass ~ .-X , data = df_train_short, kernel = "linear", cost = 1, scale = FALSE)
summary(svmfit)
write.csv(predict(svmfit, df_test_short, type="response"), 'predictions_svm.csv')

svmfit_radial <- svm(complete_pass ~ .-X , data = df_train_short, kernel = "radial", cost = 1, scale = FALSE)
summary(svmfit_radial)
write.csv(predict(svmfit_radial, df_test_short, type="response"), 'predictions_svm_radial.csv')

svmfit_pol <- svm(complete_pass ~ .-X , data = df_train_short, kernel = "radial", cost = 1, scale = FALSE)
summary(svmfit_pol)
write.csv(predict(svmfit_pol, df_test_short, type="response"), 'predictions_svm_pol.csv')


