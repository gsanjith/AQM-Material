df <- read.csv("BreastCancer.csv")[,-1]
head(df)

#Look up ?factor

df$Class <- factor(df$Class, levels=c(0,1), labels = c("benign","malignant"))
head(df)

fit.logit <- glm(Class~., data=df, family = binomial())
summary(fit.logit)

prob <- predict(fit.logit, df, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), labels = c("benign","malignant"))
#By default predict() function predicts the log odds of having a malignant outcome

logit.perf <- table(df$Class, logit.pred, dnn = c("Actual", "Predicted") )
logit.perf

#logit.fit.reduced <- step(fit.logit,  direction = "forward")



