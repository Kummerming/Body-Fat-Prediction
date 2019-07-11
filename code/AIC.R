setwd("C://Doc/19spring/STAT628/Module1")
body = read.table("BodyFat.csv", sep = ",", header = T)
body <- body[,-c(1,3)]
body[42,4] = 69.5
body[221,3] = 173.25
body[163,3] = 164.25
body = body[-c(39,172,182),]
body[,c(6:15)] <- body[,c(6:15)]/body$WRIST
body$WRIST<- NULL
body2 <- as.data.frame(sapply(1:ncol(body), FUN= function(x) body[,x]/sd(body[,x])))
colnames(body2) <- colnames(body)
rownames(body) <- NULL
full.model <- lm(formula = BODYFAT ~ ., data = body)
reduce.model <- lm(formula = BODYFAT ~ 1, data = body)
step.model <- MASS::stepAIC(full.model, direction = "both", trace = F,scope=list(upper=full.model,lower=reduce.model)
                            ,k=log(nrow(body)-1))
summary(step.model)
m <- lm(BODYFAT ~ AGE+ADIPOSITY+ABDOMEN,data = body)
summary(m)
par(mfrow=c(2,2))
plot(m)
res <- m$residuals

