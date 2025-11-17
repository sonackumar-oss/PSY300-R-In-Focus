
####outliers####

outlierprac <- Outlier_Practice

# boxplot method:

bxp.V1 <- boxplot(outlierprac$`exam score`,
                  ylab = "exam score"
)
bxp.V1$out

bxp.V2 <- boxplot(outlierprac$`past score`,
                  ylab = "past score"
)
bxp.V2$out

bxp.V3 <- boxplot(outlierprac$`study time`,
                  ylab = "study time"
)
bxp.V3$out

# location of outliers (index)

out.bxp.V2 <- boxplot.stats(outlierprac$`past score`)$out
out_ind.V2 <- which(outlierprac$`past score` %in% c(out.bxp.V2))
out_ind.V2

# Remove outliers and update data:
outlierprac.1 <- outlierprac[-out_ind.V2,]

# Model with outlier
model.outlier <- lm(outlierprac$`exam score` ~ outlierprac$`past score` + outlierprac$`study time`)
summary(model.outlier)

# Model without outlier
model1 <- lm(outlierprac.1$`exam score` ~ outlierprac.1$`past score` + outlierprac.1$`study time`)
summary(model1)






