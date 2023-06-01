SMdata <- read_csv("R-systeem/Data/ESMdata.csv")

# Load required libraries
library(gplots)
install.packages("tseriesChaos")
install.packages("nonlinearTseries")
library(nonlinearTseries)
library(tseriesChaos)

colnames(ESMdata)[1] <- "Observation"
# Find variables containing "mood" in their names
mood_variables <- grep("mood", colnames(ESMdata), value = TRUE)

# Create a subset of the data using the mood variables
mood_subset <- ESMdata[, mood_variables]
mood_subset <- mood_subset[, -ncol(mood_subset)]
summary(mood_subset)
#mood_subset$Observation <- ESMdata[1]
mood_subset <- mood_subset[complete.cases(mood_subset), ]
cor_matrix <- cor(mood_subset)
print(cor_matrix)
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(50), 
        main = "Correlation Heatmap",
        xlab = "Variables",
        ylab = "Variables"
)
std_dev <- apply(mood_subset, 2, sd)
std_dev
#Analysis
lor.x <- mood_subset$mood_down
tau.ami <- timeLag(mood_subset$mood_irritat, technique = "ami", lag.max = 100, do.plot = T)
tau.acf <- timeLag(mood_subset$mood_down, technique = "acf", lag.max = 100, do.plot = T)
tau <- tau.acf
tau.acf
fnn.out = false.nearest(lor.x, m = 20, d = tau.acf, t = 50, eps = sd(lor.x)/10 )
plot(fnn.out)
print(fnn.out)
emb.dim <- estimateEmbeddingDim(lor.x, time.lag = tau, max.embedding.dim = 15)
m <- emb.dim
m
plot(emb.dim)
emb.dim
ps_data <- buildTakens(mood_subset$mood_down, m, tau)
plot(ps_data)
#correlation plot
cd = corrDim(lor.x,
             min.embedding.dim = emb.dim,
             max.embedding.dim = emb.dim + 5,
             time.lag = tau.acf, 
             min.radius = 0.9999, max.radius = 1.0001,
             n.points.radius = 40,
             do.plot=FALSE)
plot(cd)
print(cd)
cd.est = estimate(cd, regression.range=c(0.1,1.1),
                  use.embeddings = 2:4)
cat("estimate: ",cd.est,"\n")
#Entropy
se = sampleEntropy(cd, do.plot = T)
se.est = estimate(se, do.plot = T,
                  regression.range = c(20,100))
cat("Sample entropy estimate: ", mean(se.est), "\n")
#maxLyapunov
ml = maxLyapunov(lor.x, 
                 sampling.period = .1613,
                 min.embedding.dim = emb.dim,
                 max.embedding.dim = emb.dim + 3,
                 time.lag = tau.acf, 
                 radius = 1,
                 max.time.steps = 100,
                 do.plot = FALSE)

plot(ml,type="l", xlim = c(0,8))
ml.est = estimate(ml, regression.range = c(0,3),
                  do.plot = T,type="l")
cat("estimate: ", ml.est,"\n")
sum(is.na(lor.x))

nonlinearTseries::spaceTimePlot(time.series = lor.x, embedding.dim = 4, time.lag=tau.acf)
