library(limma)
library(class)

rm(list = ls()) ## this will delete all objects in the workspace

###################################################################
# Useful functions
###################################################################

## scale rows of given matrix to have mean 0 and sd of 1
row.scale <-function(x) {
  x.scale = t(scale(t(x)))
  return(x.scale)
}

# returns performance information, incuding average sensitivity
avg.sensitivity <-function(predicted, true) {
  predicted = factor(predicted)
  true = factor(true)
  levels(predicted) = levels(true)
  t = table(predicted = predicted, true = true)
  if (nrow(t) != 2) {
    stop("only 1 row in accuracy table")
  }
  acc = diag(t) / colSums(t)
  sens1 = acc[1]
  sens2 = acc[2]
  list(table = t, sensitivity1 = sens1, sensitivity2 = sens2, avg.sensitivity = mean(acc))
}

# load the Challenge data, which includes the following objects:
# X.train - the log2 gene expression data for the training samples
# Y.train - the class labels (NMI or MI) for the training samples
# X.test - the log2 gene expression data for the test samples

load(url("https://gdancik.github.io/CSC-315/data/hw/Challenge.RData"))

## Differently expressed probes
level <- as.character(Y.train)
levels(level) <- c("NMI", "MI")
design <- model.matrix(~0+level)
colnames(design) <- c("NMI", "MI")

## limma package fits a linear model to each row of the expression matrix 
fit <- lmFit(X.train, design)

## Contrasts need to match column names of design matrix 
contrast.matrix <- makeContrasts(NMI - MI,levels=design)

## fit model based on contrasts 
fit2 <- contrasts.fit(fit, contrast.matrix)

# calculate moderated t-statistics
fit2 = eBayes(fit2)

## get all probes with FDR < 0.10, sorted by p-value 
tt.10 <- topTable(fit2,sort.by = "p", p.value = 0.10, number = nrow(X.train))

## Training 
## get ifferentially expressed probes
m <- match(rownames(tt.10), rownames(X.train))
X <- X.train[m,]

## scale
X.scale <- row.scale(X)
preds = knn.cv(t(X.scale), level, k = 3)
table(predicted = preds, true = level)

avg.sensitivity(preds, true = level)

# overall accuracy (% correct) of classifier#
sum(preds == level) / length(level)


## Testing
m2 <- match(rownames(tt.10), rownames(X.train))
X2 <- X.test[m2,]

## scale
X2.scale <- row.scale(X2)
preds2 = knn.cv(t(X2.scale), level, k = 3)
table(predicted = preds2, true = level)

avg.sensitivity(preds2, true = level)

# overall accuracy (% correct) of classifier#
sum(preds == level) / length(level)




          #### Part 2 ####
library(CCM)
## K=create.CCM(X.test, X.train, method = "spearman")
## train.expr = X.train[]

## Gettign a length error 
## p = predict(K, train.expr)
## table(pred = p, true = X.test) # check accuracy


k1 = create.CCM(X.scale, X.test, method = "spearman")

train.expr2 = Y.train[]
p = predict(k1, train.expr2)

table(predicted = p, true = level)

avg.sensitivity(p, true = level)

# overall accuracy (% correct) of classifier#
sum(p == level) / length(level)

