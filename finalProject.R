#2. Bring in the data from GEO
library(ggplot2)
library(dplyr)
library(GEOquery)
library(limma)
GSE6264 <- getGEO("GSE6264")

#3. Take log2 of expression and generate a boxplot of normalized data. Give title and labels
###########################################################
# Pull out gene expression data and pheno type data
# (same functions as before, but GSE1297 is a list)
###########################################################

GSE6264.expr <- exprs(GSE6264 [[1]])
GSE6264.p <- pData(GSE6264 [[1]])

## is data normalized (is distribution similar across samples)?
boxplot(GSE6264.expr, main = "processed data")

# If not, then take log2 (very important -- if data is not normalized,
# then downstream analysis will be WRONG. Processed data from GEO may
# or may not be normalized, so make sure to check)
GSE6264.expr <- log2(GSE6264.expr)
boxplot(GSE6264.expr, main = "log2 processed data")

#4. How many samples profiled and how many probes in the dataset? 
# (a) How many samples had their gene expression values profiled?
ncol(GSE6264.expr)

# (b) How many probes are on the array?
nrow(GSE6264.expr)

#5. Pull out the column containing the data that you would like to compare (e.g.,
#the gender column), and use R to output the number of samples in each group. 
usage <- as.character(GSE6264.p$title)
s <- strsplit(usage, "_abuse_")

get.first <- function(x) {
  return(x[[1]])
}

groups <- sapply(s, get.first)

GSE6264.p <- mutate(GSE6264.p, status = groups)

control <- filter(GSE6264.p, status == "No")
nrow(control) # number of samples with no abuse

tobacco <- filter(GSE6264.p, status == "Tobacco")
nrow(tobacco) # number of samples in tobacco abuse


#6. Using	limma,	find	the	probes	that	are	differentially	expressed	across	the	
#groups	you	are	comparing,	using	a false	discovery	rate	(FDR)	of	20%.	Output	
#the	number	of	probes	identified,	using	the	nrow function. If	there	are	less	
#than	50	probes	with	an	FDR	of	20%,	find	the	top	50	probes.

abuse <- as.character(GSE6264.p$status)
design <- model.matrix(~0+abuse)
colnames(design) <- c("control", "tobacco")

fit <- lmFit(GSE6264.expr, design)

contrast.matrix <- makeContrasts(tobacco - control,levels=design)

fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
tt.20 <- topTable(fit2,sort.by = "p", p.value = 0.20, number = nrow(GSE6264.expr))
nrow(tt.20)


#7 Top 3 probes with logFC and adjusted p-value only.
top3 <- tt.20[1:3, c(1,5)]
lowestProbePVal<-top3[c(1,5), 1]


#8 Boxplot for probe with the lowest adjusted p-value
probe <- rownames(tt.20)[1]
m <- match(probe, rownames(GSE6264.expr))
df <- data.frame(expr = GSE6264.expr[m,], abuse = abuse)

logFC <- tt.20[1,]$logFC
2**logFC

FC <- paste0("FC = ", round(2**logFC, 2))
main <- paste0("Expression of ", probe, ", ", FC)

ggplot(df, aes(x = abuse, y = expr, fill = abuse)) + geom_boxplot() +
  ylab("log2 expression") + ggtitle(main) +
  scale_fill_manual(values = c("tomato1", "turquoise4", "whitesmoke")) +
  theme_classic() + theme(legend.position = "none") 

#9 Use R to	output the annotation	(GPL platform) of the	data that	you	are	
platform <- annotation(GSE6264[[1]]) 

# 10. Using	the	getGEO function, download	the	platform (GPL) for this	data.  
pl <- getGEO(platform)
pl <- Table(pl)


# 11. Find the gene	names	corresponding to all probes, and create	a	table	based	on	
# the	top	3 genes which contains the corresponding gene names, the probe	
# names, the logFC, and	the	adjusted p-values only, and output the table. Hint:	
# you	should start with	a	table	containing the	probe	name, logFC, and adjusted	
# p-values, then add the corresponding gene names, then limit the table to the	
# first	3	genes. Note that this	table	should include only those probes that have	

probe <- rownames(tt.20)[1:3]
m <- match(probe, pl$ID)
pl$Gene_Symbol[m]


