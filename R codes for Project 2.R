### R codes for Project2 (some figures)
## Auther: Teerapon Sahwangarrom
### Master student (Cancer Informatics), Department of Surgery and Cancer, Imperial College London

### The performace of LR model after the feature selection
###
lr_b <- read.csv("/Users/teeraponsahwangarrom/Desktop/fc_RFE_newKF.csv", header = T, sep=",")
lr_a <- read.csv("/Users/teeraponsahwangarrom/Desktop/After_FS_rskf.csv", header = T, sep=",")
head(lr)
#cart <- read.csv("/Users/teeraponsahwangarrom/Desktop/learning_curve_CART.csv", header = T, sep=",")
#lda <- read.csv("/Users/teeraponsahwangarrom/Desktop/learning_curve_LDA.csv", header = T, sep=",")
#rf <- read.csv("/Users/teeraponsahwangarrom/Desktop/learning_curve_RF.csv", header = T, sep=",")
svm <- read.csv("/Users/teeraponsahwangarrom/Desktop/SVM_learningCurve_RKF.csv", header = T, sep=",")
#knn <- read.csv("/Users/teeraponsahwangarrom/Desktop/learning_curve_KNN.csv", header = T, sep=",")
#nb <- read.csv("/Users/teeraponsahwangarrom/Desktop/learning_curve_NB.csv", header = T, sep=",")
xgb <- read.csv("/Users/teeraponsahwangarrom/Desktop/LearningCurveData_P2.csv", header = T, sep=",")
head(lda)

Observation <- lr_b$number_of_features
#Classification_Performance <- lr$Accuracy

plot(Observation, lr$Accuracy, type="o", col="blue", pch="o", lty=1, xlim = c(10,100), ylim= c(60,100), xlab="Number of features", ylab="Classification Performance", main="LR Classification of Normal and Cancer in Breast, Colorectal, and Ovarian Cancers")
arrows(x0=Observation, y0=lr$Accuracy-lr$Accuracy_std, x1=Observation,y1=lr$Accuracy+lr$Accuracy_std, code = 3, angle = 90, length = 0.1)
points(Observation, lr$Sensitivity, col="red", pch="*")
lines(Observation, lr$Sensitivity, col="red", lty=2)
points(Observation, lr$Specificity, col="dark red", pch="+")
lines(Observation, lr$Specificity, col="dark red", lty=3)
legend("bottomright", legend=c("Accuracy", "Sensitivity", "Specificity"), col=c("blue", "red","dark red"), lty=1:3, cex=0.8)

### The learning curve depicting the fit time for training the LR model before and after the feature selection
plot(Observation, lr_b$Time, type="o", col="blue", pch="o", lty=1, ylim= c(0,1), xlab="The Number of Observations", ylab=" Fit Time of Training Model (seconds)", main="Fit Time of Training LR Model Before and After Feature Selection")
points(Observation, lr_a$Time, col="red", pch="*")
lines(Observation, lr_a$Time, col="red", lty=2)
points(Observation, xgb$Time, col="dark red", pch="+")
lines(Observation, xgb$Time, col="dark red", lty=3)
legend("topright", legend=c("LR Model Before Feature Selection", "LR Model After Feature Selection"), col=c("blue", "red"), lty=1:2, cex=0.8)

### The learning curve of predictive performance of the LR model responding to the top 100 selected features by RFE
ss <- read.csv("/Users/teeraponsahwangarrom/Desktop/LR_LearningCurve_FSrkf.csv", header = T, sep=",")
head(ss)
features <- ss$number_of_features
#Classification_Performance <- lr$Accuracy
plot(features, ss$Accuracy, type="o", col="blue", pch="o", lty=1, ylim= c(65,100), xlab="The number of features", ylab="Classification Performance", main="LR Classification for Tumour/Cancer/DCIS vs Normal")
arrows(x0=features, y0=ss$Accuracy-ss$Accuracy_std, x1=features,y1=ss$Accuracy+ss$Accuracy_std, code = 3, angle = 90, length = 0.1)
points(features, ss$Sensitivity, col="red", pch="*")
lines(features, ss$Sensitivity, col="red", lty=2)
points(features, ss$Specificity, col="dark red", pch="+")




lines(features, ss$Specificity, col="dark red", lty=3)
legend("bottomright", legend=c("Accuracy", "Sensitivity", "Specificity"), col=c("blue", "red","dark red"), lty=1:3, cex=0.8)




dataVars <- read.csv("/Users/teeraponsahwangarrom/Desktop/DataVolcanoPlot2.csv", header = T, sep = ",")
tissue <- read.csv("/Users/teeraponsahwangarrom/Desktop/fsRFE_newSSTissue.csv", header=T, sep=",")

dataVars <- read.csv("/Users/teeraponsahwangarrom/Desktop/fsRFE_newRSKFDataR.csv", header = T, sep = ",")
tissue <- read.csv("/Users/teeraponsahwangarrom/Desktop/Tissue_typeForTCDDvsNN.csv", header = T, sep=",")
head(dataVars)
colnames(dataVars) <- gsub("\\X","",colnames(dataVars))
class(tissue)
tissue$Tissue_type

dataV <- cbind(dataVars,tissue)


CRC <- read.delim("/Users/teeraponsahwangarrom/Downloads/CRC-Export.txt", header= T, sep=",")
head(CRC)
colnames(CRC) <- gsub("\\X","",colnames(CRC))
colnames(CRC)

CRC_Data <- CRC[,5:2152]
head(CRC_Data)
CRC_Data_pqn <- normalize_pqn(CRC_Data, measure = "Area", exclude="blank", log=TRUE)

index_at <- which(CRC$TNAU == "a" | CRC$TNAU == "t")
index_n <- which(CRC$TNAU == "n")
CRC$Class[index_at] <- c("at")
CRC$Class[index_n] <- c("n")
write.csv(CRC,"/Users/teeraponsahwangarrom/Desktop/CRC-data.csv")


idx_n <- which(CRC$TNAU == "n")
idx_t <- which(CRC$TNAU == "t")
idx_nt <- c(idx_n, idx_t)
CRC_nt <- CRC[idx_nt,]
dim(CRC_nt)

colnames(CRC_nt)
CRC_nt[, 6:2153]
CRC_Data <- CRC_nt[,6:2153]
CRC_Data <- log(CRC_Data+1)


F_RFE <- read.csv("/Users/teeraponsahwangarrom/Desktop/features_RFE_TCDvsN_SS.csv", header = T, sep=",")
F_RFE <- format(round(F_RFE$molecular_features,4), nsmall = 4)
#F_RFE$molecular_features <- as.character(F_RFE$molecular_features)

data <- data[,F_RFE]

colnames(dataV)
data <- dataV[,2:81]
colName_data <- colnames(data)
cols_to_test <- c(colName_data)



TumourGroup <- dataV$Tissue_type == "Cancer"
NormalGroup <- dataV$Tissue_type == "Normal"

### Kruskal-Wallis test
results <- ldply(cols_to_test, function(colname){
  p_val = kruskal.test(data[[colname]] ~ dataV$Tissue_type)$p.value
  log2FC = log2(mean(data[TumourGroup, colname])/mean(data[NormalGroup, colname]))
  return(data.frame(colname=colname, p_value=p_val, log2FoldChange= log2FC))
})
results$FDR <- p.adjust(results$p_value, method = "BH")

results.ordered <- results[order(results$FDR),]

rskf <- read.csv("/Users/teeraponsahwangarrom/KK_test_fsRFE_newRSKF20.csv", header = T, sep = ",")

head(results.ordered)
write.csv(results.ordered, "/Users/teeraponsahwangarrom/Desktop/Kruskal_test_fsRFE_newRSKFRvolcano2.csv")

class(results.ordered$colname)
results.ordered$colname <- as.character(results.ordered$colname)
library(EnhancedVolcano)

### Volcano plot 
EnhancedVolcano(results.ordered,
                lab = results.ordered$colname,
                x = 'log2FoldChange',
                y = 'FDR',
                selectLab = c("389.1970","794.5690","291.0870","563.5780","700.5820","750.5440","160.0590","864.6530","759.4680","316.9140","405.2760","437.2690","671.4660","239.0880","157.0520","164.0810","265.1080","896.7460","921.7620"),
                xlim = c(-4,4),
                xlab = bquote(~Log[2]~ 'fold change'),
                pCutoff = 0.01,
                FCcutoff = 0.5,
                pointSize = 2.5,
                labSize = 3.0,
                labCol = 'black',
                labFace = 'bold',
                boxedLabels = TRUE,
                colAlpha = 4/5,
                legendPosition = 'right',
                legendLabSize = 14,
                legendIconSize = 4.0,
                drawConnectors = TRUE,
                widthConnectors = 1.0,
                colConnectors = 'black')


rskf <- KK_ss[order(KK_ss$count, decreasing = T),]
rskf <- rskf[1:25,]
rskf <- arrange(rskf, rskf$count)
rskf$features <- factor(rskf$features, levels = rskf$features)


ggplot(rskf, aes(x = features, y = count)) + geom_bar(stat="identity") + coord_flip() + scale_y_continuous(name="Selection Rate by RFE (%)") + scale_x_discrete(name="Features") 

dataBP <- read.csv("/Users/teeraponsahwangarrom/Desktop/Top5_BPdata_C_N.csv", header = T, sep=",")

head(dataBP)
dataBP %>% ggplot(aes(x=mass_spectro, y=Normalized_data, fill=Tissue)) + geom_boxplot() + coord_cartesian(ylim=c(27,45)) + xlab("Molecular Features") + ylab("Normalised Intensity Data")

dataBoxplot <- read.csv("/Users/teeraponsahwangarrom/Desktop/DataForBoxplot_rkf.csv",header = T, sep = ",")
head(dataBoxplot)

dataBoxplot %>% ggplot(aes(x=Tissue, y=Normalized_data, fill=Tissue)) + geom_boxplot() + coord_cartesian(ylim = c(27,45)) + xlab("Tissue Type")+ ylab("Normalised Intensity data") + facet_wrap(~mass_spectro) + theme(axis.text.x = element_text(angle = 0, hjust=1)) 



idx_at <- which(CRC$Class == "at")
idx_n <- which(CRC$Class == "n")
idx_at_n <- c(idx_at, idx_n)
CRC_at_n <- CRC[idx_at_n,]

CRC_Data <- CRC_at_n[,6:2153]
CRC_Data <- log(CRC_Data+1)
colName_CRC <- colnames(CRC_Data)
cols_to_test <- c(colName_CRC)
TumourGroup <- CRC_at_n$Class == "at"
NormalGroup <- CRC_at_n$Class == "n"
results <- ldply(cols_to_test, function(colname){
  p_val = kruskal.test(CRC_Data[[colname]] ~ CRC_at_n$Class)$p.value
  log2FC = log2(mean(CRC_Data[TumourGroup, colname])/mean(CRC_Data[NormalGroup, colname]))
  return(data.frame(colname=colname, p_value=p_val, log2FoldChange= log2FC))
})
results$FDR <- p.adjust(results$p_value, method = "BH")
results.ordered <- results[order(results$FDR),]
head(results.ordered)
write.csv(results.ordered, "/Users/teeraponsahwangarrom/Desktop/Kruskal_test_CRC_at_n.csv")

library(EnhancedVolcano)
EnhancedVolcano(KK_ss,
                lab = KK_ss$features,
                x = 'log2FoldChange',
                y = 'FDR',
                selectLab = c("726.594","794.569","670.523","389.197","563.578","239.088","167.034","750.544","700.582","291.087","160.059","861.827","846.653","759.468","265.108","921.762","164.081","896.746","401.246","157.052"),
                xlim = c(-0.1,0.1),
                xlab = bquote(~Log[2]~ 'fold change'),
                pCutoff = 0.01,
                FCcutoff = 0.0,
                pointSize = 2.5,
                labSize = 2.5,
                labCol = 'black',
                labFace = 'bold',
                boxedLabels = TRUE,
                colAlpha = 4/5,
                legendPosition = 'right',
                legendLabSize = 14,
                legendIconSize = 4.0,
                drawConnectors = TRUE,
                widthConnectors = 1.0,
                colConnectors = 'black')
