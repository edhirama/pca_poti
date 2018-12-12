# ------------------------------------------------------------------------------
# -- @head: MECAI | MAI5002 | PCA ----------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list=ls())
cat("\014")

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

source("svd.R")

if (!require("ggplot2")) {
  
  install.packages("ggplot2");
  library("ggplot2");
}

if (!require("reshape2")) {
  
  install.packages("reshape2");
  library("reshape2");
}

# ------------------------------------------------------------------------------
# -- Variable ------------------------------------------------------------------
# ------------------------------------------------------------------------------

#INPUT.FILE.NAME <- "facebook"
#INPUT.FILE.NAME <- "wine_red"
#INPUT.FILE.NAME <- "wine_white"
#INPUT.FILE.NAME <- "superconductor"
INPUT.FILE.NAME <- "residential"

INPUT.PATH <- "../data/input/"
INPUT.FILE <- paste0(INPUT.FILE.NAME,".csv")

OUTPUT.PATH <- "../data/output/"
OUTPUT.FILE.CPV <- paste0(INPUT.FILE.NAME,"_pca.png");
OUTPUT.FILE.BIP <- paste0(INPUT.FILE.NAME,"_biplot.png");

# ------------------------------------------------------------------------------
# -- Function ------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ----------------------------------------
# -- Data --------------------------------
# ----------------------------------------

# -- Read

X <- read.csv(file.path(INPUT.PATH,INPUT.FILE));
X <- t(X);

# -- Separate target variable

target <- X[nrow(X),];
X <- X[1:(nrow(X)-1),];

# -- Center + Normalize

for (i in 1:nrow(X)) {
  
  rowMean <- 0;
  
  for (j in 1:ncol(X)) {
    
    rowMean <- rowMean + X[i,j];
  }
  
  rowMean  <- (rowMean / ncol(X));
  
  if (rowMean != 0) {
    
    X[i,] <- (X[i,] - rowMean);
    X[i,] <- (X[i,] / sd(X[i,]));
    
  }
}

# ----------------------------------------
# -- Covariance --------------------------
# ----------------------------------------

Sx <- (X %*% t(X)) / (ncol(X) - 1);

# ----------------------------------------
# -- Eigenvalues and Eigenvectors --------
# ----------------------------------------

svd <- SVD(Sx, 1.e-5);

eigen.vec <- svd$U;
eigen.val <- diag(svd$S);
eigen.val[is.na(eigen.val)] <- 0;

# ----------------------------------------
# -- CPV | Cumulative Variance -----------
# ----------------------------------------

CPV <- c(5:9)*0.1;
CPV.k <- numeric(length = length(CPV));

for (i in 1:length(CPV)) {
  
  CPV.sum <- 0;
  
  for (k in 1:length(eigen.val)) {
    
    CPV.sum <- sum(eigen.val[1:k]) / sum(eigen.val);
    
    if (CPV.sum > CPV[i]) {
      
      break;
      
    }
  }
  
  CPV.k[i] <- k;
  
}

# ----------------------------------------
# --  k principal components features ----
# ----------------------------------------

for (i in 1:length(CPV)) {
  
  Y <- t(eigen.vec[,1:CPV.k[i]]) %*% X;
  
  OUTPUT.FILE.PCA <- paste0(INPUT.FILE.NAME,(paste0("_pca_", paste0(CPV[i],".csv"))));
  write.csv(x = cbind(t(Y),target),file = file.path(OUTPUT.PATH,OUTPUT.FILE.PCA));
  
}

# ----------------------------------------
# --  Sample contribution on PC1 and PC2 -
# ----------------------------------------

C <- matrix(0, nrow = ncol(X), ncol = 2);

for (i in 1:ncol(C)) {
  
  for (j in 1:nrow(C)) {
    
    C[j,i] <- (t(eigen.vec[,i]) %*% X[,j]) / eigen.val[i];
    
  }
}

# ----------------------------------------
# --  Feature loadings on PC1 and PC2 ----
# ----------------------------------------

L <- matrix(0, nrow = nrow(X), ncol = 2);
L <- cbind(eigen.vec[,1] * sqrt(eigen.val[1]), eigen.vec[,2] * sqrt(eigen.val[2]));

rownames(L) <- rownames(X);

# ----------------------------------------
# -- Plot --------------------------------
# ----------------------------------------

# -- PCA | Explained Variance

plot.matrix <- cbind.data.frame((eigen.val/max(eigen.val)), cumsum(eigen.val/sum(eigen.val)), 1:length(eigen.val));
colnames(plot.matrix) <- c("Relative","Cumulative","PC")
plot.matrix <- melt(plot.matrix, id.vars = "PC")
colnames(plot.matrix)[2] <- "EV";

p.pca <- ggplot(data = plot.matrix, aes(x = PC, y = value, col = EV)) +
geom_point(size=2) +
geom_line(linetype="dashed",size=0.5) +
labs(title = paste0("Principal Component Analysis | ",INPUT.FILE), subtitle = "k principal components by CPV.", y="Explained Variance", x="Principal Components") +
scale_colour_manual(values=c("#42B3D5", "#b1d643")) +
theme_minimal() +
theme(legend.position = c(0.9, 0.325)) +
theme(legend.title=element_blank())

for (i in 1:length(CPV)) {
  
  p.pca <- p.pca + geom_line(y = CPV[i], linetype="dashed",size=0.5, color="#d64343");
  p.pca <- p.pca + annotate("text", x = 0.85*nrow(X), y = (CPV[i] + 0.025), label = paste0("CPV = ", paste0(CPV[i], paste0(" | k = ", CPV.k[i]))), color="#d64343", size = 4);
  
}
 
print(p.pca);
ggsave(paste0(OUTPUT.PATH,OUTPUT.FILE.CPV),width = 5,height = 5);

# -- PCA | Biplot

C <- data.frame(C);
colnames(C) <- c("PC1", "PC2");

p.bip <- ggplot(data = C, aes(x = PC1, y = PC2)) +
geom_point(shape = 21, colour = "black", fill = "#42B3D5", size = 2, stroke = 0.5) + 
labs(title = paste0("Principal Component Analysis | ",INPUT.FILE), subtitle = "Dataset projected on PC1 and PC2.", y=paste0("PC2 (",paste0(round(100*eigen.val[2] / sum(eigen.val), digits = 2),"% of explained variance)")), x=paste0("PC1 (",paste0(round(100*eigen.val[1] / sum(eigen.val), digits = 2),"% of explained variance)"))) +
theme_minimal();

for (i in 1:nrow(L)) {
  
  p.bip <- p.bip + geom_segment(x = 0, xend = L[i,1], y = 0, yend = L[i,2], linejoin = "mitre", lineend = "round", size=0.5, color = "#d64343", arrow=arrow(length = unit(0.08,"inches")))
  p.bip <- p.bip + geom_point(x = L[i,1], y = L[i,2], shape = 21, colour = "black", fill = "#d64343", size = 4, stroke = 0.5)
  p.bip <- p.bip + annotate("text", x = L[i,1], y = L[i,2], label = substr(rownames(L)[i], start = (nchar(rownames(L)[i]) - 1), stop = 1000000L), size = 2);
  
}

print(p.bip)
ggsave(paste0(OUTPUT.PATH,OUTPUT.FILE.BIP),width = 5,height = 5);


