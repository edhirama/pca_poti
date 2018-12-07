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

CPV <- 0.7;

INPUT.PATH <- "../data/input/"
INPUT.FILE <- "facebook.csv"

OUTPUT.PATH <- "../data/output/"
OUTPUT.FILE.COV <- "covariance.csv"
OUTPUT.FILE.PCA <- paste0("pca_", paste0(CPV,".csv"));
OUTPUT.FILE.FIG <- paste0("pca_", paste0(CPV,".png"));

# ------------------------------------------------------------------------------
# -- Function ------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

if(file.exists(file.path(OUTPUT.PATH,OUTPUT.FILE.COV))){
  
  file.remove(file.path(OUTPUT.PATH,OUTPUT.FILE.COV))
  
}

if(file.exists(file.path(OUTPUT.PATH,OUTPUT.FILE.PCA))){
  
  file.remove(file.path(OUTPUT.PATH,OUTPUT.FILE.PCA))
  
}

if(file.exists(file.path(OUTPUT.PATH,OUTPUT.FILE.FIG))){
  
  file.remove(file.path(OUTPUT.PATH,OUTPUT.FILE.FIG))
  
}

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
write.csv(x = Sx,file = file.path(OUTPUT.PATH,OUTPUT.FILE.COV))

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

CPV.sum <- 0;

for (k in 1:length(eigen.val)) {
  
  CPV.sum <- sum(eigen.val[1:k]) / sum(eigen.val);
  
  if (CPV.sum > CPV) {
    
    break;
    
  }
}

# ----------------------------------------
# --  k principal components features ----
# ----------------------------------------

Y <- matrix(0, nrow = k, ncol = ncol(X));

for (i in 1:k) {
  
  Y[i,] <- eigen.vec[,i] %*% X;
  
}

write.csv(x = cbind(t(Y),target),file = file.path(OUTPUT.PATH,OUTPUT.FILE.PCA));

# ----------------------------------------
# -- Plot --------------------------------
# ----------------------------------------

# -- PCA | Explained Variance

plot.matrix <- cbind.data.frame((eigen.val/max(eigen.val)), cumsum(eigen.val/sum(eigen.val)), 1:length(eigen.val));
colnames(plot.matrix) <- c("Relative","Cumulative","PC")

plot.matrix <- melt(plot.matrix, id.vars = "PC")
colnames(plot.matrix)[2] <- "EV";

ggplot(data = plot.matrix, aes(x = PC, y = value, col = EV)) +
geom_point(size=2) +
geom_line(linetype="dashed",size=0.5) +
geom_line(aes(y = CPV), linetype="dashed",size=0.75, color="#d64343") +
labs(title = paste0("Principal Component Analysis | ",INPUT.FILE), subtitle = paste0("CPV of ",paste0(CPV,paste0(" resulted in ", paste0(k, " principal components.")))), y="Explained Variance", x="Principal Components") +
annotate("text", x = 45, y = (CPV + 0.025), label = paste0("CPV = ", CPV), color="#d64343", size = 4) +
scale_colour_manual(values=c("#42B3D5", "#b1d643")) +
theme_minimal() +
theme(legend.position = c(0.9, 0.325)) +
theme(legend.title=element_blank())
ggsave(paste0(OUTPUT.PATH,OUTPUT.FILE.FIG),width = 5,height = 5);

