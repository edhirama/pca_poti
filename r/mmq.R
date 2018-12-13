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
# -- MMQ ---------------------------------
# ----------------------------------------

# ----------------------------------------
# -- Eigenvalues and Eigenvectors --------
# ----------------------------------------
solve.svd <- function(x, b = NULL, tol = 1e-015)
{
  # solves the system A %*% y = b, where
  # x=svd.default(A); if b=NULL then gives the Moore-
  # Penrose generalized inverse of A; if the system
  # is overdetermined, computes a least squares fit
  # x = svd of coefficient matrix A (computed by the
  #     default svd function)
  # b = vector/matrix giving right-hand side of
  #     system; nrow(b) must = nrow(A)
  # tol = singular value tolerance. singular values
  #       < tol * max(x$d) are treated as 0
  # output: the solution y or the generalized inverse
  # with attributes 'rank' giving the estimated rank
  # (# singular values not treated as 0) and 'rcond'
  # giving the inverse of the 2-norm condition number
  # (based only on the singular values treated as
  # positive when A is singular)
  sub <- x$d >= tol * max(x$d)
  d <- 1/x$d[sub]
  if(is.null(b)) {
    # compute G-inverse
    b <- x$v[, sub] %*% (d * t(x$u[, sub]))
  }
  else {
    # least-squares sol if overdetermined
    n <- nrow(x$u)
    b <- as.matrix(b)
    if(nrow(b) != n)
      stop("wrong # rows in b")
    b <- x$v[, sub] %*% (d * (t(x$u[, sub]) %*% b))
  }
  attr(b, "rank") <- length(d)
  attr(b, "rcond") <- min(d)/max(d)
  return(b)
}

svd <- SVD(Sx, 1.e-5);

sol <- solve.svd(svd, target[1:48])


