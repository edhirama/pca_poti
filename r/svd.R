# ------------------------------------------------------------------------------
# -- @head: MECAI | MAI5002 | SVD ----------------------------------------------
# ------------------------------------------------------------------------------

# ----------------------------------------
# -- Algorithm | QR ----------------------
# ----------------------------------------

QR <- function(A){
  
  V <- A;
  Q <- matrix(0, nrow = nrow(A), ncol = ncol(A));
  R <- matrix(0, nrow = ncol(A), ncol = ncol(A));
  
  for (i in 1:ncol(A)) {
    
    R[i,i] <- sqrt(sum(V[,i]^2))
    Q[ ,i] <- V[ ,i] / R[i,i];
    
    for (j in (i+1):ncol(A)) {
      
      if(j > ncol(A)){
        
        break;
        
      }
      
      R[i,j] <- t(Q[,i]) %*% V[,j];
      V[ ,j] <- V[,j] - (R[i,j]*Q[,i]);
      
    }
    
  }
  
  return(list(Q = Q, R = R));
  
}

# ----------------------------------------
# -- Algorithm | Francis -----------------
# ----------------------------------------

Francis <- function(A, tol){
  
  err <- Inf;
  V   <- diag(1,nrow(A));
  
  while (err > tol) {
    
    qr <- QR(A);
    
    A  <- qr$R %*% qr$Q;
    V  <- V %*% qr$Q;
    
    ERR <- A;
    ERR[upper.tri(ERR)] <- 0;
    diag(ERR) <- 0;
    err <- max(abs(ERR));
    
  }
  
  D <- diag(A);
  
  return(list(V = V, D = D));
  
}

# ----------------------------------------
# -- Algorithm | SVD ---------------------
# ----------------------------------------

SVD <- function(A, tol){
  
  k  <- min(ncol(A),nrow(A));
  S <- matrix(0, nrow = ncol(A), ncol = ncol(A));
  
  u  <- Francis(A %*% t(A), tol);
  vd <- Francis(t(A) %*% A, tol);
  
  S[1:k,1:k] <- diag(sqrt(vd$D))
  
  return(list(U = u$V, S = S, V = vd$V));
  
}