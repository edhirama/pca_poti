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
  
  k <- min(ncol(A),nrow(A));
  S <- matrix(0, nrow = ncol(A), ncol = ncol(A));
  
  u  <- Francis(A %*% t(A), tol);
  vd <- Francis(t(A) %*% A, tol);
  
  S[1:k,1:k] <- diag(sqrt(vd$D))
  
  return(list(U = u$V, S = S, V = vd$V));
  
}

# ----------------------------------------
# -- Algorithm | Jacobi SVD --------------
# ----------------------------------------

SVD.Jacobi <- function(A, tol){
  
  err <- Inf;

  U <- A;
  V <- diag(1,nrow(A));
  S <- diag(1,nrow(A));
  
  while (err > tol) {
    
    for (j in 2:nrow(A)) {
      
      for (i in 1:(j-1)) {
        
        alpha <- sum(U[,i]^2);
        beta  <- sum(U[,j]^2);
        gamma <- sum(U[,i] %*% U[,j]);
        zeta  <- (beta - alpha) / (2*gamma);
        
        t <- sign(zeta) / (abs(zeta) + sqrt(1 + zeta^2));
        c <- 1 / sqrt(1 + t^2);
        s <- c * t;
        t <- U[,i];
        
        U[,i] <- (c * t) - (s * U[,j]);
        U[,j] <- (s * t) + (c * U[,j]);
        
        V[,i] <- (c * t) - (s * V[,j]);
        V[,j] <- (s * t) + (c * V[,j]);
        
      }
    }
    
    err <- abs(gamma) / sqrt(alpha*beta);
    
  }
  
  for (i in 1:nrow(A)) {
    
    S[i,i] <- sqrt(sum(U[,i]^2));
    U[ ,i] <- U[ ,i] / S[i,i];
    
  }
  
  return(list(U = U, S = S, V = V));
  
}