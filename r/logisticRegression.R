# ------------------------------------------------------------------------------
# -- @head: MECAI | MAI5002 | Logistic Regression ------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list=ls())
cat("\014")

# ------------------------------------------------------------------------------
# -- Variable ------------------------------------------------------------------
# ------------------------------------------------------------------------------

INPUT.PATH <- "../data/input/"
INPUT.FILE <- "facebook.csv"

INPUT.PATH.PCA    <- "../data/output/"
INPUT.FILE.PCA.50 <- "pca_0.5.csv"
INPUT.FILE.PCA.60 <- "pca_0.6.csv"
INPUT.FILE.PCA.70 <- "pca_0.7.csv"
INPUT.FILE.PCA.80 <- "pca_0.8.csv"
INPUT.FILE.PCA.90 <- "pca_0.9.csv"

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

if (!require("caret")) {
  
  install.packages("caret");
  library("caret");
}

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ----------------------------------------
# -- Read data ---------------------------
# ----------------------------------------

data <- read.csv(file.path(INPUT.PATH,INPUT.FILE));
data.pca.50 <- read.csv(file.path(INPUT.PATH.PCA,INPUT.FILE.PCA.50));
data.pca.60 <- read.csv(file.path(INPUT.PATH.PCA,INPUT.FILE.PCA.60));
data.pca.70 <- read.csv(file.path(INPUT.PATH.PCA,INPUT.FILE.PCA.70));
data.pca.80 <- read.csv(file.path(INPUT.PATH.PCA,INPUT.FILE.PCA.80));
data.pca.90 <- read.csv(file.path(INPUT.PATH.PCA,INPUT.FILE.PCA.90));

# ----------------------------------------
# -- Regression --------------------------
# ----------------------------------------

set.seed(123)

  # ----------------------------------------
  # -- Train | 10-Fold Cross Validation ----
  # ----------------------------------------

  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE);

  model <- caret::train(target ~ ., data = data, method = "lm", trControl = trControl);
  model.pca.50 <- caret::train(target ~ ., data = data.pca.50, method = "lm", trControl = trControl);
  model.pca.60 <- caret::train(target ~ ., data = data.pca.60, method = "lm", trControl = trControl);
  model.pca.70 <- caret::train(target ~ ., data = data.pca.70, method = "lm", trControl = trControl);
  model.pca.80 <- caret::train(target ~ ., data = data.pca.80, method = "lm", trControl = trControl);
  model.pca.90 <- caret::train(target ~ ., data = data.pca.90, method = "lm", trControl = trControl);

  # ----------------------------------------
  # -- Performance -------------------------
  # ----------------------------------------

  cat("\014")

  print("# ----------------------------------------")
  print("# -- Dataset | facebook.csv --------------")
  print("# ----------------------------------------")
  print(paste0("# -- RMSE...............", round(mean(model$results$RMSE), digits = 4)))
  print("# ----------------------------------------")
  print(paste0("# -- RMSE.|.CPV.50%.....", round(mean(model.pca.50$results$RMSE), digits = 4)))
  print(paste0("# -- RMSE.|.CPV.60%.....", round(mean(model.pca.60$results$RMSE), digits = 4)))
  print(paste0("# -- RMSE.|.CPV.70%.....", round(mean(model.pca.70$results$RMSE), digits = 4)))
  print(paste0("# -- RMSE.|.CPV.80%.....", round(mean(model.pca.80$results$RMSE), digits = 4)))
  print(paste0("# -- RMSE.|.CPV.90%.....", round(mean(model.pca.90$results$RMSE), digits = 4)))
  print("# ----------------------------------------")
  