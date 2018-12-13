# ------------------------------------------------------------------------------
# -- @head: MECAI | MAI5002 | Compare SVD Runtime ------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list=ls())
cat("\014")

# ------------------------------------------------------------------------------
# -- Variable ------------------------------------------------------------------
# ------------------------------------------------------------------------------

INPUT.FILE.NAME <- "svd_runtime"

INPUT.PATH <- "../data/input/"
INPUT.FILE <- paste0(INPUT.FILE.NAME,".csv")

OUTPUT.PATH <- "../data/output/"
OUTPUT.FILE <- paste0(INPUT.FILE.NAME,".png")

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

if (!require("ggplot2")) {
  
  install.packages("ggplot2");
  library("ggplot2");
}

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ----------------------------------------
# -- Read data ---------------------------
# ----------------------------------------

data <- read.csv(file.path(INPUT.PATH,INPUT.FILE), na.strings = "");

# ----------------------------------------
# -- Linear complexity curve -------------
# ----------------------------------------

matrix.size <- seq(from = 10, to = 1000, by = 10);
curve <- matrix(0, nrow = length(matrix.size), ncol = 2);

colnames(curve) <- c("size","runtime");

for (i in 1:length(matrix.size)) {
  
  curve[i,1] <- matrix.size[i];
  curve[i,2] <- matrix.size[i];
  
}

# ----------------------------------------
# -- Plot --------------------------------
# ----------------------------------------

ggplot(data = as.data.frame(data), aes(x = size, y = runtime, col = source)) +
geom_line(size=1) +
geom_line(data = as.data.frame(curve), aes(x=size, y = runtime/200), size=0.5, linetype="dashed", color="#8c43d6") +
annotate("text", x=500, y=3, label="O(n)", size=4, color = "#8c43d6") +
labs(title = "Comparing SVD Runtime", subtitle = "Random square matrix with values extracted from a normal dist.", y="Runtime (seconds)", x="Square matrix size") +
scale_colour_manual(values=c('#42B3D5','#b1d643','#d643d6','#d64343')) +
coord_cartesian(ylim=c(0, 5)) +
theme_minimal() +
theme(legend.position="bottom") +
theme(legend.title=element_blank()) +
ggsave(paste0(OUTPUT.PATH,OUTPUT.FILE),width = 5,height = 5);
