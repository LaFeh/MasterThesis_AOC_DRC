# test differences spatial structures

data_mat_w <- readRDS("./data/inla_data/data_mat_w_mixed_time.RData")
data_mat_w_decay <- readRDS("./data/inla_data/data_mat_w_mixed_time_decay.RData")
data_mat_b <- readRDS("./data/inla_data/data_mat_b.RData")

data       <- readRDS("./data/inla_data/data_prepared_for_inla.RData")


# ── 3. Build W over ALL areas (including NA areas) ───────────────────────────
B <- as.matrix(data_mat_b)
B <- (B + t(B)) / 2    # enforce symmetry
diag(B) <- 0           # no self-loops
D       <- Diagonal(x = rowSums(B))
L_B       <- D - B

W <- as.matrix(data_mat_w)
W <- (W + t(W)) / 2    # enforce symmetry
diag(W) <- 0           # no self-loops
D       <- Diagonal(x = rowSums(W))
L_w       <- D - W


W_decay <- as.matrix(data_mat_w_decay)
W_decay <- (W_decay + t(W_decay)) / 2    # enforce symmetry
diag(W_decay) <- 0           # no self-loops
D       <- Diagonal(x = rowSums(W_decay))
L_w_decay       <- D - W_decay


# ── 4. Eigenvalues of Laplacian (D - W) ──────────────────────────────────────



cor(unlist(as.list(L_B)), unlist(as.list(L_w)))
cor(unlist(as.list(L_w_decay)), unlist(as.list(L_w)))

E_bin <- eigen(L_B)
E_wt  <- eigen(L_w)

cor(E_bin$vectors[,1], E_wt$vectors[,1])
cor(E_bin$vectors[,2], E_wt$vectors[,2])

