
run_model <- function(date,
                      data_lst, 
                      parameters,
                      cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_alldata"),
                      output_path = "leroux_with_priors_wo_constraint_alldata"){
  
  library(TMB)
  library(Matrix)
  

  
  # ── 1. Clean recompile ────────────────────────────────────────────────────────
  #model_files = paste0("./05_model/",cpp_name)
  try(dyn.unload(dynlib(cpp_file)), silent = TRUE)
  file.remove(paste0(cpp_file,".o"))
  file.remove(paste0(cpp_file,".dll"))
  
  compile(paste0(cpp_file,".cpp"))
  cpp_file_load = gsub("//.","",cpp_file)
  dyn.load(dynlib(cpp_file_load))
  cpp_obj = basename(cpp_file)
  
  # ── 2. Build AD objective ─────────────────────────────────────────────────────
  obj <- MakeADFun(
    data       = data_lst,
    parameters = parameters,
    random     = "phi",       # phi integrated out by Laplace for ALL N areas
    DLL        = cpp_obj,
    silent     = FALSE
  )
  
  # Quick sanity check
  cat("nll at start:", obj$fn(obj$par), "\n")
  cat("Gradient finite:", all(is.finite(obj$gr(obj$par))), "\n")

  # ── 3. Optimise ──────────────────────────────────────────────────────────────
  
  
  lower_bounds_beta = rep(-Inf, ncol(data_lst$X))
  upper_bounds_beta = rep(Inf, ncol(data_lst$X))
  
  
  fit <- nlminb(
    start     = obj$par,
    objective = obj$fn,
    gradient  = obj$gr,
    lower     = c(lower_bounds_beta, -12, -6),  # bound logit_rho
    upper     = c(upper_bounds_beta,  6,  6),
    control   = list(iter.max = 5000, eval.max = 2000)
  )
  cat("Convergence:", fit$convergence, "\n")
  cat("Message:", fit$message, "\n")
  cat("Convergences:", obj$gr(fit$par), "\n")
  
  
  # ── 11. Uncertainty ───────────────────────────────────────────────────────────
  rep <- sdreport(obj, par.fixed = fit$par)
  fixed = summary(rep, "fixed")    # beta, log_tau, logit_rho with SEs
  #cat(fixed)
  # full parameter vector: fixed + random modes
  full_par <- obj$env$last.par.best
  report_vals <- obj$report(full_par)
  
  
  save(rep,file = paste0(output_path,"/",as.character(date),"_report.RData"))
  save(report_vals,file = paste0(output_path,"/",as.character(date),"_report_vals.RData"))
  


  data$phi_w = rep$par.random
  data$phi_w_plogis = plogis(rep$par.random)
  betas = rep$par.fixed[names(rep$par.fixed)=="beta"]
  if(length(betas)>1){
    betas_minus_intercept = betas[2:length(betas)]
    data$relative_risk = plogis(data_lst$X %*%as.vector(rep$par.fixed[names(rep$par.fixed)=="beta"]))
  } else{
    data$relative_risk = plogis(data$phi_w)
  }


  data$p_w = plogis(betas+data$phi_w)
  

  # data_only_aoc= data[which(data$p_w>mean(data$p_w)),]
  # data_only_aoc= data[which(data$phi_w >mean(data$phi_w )),]
  # data_only_aoc = st_union(data_only_aoc)
  # data_only_aoc = st_boundary(data_only_aoc)
  # 

  data$p_mean   <- as.numeric(report_vals$p)
  data$phi_mean   <- as.numeric(plogis(report_vals$phi))
  data$eta_mean <- as.numeric(report_vals$eta)
       
  
  # data$bol_area = 0
  # data[data$p_mean > mean(data$p_mean),]$bol_area = 1
  
  tau = exp(fixed[,"Estimate"]["log_tau"])
  
  
  library(ggplot2)
  p <- ggplot2::ggplot(data) +
        geom_sf(aes(fill =  phi_mean)) +
        scale_fill_viridis_c() +
        theme_minimal()  +
    ggtitle(paste0("tau: ",tau,"; rho: ",data_lst$rho))
       
  ggsave(paste0(output_path,"/plots/",date,"_phi.png"),p)
  

  p <- ggplot2::ggplot(data) +
    geom_sf(aes(fill =  relative_risk )) +
    scale_fill_viridis_c() +
    theme_minimal()  +
    ggtitle(paste0("tau: ",tau,"; rho: ",data_lst$rho))
  
  ggsave(paste0(output_path,"/plots/",date,"_relative_risk.png"),p)
  
  
  
  
  
}



run_model_wrapper <- function(data,
                              data_mat_w,
                              model_covariates,
                              date,
                              path_of_eigenvalue,
                              output_path){
  

  # ── 3. Build W over ALL areas (including NA areas) ───────────────────────────
  W <- as.matrix(data_mat_w)
  W <- (W + t(W)) / 2    # enforce symmetry
  diag(W) <- 0           # no self-loops
  W_sp <- as(W, "dgCMatrix")
  
  
  # ── 4. Eigenvalues of Laplacian (D - W) ──────────────────────────────────────
  D       <- Diagonal(x = rowSums(W))
  L       <- D - W_sp
  
  
  # load or calculate eigenvalues of W
  
  
  if (file.exists(path_of_eigenvalue)){
    
    load(file =  path_of_eigenvalue)
    
  } else {
    
    eig_DmW <- eigen(as.matrix(L), symmetric = TRUE, only.values = TRUE)$values
    save(eig_DmW,file = path_of_eigenvalue)
    
  }
  
  cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")  # expect >= 0 (or tiny negative)
  cat("Negative eigenvalues (> -1e-8 is fine):", sum(eig_DmW < -1e-8), "\n")
  
  
  # ── 7. Dimension checks ───────────────────────────────────────────────────────
  stopifnot(all(is.finite(W_sp@x)))
  stopifnot(all(is.finite(eig_DmW)))
  cat("All dimension checks passed.\n")
  
  load(file = "./data/acled_conflict_mnth.RData")
  

    
  all_dates = date
    

  

  
  N <- nrow(data)
  
  if(is.null(model_covariates)){
    X <- matrix(0,nrow = nrow(data) ) 
    
  }else{
    X <- model.matrix(model_covariates, data) 
  }
  # intercept only -- replace with your formula
  
  lower_bounds_beta = rep(-Inf, ncol(X))
  upper_bounds_beta = rep(Inf, ncol(X))
  
  stopifnot(nrow(X) == N)
  stopifnot(nrow(W_sp) == N)
  stopifnot(ncol(W_sp) == N)
  stopifnot(length(eig_DmW)   == N)
  
  # ── 6. Handle NAs in response ────────────────────────────────────────────────
  obs_idx  <- which(!is.na(data$control_binom)) - 1L   # 0-based for C++
  y_filled <- data$control_binom
  y_filled[is.na(y_filled)] <- 0  
  
  weights_list = mat2listw(W,style ="W",zero.policy=TRUE)
  moran_result <- spdep::moran.test(y_filled, weights_list, adjust = T)
  cat(moran_result[["estimate"]],"\n")
  
  fileConn<-file(paste0(output_path,"/settings.txt"))
  writeLines(as.character(names(moran_result)), fileConn)
  writeLines(as.character(moran_result), fileConn)
  close(fileConn)
  

  
  cat("Observed:", length(obs_idx), "/ Missing:", sum(is.na(data$control_binom)),
      "/ Total:", N, "\n")
  
  stopifnot(length(y_filled)  == N)
  
  data_lst <- list(
    y       = y_filled,
    n       = rep(1, N),
    X       = X,
    W       = W_sp,
    eig_DmW = eig_DmW,
    obs_idx = as.integer(obs_idx),
    beta_prior_sd        = 2.5,
    tau_prior_shape   = 0.5,
    tau_prior_scale     = 2,
    #logit_rho_prior_mean = 10,
    #logit_rho_prior_sd   = 1,
    rho = model_rho
  )
  
  
  parameters <- list(
    beta      = rep(0, ncol(X)),
    phi       = rep(0, N),
    log_tau   = 0#,    # tau = 1
  )
  
  
  
  run_model(date = date,
            data_lst, 
            parameters,
            cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_alldata"),
            output_path = output_path)
  cat("model saved at", output_path)
  
  
}

