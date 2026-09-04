get_ipis_map <- function(date) {
  
  year <- substr(date, 1, 4)
  month <- substr(date, 5, 6)
  
  month_names <- c(
    "01" = "jan", "02" = "feb", "03" = "mar",
    "04" = "apr", "05" = "may", "06" = "jun",
    "07" = "jul", "08" = "aug", "09" = "sep",
    "10" = "oct", "11" = "nov", "12" = "dec"
  )
  
  file <- paste0(
    "./data/IPIS_maps/", year, "/",
    year, "_", month, "_", month_names[month],
    "_M23_aoi_ipis.gpkg"
  )
  
  if (file.exists(file)) {
    read_sf(file)
  } else {
    NULL
  }
}


run_model <- function(date,
                      data_lst, 
                      parameters,
                      estimate_rho = list("rho_fixed" = 0.6),
                      cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_alldata"),
                      output_path = "leroux_with_priors_wo_constraint_alldata",
                      run_if_exists = FALSE){
  
  library(TMB)
  library(Matrix)
  library(spdep)
  library(ggplot2)
  
  
  
  if(run_if_exists | !file.exists(paste0(output_path,"/",as.character(date),"_report.RData"))){
    
    # ── 1. Clean recompile ────────────────────────────────────────────────────────
    # NOTE: cpp_file must point at the FIXED cpp, i.e. the version whose data
    # likelihood loop iterates over `obs_idx` only:
    #     for (int k = 0; k < obs_idx.size(); k++) { int i = obs_idx(k); ... }
    # instead of `for (int k = 0; k < N; k++)`. Otherwise NA rows (filled with
    # y=0) are silently scored as observed failures, which corrupts both the
    # fit and any downstream CV/IC comparison.
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
    
    # FIX: parameters are now beta + log_tau only (rho is a fixed DATA_SCALAR,
    # not estimated -- there is no logit_rho parameter any more). obj$par has
    # length ncol(X) + 1, so lower/upper must match that -- the old extra
    # bound (originally meant for logit_rho) is dropped.
    fit <- nlminb(
      start     = obj$par,
      objective = obj$fn,
      gradient  = obj$gr,
      lower     = c(lower_bounds_beta, -12),  # bound log_tau
      upper     = c(upper_bounds_beta,  6),
      control   = list(iter.max = 5000, eval.max = 2000)
    )
    cat("Convergence:", fit$convergence, "\n")
    cat("Message:", fit$message, "\n")
    cat("Convergences:", obj$gr(fit$par), "\n")
    
    
    # ── 11. Uncertainty ───────────────────────────────────────────────────────────
    rep <- sdreport(obj, par.fixed = fit$par)
    fixed = summary(rep, "fixed")    # beta, log_tau with SEs
    #cat(fixed)
    # full parameter vector: fixed + random modes
    full_par <- obj$env$last.par.best
    report_vals <- obj$report(full_par)
    
    
    save(rep,file = paste0(output_path,"/",as.character(date),"_report.RData"))
    save(report_vals,file = paste0(output_path,"/",as.character(date),"_report_vals.RData"))
    

    
    
  }else {
    load(paste0(output_path,"/",as.character(date),"_report.RData"))
    load(paste0(output_path,"/",as.character(date),"_report_vals.RData"))
    fixed = summary(rep, "fixed") 
    
  }

  
  data$phi_w = rep$par.random
  data$phi_w_plogis = plogis(rep$par.random)
  betas = rep$par.fixed[names(rep$par.fixed)=="beta"]
  if(length(betas)>1){
    betas_minus_intercept = betas[2:length(betas)]
    X_minus_intercept = data_lst$X[, -1, drop = FALSE]
    data$risk = plogis(X_minus_intercept %*% as.vector(betas_minus_intercept) + data$phi_w)
    data$p_w = plogis(betas[1]+data$phi_w)
  } else{
    data$risk = plogis(data$phi_w)
    data$p_w = data$risk
  }
  
  

  
  
  
  compare_ipis = T
  if (compare_ipis){
    
    ipis_map <- get_ipis_map(date)

    
    if (!is.null(ipis_map)) {
    
    
      ipis_map = st_transform(st_union(ipis_map),st_crs(data))
      aoc = st_union(data[which(data$risk>mean(data$risk)),])
      total_area = st_area(ipis_map)+st_area(aoc)
      
      
      overlapping_area = sum(st_area(st_intersection(ipis_map, aoc)))
      overlapping_percent = 2*overlapping_area / total_area
      cat(output_path)
      saveRDS(overlapping_percent,paste0(output_path,"/overlapping_area_",as.character(date),".rds"))
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = st_as_sf(data),
          fill = NA,
          color = "black",
          linewidth = 0.2
        ) +
        geom_sf(data = st_as_sf(aoc), fill = "blue", alpha = 0.5) +
        geom_sf(data = st_as_sf(ipis_map), fill = "red", alpha = 0.5) +
        theme_minimal() 
      
      ggsave(paste0(output_path,"/plots/",date,"_overlapping_area.png"),p)
    }
      
  }
  
  
  # data_only_aoc= data[which(data$p_w>mean(data$p_w)),]
  # data_only_aoc= data[which(data$phi_w >mean(data$phi_w )),]
  # data_only_aoc = st_union(data_only_aoc)
  # data_only_aoc = st_boundary(data_only_aoc)
  # 
  
  data$p_mean   <- as.numeric(report_vals$p)
  data$phi_mean   <- as.numeric(plogis(report_vals$phi))
  data$eta_mean <- as.numeric(report_vals$eta)
  
  
  
  tau = exp(fixed[,"Estimate"]["log_tau"])
  rho = plogis(fixed[,"Estimate"]["logit_rho"])
  cat("rho: ",rho)
  
  library(ggplot2)
  p <- ggplot2::ggplot(data) +
    geom_sf(aes(fill =  phi_mean)) +
    scale_fill_viridis_c() +
    theme_minimal()  +
    ggtitle(paste0("tau: ",tau,"; rho: ",rho))
  
  ggsave(paste0(output_path,"/plots/",date,"_phi.png"),p)
  
  
  p <- ggplot2::ggplot(data) +
    geom_sf(aes(fill =  risk )) +
    scale_fill_viridis_c() +
    theme_minimal()  +
    ggtitle(paste0("tau: ",tau,"; rho: ",rho))
  
  ggsave(paste0(output_path,"/plots/",date,"_relative_risk.png"),p)
  
  

  

  
}





run_model_wrapper <- function(data,
                              data_mat_w,
                              model_covariates,
                              estimate_rho,
                              date,
                              path_of_eigenvalue,
                              output_path,
                              run_if_exists = FALSE){
  
  
  # Keep W sparse from the beginning
  W_sp <- as(data_mat_w, "dgCMatrix")
  
  # Enforce symmetry without creating a dense matrix
  W_sp <- (W_sp + t(W_sp)) / 2
  
  # Remove self-loops
  diag(W_sp) <- 0
  
  # ── 4. Laplacian (D - W) ─────────────────────────────────────────────────────
  
  #D <- Diagonal(x = rowSums(W_sp))
  
  #L <- D - W_sp
  

  
  N <- nrow(data)
  # load or calculate eigenvalues of W
  
  
  if (file.exists(path_of_eigenvalue)){
    
    load(file =  path_of_eigenvalue)

    
  } 
  
  # if (!file.exists(path_of_eigenvalue)) {
  #   cat("calculate eigenvalue /n")
  #   
  #   # Sys.setenv(OMP_NUM_THREADS = "1")
  #   # Sys.setenv(OPENBLAS_NUM_THREADS = "1")
  #   # Sys.setenv(MKL_NUM_THREADS = "1")
  #   
  #   A <- readRDS("~/MasterThesis_AOC_DRC/data/data_for_prediction/mat_w_mixedtime_neighbourfirst_distance_FALSE_FALSE_grid_surface_3000_water_park_street.RData")  # or however you load it
  #   library(RSpectra)
  #   library(Matrix)
  #   
  #   n <- nrow(A)
  #   
  #   # 1. Get spectral range (cheap)
  #   lambda_max <- eigs_sym(A, k = 1, which = "LA")$values
  #   lambda_min <- eigs_sym(A, k = 1, which = "SA")$values
  #   
  #   #lambda_min_all <- eigs_sym(A, k = floor(n/2), which = "SA")$values
  #   # 2. Sweep shifts, using shift-invert near each one
  #   shifts <- seq(lambda_min, lambda_max, length.out = 50)
  #   all_eigs <- c()
  #   
  #   for (s in shifts) {
  #     print(s)
  #     eigs_sym(A, k = 1, sigma = s, which = "LM",opts = list("tol" = 0.001,"retvec" = F))  # LM required when sigma is set
  # 
  #     if (!is.null(r)) all_eigs <- c(all_eigs, r$values)
  #   }
  #   
  #   all_eigs <- sort(unique(round(all_eigs, 8)))
  #   save(all_eigs,file = "~/all_eigs.RData")
  #   
  #   eig_DmW <- eigen(L, symmetric = TRUE, only.values = TRUE)$values
  #   save(eig_DmW,file = path_of_eigenvalue)
  #   
  # } else if(length(eig_DmW)   != N){
  #   cat("calculate eigenvalue /n")
  #   eig_DmW <- eigen(L, symmetric = TRUE, only.values = TRUE)$values
  #   save(eig_DmW,file = path_of_eigenvalue)
  #   
  #   
  # }
  
  #stopifnot(length(eig_DmW)   == N)
  
  #cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")  # expect >= 0 (or tiny negative)
  #cat("Negative eigenvalues (> -1e-8 is fine):", sum(eig_DmW < -1e-8), "\n")
  
  
  # ── 7. Dimension checks ───────────────────────────────────────────────────────
  stopifnot(all(is.finite(W_sp@x)))
  #stopifnot(all(is.finite(eig_DmW)))
  cat("All dimension checks passed.\n")
  
  load(file = "./data/acled_conflict_mnth.RData")
  
  
  
  all_dates = date
  
  
  

  
  X <- model.matrix(model_covariates, data) 
    

  # intercept only -- replace with your formula
  
  lower_bounds_beta = rep(-Inf, ncol(X))
  upper_bounds_beta = rep(Inf, ncol(X))
  
  stopifnot(nrow(X) == N)
  stopifnot(nrow(W_sp) == N)
  stopifnot(ncol(W_sp) == N)

  
  # ── 6. Handle NAs in response ────────────────────────────────────────────────
  obs_idx  <- which(!is.na(data$control_binom)) - 1L   # 0-based for C++
  y_filled <- data$control_binom
  y_filled[is.na(y_filled)] <- 0  
  
  #weights_list = spdep::mat2listw(W_sp,style ="W",zero.policy=TRUE)
  #moran_result <- spdep::moran.test(y_filled, weights_list, adjust = T)
  #cat(moran_result[["estimate"]],"\n")
  
  # fileConn<-file(paste0(output_path,"/settings.txt"))
  # writeLines(as.character(names(moran_result)), fileConn)
  # writeLines(as.character(moran_result), fileConn)
  # close(fileConn)
  
  
  
  cat("Observed:", length(obs_idx), "/ Missing:", sum(is.na(data$control_binom)),
      "/ Total:", N, "\n")
  
  stopifnot(length(y_filled)  == N)
  
  data_lst <- list(
    y       = y_filled,
    n       = rep(1, N),
    X       = X,
    W       = W_sp,
    #Q = L,
    #eig_DmW = eig_DmW,
    obs_idx = as.integer(obs_idx),
    beta_prior_sd        = 2.5,
    tau_prior_shape   = 0.5,
    tau_prior_scale     = 2
    #logit_rho_prior_mean = 0.5,
    #logit_rho_prior_sd   = 0.32
    #rho = model_rho
  )
  
  if ( c("model_rho") %in%names(estimate_rho)){
    data_lst["rho"] = estimate_rho["model_rho"]
    
    cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_all_data_fixed_rho_no_eigenvalues")
    
  } else{
    data_lst["logit_rho_prior_mean"] = estimate_rho["model_logit_rho_prior_mean"]
    data_lst["logit_rho_prior_sd"] = estimate_rho["model_logit_rho_prior_sd"]
    
    cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_all_data_estimate_rho_no_eigenvalues")
    
  }
  
  
  parameters <- list(
    beta      = rep(0, ncol(X)),
    phi       = rep(0, N),
    log_tau   = 1,    # tau = 1
    logit_rho = 0
  )
  
  
  
  run_model(date = date,
            data_lst, 
            parameters,
            cpp_file = cpp_file,
            output_path = output_path,
            run_if_exists = run_if_exists)
  
  cat("model saved at", output_path)
  
  
}

