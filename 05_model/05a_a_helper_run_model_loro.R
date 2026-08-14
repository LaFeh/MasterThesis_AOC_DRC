nlminb_with_restart <- function(start, fn, gr, lower, upper, iter.max, eval.max,
                                max_retries = 3){
  fit <- tryCatch(
    nlminb(start = start, objective = fn, gradient = gr, lower = lower, upper = upper,
           control = list(iter.max = iter.max, eval.max = eval.max)),
    error = function(e) list(.error = conditionMessage(e))
  )
  if (!is.null(fit$.error)) return(fit)
  
  retries <- 0
  while (fit$convergence %in% c(1, 8) && retries < max_retries) {
    retries <- retries + 1
    fit_retry <- tryCatch(
      nlminb(start = fit$par, objective = fn, gradient = gr, lower = lower, upper = upper,
             control = list(iter.max = iter.max, eval.max = eval.max, rel.tol = 1e-12)),
      error = function(e) NULL
    )
    if (is.null(fit_retry)) break
    fit <- fit_retry
  }
  fit
}


run_model_loro <- function(date,
                      data_lst, 
                      parameters,
                      cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_alldata"),
                      output_path = "leroux_with_priors_wo_constraint_alldata",
                      do_loro_cv = FALSE){
  
  library(TMB)
  library(Matrix)
  library(spdep)
  
  
  
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
  
  
  
  data$phi_w = rep$par.random
  data$phi_w_plogis = plogis(rep$par.random)
  betas = rep$par.fixed[names(rep$par.fixed)=="beta"]
  if(length(betas)>1){
    betas_minus_intercept = betas[2:length(betas)]
    X_minus_intercept = data_lst$X[, -1, drop = FALSE]
    data$relative_risk = plogis(X_minus_intercept %*% as.vector(betas_minus_intercept) + data$phi_w)
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
  
  
  # ── 12. Leave-one-region-out CV (optional) ────────────────────────────────
  # Only regions in data_lst$obs_idx are ever held out -- missing areas were
  # never in the likelihood and stay untouched. Reuses the DLL already
  # loaded above, and warm-starts every fold from the full-data optimum
  # `fit$par` to keep refitting fast.
  if (do_loro_cv) {
    
    loro_df <- run_loro_cv(
      data_lst         = data_lst,
      parameters       = parameters,
      cpp_obj          = cpp_obj,
      warm_start_fixed = fit$par,
      lower            = c(lower_bounds_beta, -12),
      upper            = c(upper_bounds_beta,  6)
    )
    
    #loro_ok <- loro_df[loro_df$converged, ]
    loro_ok <- loro_df
    
    cv_summary <- list(
      n_folds       = nrow(loro_df),
      n_converged   = nrow(loro_ok),
      elpd          = sum(loro_ok$log_score),
      mean_log_score = mean(loro_ok$log_score),
      brier         = mean((loro_ok$y_true - loro_ok$rr_pred)^2),
      # baseline: leave-one-out prevalence, no covariates, no spatial term
      baseline_elpd           = sum(loro_ok$baseline_log_score),
      baseline_mean_log_score = mean(loro_ok$baseline_log_score),
      baseline_brier          = mean((loro_ok$y_true - loro_ok$baseline_pred)^2)
    )
    # positive = model beats baseline; negative = model is worse than just
    # knowing the prevalence
    cv_summary$log_score_gain_vs_baseline <- cv_summary$mean_log_score - cv_summary$baseline_mean_log_score
    # Brier skill score: 1 = perfect, 0 = no better than baseline, <0 = worse than baseline
    cv_summary$brier_skill_score <- 1 - (cv_summary$brier / cv_summary$baseline_brier)
    
    if (requireNamespace("pROC", quietly = TRUE) &&
        length(unique(loro_ok$y_true)) == 2) {
      cv_summary$auc <- as.numeric(pROC::auc(
        pROC::roc(loro_ok$y_true, loro_ok$rr_pred, quiet = TRUE)
      ))
    }
    
    cat("\nLORO-CV summary for", output_path, ":\n")
    cat("  Converged folds:      ", cv_summary$n_converged, "/", cv_summary$n_folds, "\n")
    cat("  ELPD (model):         ", cv_summary$elpd, "\n")
    cat("  ELPD (baseline):      ", cv_summary$baseline_elpd, "\n")
    cat("  Mean log score:       ", cv_summary$mean_log_score, "\n")
    cat("  Baseline log score:   ", cv_summary$baseline_mean_log_score, "\n")
    cat("  Log score gain:       ", cv_summary$log_score_gain_vs_baseline,
        "  (>0 means model beats prevalence-only baseline)\n")
    cat("  Brier (model):        ", cv_summary$brier, "\n")
    cat("  Brier (baseline):     ", cv_summary$baseline_brier, "\n")
    cat("  Brier skill score:    ", cv_summary$brier_skill_score,
        "  (1=perfect, 0=no better than baseline, <0=worse)\n")
    if (!is.null(cv_summary$auc)) cat("  AUC:                  ", cv_summary$auc, "\n")
    
    save(loro_df, cv_summary,
         file = paste0(output_path,"/",as.character(date),"_loro_cv.RData"))
  }
  
}


# ── Leave-one-region-out CV, restricted to obs_idx regions ───────────────────
# Standalone so it can be reused both from run_model() and called directly
# (e.g. on an already-fitted model object) without repeating the compile step.
run_loro_cv <- function(data_lst, parameters, cpp_obj, warm_start_fixed,
                        lower, upper){
  
  obs_idx0 <- data_lst$obs_idx     # 0-based, full set entering the likelihood
  y_filled <- data_lst$y
  n_folds  <- length(obs_idx0)
  
  results <- vector("list", n_folds)
  
  for (k in seq_len(n_folds)) {
    
    held_out_idx <- obs_idx0[k]
    fold_obs_idx <- obs_idx0[-k]
    
    # ── baseline: leave-one-out prevalence, no covariates, no spatial term.
    # Just the mean of y over every OTHER observed region. Cheap (no refit),
    # computed for every fold so it's directly comparable to the model score.
    baseline_pred <- mean(y_filled[fold_obs_idx + 1])
    
    data_fold <- data_lst
    data_fold$obs_idx <- as.integer(fold_obs_idx)
    
    obj_fold <- MakeADFun(
      data       = data_fold,
      parameters = parameters,
      random     = "phi",
      DLL        = cpp_obj,
      silent     = TRUE
    )
    
    fit_fold <- tryCatch(
      nlminb(
        start     = warm_start_fixed,
        objective = obj_fold$fn,
        gradient  = obj_fold$gr,
        lower     = lower,
        upper     = upper,
        control   = list(iter.max = 5000, eval.max = 2000)
      )

      ,
      error = function(e) {
        if (verbose_failures) cat(sprintf("  LORO fold %d FAILED (error): %s\n", k, conditionMessage(e)))
        NULL
      }
      
    )
    
    if (is.null(fit_fold)) {
      y_true_fail <- y_filled[held_out_idx + 1]
      baseline_pred_clamped_fail <- min(max(baseline_pred, 1e-10), 1 - 1e-10)
      baseline_log_score_fail <- y_true_fail * log(baseline_pred_clamped_fail) +
        (1 - y_true_fail) * log(1 - baseline_pred_clamped_fail)
      results[[k]] <- data.frame(
        idx0 = held_out_idx, y_true = y_true_fail,
        rr_pred = NA, p_pred = NA, log_score = NA,
        baseline_pred = baseline_pred, baseline_log_score = baseline_log_score_fail,
        converged = FALSE
      )
      next
    }
    
    full_par_fold <- obj_fold$env$last.par.best
    par_names_fold <- names(full_par_fold)
    report_fold   <- obj_fold$report(full_par_fold)
    
    # ── relative_risk, computed exactly as in run_model():
    #    - with >1 covariate: plogis(X %*% beta)  (covariate part only)
    #    - with intercept-only: plogis(phi)         (spatial part only)
    betas_fold <- full_par_fold[par_names_fold == "beta"]
    
    if (length(betas_fold) > 1) {
      betas_fold_minus_intercept <- betas_fold[2:length(betas_fold)]
      X_minus_intercept <- data_lst$X[, -1, drop = FALSE]
      eta_rr_fold <- as.numeric(X_minus_intercept %*% as.vector(betas_fold_minus_intercept)) + report_fold$phi
      rr_pred <- plogis(eta_rr_fold)[held_out_idx + 1]
    } else {
      rr_pred <- plogis(report_fold$phi)[held_out_idx + 1]
    }
    
    p_pred <- report_fold$p[held_out_idx + 1]   # kept for diagnostics only
    y_true <- y_filled[held_out_idx + 1]
    
    rr_pred_clamped <- min(max(rr_pred, 1e-10), 1 - 1e-10)
    log_score <- y_true * log(rr_pred_clamped) + (1 - y_true) * log(1 - rr_pred_clamped)
    
    baseline_pred_clamped <- min(max(baseline_pred, 1e-10), 1 - 1e-10)
    baseline_log_score <- y_true * log(baseline_pred_clamped) +
      (1 - y_true) * log(1 - baseline_pred_clamped)
    
    results[[k]] <- data.frame(
      idx0 = held_out_idx, y_true = y_true,
      rr_pred = rr_pred, p_pred = p_pred,
      log_score = log_score,
      baseline_pred = baseline_pred, baseline_log_score = baseline_log_score,
      converged = TRUE
    )
    
    cat(sprintf("  LORO fold %d/%d | idx0=%d | y=%s | rr=%.4f | p=%.4f | log_score=%.4f | baseline=%.4f | baseline_ls=%.4f\n",
                k, n_folds, held_out_idx, y_true, rr_pred, p_pred, log_score,
                baseline_pred, baseline_log_score))
  }
  
  do.call(rbind, results)
}



run_model_wrapper_loro <- function(data,
                              data_mat_w,
                              model_covariates,
                              date,
                              path_of_eigenvalue,
                              output_path,
                              do_loro_cv = FALSE){
  
  
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
  
  
  
  run_model_loro(date = date,
            data_lst, 
            parameters,
            cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_alldata"),
            output_path = output_path,
            do_loro_cv = do_loro_cv)
  cat("model saved at", output_path)
  
  
}

