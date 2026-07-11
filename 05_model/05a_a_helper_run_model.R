setwd("D:/DRC/gaussian_process_AOC")

run_model <- function(date,model_name = "leroux_with_priors_wo_constraint_alldata",
                      data_lst, parameters,
                      name_identifier = NULL){
  
  library(TMB)
  library(Matrix)
  

  
  # ── 1. Clean recompile ────────────────────────────────────────────────────────
  model_files = paste0("./05_model/",model_name)
  try(dyn.unload(dynlib(model_files)), silent = TRUE)
  file.remove(paste0(model_files,".o"))
  file.remove(paste0(model_files,".dll"))
  
  compile(paste0(model_files,".cpp"))
  dyn.load(dynlib(model_files))
  
  # ── 2. Build AD objective ─────────────────────────────────────────────────────
  obj <- MakeADFun(
    data       = data_lst,
    parameters = parameters,
    random     = "phi",       # phi integrated out by Laplace for ALL N areas
    DLL        = model_name,
    silent     = FALSE
  )
  
  # Quick sanity check
  cat("nll at start:", obj$fn(obj$par), "\n")
  cat("Gradient finite:", all(is.finite(obj$gr(obj$par))), "\n")
  
  # ── 3. Optimise ──────────────────────────────────────────────────────────────
  fit <- nlminb(
    start     = obj$par,
    objective = obj$fn,
    gradient  = obj$gr,
    lower     = c(lower_bounds_beta, -6, -6),  # bound logit_rho
    upper     = c(upper_bounds_beta,  6,  6),
    control   = list(iter.max = 5000, eval.max = 2000)
  )
  cat("Convergence:", fit$convergence, "\n")
  cat("Message:", fit$message, "\n")
  cat("Convergences:", obj$gr(fit$par), "\n")
  
  
  # ── 11. Uncertainty ───────────────────────────────────────────────────────────
  rep <- sdreport(obj, par.fixed = fit$par)
  fixed = summary(rep, "fixed")    # beta, log_tau, logit_rho with SEs
  cat(fixed)
  # full parameter vector: fixed + random modes
  full_par <- obj$env$last.par.best
  report_vals <- obj$report(full_par)
  
  
  save(rep,file = paste0("./05_model/",date,"_",name_identifier,"_report.RData"))
  save(report_vals,file = paste0("./05_model/",date,"_",name_identifier,"_report_vals.RData"))
       
  data$p_mean   <- as.numeric(report_vals$p)
  data$phi_mean   <- as.numeric(plogis(report_vals$phi))
  data$eta_mean <- as.numeric(report_vals$eta)
       
  
  tau = exp(fixed[,"Estimate"]["log_tau"])
  library(ggplot2)
  p <- ggplot2::ggplot(data) +
        geom_sf(aes(fill =  phi_mean)) +
        scale_fill_viridis_c() +
        theme_minimal()  +
    ggtitle(paste0("tau: ",tau,"; rho: ",data_lst$rho))
       
  ggsave(paste0("./05_model/plots/",date,"_",name_identifier,"_phi.png"),p)
  
  
  
}