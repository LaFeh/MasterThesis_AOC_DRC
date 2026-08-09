
create_covariates_list<-function(){
  
  
  covariates_list = list()
  covariates_list[["Intercept"]] = as.formula("~1")
  covariates_list[["total_lag_lead_events_fatalities"]] =  as.formula("~ total_events+total_events_lag+total_events_lead+total_fatalities+total_fatalities_lead+total_fatalities_lag")

  
  return(covariates_list)
}



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
  
  
  save(rep,file = paste0(output_path,"/",date,"_report.RData"))
  save(report_vals,file = paste0(output_path,"/",date,"_report_vals.RData"))
       
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
       
  ggsave(paste0(output_path,"/plots/",date,"_phi.png"),p)
  
  
  
}