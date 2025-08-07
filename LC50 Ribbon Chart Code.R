  library(readxl)
  
  
  ##load excel files into R as tables, load as many as you would like
  Ebra_storm_copper_LC50_EPA <- read_excel("Ebra_storm_copper_LC50_EPA.xlsx")
  Echi_Ntemp_copper_LC50_EPA <- read_excel("Echi_Ntemp_copper_LC50_EPA.xlsx")
  
  
  #Make chart for first data set 
  # Remove any possible zero doses to avoid log(0)
  dat_storm <- Ebra_storm_copper_LC50_EPA[Ebra_storm_copper_LC50_EPA$dose_mg_L != 0, ]
  
  # Make sure response and total exist and are numeric
  # Fit the probit model
  fit_storm <- glm(
    cbind(response, total - response) ~ log10(dose_mg_L),
    family = binomial(link = "probit"),
    data = dat_storm
  )
  
  # Sequence of doses for smooth curve
  dose_seq_storm <- seq(min(dat_storm$dose_mg_L), max(dat_storm$dose_mg_L), length.out = 200)
  pred_dat_storm <- data.frame(dose_mg_L = dose_seq_storm)
  
  # Predict mortality and confidence intervals (on probit scale)
  pred_storm <- predict(fit_storm, newdata = pred_dat_storm, type = "link", se.fit = TRUE)
  
  # Convert to probability scale
  pred_dat_storm$mortality <- pnorm(pred_storm$fit)
  pred_dat_storm$upper <- pnorm(pred_storm$fit + 1.96 * pred_storm$se.fit)
  pred_dat_storm$lower <- pnorm(pred_storm$fit - 1.96 * pred_storm$se.fit)
  
  library(ggplot2)
  
  ggplot() +
    # Confidence ribbon
    geom_ribbon(
      data = pred_dat_storm,
      aes(x = dose_mg_L, ymin = lower, ymax = upper),
      fill = "#69b3a2", alpha = 0.5
    ) +
    # Fitted curve line - removing but can return if requested
   ## geom_line(
   ##   data = pred_dat_storm,
    ##  aes(x = dose_mg_L, y = mortality),
     ## color = "#4183c4", size = 1.2
   ## ) +
    # Experimental points - removing for now but can be added back as needed
    ###geom_point(
     ### data = dat_storm,
    ###  aes(x = dose_mg_L, y = response / total),
     ### size = 2, shape = 21, fill = "white", color = "#222"
  ###  ) +
    labs(
      x = expression(Cu~(mg/L)),   # Edit 'Cu' or 'CO[2]' as appropriate.
      y = "Mortality",
      title = "LC50 Probit: Storm"
    ) +
    theme_classic(base_size = 15) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold")
    )
  
  ##### make chart for second data set
  ##### Ntemp Data 
  
  
  # Remove any possible zero doses to avoid log(0)
  dat_ntemp <- Echi_Ntemp_copper_LC50_EPA[Echi_Ntemp_copper_LC50_EPA$dose_mg_L != 0, ]
  
  # Make sure response and total exist and are numeric
  # Fit the probit model
  fit_ntemp <- glm(
    cbind(response, total - response) ~ log10(dose_mg_L),
    family = binomial(link = "probit"),
    data = dat_ntemp
  )
  
  # Sequence of doses for smooth curve
  dose_seq_ntemp <- seq(min(dat_ntemp$dose_mg_L), max(dat_ntemp$dose_mg_L), length.out = 200)
  pred_dat_ntemp <- data.frame(dose_mg_L = dose_seq_ntemp)
  
  # Predict mortality and confidence intervals (on probit scale)
  pred_ntemp <- predict(fit_ntemp, newdata = pred_dat_ntemp, type = "link", se.fit = TRUE)
  
  # Convert to probability scale
  pred_dat_ntemp$mortality <- pnorm(pred_ntemp$fit)
  pred_dat_ntemp$upper <- pnorm(pred_ntemp$fit + 1.96 * pred_ntemp$se.fit)
  pred_dat_ntemp$lower <- pnorm(pred_ntemp$fit - 1.96 * pred_ntemp$se.fit)
  
  library(ggplot2)
  
  ggplot() +
    # Confidence ribbon
    geom_ribbon(
      data = pred_dat_ntemp,
      aes(x = dose_mg_L, ymin = lower, ymax = upper),
      fill = "#FFD580", alpha = 0.5
    ) +
    # Fitted curve line - removing but can return if requested
    ## geom_line(
    ##   data = pred_dat_ntemp,
    ##  aes(x = dose_mg_L, y = mortality),
    ## color = "#4183c4", size = 1.2
    ## ) +
    # Experimental points - removing for now but can be added back as needed
    ###geom_point(
    ### data = dat_ntemp,
    ###  aes(x = dose_mg_L, y = response / total),
    ### size = 2, shape = 21, fill = "white", color = "#222"
    ###  ) +
    labs(
      x = expression(Cu~(mg/L)),   # Edit 'Cu' or 'CO[2]' as appropriate.
      y = "Mortality",
      title = "LC50 Probit: ntemp"
    ) +
    theme_classic(base_size = 15) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold")
    )
  
  
  
  pred_dat_storm$Group <- "E. Brac Storm"
  pred_dat_ntemp$Group <- "E. Chi N Temp"
  combined_pred <- rbind(pred_dat_storm, pred_dat_ntemp)
  library(ggplot2)
  
  LC50_double_ribbon<-ggplot(combined_pred, aes(x = dose_mg_L, fill = Group)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      alpha = 0.5
    ) +
    scale_fill_manual(
      values = c("E. Brac Storm" = "darkorange", "E. Chi N Temp" = "#FFD580")
    ) +
    coord_cartesian(xlim = c(0, 0.20)) +
    labs(
      x = expression(Cu~(mg/L)),
      y = "Mortality",
      #title=expression("LC"[50]*" Probit - E. brachionus Storm & E. chihuahuaensis N Temp"),
      fill =  expression(LC[50]*" Population Results")
    ) +
    theme_classic(base_size = 15) +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = c(0.7, 0.45),        # <--- Moves legend inside plot
      legend.justification = "center", 
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 14)
    )
  
  print(LC50_double_ribbon)
  
  ### for savings as 300 dpi
  ggsave("LC50_double_rib.png", plot = LC50_double_ribbon, dpi = 300)