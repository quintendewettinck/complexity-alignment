# Meta --------------------------------------------------------------------
# Author: Quinten De Wettinck

# Packages ----------------------------------------------------------------
library(dplyr)
library(economiccomplexity)
library(flexplot)
library(SciViews)
library(tidyr)
library(tibble)
library(stringr)
library(beepr)
library(openxlsx)
library(plm)
library(readr)
library(data.table)
library(rlang) 
library(fst)
library(glue)
library(car)
library(precrec)
library(ggplot2)
library(gridExtra)
library(kableExtra)

# Import data -------------------------------------------------------------
cp_df <- read_fst(file.path("data", "data_raw", "cp_df_external.fst"), 
                  as.data.table = TRUE)

head(cp_df)

# Config ------------------------------------------------------------------
# Source global objects
source("global_objects.R")

# Define years
logistic_years <- seq(from = min(cp_df$year), to = max(cp_df$year))

# Country-Product ID panel.id
cp_df[, cp_id := interaction(country_iso3, prod_code, drop = TRUE)]

new_M_cp_year_window <- 3L # m = 3 = window length

FONTSIZE_SMALL <- 10
FONTSIZE_MED <- 12
FONTSIZE_LARGE <- 14
FONTSIZE_CAPTION <- 16
FONTSIZE_LATEX_TABLE <- "footnotesize"
FONT_FAMILY <- "Times"

plt_theme_2 <- theme(
  text = element_text(family = FONT_FAMILY),
  plot.title = element_text(face = "plain", size = FONTSIZE_LARGE),
  axis.title = element_text(face = "plain", size = FONTSIZE_LARGE),
  axis.text = element_text(size = FONTSIZE_SMALL),
  legend.direction = "vertical", 
  legend.position = "right", 
  legend.title = element_text(face = "plain", size = FONTSIZE_LARGE),
  legend.text = element_text(size = FONTSIZE_MED), 
  strip.text = element_text(size = FONTSIZE_LARGE)
)


# RESULTS WITH ORTHOGONALISING PSPI & PEPI --------------------------------
{ # ⚙️ Common params ####
  
  use_common_FE <- TRUE # if TRUE, then the individually specified FE in the 
  # model calls are replaced by the ones in common_fixed_effects
  
  common_fixed_effects <- paste(c(
    # "year",
    # "country_iso3",
    # "prod_code",
    # "prod_code^year",
    # "country_iso3^prod_code",
    "country_iso3^year",
    0 ), collapse = " + ")
}

{ # ⚠️ mc_0_tpi_ALL -----------------------------------------------
  # PSPI and PEPI, NO PCI
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    # "pci_p_lag",
    "spi_orth_p_lag",
    "epi_orth_p_lag"
    
    # "pci_p_lag * dens_cp_lag",
    # "spi_orth_p_lag * dens_cp_lag",
    # "epi_orth_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_p_lag * dens_cp_lag"
    # "TPI_orth_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      # income_lvl %in% c("H", "UM") &
      # income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mc_0_tpi_ALL <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mc_0_tpi_ALL:")
  print(summary(mc_0_tpi_ALL))
}

{ # ⚠️ mC_1_tpi_ALL -----------------------------------------------
  # PCI, PSPI and PEPI, no interactions
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag",
    "spi_orth_p_lag",
    "epi_orth_p_lag"
    
    # "pci_p_lag * dens_cp_lag",
    # "spi_orth_p_lag * dens_cp_lag",
    # "epi_orth_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_p_lag * dens_cp_lag"
    # "TPI_orth_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      # income_lvl %in% c("H", "UM") &
      # income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_1_tpi_ALL <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_1_tpi_ALL:")
  print(summary(mC_1_tpi_ALL))
}

{ # ⚠️ mC_2_tpi_ALL -----------------------------------------------
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag * dens_cp_lag",
    "spi_orth_p_lag * dens_cp_lag",
    "epi_orth_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_p_lag * dens_cp_lag"
    # "TPI_orth_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      # income_lvl %in% c("H", "UM") &
      # income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_2_tpi_ALL <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_2_tpi_ALL:")
  print(summary(mC_2_tpi_ALL))
}

{ ## Multicollinearity ####
  formula_template_glm <- paste0("new_M_cp ~ ", predictors)
  formula_tpi_glm <- as.formula(gsub("TPI", "tpi", formula_template_glm))
  
  year_to_plot <- 2016
  
  mC_2_tpi_glm <- glm(
    # formula = formula_tpi_glm,
    formula = new_M_cp ~ 
      dens_cp_lag + 
      log_RCA_cp_lag + 
      pci_p_lag + 
      spi_orth_p_lag + 
      epi_orth_p_lag + 
      pci_p_lag * dens_cp_lag +
      spi_orth_p_lag * dens_cp_lag +
      epi_orth_p_lag * dens_cp_lag,
    
    family = binomial(link = "logit"), 
    data = cp_df %>% filter(year == year_to_plot)
  )
  
  message("mC_2_tpi_glm:")
  print(vif(mC_2_tpi_glm))
  # multicollin for PCI and tpi
  
}

{ ## Confusion matrix ####
  classification_threshold <- 0.50 # probability used for classifying pred
  # classification_threshold <- 0.20
  
  message("ℹ️ TPI Model: ")
  print(caret::confusionMatrix(
    factor(ifelse(fitted(mC_2_tpi_ALL) > classification_threshold, 
                  1, 0), levels = c(0,1)),
    factor(mC_2_tpi_ALL$y, levels = c(0,1)),
    positive = "1"
  ))
}

{ ## ROC-AUC ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_ALL)), # just one model
    labels   = list(as.integer(mC_2_tpi_ALL$y)),
    modnames = "TPI Model", # length-1 character
    dsids    = 1 # single ID
  ) %>%
    { ggplot2::autoplot(., "ROC") + # no facet needed
        geom_text(
          data = auc(.) %>% # compute AUC table
            as.data.frame() %>%
            filter(curvetypes == "ROC") %>%
            mutate(
              label = sprintf("AUC = %.3f", aucs), # format AUC
              x     = 0.45, # tweak coords
              y     = 0.15
            ),
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0, size = 4, family = FONT_FAMILY
        ) +
        theme_minimal() +
        labs(
          x       = "False Positive Rate",
          y       = "True Positive Rate",
          colour  = "Model",
          title   = "ROC Curve for TPI Model (Full sample)"
        ) +
        plt_theme_2
    }
}

{ ## Precision-Recall ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_ALL)),
    labels   = list(as.integer(mC_2_tpi_ALL$y)),
    modnames = "TPI Model",
    dsids    = 1
  ) %>%
    { autoplot(., "PRC") +
        annotate(
          "text",
          x     = 0.6,
          y     = 0.15,
          hjust = 0,
          size  = 2.5,
          label = with(
            auc(.),
            sprintf("AUPRC = %.3f", aucs[curvetypes == "PRC"])
          )
        )
    } +
    theme_minimal() +
    labs(
      x      = "Recall = TP / (TP + FN)",
      y      = "Precision = TP / (TP + FP)",
      colour = "Model",
      title  = "Precision–Recall Curve for TPI Model"
    ) +
    plt_theme_2
}

## Marginal effects ####
{ ### T_p ~ dens_cp ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ## PCI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        pci10    = quantile(pci_p_lag, percentile_low, na.rm = TRUE),
        pci90    = quantile(pci_p_lag, percentile_high, na.rm = TRUE),
        spi_med  = median(spi_orth_p_lag, na.rm = TRUE),
        epi_med  = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci10,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci90,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]~"(relatedness density)"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## SPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        spi10    = quantile(spi_orth_p_lag, percentile_low, na.rm = TRUE),
        spi90    = quantile(spi_orth_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        epi_med  = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi10,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi90,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]~"(relatedness density)"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## EPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        epi10    = quantile(epi_orth_p_lag, percentile_low, na.rm = TRUE),
        epi90    = quantile(epi_orth_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        spi_med  = median(spi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi10,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi90,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]~"(relatedness density)"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

{ ### dens_cp ~ T_p ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ## PCI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        pci_min = min(pci_p_lag,      na.rm = TRUE),
        pci_max = max(pci_p_lag,      na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        spi_med = median(spi_orth_p_lag, na.rm = TRUE),
        epi_med = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          spi_orth_p_lag = .$spi_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          spi_orth_p_lag = .$spi_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(pci_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PCI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PSPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        spi_min = min(spi_orth_p_lag, na.rm = TRUE),
        spi_max = max(spi_orth_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        epi_med = median(epi_orth_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          spi_orth_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          spi_orth_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(spi_orth_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PSPI_{pt-1}^{\\perp}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PEPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        epi_min = min(epi_orth_p_lag, na.rm = TRUE),
        epi_max = max(epi_orth_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        spi_med = median(spi_orth_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          epi_orth_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          spi_orth_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct",
                                   percentile_low * 100)
        ),
        data.frame(
          epi_orth_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          spi_orth_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(epi_orth_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        # x = latex2exp::TeX("$PEPI_{pt-1}^{\\perp}$"),
        x = latex2exp::TeX("$PEPI_{pt-1}^{\\perp}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

{ # ⚠️ mC_2_tpi_HUM -----------------------------------------------
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag * dens_cp_lag",
    "spi_orth_p_lag * dens_cp_lag",
    "epi_orth_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_p_lag * dens_cp_lag"
    # "TPI_orth_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      income_lvl %in% c("H", "UM") &
      # income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_2_tpi_HUM <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_2_tpi_HUM:")
  print(summary(mC_2_tpi_HUM))
}

{ ## Multicollinearity ####
  formula_template_glm <- paste0("new_M_cp ~ ", predictors)
  formula_tpi_glm <- as.formula(gsub("TPI", "tpi", formula_template_glm))
  
  year_to_plot <- 2016
  
  mC_2_tpi_glm <- glm(
    # formula = formula_tpi_glm,
    formula = new_M_cp ~ 
      dens_cp_lag + 
      log_RCA_cp_lag + 
      pci_p_lag + 
      spi_orth_p_lag + 
      epi_orth_p_lag + 
      pci_p_lag * dens_cp_lag +
      spi_orth_p_lag * dens_cp_lag +
      epi_orth_p_lag * dens_cp_lag,
    
    family = binomial(link = "logit"), 
    data = cp_df %>% filter(year == year_to_plot)
  )
  
  message("mC_2_tpi_glm:")
  print(vif(mC_2_tpi_glm))
  # multicollin for PCI and tpi
  
}

{ ## Confusion matrix ####
  classification_threshold <- 0.50 # probability used for classifying pred
  # classification_threshold <- 0.20
  
  message("ℹ️ TPI Model: ")
  print(caret::confusionMatrix(
    factor(ifelse(fitted(mC_2_tpi_HUM) > classification_threshold, 
                  1, 0), levels = c(0,1)),
    factor(mC_2_tpi_HUM$y, levels = c(0,1)),
    positive = "1"
  ))
}

{ ## ROC-AUC ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_HUM)),                    # just one model
    labels   = list(as.integer(mC_2_tpi_HUM$y)),
    modnames = "TPI Model",                                    # length-1 character
    dsids    = 1                                               # single ID
  ) %>%
    { ggplot2::autoplot(., "ROC") +                            # no facet needed
        geom_text(
          data = auc(.) %>%                                   # compute AUC table
            as.data.frame() %>%
            filter(curvetypes == "ROC") %>%
            mutate(
              label = sprintf("AUC = %.3f", aucs),            # format AUC
              x     = 0.45,                                   # tweak coords
              y     = 0.15
            ),
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0, size = 4, family = FONT_FAMILY
        ) +
        theme_minimal() +
        labs(
          x       = "False Positive Rate",
          y       = "True Positive Rate",
          colour  = "Model",
          title   = "ROC Curve for TPI Model"
        ) +
        plt_theme_2
    }
}

{ ## Precision-Recall ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_HUM)),
    labels   = list(as.integer(mC_2_tpi_HUM$y)),
    modnames = "TPI Model",
    dsids    = 1
  ) %>%
    { autoplot(., "PRC") +
        annotate(
          "text",
          x     = 0.6,
          y     = 0.15,
          hjust = 0,
          size  = 2.5,
          label = with(
            auc(.),
            sprintf("AUPRC = %.3f", aucs[curvetypes == "PRC"])
          )
        )
    } +
    theme_minimal() +
    labs(
      x      = "Recall = TP / (TP + FN)",
      y      = "Precision = TP / (TP + FP)",
      colour = "Model",
      title  = "Precision–Recall Curve for TPI Model"
    ) +
    plt_theme_2
}

{ # ⚠️ mC_2_tpi_LML -----------------------------------------------
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag * dens_cp_lag",
    "spi_orth_p_lag * dens_cp_lag",
    "epi_orth_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_p_lag * dens_cp_lag"
    # "TPI_orth_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_orth_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      # income_lvl %in% c("H", "UM") &
      income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_2_tpi_LML <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_2_tpi_LML:")
  print(summary(mC_2_tpi_LML))
}

{ ## Multicollinearity ####
  formula_template_glm <- paste0("new_M_cp ~ ", predictors)
  formula_tpi_glm <- as.formula(gsub("TPI", "tpi", formula_template_glm))
  
  year_to_plot <- 2016
  
  mC_2_tpi_glm <- glm(
    # formula = formula_tpi_glm,
    formula = new_M_cp ~ 
      dens_cp_lag + 
      log_RCA_cp_lag + 
      pci_p_lag + 
      spi_orth_p_lag + 
      epi_orth_p_lag + 
      pci_p_lag * dens_cp_lag +
      spi_orth_p_lag * dens_cp_lag +
      epi_orth_p_lag * dens_cp_lag,
    
    family = binomial(link = "logit"), 
    data = cp_df %>% filter(year == year_to_plot)
  )
  
  message("mC_2_tpi_glm:")
  print(vif(mC_2_tpi_glm))
  # multicollin for PCI and tpi
  
}

{ ## Confusion matrix ####
  classification_threshold <- 0.50 # probability used for classifying pred
  # classification_threshold <- 0.20
  
  message("ℹ️ TPI Model: ")
  print(caret::confusionMatrix(
    factor(ifelse(fitted(mC_2_tpi_LML) > classification_threshold, 
                  1, 0), levels = c(0,1)),
    factor(mC_2_tpi_LML$y, levels = c(0,1)),
    positive = "1"
  ))
}

{ ## ROC-AUC ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_LML)),                    # just one model
    labels   = list(as.integer(mC_2_tpi_LML$y)),
    modnames = "TPI Model",                                    # length-1 character
    dsids    = 1                                               # single ID
  ) %>%
    { ggplot2::autoplot(., "ROC") +                            # no facet needed
        geom_text(
          data = auc(.) %>%                                   # compute AUC table
            as.data.frame() %>%
            filter(curvetypes == "ROC") %>%
            mutate(
              label = sprintf("AUC = %.3f", aucs),            # format AUC
              x     = 0.45,                                   # tweak coords
              y     = 0.15
            ),
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0, size = 4, family = FONT_FAMILY
        ) +
        theme_minimal() +
        labs(
          x       = "False Positive Rate",
          y       = "True Positive Rate",
          colour  = "Model",
          title   = "ROC Curve for TPI Model"
        ) +
        plt_theme_2
    }
}

{ ## Precision-Recall ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_LML)),
    labels   = list(as.integer(mC_2_tpi_LML$y)),
    modnames = "TPI Model",
    dsids    = 1
  ) %>%
    { autoplot(., "PRC") +
        annotate(
          "text",
          x     = 0.6,
          y     = 0.15,
          hjust = 0,
          size  = 2.5,
          label = with(
            auc(.),
            sprintf("AUPRC = %.3f", aucs[curvetypes == "PRC"])
          )
        )
    } +
    theme_minimal() +
    labs(
      x      = "Recall = TP / (TP + FN)",
      y      = "Precision = TP / (TP + FP)",
      colour = "Model",
      title  = "Precision–Recall Curve for TPI Model"
    ) +
    plt_theme_2
}

## Marginal effects HUM vs LML ####
{ ### T_p ~ dens_cp ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ### HUM ####
    ## PCI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        pci10    = quantile(pci_p_lag, percentile_low, na.rm = TRUE),
        pci90    = quantile(pci_p_lag, percentile_high, na.rm = TRUE),
        spi_med  = median(spi_orth_p_lag, na.rm = TRUE),
        epi_med  = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci10,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci90,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## SPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        spi10    = quantile(spi_orth_p_lag, percentile_low, na.rm = TRUE),
        spi90    = quantile(spi_orth_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        epi_med  = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi10,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi90,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## EPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        epi10    = quantile(epi_orth_p_lag, percentile_low, na.rm = TRUE),
        epi90    = quantile(epi_orth_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        spi_med  = median(spi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi10,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi90,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ### LML ####
    ## PCI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        pci10    = quantile(pci_p_lag, percentile_low, na.rm = TRUE),
        pci90    = quantile(pci_p_lag, percentile_high, na.rm = TRUE),
        spi_med  = median(spi_orth_p_lag, na.rm = TRUE),
        epi_med  = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci10,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci90,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## SPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        spi10    = quantile(spi_orth_p_lag, percentile_low, na.rm = TRUE),
        spi90    = quantile(spi_orth_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        epi_med  = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi10,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi90,
            epi_orth_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## EPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        epi10    = quantile(epi_orth_p_lag, percentile_low, na.rm = TRUE),
        epi90    = quantile(epi_orth_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        spi_med  = median(spi_orth_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi10,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_orth_p_lag = .$spi_med,
            epi_orth_p_lag = .$epi90,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

{ ### dens_cp ~ T_p ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ### HUM ####
    ## PCI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        pci_min = min(pci_p_lag,      na.rm = TRUE),
        pci_max = max(pci_p_lag,      na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        spi_med = median(spi_orth_p_lag, na.rm = TRUE),
        epi_med = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          spi_orth_p_lag = .$spi_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          spi_orth_p_lag = .$spi_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(pci_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PCI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PSPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        spi_min = min(spi_orth_p_lag, na.rm = TRUE),
        spi_max = max(spi_orth_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        epi_med = median(epi_orth_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          spi_orth_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          spi_orth_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(spi_orth_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PSPI_{pt-1}^{\\perp}$"), 
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PEPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        epi_min = min(epi_orth_p_lag, na.rm = TRUE),
        epi_max = max(epi_orth_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        spi_med = median(spi_orth_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          epi_orth_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          spi_orth_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct",
                                   percentile_low * 100)
        ),
        data.frame(
          epi_orth_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          spi_orth_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(epi_orth_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PEPI_{pt-1}^{\\perp}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ### LML ####
    ## PCI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        pci_min = min(pci_p_lag,      na.rm = TRUE),
        pci_max = max(pci_p_lag,      na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        spi_med = median(spi_orth_p_lag, na.rm = TRUE),
        epi_med = median(epi_orth_p_lag, na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          spi_orth_p_lag = .$spi_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          spi_orth_p_lag = .$spi_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(pci_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PCI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PSPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        spi_min = min(spi_orth_p_lag, na.rm = TRUE),
        spi_max = max(spi_orth_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        epi_med = median(epi_orth_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          spi_orth_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          spi_orth_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          epi_orth_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(spi_orth_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PSPI_{pt-1}^{\\perp}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PEPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        epi_min = min(epi_orth_p_lag, na.rm = TRUE),
        epi_max = max(epi_orth_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        spi_med = median(spi_orth_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          epi_orth_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          spi_orth_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct",
                                   percentile_low * 100)
        ),
        data.frame(
          epi_orth_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          spi_orth_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(epi_orth_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PEPI_{pt-1}^{\\perp}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

# Marginal effects line plots ####
# ⚠️ SEE logistic_persistent_marginal_effects_line.R 


# Check models ------------------------------------------------------------
summary(mC_1_tpi_ALL) 
summary(mC_2_tpi_ALL)
summary(mC_2_tpi_HUM)
summary(mC_2_tpi_LML)

{ # Verify nr observations --------------------------------------------------
  # Manually check the number of countries, products and years
  verify_nr_obs <- FALSE
  
  if (verify_nr_obs == TRUE) {
    # Variables 
    logistic_vars <- unique(c(
      all.vars(as.formula(paste0("~", predictors))),
      paste0(c("new_M_cp"))
    ))
    
    cp_df %>%
      filter(!is.na(new_M_cp)) %>%
      select(year, country_iso3, prod_code, 
             M_cp, new_M_cp) %>% 
      arrange(-year) %>% 
      head(n = 20)
    
    bind_rows(
      # row for “Without controls”
      cp_df %>%
        filter(
          year %in% logistic_years,
          if_all(all_of(logistic_vars), ~ !is.na(.))
        ) %>%
        group_by(year) %>% 
        # keep only those years where at least one new_M_cp != 0
        filter(any(new_M_cp != 0)) %>%  
        ungroup() %>%
        summarise(
          n_countries = n_distinct(country_iso3),
          n_products  = n_distinct(prod_code),
          n_years     = n_distinct(year)
        ) %>%
        mutate(set = "All"),
      
      cp_df %>%
        filter(
          year %in% logistic_years,
          income_lvl %in% c("H", "UM"), 
          if_all(all_of(logistic_vars), ~ !is.na(.))
        ) %>%
        group_by(year) %>% 
        # keep only those years where at least one new_M_cp != 0
        filter(any(new_M_cp != 0)) %>%  
        ungroup() %>%
        summarise(
          n_countries = n_distinct(country_iso3),
          n_products  = n_distinct(prod_code),
          n_years     = n_distinct(year)
        ) %>%
        mutate(set = "H & UM"),
      
      cp_df %>%
        filter(
          year %in% logistic_years,
          income_lvl %in% c("LM", "L"), 
          if_all(all_of(logistic_vars), ~ !is.na(.))
        ) %>%
        group_by(year) %>% 
        # keep only those years where at least one new_M_cp != 0
        filter(any(new_M_cp != 0)) %>%  
        ungroup() %>%
        summarise(
          n_countries = n_distinct(country_iso3),
          n_products  = n_distinct(prod_code),
          n_years     = n_distinct(year)
        ) %>%
        mutate(set = "LM & L"),
      
    ) %>% select(set, n_countries, n_years)
    # corresponds to infra, except for one country for H & UM
    # perhaps this country does not have new_M_cp = 1 for any rows and is 
    # therefore removed? 
    
    cp_df %>%
      group_by(country_iso3, year) %>%
      filter(year %in% logistic_years,
             all(new_M_cp == 0)) %>%
      ungroup() %>%
      distinct(country_iso3, year)
    # this is correct, but I don't see a country that never has 
    
    # Country for which new_M_cp is always 0
    cp_df %>%
      group_by(country_iso3) %>%
      filter(all(new_M_cp == 0, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct(country_iso3)
    # GNQ --> this country is probably removed
  }
}

{ # ➡️ Export results ----------------------------------------------------------
  
  coef_digits <- 3 # nr of digits for the exported coefficients
  
  { ## Export models to .rds ####
    # store_models <- TRUE # TRUE: save the estimated models to .rds files
    store_models <- FALSE
    
    export_models_date_time <- format(Sys.time(), "%Y-%m-%d_%Hh%M", 
                                      tz = "Europe/Brussels")
    
    if (store_models == TRUE) { # export fixest models in one go
      out_dir <- file.path(
        "04_Analysis",
        "Logistic regressions complexity over sustainability",
        "persistent RCAs (post FDS)",
        "stored_models",
        paste0("logistic_models_", export_models_date_time)
      )
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      for (nm in c("mC_1_tpi_ALL", "mC_2_tpi_ALL", 
                   "mC_2_tpi_HUM", "mC_2_tpi_LML")) {
        saveRDS(get(nm), file.path(out_dir, paste0(nm, ".rds")), 
                compress = "xz")
      }
      
      writeLines(
        c(
          sprintf("date_time: %s", export_models_date_time),
          sprintf("git_commit_id: %s",
                  tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                           error = function(e) NA_character_)),
          sprintf("note: %s", "CUSTOM NOTE")
        ),
        file.path(out_dir, "metadata.txt")
      )
      
      message("Saved models to: ", out_dir)
    } else {
      message("`store_models = FALSE`: models not stored")
    }
  }
  
  { ## Additional fit stats ####
    # Nr countries
    fitstat_register(
      type  = "ncountries",
      alias = "Countries",
      fun   = function(x){
        fe_list <- fixef(x, sorted = FALSE)
        # 1a. exact country_FE if present
        if ("country_iso3" %in% names(fe_list)) {
          return( length(fe_list[["country_iso3"]]) )
        }
        # 1b. else, look for the country*year interaction
        int_nm <- grep("^country_iso3\\^", names(fe_list), value = TRUE)
        if (length(int_nm)) {
          lvl <- names(fe_list[[int_nm[1]]])           # e.g. "BEL_2016"
          return( length(unique(sub("_.*$", "", lvl))) )
        }
        # 1c. fallback: pick the first FE and parse
        lvl <- names(fe_list[[1L]])
        length(unique(sub("_.*$", "", lvl)))
      }
    )
    
    # Nr years
    fitstat_register(
      type  = "nyears",
      alias = "Years",
      fun   = function(x){
        fe_list <- fixef(x, sorted = FALSE)
        # 2a. exact year_FE if present
        if ("year" %in% names(fe_list)) {
          return( length(fe_list[["year"]]) )
        }
        # 2b. else, look for the country*year interaction
        int_nm <- grep("\\^year$", names(fe_list), value = TRUE)
        if (length(int_nm)) {
          lvl <- names(fe_list[[int_nm[1]]])           # e.g. "BEL_2016"
          return( length(unique(sub(".*_", "", lvl))) )
        }
        # 2c. fallback: pick the first FE and parse
        lvl <- names(fe_list[[1L]])
        length(unique(sub(".*_", "", lvl)))
      }
    )
    
    # TODO: nyears and ncountries not correctly extracted if country*year FE 
    # are not used... 
    
    # ROC-AUC
    fitstat_register(
      type  = "auc",
      alias = "ROC-AUC",
      fun   = function(x){
        resp  <- x$y                # the observed 0/1 outcome
        preds <- fitted(x)          # the model’s fitted probabilities
        as.numeric( pROC::auc(resp, preds) )
      }
    )
    
    # F1
    fitstat_register(
      type  = "f1",
      alias = "F$_1$",
      fun   = function(x) {
        
        resp  <- x$y         # observed 0/1 outcome
        probs <- fitted(x)   # predicted probabilities
        
        # ── 1. choose a threshold ────────────────────────────────────────────────
        thr   <- 0.1 # fixed cut-off
        # 0.1275 for 0.03; 0.15 for 0.04; 0.1713 for 0.08
        # thr <- pROC::coords(pROC::roc(resp, probs), "best",
        #                     ret = "threshold", best.method = "youden")
        # ─────────────────────────────────────────────────────────────────────────
        
        pred  <- as.integer(probs >= thr)
        
        TP <- sum(pred == 1 & resp == 1)
        FP <- sum(pred == 1 & resp == 0)
        FN <- sum(pred == 0 & resp == 1)
        
        if (TP + FP + FN == 0) return(NaN)          # edge case
        return( 2 * TP / (2 * TP + FP + FN) )       # F1 formula
      }
    )
  }
  
  ## Model table ####
  modB <- etable(
    # Models
    mC_1_tpi_ALL, mC_2_tpi_ALL, mC_2_tpi_HUM, mC_2_tpi_LML, 
    headers = list("Full Sample" = 2, "H & UM" = 1, "LM & L" = 1), 
    dict = c(
      setNames("$\\mathbb{{P}}(\\Delta M_{cpt} = 1)$",
               as.character(formula(mC_2_tpi_ALL))[2]),
      country_iso3     = "$c$",
      prod_code        = "$p$",
      year             = "$t$",
      dens_cp_lag      = "$\\omega_{cpt-1}$",
      log_RCA_cp_lag   = "$\\log(\\mathit{RCA}_{cpt-1})$",
      pci_p_lag        = "$\\mathit{PCI}_{pt-1}$",
      spi_orth_p_lag   = "$\\mathit{PSPI}^{\\perp}_{pt-1}$",
      epi_orth_p_lag   = "$\\mathit{PEPI}^{\\perp}_{pt-1}$"
    ), 
    tex = TRUE, 
    style.tex = style.tex(
      line.top    = "\\toprule",    
      line.bottom = "\\bottomrule", 
      caption.after = "\\vspace{6pt}\n\\footnotesize"
      # tablefoot   = FALSE # drop the extra mid-rules at the foot
    ), 
    # fitstat = c("n", "pr2", "bic"),
    fitstat = c(
      "n", "ncountries", "nyears", 
      # "nproducts", # does not work
      "pr2", 
      # "pr2_p", 
      # "aic", "bic", 
      "auc", 
      "f1"
    ),
    digits = paste0("r", coef_digits), digits.stats = paste0("r", coef_digits),
    label = "modB", 
    title = paste0("Product Entry Model Estimates")
  )
  
  # Replace [htbp] with [H]
  modB <- gsub(
    x = modB,
    pattern = "\\\\begin\\{table\\}\\[.*?\\]",
    replacement = "\\\\begin{table}[H]"
  )
  
  # remove the centering
  modB <- sub("\\\\centering\\s*", "", modB, perl = TRUE)
  
  # { # Add resizebox around the tabular
  #   modB <- gsub(
  #     pattern = "(\\\\begin\\{tabular\\}\\{lcccc\\})",
  #     replacement = "\\\\resizebox{\\\\columnwidth}{!}{%\n\\1",
  #     x = modB
  #   )
  #   
  #   modB <- gsub(
  #     pattern = "(\\\\end\\{tabular\\})",
  #     replacement = "\\1\n}",
  #     x = modB
  #   )
  # }
  
  writeLines(
    text = as.character(modB), 
    "data/data_processed/LaTeX data values/modB.txt"
  )
  # TODO: add one model without interactions (only direct effects)
  
  ## Export coefficients ####
  ### VIFs ####
  cat(sub(
    # Add fontsize to table
    "(\\\\caption\\{[^}]*\\})", 
    paste0("\\1\n\\\\", "small"), # use small here because the table is small
    kable(
      tibble(
        Variable = names(vif(mC_2_tpi_glm, type = "terms")),
        VIF      = as.numeric(vif(mC_2_tpi_glm, type = "terms"))
      ) |>
        arrange(match(
          Variable,
          c(
            "dens_cp_lag",
            "log_RCA_cp_lag",
            "pci_p_lag",
            "spi_orth_p_lag",
            "epi_orth_p_lag",
            "dens_cp_lag:pci_p_lag",
            "dens_cp_lag:spi_orth_p_lag",
            "dens_cp_lag:epi_orth_p_lag"
          ))) |>
        mutate(
          Variable = dplyr::recode(Variable, !!!c(
            "dens_cp_lag"             = "$\\omega_{cpt-1}$",
            "log_RCA_cp_lag"          = "$\\log(\\mathit{RCA}_{cpt-1})$",
            "pci_p_lag"               = "$\\mathit{PCI}_{pt-1}$",
            "spi_orth_p_lag"               = "$\\mathit{PSPI}_{pt-1}^{\\perp}$",
            "epi_orth_p_lag"               = "$\\mathit{PEPI}_{pt-1}^{\\perp}$",
            "dens_cp_lag:pci_p_lag"   = "$\\omega_{cpt-1} \\times \\mathit{PCI}_{pt-1}$",
            "dens_cp_lag:spi_orth_p_lag"   = "$\\omega_{cpt-1} \\times \\mathit{PSPI}_{pt-1}^{\\perp}$",
            "dens_cp_lag:epi_orth_p_lag"   = "$\\omega_{cpt-1} \\times \\mathit{PEPI}_{pt-1}^{\\perp}$"
          ))
        ),
      format   = "latex",
      caption  = "VIFs for Product Entry Model (with orthogonalisation)",
      booktabs = TRUE,
      digits   = 2,
      escape   = FALSE,
      col.names = c("Variable", "VIF")
    ) |>
      kable_styling(
        latex_options = "HOLD_position", # ← produces [H]
        full_width    = FALSE,
        position      = "left"
      )
  ),
  file = "data/data_processed/LaTeX data values/mC_2_tpi_glm_orth_vifs.txt"
  )
  
  { # Export new_M_cp prevalence (proportion of new_M_cp = 1 out of all rows 
    # for which the backward condition is met)
    # Prevalence (%)
    export_latex_variable(
      variable = "new_M_cp_prevalence", 
      value = cp_df[
        year %in% logistic_years & prev_M_cp_zero == 1,
        .(count = .N),
        by = new_M_cp
      ][
        , pct := 100 * count / sum(count)
      ][
        new_M_cp == 1,
        sprintf("%.2f", pct)
      ]
    )
    
    # Prevalence (count)
    export_latex_variable(
      variable = "new_M_cp_count", 
      value = cp_df[
        year %in% logistic_years & prev_M_cp_zero == 1,
        .(count = .N),
        by = new_M_cp
      ][
        new_M_cp == 1,
        count
      ]
    )
    
    # Total number of rows
    export_latex_variable(
      variable = "eligible_rows_count", 
      value = format(
        cp_df[
          year %in% logistic_years & prev_M_cp_zero == 1,
          .N
        ],
        big.mark = ",",
        scientific = FALSE
      )
    )
  }
  
  { # Coefficients
    
    ### MODEL mC_1 ####
    # Coefficient of relatedness
    # A 0.1 point increase in density corresponds to an increase in the 
    # log-odds of entry of
    coef(mC_1_tpi_ALL)["dens_cp_lag"] * 0.1
    
    export_latex_variable(
      variable = "mC_1_tpi_ALL_dens_logodds", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      coef(mC_1_tpi_ALL)["dens_cp_lag"])
    )
    
    export_latex_variable(
      variable = "mC_1_tpi_ALL_dens_logodds_0.1", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      coef(mC_1_tpi_ALL)["dens_cp_lag"] * 0.1)
    )
    
    # which corresponds to an odds-ratio of
    exp(coef(mC_1_tpi_ALL)["dens_cp_lag"] * 0.1)
    
    export_latex_variable(
      variable = "mC_1_tpi_ALL_dens_oddsratio", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      exp(coef(mC_1_tpi_ALL)["dens_cp_lag"] * 0.1))
    )
    
    # the odds of entry are *doubled* at an increase in relatedness of 
    log(2) / coef(mC_1_tpi_ALL)["dens_cp_lag"]
    
    ## MODEL mC_2 ####
    # Interaction PEPI * relatedness
    (mC_2_tpi_ALL_delta_5 <- coef(mC_2_tpi_ALL)["epi_orth_p_lag"])
    (mC_2_tpi_ALL_delta_8 <- coef(mC_2_tpi_ALL)["dens_cp_lag:epi_orth_p_lag"])
    (mC_2_tpi_ALL_dens_threshold_epi <- - mC_2_tpi_ALL_delta_5 / 
        mC_2_tpi_ALL_delta_8)
    
    export_latex_variable(
      variable = "mC_2_tpi_ALL_delta_5", 
      value = sprintf(paste0("%.", coef_digits, "f"), mC_2_tpi_ALL_delta_5)
    )
    export_latex_variable(
      variable = "mC_2_tpi_ALL_delta_8", 
      value = sprintf(paste0("%.", coef_digits, "f"), mC_2_tpi_ALL_delta_8)
    )
    export_latex_variable(
      variable = "mC_2_tpi_ALL_dens_threshold_epi", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      mC_2_tpi_ALL_dens_threshold_epi)
    )
    
    # Interaction PCI * relatedness
    (mC_2_tpi_ALL_delta_3 <- coef(mC_2_tpi_ALL)["pci_p_lag"])
    (mC_2_tpi_ALL_delta_6 <- coef(mC_2_tpi_ALL)["dens_cp_lag:pci_p_lag"])
    (mC_2_tpi_ALL_dens_threshold_pci <- - mC_2_tpi_ALL_delta_3 / 
        mC_2_tpi_ALL_delta_6)
    (logistic_threshold_year <- max(logistic_years))
    # Percentage of products in 2021 for which density is > the threshold
    (pct_above_mC_2_tpi_ALL_dens_threshold_pci <- cp_df[
      year == logistic_threshold_year,
      mean(dens_cp > mC_2_tpi_ALL_dens_threshold_pci, na.rm = TRUE) * 100
    ])
    
    # Number of countries that have at least 1 product for which density is > 
    # the threshold. These are the countries that have at least 1 opportunity
    # for which their density is high enough so that they can diversify into
    # a more complex product
    (n_countries_above_mC_2_tpi_ALL_dens_threshold_pci <- cp_df[
      year == logistic_threshold_year & 
        M_cp   == 0 & 
        dens_cp > mC_2_tpi_ALL_dens_threshold_pci,
      uniqueN(country_iso3)
    ])
    
    # Identify which countries these are
    cp_df[
      year == logistic_threshold_year & 
        M_cp == 0 & 
        dens_cp > mC_2_tpi_ALL_dens_threshold_pci,
      .(country_iso3, income_lvl)
    ] %>%
      arrange(income_lvl) %>% 
      unique()
    
    # Average density in 2021 for HUM vs LML
    (avg_dens_HUM <- cp_df[
      year == 2021 &
        income_lvl %in% c("H","UM") &
        M_cp == 0,
      mean(dens_cp)
    ])
    
    (avg_dens_LML <- cp_df[
      year == 2021 &
        income_lvl %in% c("LM","L") &
        M_cp == 0,
      mean(dens_cp)
    ])
    
    ## dens_cp_P90 ####
    # 90th percentile of density for unattained products in 2021
    # For all c
    (dens_cp_P90 <- cp_df[
      year == 2021 & M_cp == 0,
      quantile(dens_cp, 0.90, na.rm = TRUE)
    ])
    
    # For HUM
    (dens_cp_P90_HUM <- cp_df[
      year == 2021 & M_cp == 0 & income_lvl %in% c("H", "UM"),
      quantile(dens_cp, 0.90, na.rm = TRUE)
    ])
    
    # For LML
    (dens_cp_P90_LML <- cp_df[
      year == 2021 & M_cp == 0 & income_lvl %in% c("LM", "L"),
      quantile(dens_cp, 0.90, na.rm = TRUE)
    ])
    
    ## Avg dens for top k = 50 closest p ####
    (avg_dens_cp_topk50_HUM <- cp_df[
      year == 2021 & M_cp == 0 & top_k50_dens == 1 & income_lvl %in% c("H", 
                                                                       "UM"),
      mean(dens_cp, na.rm = TRUE)
    ])
    
    (avg_dens_cp_topk50_LML <- cp_df[
      year == 2021 & M_cp == 0 & top_k50_dens == 1 & income_lvl %in% c("LM", 
                                                                       "L"),
      mean(dens_cp, na.rm = TRUE)
    ])
    
    export_latex_variable(
      variable = "avg_dens_cp_topk50_HUM", 
      value = sprintf(paste0("%.", coef_digits, "f"), avg_dens_cp_topk50_HUM)
    )
    export_latex_variable(
      variable = "avg_dens_cp_topk50_LML", 
      value = sprintf(paste0("%.", coef_digits, "f"), avg_dens_cp_topk50_LML)
    )
    
    # Dens threshold pci
    # density thresholds after which PCI has positive effect on entry for HUM 
    # vs LML
    (mC_2_tpi_HUM_dens_threshold_pci <- - coef(mC_2_tpi_HUM)["pci_p_lag"] / 
        coef(mC_2_tpi_HUM)["dens_cp_lag:pci_p_lag"])
    (mC_2_tpi_LML_dens_threshold_pci <- - coef(mC_2_tpi_LML)["pci_p_lag"] / 
        coef(mC_2_tpi_LML)["dens_cp_lag:pci_p_lag"])
    
    # Dens threshold epi
    (mC_2_tpi_HUM_dens_threshold_epi <- - coef(mC_2_tpi_HUM)["epi_orth_p_lag"] / 
        coef(mC_2_tpi_HUM)["dens_cp_lag:epi_orth_p_lag"])
    (mC_2_tpi_LML_dens_threshold_eci <- - coef(mC_2_tpi_LML)["epi_orth_p_lag"] / 
        coef(mC_2_tpi_LML)["dens_cp_lag:epi_orth_p_lag"])
    
    # Export values
    export_latex_variable(
      variable = "mC_2_tpi_ALL_delta_3", 
      value = sprintf(paste0("%.", coef_digits, "f"), mC_2_tpi_ALL_delta_3)
    )
    export_latex_variable(
      variable = "mC_2_tpi_ALL_delta_6", 
      value = sprintf(paste0("%.", coef_digits, "f"), mC_2_tpi_ALL_delta_6)
    )
    export_latex_variable(
      variable = "mC_2_tpi_ALL_dens_threshold_pci", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      mC_2_tpi_ALL_dens_threshold_pci)
    )
    export_latex_variable(
      variable = "pct_above_mC_2_tpi_ALL_dens_threshold_pci", 
      value = sprintf("%.2f", pct_above_mC_2_tpi_ALL_dens_threshold_pci)
    )
    export_latex_variable(
      variable = "n_countries_above_mC_2_tpi_ALL_dens_threshold_pci", 
      value = sprintf("%.0f", n_countries_above_mC_2_tpi_ALL_dens_threshold_pci)
    )
    export_latex_variable(
      variable = "logistic_threshold_year", 
      value = sprintf("%.0f", logistic_threshold_year)
    )
    export_latex_variable(
      variable = "dens_cp_P90", 
      value = sprintf(paste0("%.", coef_digits, "f"), dens_cp_P90)
    )
    
    export_latex_variable(
      variable = "mC_2_tpi_HUM_dens_threshold_pci", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      mC_2_tpi_HUM_dens_threshold_pci)
    )
    export_latex_variable(
      variable = "mC_2_tpi_LML_dens_threshold_pci", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      mC_2_tpi_LML_dens_threshold_pci)
    )
    
    export_latex_variable(
      variable = "avg_dens_HUM", 
      value = sprintf("%.2f", avg_dens_HUM)
    )
    export_latex_variable(
      variable = "avg_dens_LML", 
      value = sprintf("%.2f", avg_dens_LML)
    )
    
    ## Coef of EPI-Density interaction ####
    export_latex_variable(
      variable = "mC_2_tpi_HUM_delta_8", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      coef(mC_2_tpi_HUM)["dens_cp_lag:epi_orth_p_lag"])
    )
    export_latex_variable(
      variable = "mC_2_tpi_LML_delta_8", 
      value = sprintf(paste0("%.", coef_digits, "f"), 
                      coef(mC_2_tpi_LML)["dens_cp_lag:epi_orth_p_lag"])
    )
  }
  
  modB
  
  beep(10)
}

# RESULTS WITHOUT ORTHOGONALISING PSPI & PEPI -----------------------------
{ # ⚙️ Common params ####
  
  use_common_FE <- TRUE # if TRUE, then the individually specified FE in the 
  # model calls are replaced by the ones in common_fixed_effects
  
  common_fixed_effects <- paste(c(
    # "year",
    # "country_iso3",
    # "prod_code",
    # "prod_code^year",
    # "country_iso3^prod_code",
    "country_iso3^year",
    0 ), collapse = " + ")
}

{ # ⚠️ mC_1_tpi_ALL -----------------------------------------------
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag",
    "spi_p_lag",
    "epi_p_lag"
    
    # "pci_p_lag * dens_cp_lag",
    # "spi_p_lag * dens_cp_lag",
    # "epi_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_p_lag * dens_cp_lag"
    # "TPI_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      # income_lvl %in% c("H", "UM") &
      # income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_1_tpi_ALL <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_1_tpi_ALL:")
  print(summary(mC_1_tpi_ALL))
}

{ # ⚠️ mC_2_tpi_ALL -----------------------------------------------
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag * dens_cp_lag",
    "spi_p_lag * dens_cp_lag",
    "epi_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_p_lag * dens_cp_lag"
    # "TPI_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      # income_lvl %in% c("H", "UM") &
      # income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_2_tpi_ALL <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_2_tpi_ALL:")
  print(summary(mC_2_tpi_ALL))
}

{ ## Multicollinearity ####
  formula_template_glm <- paste0("new_M_cp ~ ", predictors)
  formula_tpi_glm <- as.formula(gsub("TPI", "tpi", formula_template_glm))
  
  year_to_plot <- 2016
  
  mC_2_tpi_glm <- glm(
    # formula = formula_tpi_glm,
    formula = new_M_cp ~ 
      dens_cp_lag + 
      log_RCA_cp_lag + 
      pci_p_lag + 
      spi_p_lag + 
      epi_p_lag + 
      pci_p_lag * dens_cp_lag +
      spi_p_lag * dens_cp_lag +
      epi_p_lag * dens_cp_lag,
    
    family = binomial(link = "logit"), 
    data = cp_df %>% filter(year == year_to_plot)
  )
  
  message("mC_2_tpi_glm:")
  print(vif(mC_2_tpi_glm))
  # multicollin for PCI and tpi
  
}

{ ## Confusion matrix ####
  classification_threshold <- 0.50 # probability used for classifying pred
  # classification_threshold <- 0.20
  
  message("ℹ️ TPI Model: ")
  print(caret::confusionMatrix(
    factor(ifelse(fitted(mC_2_tpi_ALL) > classification_threshold, 
                  1, 0), levels = c(0,1)),
    factor(mC_2_tpi_ALL$y, levels = c(0,1)),
    positive = "1"
  ))
}

{ ## ROC-AUC ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_ALL)),                    # just one model
    labels   = list(as.integer(mC_2_tpi_ALL$y)),
    modnames = "TPI Model",                                    # length-1 character
    dsids    = 1                                               # single ID
  ) %>%
    { ggplot2::autoplot(., "ROC") +                            # no facet needed
        geom_text(
          data = auc(.) %>%                                   # compute AUC table
            as.data.frame() %>%
            filter(curvetypes == "ROC") %>%
            mutate(
              label = sprintf("AUC = %.3f", aucs),            # format AUC
              x     = 0.45,                                   # tweak coords
              y     = 0.15
            ),
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0, size = 4
        ) +
        theme_minimal() +
        labs(
          x       = "False Positive Rate",
          y       = "True Positive Rate",
          colour  = "Model",
          title   = "ROC Curve for TPI Model"
        ) +
        plt_theme_2
    }
}

{ ## Precision-Recall ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_ALL)),
    labels   = list(as.integer(mC_2_tpi_ALL$y)),
    modnames = "TPI Model",
    dsids    = 1
  ) %>%
    { autoplot(., "PRC") +
        annotate(
          "text",
          x     = 0.6,
          y     = 0.15,
          hjust = 0,
          size  = 2.5,
          label = with(
            auc(.),
            sprintf("AUPRC = %.3f", aucs[curvetypes == "PRC"])
          )
        )
    } +
    theme_minimal() +
    labs(
      x      = "Recall = TP / (TP + FN)",
      y      = "Precision = TP / (TP + FP)",
      colour = "Model",
      title  = "Precision–Recall Curve for TPI Model"
    ) +
    plt_theme_2
}

## Marginal effects ####
{ ### T_p ~ dens_cp ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ## PCI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        pci10    = quantile(pci_p_lag, percentile_low, na.rm = TRUE),
        pci90    = quantile(pci_p_lag, percentile_high, na.rm = TRUE),
        spi_med  = median(spi_p_lag, na.rm = TRUE),
        epi_med  = median(epi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci10,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci90,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]~"(relatedness density)"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## SPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        spi10    = quantile(spi_p_lag, percentile_low, na.rm = TRUE),
        spi90    = quantile(spi_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        epi_med  = median(epi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi10,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi90,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]~"(relatedness density)"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## EPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        epi10    = quantile(epi_p_lag, percentile_low, na.rm = TRUE),
        epi90    = quantile(epi_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        spi_med  = median(spi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi10,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi90,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]~"(relatedness density)"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

{ ### dens_cp ~ T_p ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ## PCI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        pci_min = min(pci_p_lag,      na.rm = TRUE),
        pci_max = max(pci_p_lag,      na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        spi_med = median(spi_p_lag, na.rm = TRUE),
        epi_med = median(epi_p_lag, na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          spi_p_lag = .$spi_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          spi_p_lag = .$spi_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(pci_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PCI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PSPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        spi_min = min(spi_p_lag, na.rm = TRUE),
        spi_max = max(spi_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        epi_med = median(epi_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          spi_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          spi_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(spi_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PSPI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PEPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years) %>%
      summarise(
        epi_min = min(epi_p_lag, na.rm = TRUE),
        epi_max = max(epi_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        spi_med = median(spi_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          epi_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          spi_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct",
                                   percentile_low * 100)
        ),
        data.frame(
          epi_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          spi_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_ALL, newdata = ., type = "response")) %>%
      ggplot(aes(epi_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        # x = latex2exp::TeX("$PEPI_{pt-1}$"),
        x = latex2exp::TeX("$PEPI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

{ # ⚠️ mC_2_tpi_HUM -----------------------------------------------
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag * dens_cp_lag",
    "spi_p_lag * dens_cp_lag",
    "epi_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_p_lag * dens_cp_lag"
    # "TPI_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      income_lvl %in% c("H", "UM") &
      # income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_2_tpi_HUM <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_2_tpi_HUM:")
  print(summary(mC_2_tpi_HUM))
}

{ ## Multicollinearity ####
  formula_template_glm <- paste0("new_M_cp ~ ", predictors)
  formula_tpi_glm <- as.formula(gsub("TPI", "tpi", formula_template_glm))
  
  year_to_plot <- 2016
  
  mC_2_tpi_glm <- glm(
    # formula = formula_tpi_glm,
    formula = new_M_cp ~ 
      dens_cp_lag + 
      log_RCA_cp_lag + 
      pci_p_lag + 
      spi_p_lag + 
      epi_p_lag + 
      pci_p_lag * dens_cp_lag +
      spi_p_lag * dens_cp_lag +
      epi_p_lag * dens_cp_lag,
    
    family = binomial(link = "logit"), 
    data = cp_df %>% filter(year == year_to_plot)
  )
  
  message("mC_2_tpi_glm:")
  print(vif(mC_2_tpi_glm))
  # multicollin for PCI and tpi
  
}

{ ## Confusion matrix ####
  classification_threshold <- 0.50 # probability used for classifying pred
  # classification_threshold <- 0.20
  
  message("ℹ️ TPI Model: ")
  print(caret::confusionMatrix(
    factor(ifelse(fitted(mC_2_tpi_HUM) > classification_threshold, 
                  1, 0), levels = c(0,1)),
    factor(mC_2_tpi_HUM$y, levels = c(0,1)),
    positive = "1"
  ))
}

{ ## ROC-AUC ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_HUM)),                    # just one model
    labels   = list(as.integer(mC_2_tpi_HUM$y)),
    modnames = "TPI Model",                                    # length-1 character
    dsids    = 1                                               # single ID
  ) %>%
    { ggplot2::autoplot(., "ROC") +                            # no facet needed
        geom_text(
          data = auc(.) %>%                                   # compute AUC table
            as.data.frame() %>%
            filter(curvetypes == "ROC") %>%
            mutate(
              label = sprintf("AUC = %.3f", aucs),            # format AUC
              x     = 0.45,                                   # tweak coords
              y     = 0.15
            ),
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0, size = 4
        ) +
        theme_minimal() +
        labs(
          x       = "False Positive Rate",
          y       = "True Positive Rate",
          colour  = "Model",
          title   = "ROC Curve for TPI Model"
        ) +
        plt_theme_2
    }
}

{ ## Precision-Recall ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_HUM)),
    labels   = list(as.integer(mC_2_tpi_HUM$y)),
    modnames = "TPI Model",
    dsids    = 1
  ) %>%
    { autoplot(., "PRC") +
        annotate(
          "text",
          x     = 0.6,
          y     = 0.15,
          hjust = 0,
          size  = 2.5,
          label = with(
            auc(.),
            sprintf("AUPRC = %.3f", aucs[curvetypes == "PRC"])
          )
        )
    } +
    theme_minimal() +
    labs(
      x      = "Recall = TP / (TP + FN)",
      y      = "Precision = TP / (TP + FP)",
      colour = "Model",
      title  = "Precision–Recall Curve for TPI Model"
    ) +
    plt_theme_2
}

{ # ⚠️ mC_2_tpi_LML -----------------------------------------------
  # This model adds both SPI and EPI in one model
  predictors <- paste(c(
    "dens_cp_lag", 
    # "log_dens_cp_lag", 
    # "RCA_cp_lag",
    "log_RCA_cp_lag",
    
    # "diversity_c", 
    # "TPI_c", 
    # "eci_c", # neg and insig
    # "log(gdp_percap_ppp_2021intdollars)",
    # "log(pop_total)", # insig
    
    # "prev_M_cp_zero", 
    
    "pci_p_lag * dens_cp_lag",
    "spi_p_lag * dens_cp_lag",
    "epi_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_p_lag * dens_cp_lag"
    # "TPI_prody_p_lag * dens_cp_lag"
    # "TPI_p_lag * dens_cp_lag"
    
    # "prody_p_lag * dens_cp_lag",
    # "TPI_prody_p_lag * dens_cp_lag"
  ), collapse = " + ")
  
  # Use common or individual FE (set use_common_FE to TRUE if common FE should
  # be used; see common model params)
  fixed_effects <- if (isTRUE(use_common_FE)) {
    common_fixed_effects
  } else {
    paste(c(
      # "year",
      # "country_iso3",
      # "prod_code",
      # "prod_code^year",
      # "country_iso3^prod_code",
      "country_iso3^year",
      0
    ), collapse = " + ")
  }
  
  # Model family
  glm_family <- (
    binomial(link = "logit")                # Logit
    # family = binomial(link = "probit")      # Probit
    # family = binomial(link = "cloglog")     # Complementary log-log
    # family = quasibinomial(link = "logit")    # Quasibinomial
    # family   = gaussian(link = "identity")  # Linear probability
  )
  
  # Data filters
  cp_df_filter_conditions <- quote(
    prev_M_cp_zero == 1 &
      # income_lvl %in% c("H", "UM") &
      income_lvl %in% c("LM", "L") &
      # income_lvl == "H" &
      # income_lvl != "H" &
      # income_lvl == "UM" &
      # income_lvl == "L" &
      year %in% logistic_years
  )
  
  ## Est ####
  # Define formulae
  formula_template <- paste0("new_M_cp ~ ", predictors, " | ", fixed_effects)
  formula_tpi <- as.formula(gsub("TPI", "spi", formula_template))
  
  # Estimate
  (est_start <- Sys.time())
  
  mC_2_tpi_LML <- feglm(
    formula_tpi,
    panel.id = ~ cp_id + year,
    # cluster = ~ country_iso3,
    cluster = ~ country_iso3 + prod_code,
    family = glm_family, 
    combine.quick = FALSE, 
    data = cp_df[eval(cp_df_filter_conditions)]
    # data = cp_df_oversampled[eval(cp_df_filter_conditions)]
  )
  
  # Estimation duration
  (est_stop <- Sys.time())
  cat("\n")
  message(paste0(
    "Model estimation duration: ", 
    round(as.numeric(difftime(est_stop, est_start, units = "sec")), 4), 
    " seconds (", format(Sys.Date(), "%d-%m-%Y"), ", ", 
    format(Sys.time(), "%H:%M"), ")", 
    sprintf("\nModel family: %s (link = %s)",
            glm_family$family,
            glm_family$link), 
    "\n", length(logistic_years), " Years: ", min(logistic_years), "-", 
    max(logistic_years), 
    "\nnew_M_cp year window = ", new_M_cp_year_window, " years"
  ))
  
  # Output 
  cat("\n")
  message("ℹ️ Model: mC_2_tpi_LML:")
  print(summary(mC_2_tpi_LML))
}

{ ## Multicollinearity ####
  formula_template_glm <- paste0("new_M_cp ~ ", predictors)
  formula_tpi_glm <- as.formula(gsub("TPI", "tpi", formula_template_glm))
  
  year_to_plot <- 2016
  
  mC_2_tpi_glm <- glm(
    # formula = formula_tpi_glm,
    formula = new_M_cp ~ 
      dens_cp_lag + 
      log_RCA_cp_lag + 
      pci_p_lag + 
      spi_p_lag + 
      epi_p_lag + 
      pci_p_lag * dens_cp_lag +
      spi_p_lag * dens_cp_lag +
      epi_p_lag * dens_cp_lag,
    
    family = binomial(link = "logit"), 
    data = cp_df %>% filter(year == year_to_plot)
  )
  
  message("mC_2_tpi_glm:")
  print(vif(mC_2_tpi_glm))
  # multicollin for PCI and tpi
  
}

{ ## Confusion matrix ####
  classification_threshold <- 0.50 # probability used for classifying pred
  # classification_threshold <- 0.20
  
  message("ℹ️ TPI Model: ")
  print(caret::confusionMatrix(
    factor(ifelse(fitted(mC_2_tpi_LML) > classification_threshold, 
                  1, 0), levels = c(0,1)),
    factor(mC_2_tpi_LML$y, levels = c(0,1)),
    positive = "1"
  ))
}

{ ## ROC-AUC ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_LML)),                    # just one model
    labels   = list(as.integer(mC_2_tpi_LML$y)),
    modnames = "TPI Model",                                    # length-1 character
    dsids    = 1                                               # single ID
  ) %>%
    { ggplot2::autoplot(., "ROC") +                            # no facet needed
        geom_text(
          data = auc(.) %>%                                   # compute AUC table
            as.data.frame() %>%
            filter(curvetypes == "ROC") %>%
            mutate(
              label = sprintf("AUC = %.3f", aucs),            # format AUC
              x     = 0.45,                                   # tweak coords
              y     = 0.15
            ),
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0, size = 4
        ) +
        theme_minimal() +
        labs(
          x       = "False Positive Rate",
          y       = "True Positive Rate",
          colour  = "Model",
          title   = "ROC Curve for TPI Model"
        ) +
        plt_theme_2
    }
}

{ ## Precision-Recall ####
  evalmod(
    scores   = list(fitted(mC_2_tpi_LML)),
    labels   = list(as.integer(mC_2_tpi_LML$y)),
    modnames = "TPI Model",
    dsids    = 1
  ) %>%
    { autoplot(., "PRC") +
        annotate(
          "text",
          x     = 0.6,
          y     = 0.15,
          hjust = 0,
          size  = 2.5,
          label = with(
            auc(.),
            sprintf("AUPRC = %.3f", aucs[curvetypes == "PRC"])
          )
        )
    } +
    theme_minimal() +
    labs(
      x      = "Recall = TP / (TP + FN)",
      y      = "Precision = TP / (TP + FP)",
      colour = "Model",
      title  = "Precision–Recall Curve for TPI Model"
    ) +
    plt_theme_2
}

## Marginal effects HUM vs LML ####
{ ### T_p ~ dens_cp ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ### HUM ####
    ## PCI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        pci10    = quantile(pci_p_lag, percentile_low, na.rm = TRUE),
        pci90    = quantile(pci_p_lag, percentile_high, na.rm = TRUE),
        spi_med  = median(spi_p_lag, na.rm = TRUE),
        epi_med  = median(epi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci10,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci90,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## SPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        spi10    = quantile(spi_p_lag, percentile_low, na.rm = TRUE),
        spi90    = quantile(spi_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        epi_med  = median(epi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi10,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi90,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## EPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        epi10    = quantile(epi_p_lag, percentile_low, na.rm = TRUE),
        epi90    = quantile(epi_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        spi_med  = median(spi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi10,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi90,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ### LML ####
    ## PCI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        pci10    = quantile(pci_p_lag, percentile_low, na.rm = TRUE),
        pci90    = quantile(pci_p_lag, percentile_high, na.rm = TRUE),
        spi_med  = median(spi_p_lag, na.rm = TRUE),
        epi_med  = median(epi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci10,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci90,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PCI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## SPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        spi10    = quantile(spi_p_lag, percentile_low, na.rm = TRUE),
        spi90    = quantile(spi_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        epi_med  = median(epi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi10,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi90,
            epi_p_lag = .$epi_med,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PSPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## EPI marginal‐effects
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        dens_min = min(dens_cp_lag, na.rm = TRUE),
        dens_max = max(dens_cp_lag, na.rm = TRUE),
        epi10    = quantile(epi_p_lag, percentile_low, na.rm = TRUE),
        epi90    = quantile(epi_p_lag, percentile_high, na.rm = TRUE),
        pci_med  = median(pci_p_lag, na.rm = TRUE),
        spi_med  = median(spi_p_lag, na.rm = TRUE),
        rca_med  = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { 
        bind_rows(
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi10,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code,  
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_low * 100)
          ),
          data.frame(
            dens_cp_lag    = seq(.$dens_min, .$dens_max, length.out = 100),
            pci_p_lag      = .$pci_med,
            spi_p_lag = .$spi_med,
            epi_p_lag = .$epi90,
            log_RCA_cp_lag = .$rca_med,
            country_iso3   = .$country_iso3,
            prod_code      = .$prod_code, 
            year           = .$year,
            scenario       = sprintf("PEPI = %.0fth pct", 
                                     percentile_high * 100)
          )
        )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(dens_cp_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x     = expression(omega[cp]),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

{ ### dens_cp ~ T_p ####
  percentile_low <- 0.10
  percentile_high <- 1 - percentile_low
  
  grid.arrange(
    ### HUM ####
    ## PCI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        pci_min = min(pci_p_lag,      na.rm = TRUE),
        pci_max = max(pci_p_lag,      na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        spi_med = median(spi_p_lag, na.rm = TRUE),
        epi_med = median(epi_p_lag, na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          spi_p_lag = .$spi_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          spi_p_lag = .$spi_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(pci_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PCI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PSPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        spi_min = min(spi_p_lag, na.rm = TRUE),
        spi_max = max(spi_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        epi_med = median(epi_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          spi_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          spi_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(spi_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PSPI_{pt-1}$"), 
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PEPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("H", "UM")) %>%
      summarise(
        epi_min = min(epi_p_lag, na.rm = TRUE),
        epi_max = max(epi_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        spi_med = median(spi_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          epi_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          spi_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct",
                                   percentile_low * 100)
        ),
        data.frame(
          epi_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          spi_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_HUM, newdata = ., type = "response")) %>%
      ggplot(aes(epi_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PEPI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ### LML ####
    ## PCI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        pci_min = min(pci_p_lag,      na.rm = TRUE),
        pci_max = max(pci_p_lag,      na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        spi_med = median(spi_p_lag, na.rm = TRUE),
        epi_med = median(epi_p_lag, na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag, na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          spi_p_lag = .$spi_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          pci_p_lag      = seq(.$pci_min, .$pci_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          spi_p_lag = .$spi_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(pci_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PCI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PCI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PSPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        spi_min = min(spi_p_lag, na.rm = TRUE),
        spi_max = max(spi_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        epi_med = median(epi_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          spi_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_low * 100)
        ),
        data.frame(
          spi_p_lag = seq(.$spi_min, .$spi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          epi_p_lag = .$epi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code, 
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(spi_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PSPI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PSPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ## PEPI ↦ scenarios of density
    cp_df %>%
      filter(prev_M_cp_zero == 1, year %in% logistic_years, 
             income_lvl %in% c("LM", "L")) %>%
      summarise(
        epi_min = min(epi_p_lag, na.rm = TRUE),
        epi_max = max(epi_p_lag, na.rm = TRUE),
        dens10  = quantile(dens_cp_lag, percentile_low, na.rm = TRUE),
        dens90  = quantile(dens_cp_lag, percentile_high, na.rm = TRUE),
        pci_med = median(pci_p_lag,        na.rm = TRUE),
        spi_med = median(spi_p_lag,   na.rm = TRUE),
        rca_med = median(log_RCA_cp_lag,   na.rm = TRUE),
        country_iso3 = first(country_iso3),
        prod_code    = first(prod_code),  
        year         = first(year)
      ) %>%
      { bind_rows(
        data.frame(
          epi_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens10,
          pci_p_lag      = .$pci_med,
          spi_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct",
                                   percentile_low * 100)
        ),
        data.frame(
          epi_p_lag = seq(.$epi_min, .$epi_max, length.out = 100),
          dens_cp_lag    = .$dens90,
          pci_p_lag      = .$pci_med,
          spi_p_lag = .$spi_med,
          log_RCA_cp_lag = .$rca_med,
          country_iso3   = .$country_iso3,
          prod_code      = .$prod_code,  
          year           = .$year,
          scenario       = sprintf("Density = %.0fth pct", 
                                   percentile_high * 100)
        )
      )
      } %>%
      mutate(Pr = predict(mC_2_tpi_LML, newdata = ., type = "response")) %>%
      ggplot(aes(epi_p_lag, Pr, colour = scenario)) +
      geom_line(size = 1) +
      labs(
        x = latex2exp::TeX("$PEPI_{pt-1}$"),
        y = latex2exp::TeX("$\\hat{P}(\\Delta M_{cpt} = 1)$"), 
        title = "Marginal Effect of PEPI", 
        colour = "Scenario"
      ) +
      theme_bw() + plt_theme_2 +
      theme(legend.position = "top", legend.direction = "horizontal"),
    
    ncol = 3
  )
}

{ # Verify nr observations --------------------------------------------------
  # Manually check the number of countries, products and years
  verify_nr_obs <- FALSE
  
  if (verify_nr_obs == TRUE) {
    # Variables 
    logistic_vars <- unique(c(
      all.vars(as.formula(paste0("~", predictors))),
      paste0(c("new_M_cp"))
    ))
    
    cp_df %>%
      filter(!is.na(new_M_cp)) %>%
      select(year, country_iso3, prod_code, 
             M_cp, new_M_cp) %>% 
      arrange(-year) %>% 
      head(n = 20)
    
    bind_rows(
      # row for “Without controls”
      cp_df %>%
        filter(
          year %in% logistic_years,
          if_all(all_of(logistic_vars), ~ !is.na(.))
        ) %>%
        group_by(year) %>% 
        # keep only those years where at least one new_M_cp != 0
        filter(any(new_M_cp != 0)) %>%  
        ungroup() %>%
        summarise(
          n_countries = n_distinct(country_iso3),
          n_products  = n_distinct(prod_code),
          n_years     = n_distinct(year)
        ) %>%
        mutate(set = "All"),
      
      cp_df %>%
        filter(
          year %in% logistic_years,
          income_lvl %in% c("H", "UM"), 
          if_all(all_of(logistic_vars), ~ !is.na(.))
        ) %>%
        group_by(year) %>% 
        # keep only those years where at least one new_M_cp != 0
        filter(any(new_M_cp != 0)) %>%  
        ungroup() %>%
        summarise(
          n_countries = n_distinct(country_iso3),
          n_products  = n_distinct(prod_code),
          n_years     = n_distinct(year)
        ) %>%
        mutate(set = "H & UM"),
      
      cp_df %>%
        filter(
          year %in% logistic_years,
          income_lvl %in% c("LM", "L"), 
          if_all(all_of(logistic_vars), ~ !is.na(.))
        ) %>%
        group_by(year) %>% 
        # keep only those years where at least one new_M_cp != 0
        filter(any(new_M_cp != 0)) %>%  
        ungroup() %>%
        summarise(
          n_countries = n_distinct(country_iso3),
          n_products  = n_distinct(prod_code),
          n_years     = n_distinct(year)
        ) %>%
        mutate(set = "LM & L"),
      
    ) %>% select(set, n_countries, n_years)
    # corresponds to infra, except for one country for H & UM
    # perhaps this country does not have new_M_cp = 1 for any rows and is 
    # therefore removed? 
    
    cp_df %>%
      group_by(country_iso3, year) %>%
      filter(year %in% logistic_years,
             all(new_M_cp == 0)) %>%
      ungroup() %>%
      distinct(country_iso3, year)
    # this is correct, but I don't see a country that never has 
    
    # Country for which new_M_cp is always 0
    cp_df %>%
      group_by(country_iso3) %>%
      filter(all(new_M_cp == 0, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct(country_iso3)
    # GNQ --> this country is probably removed
  }
}

{ # ➡️ Export results ----------------------------------------------------------
  coef_digits # set in logistic_persistent.R
  
  { ## Additional fit stats ####
    # Nr countries
    fitstat_register(
      type  = "ncountries",
      alias = "Countries",
      fun   = function(x){
        fe_list <- fixef(x, sorted = FALSE)
        # 1a. exact country_FE if present
        if ("country_iso3" %in% names(fe_list)) {
          return( length(fe_list[["country_iso3"]]) )
        }
        # 1b. else, look for the country*year interaction
        int_nm <- grep("^country_iso3\\^", names(fe_list), value = TRUE)
        if (length(int_nm)) {
          lvl <- names(fe_list[[int_nm[1]]])           # e.g. "BEL_2016"
          return( length(unique(sub("_.*$", "", lvl))) )
        }
        # 1c. fallback: pick the first FE and parse
        lvl <- names(fe_list[[1L]])
        length(unique(sub("_.*$", "", lvl)))
      }
    )
    
    # Nr years
    fitstat_register(
      type  = "nyears",
      alias = "Years",
      fun   = function(x){
        fe_list <- fixef(x, sorted = FALSE)
        # 2a. exact year_FE if present
        if ("year" %in% names(fe_list)) {
          return( length(fe_list[["year"]]) )
        }
        # 2b. else, look for the country*year interaction
        int_nm <- grep("\\^year$", names(fe_list), value = TRUE)
        if (length(int_nm)) {
          lvl <- names(fe_list[[int_nm[1]]])           # e.g. "BEL_2016"
          return( length(unique(sub(".*_", "", lvl))) )
        }
        # 2c. fallback: pick the first FE and parse
        lvl <- names(fe_list[[1L]])
        length(unique(sub(".*_", "", lvl)))
      }
    )
    
    # TODO: nyears and ncountries not correctly extracted if country*year FE 
    # are not used... 
    
    # ROC-AUC
    fitstat_register(
      type  = "auc",
      alias = "ROC-AUC",
      fun   = function(x){
        resp  <- x$y                # the observed 0/1 outcome
        preds <- fitted(x)          # the model’s fitted probabilities
        as.numeric( pROC::auc(resp, preds) )
      }
    )
    
    # F1
    fitstat_register(
      type  = "f1",
      alias = "F$_1$",
      fun   = function(x) {
        
        resp  <- x$y         # observed 0/1 outcome
        probs <- fitted(x)   # predicted probabilities
        
        # ── 1. choose a threshold ────────────────────────────────────────────────
        thr   <- 0.1 # fixed cut-off
        # 0.1275 for 0.03; 0.15 for 0.04; 0.1713 for 0.08
        # thr <- pROC::coords(pROC::roc(resp, probs), "best",
        #                     ret = "threshold", best.method = "youden")
        # ─────────────────────────────────────────────────────────────────────────
        
        pred  <- as.integer(probs >= thr)
        
        TP <- sum(pred == 1 & resp == 1)
        FP <- sum(pred == 1 & resp == 0)
        FN <- sum(pred == 0 & resp == 1)
        
        if (TP + FP + FN == 0) return(NaN)          # edge case
        return( 2 * TP / (2 * TP + FP + FN) )       # F1 formula
      }
    )
  }
  
  ## Model table ####
  modB_notorth <- etable(
    # Models
    mC_1_tpi_ALL, mC_2_tpi_ALL, mC_2_tpi_HUM, mC_2_tpi_LML, 
    headers = list("Full Sample" = 2, "H & UM" = 1, "LM & L" = 1), 
    dict = c(
      setNames("$\\mathbb{{P}}(\\Delta M_{cpt} = 1)$",
               as.character(formula(mC_2_tpi_ALL))[2]),
      country_iso3     = "$c$",
      prod_code        = "$p$",
      year             = "$t$",
      dens_cp_lag      = "$\\omega_{cpt-1}$",
      log_RCA_cp_lag   = "$\\log(\\mathit{RCA}_{cpt-1})$",
      pci_p_lag        = "$\\mathit{PCI}_{pt-1}$",
      spi_p_lag   = "$\\mathit{PSPI}_{pt-1}$",
      epi_p_lag   = "$\\mathit{PEPI}_{pt-1}$"
    ), 
    tex = TRUE, 
    style.tex = style.tex(
      line.top    = "\\toprule",    
      line.bottom = "\\bottomrule", 
      caption.after = "\\vspace{6pt}\n\\footnotesize"
      # tablefoot   = FALSE # drop the extra mid-rules at the foot
    ), 
    # fitstat = c("n", "pr2", "bic"),
    fitstat = c(
      "n", "ncountries", "nyears", 
      # "nproducts", # does not work
      "pr2", 
      # "pr2_p", 
      # "aic", "bic", 
      "auc", 
      "f1"
    ),
    digits = paste0("r", coef_digits), digits.stats = paste0("r", coef_digits),
    label = "modB_notorth", 
    title = paste0(
      "Product Entry Model Estimates without Orthogonalisation")
  )
  
  # Replace [htbp] with [H]
  modB_notorth <- gsub(
    x = modB_notorth,
    pattern = "\\\\begin\\{table\\}\\[.*?\\]",
    replacement = "\\\\begin{table}[H]"
  )
  
  # Remove the centering
  modB_notorth <- sub("\\\\centering\\s*", "", modB_notorth, perl = TRUE)
  
  # Add resizebox around the tabular
  # modB_notorth <- gsub(
  #   pattern = "(\\\\begin\\{tabular\\}\\{lcccc\\})",
  #   replacement = "\\\\resizebox{\\\\columnwidth}{!}{%\n\\1",
  #   x = modB_notorth
  # )
  # 
  # modB_notorth <- gsub(
  #   pattern = "(\\\\end\\{tabular\\})",
  #   replacement = "\\1\n}",
  #   x = modB_notorth
  # )
  
  writeLines(
    text = as.character(modB_notorth), 
    "data/data_processed/LaTeX data values/modB_notorth.txt"
  )
  # TODO: add one model without interactions (only direct effects)
  
  ## Export coefficients ####
  ### VIFs ####
  cat(sub(
    # Add fontsize to table
    "(\\\\caption\\{[^}]*\\})", 
    paste0("\\1\n\\\\", "small"),
    kable(
      tibble(
        Variable = names(vif(mC_2_tpi_glm, type = "terms")),
        VIF      = as.numeric(vif(mC_2_tpi_glm, type = "terms"))
      ) |>
        arrange(match(
          Variable,
          c(
            "dens_cp_lag",
            "log_RCA_cp_lag",
            "pci_p_lag",
            "spi_p_lag",
            "epi_p_lag",
            "dens_cp_lag:pci_p_lag",
            "dens_cp_lag:spi_p_lag",
            "dens_cp_lag:epi_p_lag"
          ))) |>
        mutate(
          Variable = dplyr::recode(Variable, !!!c(
            "dens_cp_lag"             = "$\\omega_{cpt-1}$",
            "log_RCA_cp_lag"          = "$\\log(\\mathit{RCA}_{cpt-1})$",
            "pci_p_lag"               = "$\\mathit{PCI}_{pt-1}$",
            "spi_p_lag"               = "$\\mathit{PSPI}_{pt-1}$",
            "epi_p_lag"               = "$\\mathit{PEPI}_{pt-1}$",
            "dens_cp_lag:pci_p_lag"   = "$\\omega_{cpt-1} \\times \\mathit{PCI}_{pt-1}$",
            "dens_cp_lag:spi_p_lag"   = "$\\omega_{cpt-1} \\times \\mathit{PSPI}_{pt-1}$",
            "dens_cp_lag:epi_p_lag"   = "$\\omega_{cpt-1} \\times \\mathit{PEPI}_{pt-1}$"
          ))
        ),
      format   = "latex",
      caption  = "VIFs for Product Entry Model (without orthogonalisation)",
      booktabs = TRUE,
      digits   = 2,
      escape   = FALSE,
      col.names = c("Variable", "VIF")
    ) |>
      kable_styling(
        latex_options = "HOLD_position", # ← produces [H]
        full_width    = FALSE,
        position      = "left"
      )
  ),
  file = "data/data_processed/LaTeX data values/mC_2_tpi_glm_notorth_vifs.txt"
  )
  
  modB_notorth
  
  beep(10)
}

