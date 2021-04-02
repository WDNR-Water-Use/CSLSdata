#' Import Model Results
#'
#' This function loads model outputs from csv files and saves as combined data
#' frame. Important assumptions:
#'
#'  * Start date is "01-01-1981" for long runs
#'  * Start date is "01-01-2012" for calibration runs
#'  * LAK package information is read out on the first of the next month (e.g.,
#'    January rates are read out on 2/1)
#'  * Signs are such that 0 = P + E + GWin + GWout + dV
#'
#' @return df, a data frame with the following columns:
#'  \item{sim}{type of model simulation, e.g. "irr", or "no_irr"}
#'  \item{lake}{name of lake, i.e. "Pleasant", "Long", "Plainfield"}
#'  \item{date}{date of output, POSIXct}
#'  \item{level_m}{mean lake stage for the month, mamsl}
#'  \item{P_m3}{precipitation volume this month (m^3)}
#'  \item{E_m3}{evaporation volume this month (m^3)}
#'  \item{GWin_m3}{groundwater inflow volume this month (m^3)}
#'  \item{GWout_m3}{groundwater outflow volume this month (m^3)}
#'  \item{dV_m3}{change in lake volume this month (m^3)}
#'  \item{P_m3_d}{precipitation flow rate (m^3/d)}
#'  \item{E_m3_d}{evaporation flow rate (m^3/d)}
#'  \item{GWin_m3_d}{groundwater inflow rate into lake (m^3/d)}
#'  \item{GWout_m3_d}{groundwater outflow rate into lake (m^3/d)}
#'  \item{dV_m3_d}{change in lake volume as a flow rate (m^3/d)}

import_model_results <- function(scenarios = c("cal", "cur_irr", "no_irr", "wells_off"),
                                 lakes = c("Pleasant", "Long", "Plainfield")) {

  library(dplyr)
  library(stringr)
  library(lubridate)
  library(zoo)
  library(reshape2)

  df <- NULL
  for (scenario in scenarios) {
    # Set appropriate start_date for the scenario
    if (scenario == "cal") {
      start_date <- as_datetime(mdy("01-01-2012"))
    } else {
      start_date <- as_datetime(mdy("01-01-1981"))
    }

    # 1. Fluxes ----------------------------------------------------------------
    # Read in csv for each lake
    ls <- list()
    for (lake in lakes) {
      ls[[lake]] <- read.csv(sprintf("data-raw/MODFLOW/MODFLOW_%s_fluxes_%s.csv",
                                     lake, scenario))
    }

    # Combine all lakes into one data frame
    lk_fluxes <- NULL
    for (lake in names(ls)) {
      this_df      <- ls[[lake]]
      this_df$lake <- lake
      lk_fluxes    <- bind_rows(lk_fluxes, this_df)
    }

    # Set date to the start_datetime
    lk_fluxes <- lk_fluxes %>%
                 mutate(date = start_date + days(.data$mf_time) -
                          months(1) - days(1)) %>%
                 filter(.data$date >= start_date)
    lk_fluxes$mf_time <- NULL

    # Read in monte carlo simulations
    if (!"gw_in" %in% colnames(lk_fluxes)) {
      lk_fluxes <- lk_fluxes %>%
                   melt(id.vars = c("lake", "date")) %>%
                   mutate(flux = str_replace(.data$variable, "_real.+", ""),
                          scenario = scenario,
                          sim = str_replace(.data$variable, ".+_real", ""),
                          sim = as.numeric(.data$sim) + 1) %>%
                   select(.data$lake, .data$date, .data$scenario, .data$sim,
                          .data$flux, .data$value) %>%
                   dcast(lake+date+scenario+sim~flux, value.var = "value")
    } else if (scenario %in% c("irr_baseline", "no_irr_baseline")) {
      lk_fluxes <- lk_fluxes %>%
                   mutate(scenario = str_replace(scenario, "_baseline", ""),
                          sim = 0) %>%
                   select(.data$lake, .data$date, .data$scenario, .data$sim,
                          .data$gw_in, .data$gw_out, .data$gw_net)
    } else {
      lk_fluxes <- lk_fluxes %>%
                   mutate(scenario = scenario,
                          sim = 0) %>%
                   select(.data$lake, .data$date, .data$scenario, .data$sim,
                          .data$gw_in, .data$gw_out, .data$gw_net)
    }

    # 2. Stages ----------------------------------------------------------------
    # Import CSVs to a list
    ls <- list()
    for (lake in lakes) {
      ls[[lake]] <- read.csv(sprintf("data-raw/MODFLOW/MODFLOW_%s_stages_%s.csv",
                                     lake, scenario))
    }

    # Combine in one data frame, rename funny column names
    lk_pkg <- NULL
    for (lake in names(ls)) {
      this_df      <- ls[[lake]]
      this_df$lake <- lake
      lk_pkg       <- bind_rows(lk_pkg, this_df)
    }
    colnames(lk_pkg) <- str_to_lower(colnames(lk_pkg))
    colnames(lk_pkg) <- str_replace(colnames(lk_pkg), "\\.", "_")

    # Convert time to dates, keep only the 1st of the month
    lk_pkg$date <- start_date + ddays(lk_pkg$time) - days(1)
    lk_pkg      <- lk_pkg %>% filter(day(.data$date) == 1)

    # Retain only useful columns
    col_matches <- "lake|stage|rainfall|lak|storage|evaporation|date|volume"
    lk_pkg      <- lk_pkg[,str_detect(colnames(lk_pkg), col_matches)]

    # Read in monte carlo simulations
    if (!"stage" %in% colnames(lk_pkg)) {
      lk_pkg <- lk_pkg %>%
                melt(id.vars = c("lake", "date")) %>%
                mutate(flux = str_replace(.data$variable, "_real.+", ""),
                       scenario = scenario,
                       sim = str_replace(.data$variable, ".+_real", ""),
                       sim = as.numeric(.data$sim) + 1) %>%
                select(.data$lake, .data$date, .data$scenario, .data$sim,
                       .data$flux, .data$value) %>%
                dcast(lake+date+scenario+sim~flux, value.var = "value")
    } else if (scenario %in% c("irr_baseline", "no_irr_baseline")) {
      lk_pkg <- lk_pkg %>%
                mutate(scenario = str_replace(scenario, "_baseline", ""),
                       sim = 0)
    } else {
      lk_pkg <- lk_pkg %>%
                mutate(scenario = scenario,
                       sim = 0)

    }

    # Calculate lake stage as average between 1st of month & 1st of next month
    lk_stages <- NULL
    for (lake in lakes) {
      for (sim in unique(lk_pkg$sim)) {
        this_stage <- lk_pkg %>%
                      filter(.data$lake == !!lake,
                             .data$sim == !!sim) %>%
                      arrange(.data$date) %>%
                      mutate(stage_next = lead(.data$stage, 1, order_by = .data$date),
                             stage = 0.5*(.data$stage + .data$stage_next)) %>%
                      select(.data$lake, .data$date, .data$scenario,
                             .data$sim, .data$stage)
        lk_stages <- bind_rows(lk_stages, this_stage)
      }
    }

    lk_vols   <- lk_pkg %>%
                 select(.data$lake, .data$date, .data$scenario, .data$sim,
                        .data$volume)
    lk_stages <- left_join(lk_stages, lk_vols,
                           by = c("lake", "date", "scenario", "sim"))

    # lk_stages <- lk_pkg %>%
    #              select(.data$lake, .data$date, .data$scenario, .data$sim,
    #                     .data$stage, .data$volume)

    # Roll back dates for lake fluxes to first of the previous month
    # For example, a LAK values on 2/1 represent the net groundwater flow from
    # 1/1 to 2/1, so assign it the date 1/1.
    lk_pkg$date <- lk_pkg$date - months(1)
    lk_pkg      <- lk_pkg %>%
                   filter(.data$date >= start_date) %>%
                   select(.data$lake, .data$date, .data$scenario, .data$sim,
                          .data$rainfall, .data$lak, .data$evaporation,
                          .data$storage)

    # Merge stages and fluxes back together
    lk_stages <- full_join(lk_stages, lk_pkg,
                           by = c("lake", "date", "scenario", "sim"))

    # 3. Combine to single data frame ------------------------------------------
    this_sim     <- full_join(lk_stages, lk_fluxes,
                              by = c("lake", "date", "scenario", "sim"))

    # Fill in missing gw values
    this_sim <- this_sim %>%
                mutate(gw_net = ifelse(is.na(.data$gw_net),
                                       .data$lak, .data$gw_net),
                gw_out = ifelse(is.na(.data$gw_out),
                                .data$gw_net - .data$gw_in,
                                .data$gw_out),
                gw_in = ifelse(is.na(.data$gw_in),
                               .data$gw_net - .data$gw_out,
                               .data$gw_in))

    # Include values as m3/month and m3/day
    this_sim <- this_sim %>%
                filter(!is.na(.data$stage)) %>%
                mutate(days = day(.data$date + months(1) - days(1)),
                       sim = ifelse(.data$sim == 10000,
                                    0, .data$sim),
                       P_m3 = .data$rainfall*.data$days,
                       E_m3 = .data$evaporation*.data$days,
                       GWin_m3 = .data$gw_in*.data$days,
                       GWout_m3 = .data$gw_out*.data$days,
                       dV_m3 = .data$storage*.data$days,
                       vol_m3 = .data$volume) %>%
                select(scenario = .data$scenario,
                       sim = .data$sim,
                       lake = .data$lake,
                       date = .data$date,
                       level_m = .data$stage,
                       vol_m3 = .data$vol_m3,
                       P_m3 = .data$P_m3,
                       E_m3 = .data$E_m3,
                       GWin_m3 = .data$GWin_m3,
                       GWout_m3 = .data$GWout_m3,
                       dV_m3 = .data$dV_m3,
                       P_m3_d = .data$rainfall,
                       E_m3_d = .data$evaporation,
                       GWin_m3_d = .data$gw_in,
                       GWout_m3_d = .data$gw_out,
                       dV_m3_d = .data$storage)
    this_sim$lake <- factor(this_sim$lake, levels = lakes)

    df <- rbind(df, this_sim)
  }

  return(df)

  # # Code used to explore LAK vs. NET_GW relationship
  # NA_values <- this_sim %>% filter(is.na(.data$gw_net))
  # ggplot(NA_values) +
  #   geom_point(aes(x = .data$date,
  #                 y = .data$lak,
  #                 color = "LAK",
  #                 shape = "LAK"),
  #              size = 3) +
  #   geom_point(aes(x = .data$date,
  #                 y = .data$gw_in,
  #                 color = "GW_IN",
  #                 shape = "GW_IN"),
  #              size = 2) +
  #   geom_point(aes(x = .data$date,
  #                 y = .data$gw_out,
  #                 color = "GW_OUT",
  #                 shape = "GW_OUT"),
  #              size = 2) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "", y = "Flux (m3/d)", color = "") +
  #   scale_color_manual(name = "",
  #                      breaks = c("LAK", "GW_IN", "GW_OUT"),
  #                      values = c("grey80", "blue", "darkred")) +
  #   scale_shape_manual(name = "",
  #                      breaks = c("LAK", "GW_IN", "GW_OUT"),
  #                      values = c(15, 16, 17))
  #
  # ggplot(filter(this_sim, .data$lake == "Pleasant")) +
  #   geom_line(aes(x = .data$date,
  #                 y = .data$lak,
  #                 color = "LAK")) +
  #   geom_line(aes(x = .data$date,
  #                 y = .data$gw_net,
  #                 color = "GW_NET")) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "", y = "Flux (m3/d)", color = "") +
  #   scale_color_manual(name = "",
  #                      breaks = c("LAK", "GW_NET"),
  #                      values = c("black", "blue"))
  #
  # ggplot(this_sim) +
  #   geom_line(aes(x = .data$date,
  #                 y = .data$lak_balance,
  #                 color = "LAK")) +
  #   geom_line(aes(x = .data$date,
  #                 y = .data$gw_balance,
  #                 color = "GW_NET")) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "", y = "Water Balance (m3/d)", color = "", title = "Water Balance ('irr' scenario) derived via 'LAK' and 'GW_NET'") +
  #   scale_color_manual(name = "",
  #                      breaks = c("LAK", "GW_NET"),
  #                      values = c("black", "blue"))
  #
  # ggplot(filter(this_sim, .data$lake == "Pleasant")) +
  #   geom_line(aes(x = .data$date,
  #                 y = abs(.data$diff))) +
  #   theme_bw() +
  #   scale_y_continuous(limits = c(0,200)) +
  #   labs(x = "", y = "LAK - GW_NET (%)")
  #
  # ggplot(this_sim) +
  #   geom_line(aes(x = .data$date,
  #                 y = .data$lak - .data$gw_net),
  #             color = "black") +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "", y = "LAK - GW_NET (m3/d)")
  #
  # ggplot(df) +
  #   geom_line(aes(x = .data$date,
  #                 y = .data$GWin_m3,
  #                 color = .data$sim)) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "", y = "GW_IN (m3/d)")


  # Code used to explore GW vs. stage relationships

  # plot_df <- df %>%
  #            group_by(.data$sim, .data$lake, year(.data$date)) %>%
  #            summarise(level_m = mean(level_m),
  #                      P_m3 = sum(P_m3),
  #                      E_m3 = sum(E_m3),
  #                      GWin_m3 = sum(GWin_m3),
  #                      GWout_m3 = sum(GWout_m3),
  #                      dV_m3 = sum(dV_m3))%>%
  #            ungroup()
  #
  # ggplot(plot_df) +
  #   geom_point(aes(x = NISTunits::NISTmeterTOft(level_m),
  #                  y = -100*GWin_m3/(-GWin_m3 + P_m3),
  #                  color = sim)) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "Mean Lake Elevation (ft)", y = "Groundwater Inflow (%)")
  #
  # ggplot(plot_df) +
  #   geom_point(aes(x = NISTunits::NISTmeterTOft(level_m),
  #                  y = NISTunits::NISTcubMeterTOacreFt(-GWin_m3),
  #                  color = sim)) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "Mean Lake Elevation (ft)", y = "Groundwater Inflow (ac-ft)")
  #
  # ggplot(plot_df) +
  #   geom_point(aes(x = NISTunits::NISTmeterTOft(level_m),
  #                  y = NISTunits::NISTcubMeterTOacreFt(GWout_m3),
  #                  color = sim)) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "Mean Lake Elevation (ft)", y = "Groundwater Outflow (ac-ft)")
  #
  # ggplot(plot_df) +
  #   geom_point(aes(x = NISTunits::NISTcubMeterTOacreFt(-GWin_m3),
  #                  y = NISTunits::NISTcubMeterTOacreFt(GWout_m3),
  #                  color = sim)) +
  #   facet_wrap(~lake, scales = "free") +
  #   theme_bw() +
  #   labs(x = "Groundwater Inflow (ac-ft)", y = "Groundwater Outflow (ac-ft)")
}
