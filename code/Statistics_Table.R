
###########################################
Statistics_table <- function(data, BM, Yrs_lookback, NA_Check){

    data_included <- data %>%
        filter(date >= fmxdat::safe_year_min(datesel = last(date), N = Yrs_lookback))

    data_considered <- data_included %>%
        group_by(Return_Type) %>%
        summarise(N_noNA = sum(!is.na(Returns))/length(unique(data_included$date))) %>%
        filter(N_noNA > NA_Check) %>%
        pull(Return_Type)

    data_xts <- data_included %>%
        filter(Return_Type %in% data_considered) %>%
        tbl_xts(cols_to_xts = Returns, spread_by = Return_Type, Colnames_Exact = T)

    Bench_xts <- data_included %>%
        filter(Return_Type %in% BM) %>%
        tbl_xts(cols_to_xts = Returns, Colnames_Exact = T)

    library(PerformanceAnalytics)

    Moments <- bind_rows(
        data.frame(Return.cumulative(data_xts) ) %>% round(., 3),
        data.frame(Return.annualized(data_xts, scale = 12, geometric = T)) %>% round(., 3),
        data.frame(PerformanceAnalytics::Return.annualized.excess(data_xts, Bench_xts) ) %>% round(., 3),
        data.frame(sd.annualized(data_xts, scale = 12, geometric = T)) %>% round(., 3),
        data.frame(PerformanceAnalytics::AdjustedSharpeRatio( data_xts ) ) %>% round(., 3),
        data.frame(PainIndex(data_xts, scale = 12, geometric = T)) %>% round(., 3),
        data.frame(AverageDrawdown(data_xts, scale = 12)) %>% round(., 3),
        data.frame(fmxdat::Safe_TE(Ra = data_xts, Rb = Bench_xts, scale = 12)) %>% round(., 3),
        data.frame(PerformanceAnalytics::InformationRatio(Ra = data_xts, Rb = Bench_xts)) %>% round(., 3),
        data.frame(PerformanceAnalytics::CAPM.beta(Ra = data_xts, Rb = Bench_xts, Rf = 0)) %>% round(., 3),
        data.frame(PerformanceAnalytics::CAPM.beta.bull(Ra = data_xts, Rb = Bench_xts, Rf = 0)) %>% round(., 3),
        data.frame(PerformanceAnalytics::CAPM.beta.bear(Ra = data_xts, Rb = Bench_xts, Rf = 0)) %>% round(., 3),
        data.frame(PerformanceAnalytics::UpDownRatios(Ra = data_xts, Rb = Bench_xts, method = "Percent", side = "Up")) %>% round(., 3),
        data.frame(PerformanceAnalytics::CVaR(R = data_xts, p = 0.05, method = "modified")) %>%
            round(., 3)
    ) %>%
        tibble::rownames_to_column("Info") %>%
        mutate(Period = glue::glue("Last {Yrs_lookback} Years"),
               Info = c("Cum Returns", "Returns (Ann.)", "Returns Excess (Ann.)", "SD (Ann.)", "Adj. Sharpe Ratio", "Pain Index",                                                                          "Avg DD", "Tracking Error", "Information Ratio", "Beta", "Beta Bull", "Beta Bear", "Up-Down Ratio", "Modified CVaR")) %>%
        relocate(Period, .before = Info) %>%
        as_tibble()

    # This line replaces the `.` with a space.
    # Note the forward slashes, as `.` there means everything, `\\.` means a full-stop
    colnames(Moments) <- gsub("\\.", " ", colnames(Moments))
    Moments
}
##################################################
