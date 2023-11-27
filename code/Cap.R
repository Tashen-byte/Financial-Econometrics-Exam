Proportional_Cap_Foo <- function(data, W_Cap  ){

    # Let's require a specific form from the user... Alerting when it does not adhere this form
    if( !"weight" %in% names(data)) stop("... for Calc capping to work, provide weight column called 'weight'")

    if( !"Tickers" %in% names(data)) stop("... for Calc capping to work, provide id column called 'Tickers'")

    # First identify the cap breachers...
    Breachers <-
        data %>% filter(weight > W_Cap) %>% pull(Tickers)

    # Now keep track of breachers, and add to it to ensure they remain at 10%:
    if(length(Breachers) > 0) {

        while( data %>% filter(weight > W_Cap) %>% nrow() > 0 ) {


            data <-

                bind_rows(

                    data %>% filter(Tickers %in% Breachers) %>% mutate(weight = W_Cap),

                    data %>% filter(!Tickers %in% Breachers) %>%
                        mutate(weight = (weight / sum(weight, na.rm=T)) * (1-length(Breachers)*W_Cap) )

                )

            Breachers <- c(Breachers, data %>% filter(weight > W_Cap) %>% pull(Tickers))

        }

        if( sum(data$weight, na.rm=T) > 1.001 | sum(data$weight, na.rm=T) < 0.999 | max(data$weight, na.rm = T) > W_Cap) {

            stop( glue::glue("For the Generic weight trimming function used: the weight trimming causes non unit
      summation of weights for date: {unique(data$date)}...\n
      The restriction could be too low or some dates have extreme concentrations...") )

        }

    } else {

    }

    data

}
