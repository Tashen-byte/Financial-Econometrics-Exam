filling_in_the_gaps <- function(data, fill_amalgam = "NONE"){
    if(!"date" %in% colnames(data)) stop("Get a date!\n In here and Out there!!")

    if(fill_amalgam %in% c("None", "NONE","none")) {
        if(any(is.na(data))) warning("There is a need for amalgam or composite, \nCheck each Method.")
        return(data)
    }

    if(fill_amalgam == "Average_Returns"){
        data <- data %>%
            gather(Stocks, Returns, -date) %>%
            group_by(date) %>%
            mutate(Average = mean(Returns, na.rm =T)) %>% #getting the average returns per date
            mutate(Avergae = coalesce(Average,0)) %>%
            ungroup() %>%
            mutate(Returns = coalesce(Returns, Average)) %>%
            select(-Average) %>%
            spread(Stocks, Returns)

        return(data)
    } else
        if(fill_amalgam == "Drawn_Distribution_Own"){ #strictly looking at own column distribution
            N <- nrow(data)
            data <- left_join(data %>% gather(Stocks, Returns, -date),
                                         data %>%
                                             gather(Stocks, Returns, -date) %>%
                                             group_by(Stocks) %>%
                                             mutate(Dens = list(density(Returns, na.rm=T))) %>% #density provides moments for each stock. therefore must make as a list, due to it being an object
                                             summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y))),
                                         by = "Stocks"
            ) %>% #Left Join ends
                group_by(Stocks) %>%
                # Random draw from sample:
                mutate(Returns = coalesce(Returns, Random_Draws[[1]][row_number()])) %>% #replacing NA's with random draws created
                select(-Random_Draws) %>%
                ungroup() %>%
                spread(Stocks, Returns)
            return(data)
        } else
            if(fill_amalgam == "Drawn_Distribution_Collective"){ #looking across data set for each time period/date.
                NAll <- nrow(data %>%
                                 gather(Stocks, Returns, -date))
                # DIY: see what density function does
                data <-
                    bind_cols(
                        data %>% gather(Stocks, Returns, -date),
                        data %>% gather(Stocks, Returns, -date) %>%
                            mutate(Dens = list(density(Returns, na.rm=T))) %>%
                            summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y))) %>%
                            unnest(Random_Draws)
                    ) %>%
                    mutate(Returns = coalesce(Returns, Random_Draws)) %>%
                    select(-Random_Draws) %>%
                    spread(Stocks, Returns)
                return(data)
            } else
                if( fill_amalgam  == "Zero") {
                    warning("This is not the way")
                    data[is.na(data)] <- 0
                    return(data)
                } else
                    stop("Please provide a valid fill_amalgam method. Options include:\n'Average_Returns', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")

    data

}