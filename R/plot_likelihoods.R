
plot_likelihoods <- function(
    dat,
    by_fleet=FALSE,
    summarise_fleets=FALSE,
    facet = NULL,
    group = NULL,
    make_rda = FALSE,
    figures_dir = getwd(),
    interactive = TRUE,
    ...
){
    check_data(dat)

    likelihood_type <- "values"
    if(by_fleet){
        cli::cli_alert_info("Plotting likelihood components by fleet.")
        likelihood_type <- "like"
        facet <- "fleet"
    }

    prepared_data <- filter_data(
        dat = dat,
        label_name = "",
        geom=geom,
        era=NULL,
        group=group,
        facet=facet,
        module="likelihoods",
        scale_amount=1,
        interactive=TRUE
    )

    if(!by_fleet){
        prepared_data <- prepared_data |> 
            dplyr::filter(is.na(fleet))
    }else{
        prepared_data <- prepared_data |>
            dplyr::filter(!is.na(fleet))
    }

    processing <- process_data(
        dat = prepared_data |> dplyr::filter(type==likelihood_type),
        group = group,
        facet = facet
    )

    plot_data <- processing[[1]] |> dplyr::filter(!is.na(estimate), estimate!=0)

    if(by_fleet && summarise_fleets){
        plot_data <- plot_data |> 
            dplyr::group_by(model, group_var, label) |>
            dplyr::summarise(
                fleet="All",
                estimate = sum(estimate)
            ) |> dplyr::bind_rows(plot_data)
    }
    
    group <- processing[[2]]
    # if (!is.null(processing[[3]])) facet <- processing[[3]]

    plt <- ggplot2::ggplot(plot_data)+
        ggplot2::geom_col(
            ggplot2::aes(y=label, x=estimate, fill=label)
        )+
        ggplot2::geom_vline(xintercept=0, linetype="dashed")+
        ggplot2::scale_x_continuous(
            limits= ~ c(-max(abs(.x)), max(abs(.x)))
        )+
        ggplot2::guides(fill="none")+
        ggplot2::labs(y="Likelihood Component", x="Likelihood Contribution")+
        theme_noaa()

    # Check if facet(s) are desired
    if (!is.null(facet) & length(facet) > 0) {
        facet <- paste("~", paste(facet, collapse = " + "))
        facet_formula <- stats::reformulate(facet)

        plt <- plt + ggplot2::facet_wrap(facet_formula, labeller = function(labels) {
            # Clean column names (e.g., "growth_pattern" -> "Growth Pattern")
            names(labels) <- tools::toTitleCase(gsub("_", " ", names(labels)))

            # Combine into "Variable: Value" format
            lapply(names(labels), function(var) paste0(var, ": ", labels[[var]]))
        })
    }

    return(plt)
}