#' Plot likelihood components
#' 
#' Plot likelihood components as a horizontal bar chart.
#' 
#' @inheritParams plot_spawning_biomass
#' 
#' @param by_fleet Logical, whether to plot fleet specific likelihood
#' components or not
#' 
#' Default: FALSE
#' 
#' @param summarise_fleets Logical, whether to aggregate fleet specific 
#' likelihood components into an additional "total" likelihood when plotting
#' fleet specific likelihoods. Only applied when `by_fleet=TRUE`.
#' 
#' Default: FALSE
#' 
#' @return A plot showing the value of likelihood components, optionally
#' across fleets.
#' 
#' @export 
#' 
#' @examples
#' \dontrun{
#' plot_likelhoods(
#'   dat = stockplotr::example_data,
#' )
#' plot_likelhoods(
#'   dat = stockplotr::example_data,
#'   by_fleet = TRUE
#' )
#' plot_likelhoods(
#'   dat = stockplotr::example_data,
#'   by_fleet = TRUE,
#'   summarise_fleets = TRUE
#' )
#' }
plot_likelihoods <- function(
    dat,
    by_fleet=FALSE,
    summarise_fleets=FALSE,
    make_rda = FALSE,
    figures_dir = getwd(),
    interactive = TRUE,
    geom="bar",
    group=NULL,
    facet=NULL,
    model=NULL,
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
        model=model,
        module="likelihoods",
        scale_amount=1,
        interactive=TRUE
    ) |> dplyr::filter(!grepl("laplace_obj_fun", label))

    if(!by_fleet){
        prepared_data <- prepared_data |> 
            dplyr::filter(is.na(fleet))
    }else{
        prepared_data <- prepared_data |>
            dplyr::filter(!is.na(fleet))

        prepared_data <- prepared_data |> dplyr::group_by(fleet, type, model) |>
            dplyr::summarise(
                estimate = sum(estimate)
            ) |>
            dplyr::mutate(label="total") |>
            dplyr::bind_rows(prepared_data)

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
                fleet="Total",
                estimate = sum(estimate)
            ) |> dplyr::bind_rows(plot_data) |>
            dplyr::mutate(
                fleet = factor(fleet, levels=c("Total", unique(dat$fleet)))
            )
    }
    
    group <- processing[[2]]
    # if (!is.null(processing[[3]])) facet <- processing[[3]]

    plt <- ggplot2::ggplot(plot_data)+
        ggplot2::geom_col(
            ggplot2::aes(
                y=label, 
                x=estimate, 
                fill=if(length(unique(plot_data$model))>1){
                    .data[["model"]]
                }else{
                    .data[["label"]]
                }
            ),
            position="dodge", stat="identity"
        )+
        ggplot2::geom_vline(xintercept=0, linetype="dashed")+
        ggplot2::scale_x_continuous(
            limits= ~ c(-max(abs(.x)), max(abs(.x)))
        )+
        ggplot2::guides()+
        ggplot2::labs(
            y="Likelihood Component", 
            x="Likelihood Contribution",
            fill = if(length(unique(plot_data$model))>1){
                    "Model"
                }else{
                    "Component"
                }
        )+
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

    plt
}