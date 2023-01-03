#' Wrapper for plot.gadget.fit that saves all diagnostic graphs to a directory
#'
#' @inheritParams plot_annual
#' @param path Directory path for saving figures
#' @param quiet Logical indicating whether to print messages about the plotting process (set to \code{FALSE} to suppress the messages.
#' @param file_type Character. Either one of the \code{device}s in \code{\link[ggplot2]{ggsave}}, in which case files of the defined type are printed to \code{path}, or "html" which compiles all plots into one html file using \code{\link[knitr]{knit}} and the \code{\link{make_html}} function.
#' @param width,height Plot size given in \code{units}. If \code{NULL}, reasonable standard values are used.
#' @param units Units for plot size. See \code{\link[ggplot2]{ggsave}}
#' @param res Plot resolution. See the \code{dpi} argument in \code{\link[ggplot2]{ggsave}}
#' @return Returns nothing, but makes the requested files.
#' @export

# Debugging params:
# path = NULL; quiet = FALSE; width = NULL; height = NULL; units = "cm"; res = 300
gadget_plots <- function(fit, path, file_type = "png", quiet = FALSE, width = NULL, height = NULL, units = "cm", res = 300){

  if(is.null(width) & units == "cm") width <- 18
  if(is.null(height) & units == "cm") height <- 10

  # if (is.null(path)){
  #   path <- file.path(getwd(), 'figs')
  # }

  ## Stop if the directory for saving figures does not exist
  if (!dir.exists(path)){
    dir.create(path) # This might not be allowed when submitting to CRAN
    # stop(paste0("The following directory for saving figures does not exist: path = ", path))
  }

  if(file_type == "html") {
    make_html(fit = fit, path = path)
  } else {

    if(!file_type %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")) stop("file_type has to be one of those listed in device argument for ggplot2")
    if(!quiet) message("Plotting .", file_type, "s to ", path)

    ## ICES
    if(!quiet) message("Plotting annual output (ICES plot)")

    grDevices::png(
      file = paste(file.path(path, "Annual_plot"), file_type, sep = "."),
      width = width, height = height,
      units = units,res = res)
    print(plot_annual(fit))
    grDevices::dev.off()

    ## Catches
    if(!quiet) message("Catches")
    ggplot2::ggsave(
      file = paste0("Catches_by_fleet.", file_type),
      plot = plot_catch(fit, type = "fleet") +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE)) +
      ggplot2::theme(legend.position = "bottom"),
      path = path, bg = "white", width = width, height = height, units = units,
      dpi = res)

    ## Survey indices
    if(!quiet) message("Plotting survey indices")
    ggplot2::ggsave(
      file = paste0("Survey_indices.", file_type),
      plot = plot_si(fit),
      path = path, bg = "white", width = width, height = height, units = units,
      dpi = res)

    ## Catch distributions

    if(!quiet) message("Plotting catch distributions")
    tmp <- plot_catchdist(fit)

    lapply(seq_along(tmp), function(i) {
      if(!quiet) message(i, "/", length(tmp))

      ggplot2::ggsave(
        file = paste0("Catch_distribution_", names(tmp)[i], ".", file_type),
        plot = print(tmp[[i]]),
        path = path, bg = "white", width = width, height = height*2,
        units = units, dpi = res)
    })

    ## Stock distribution
    if (!is.null(fit$stockdist)){

      if(!quiet) message("Plotting stock distribution")
      tmp <- plot_stockdist(fit, stocks = "separate")

      lapply(seq_along(tmp), function(i) {
        if(!quiet) message(i, "/", length(tmp))

        ggplot2::ggsave(
          file = paste0("Stock_distribution_", names(tmp)[i], ".", file_type),
          plot = print(tmp[[i]]),
          path = path, bg = "white", width = width, height = height*2,
          units = units, dpi = res)
      })

      if(!quiet) message("Plotting model stock composition")
      tmp <- plot_stockdist(fit, type = "stock_composition", geom_area = TRUE)

      # lapply(seq_along(tmp), function(i) {
      #   if(!quiet) message(i, "/", length(tmp))

        ggplot2::ggsave(
          file = paste0("Stock_composition", ".", file_type),
          plot = print(tmp),
          path = path, bg = "white", width = width, height = height*2,
          units = units, dpi = res)
      # })
    }
    else{
      if (!quiet) message("No stockdist data to plot")
    }

    ## Suitability
    if(!quiet) message("Plotting suitability")
    ggplot2::ggsave(
      file = paste0("Suitability.", file_type),
      plot = print(plot_suitability(fit) + ggplot2::theme(legend.position = "bottom")),
      path = path, bg = "white", width = width, height = height*1.5, units = units,
      dpi = res)

    ## Average length by age (growth)
    if(!quiet) message("Plotting growth")
    ggplot2::ggsave(
      file = paste0("Growth.", file_type),
      plot = print(plot_growth(fit)),
      path = path, bg = "white", width = width, height = height, units = units,
      dpi = res)

    ## Age-length
    if(!quiet) message("Plotting age-length")
    if (any(grepl('aldist', unique(fit$catchdist.fleets$name)))){
      tmp <- plot_agelength(
        fit,
        name = grep('aldist', unique(fit$catchdist.fleets$name), value = TRUE)
        )

      lapply(seq_along(tmp), function(i) {
        if(!quiet) message(i, "/", length(tmp))
        ggplot2::ggsave(
          file = paste0("Agelength_", names(tmp)[i], ".", file_type),
          plot = print(tmp[[i]]),
          path = path, bg = "white", width = width,
          height = height,
          units = units, dpi = res)
      })
    }

    ## Parameters
    #  tmp <- plot(fit, data="params", height=par("din")[1]*2, width=par("din")[2])
    #  ggplot2::ggsave("parameters.png", path=path, plot = tmp, height=par("din")[1]*2, width=par("din")[2]*1.25, bg = "white")

    ## Age composiition
    if(!quiet) message("Plotting age composition")
    ggplot2::ggsave(
      file = paste0("Age_composition.", file_type),
      plot = print(plot_agecomp(fit)),
      path = path, bg = "white", width = width, height = height*2, units = units,
      dpi = res)


    ## Residuals
    if(!quiet) message("Plotting residuals")

    ggplot2::ggsave(
      file = paste0("Residuals.", file_type),
      plot = print(plot_resid(fit)),
      path=path, bg = "white", width = width,
      height = height, units = units, dpi = res
    )

    ## Likelihood

    if(!quiet) message("Plotting likelihood")

    ggplot2::ggsave(
      file = paste0("Likelihood.", file_type),
      plot = print(plot_likelihood(fit)),
      path = path, bg = "white", width = width, height = height, units = units,
      dpi = res)

    ggplot2::ggsave(
      file = paste0("Likelihood_weighted.", file_type),
      plot = print(plot_likelihood(fit, type = "weighted")),
      path = path, bg = "white", width = width, height = height, units = units,
      dpi = res)

    ggplot2::ggsave(
      file = paste0("Likelihood_sums.", file_type),
      plot = print(plot_likelihood(fit, type = "sums")),
      path = path, bg = "white", width = width, height = height,
      units = units,dpi = res)

    ### Parameters
    if(!quiet) message("Plotting parameters")

    ggplot2::ggsave(
      file = paste0("Parameters.", file_type),
      plot = print(plot_param(fit, out_only = TRUE)),
      path = path, bg = "white", width = width, height = height,
      units = units,dpi = res)

    ggplot2::ggsave(
      file = paste0("Parameter_weights.", file_type),
      plot = print(plot_weight(fit)),
      path = path, bg = "white", width = width, height = height,
      units = units,dpi = res)

  }
}
