#' @title Plot of data passed into a gadget model
#' @description Plot all observational data from a gadget model
#' @inheritParams plot_annual
#' @param model R or TMB model. A model object from \link[gadget3]{g3_to_r} or \link[gadget3]{g3_to_tmb} functions.
#' @param regexp Either \code{NULL} (no filtering) or a character string giving a regular expression to filter model-data components. Useful alternatives: \code{"adist"} for abundance distribution data, \code{"cdist"} for catch distribution data, \code{"surveyindices"} for survey indices, and \code{"catch"} for catches.
#' @param scales Character defining the \code{\link[ggplot2]{facet_wrap}} \code{scales} argument to use.
#' @param ncol Number of columns passed to  \code{\link[ggplot2]{facet_wrap}}
#' @details Plots data as contained in a gadget model. Helpful in checking models for possible data issues and for documentation of models.
#' @return A list of \link[ggplot2]{ggplot} objects.
#' @export

# model; regexp = NULL; scales = "fixed"; ncol = NULL; base_size = 8
g3d_plot <- function(model, regexp = NULL, scales = "fixed", ncol = NULL, base_size = 8) {

  if(inherits(model, "g3_r")) {
    x <- grep("num$|wgt$|catch", ls(environment(model)), value = TRUE)
  } else if(inherits(model, "g3_cpp")) {
    x <- grep("num$|wgt$|catch", ls(attr(model, 'model_data')), value = TRUE)
  } else {
    stop("The model object has to be created using the gadget3::g3_to_tmb or
         gadget3::g3_to_r functions")
  }

  if(!is.null(regexp)) {
    x <- grep(regexp, x, value = TRUE)
  }

  ## Deal with catches which are different format

  if(any(grepl("catch", x))) {
    tmp <- unique(gsub("__keys|__values", "", grep("catch", x, value = TRUE)))

    x <- c(
      lapply(x[!grepl("catch", x)], function(k) k),
      lapply(tmp, function(k) grep(k, x, value = TRUE))
    )

  } else {
    x <- lapply(x, function(k) k)
  }

  # i = 3
  lapply(seq_along(x), function(i) {

    message(paste(x[[i]], collapse = ", "))

    if(any(grepl("catch", x[[i]]))) {
      if(length(x[[i]]) == 1) {
        if(!grepl("keys$", x)[[i]]) {
          stop("No keys attribute in ", x[[i]])
        }
        if(inherits(model, "g3_r")) {
          y <- environment(model)[[x[[i]]]]
        } else if(inherits(model, "g3_cpp")) {
          y <- attributes(model)$model_data[[x[[i]]]]
        }

        y <- data.frame(time = lubridate::yq(y), value = 0)
      } else {

        if(inherits(model, "g3_r")) {
          y <-
            stats::setNames(
              lapply(x[[i]], function(k) environment(model)[[k]]),
              x[[i]]
            )
        } else if(inherits(model, "g3_cpp")) {
          y <-
            stats::setNames(
            lapply(x[[i]], function(k) attributes(model)$model_data[[k]]),
            x[[i]]
            )
        }

        y <- data.frame(
          time = lubridate::yq(y[[grep("keys$", names(y))]]),
          value = y[[grep("values$", names(y))]]/1e6 # kt
          )
      }
    } else {
      if(inherits(model, "g3_r")) {
        y <- environment(model)[[x[[i]]]]
      } else if(inherits(model, "g3_cpp")) {
        y <- attributes(model)$model_data[[x[[i]]]]
      }

      y_lab <- ifelse(grepl("num$", x[i]), "Number", "Weight")
    }

    if(any(grepl("catch", x[[i]]))) {

      ggplot2::ggplot(y, ggplot2::aes(.data$time, .data$value)) +
        ggplot2::geom_col(fill = "grey", color = "black", linewidth = LS(0.5)) +
        ggplot2::labs(y = "Catch (kt)", x= 'Year',
                      title = unique(gsub("__keys|__values", "", x[[i]]))
                      ) +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::theme_classic(base_size = base_size)

    } else if(length(dim(y)) == 2) {
      ggplot2::ggplot(
        data.frame(
          time = lubridate::yq(colnames(y)),
          value = unname(y[1,])
        ),
        ggplot2::aes(x = .data$time, y = .data$value)
      ) +
        ggplot2::geom_path() +
        ggplot2::geom_point() +
        ggplot2::labs(y = y_lab,
                      title = x[i],
                      subtitle = paste(names(dim(y))[1], rownames(y))
        ) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(strip.background = ggplot2::element_blank())

    } else if(length(dim(y)) == 3) {

      dat <-
        lapply(1:ncol(y[,,1]), function(j) {
          data.frame(
            time = lubridate::yq(colnames(y))[j],
            xvar = names(y[,j,1]),
            xvar_cont = sapply(strsplit(names(y[,j,1]), ":"), function(k) mean(as.numeric(gsub("Inf", "", k)), na.rm = TRUE)),
            lower = sapply(strsplit(names(y[,j,1]), ":"), function(k) min(as.numeric(k))),
            upper = sapply(strsplit(names(y[,j,1]), ":"), function(k) max(as.numeric(k))),
            value = unname(y[,j,1])
          )
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(width = .data$upper - .data$lower)

      dat$width[is.infinite(dat$width)] <- max(dat$width[!is.infinite(dat$width)])

      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = .data$xvar_cont,
                     y = .data$value,
                     width = .data$width)
      ) +
        ggplot2::geom_col() +
        ggplot2::facet_wrap(~time, scales = scales, ncol = ncol) +
        ggplot2::labs(y = y_lab,
                      title = x[i],
                      x = names(dim(y[,,1]))[1]
        ) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(strip.background = ggplot2::element_blank())

    } else if(length(dim(y)) == 4) {

      dat <-
        lapply(1:dim(y)["time"], function(j) {
          out <- as.data.frame(y[,,j,1])
          out$xvar <- rownames(out)
          out$time <- lubridate::yq(colnames(y[1,,,]))[j]
          out$xvar_cont <- sapply(strsplit(out$xvar, ":"), function(k) mean(as.numeric(gsub("Inf", "", k)), na.rm = TRUE))
          out$lower <- sapply(strsplit(out$xvar, ":"), function(k) min(as.numeric(k)))
          out$upper <- sapply(strsplit(out$xvar, ":"), function(k) max(as.numeric(k)))

          tidyr::pivot_longer(out, -c(.data$time, .data$xvar, .data$xvar_cont, .data$lower, .data$upper))
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(width = .data$upper - .data$lower)

      if(names(dim(y))[2] == "age") {

        if(all(sapply(strsplit(dat$name, ":"), function(k) diff(as.numeric(k))) == 0)) {
          dat$name <- sapply(strsplit(dat$name, ":"), function(k) min(as.numeric(k)))
        }

        ggplot2::ggplot(
          dat,
          ggplot2::aes(xmin = .data$lower, xmax = .data$upper, ymin = 0, ymax = .data$value, fill = .data$name)) +
          ggplot2::geom_rect() +
          ggplot2::facet_wrap(~time, scales = scales, ncol = ncol) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::labs(y = y_lab,
                        title = x[i],
                        x = names(dim(y))[1],
                        fill = names(dim(y))[2]
          ) +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme(strip.background = ggplot2::element_blank())

      } else {

        dat %>% dplyr::select(-.data$upper) %>%
          dplyr::bind_rows(
            dat %>%
              dplyr::filter(.data$upper == max(.data$upper)) %>%
              dplyr::select(-.data$lower) %>%
              dplyr::rename("lower"= "upper")
          ) %>%
          dplyr::arrange(.data$time, .data$lower) %>%
          ggplot2::ggplot() +
          ggplot2::geom_step(
            ggplot2::aes(x = .data$lower, y = .data$value, color = .data$name)) +
          ggplot2::facet_wrap(~time, scales = scales, ncol = ncol) +
          ggplot2::labs(y = y_lab,
                        title = x[i],
                        x = names(dim(y))[1],
                        color = names(dim(y))[2]
          ) +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme(strip.background = ggplot2::element_blank())

      }
    }
  })

}
