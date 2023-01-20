#' Export information of an `WuGe` object into an image
#'
#' `export()` will write all information of WuGe into an image. This makes it
#' easy to share it around. For `WuGeName`, all information will be written into
#' an Excel file, including all possible name combinations, WuGe information of
#' each stroke combination.
#'
#' @param x An `WuGe` object created using [wuge()] or an `WuGeName` object
#'        created using [name()].
#'
#' @param file For `WuGe` object, a path of image passed to [ggplot2::ggsave()];
#'        for `WuGeName` object, a path of an Excel file passed to
#'        [openxlsx::saveWorkbook()].
#'
#' @param width,height,dpi The size and resolution of image to create. Directly
#'        passed to [ggplot2::ggsave()]. Default values are `10.5` inches for
#'        `width`, `27` inches for `height` and `300` for dpi. Those values are
#'        tested to make sure all possible information can fit into a single
#'        image, but also make the saved image large (~2.8MB).
#'
#' @param ... Other arguments to be passed to [ggplot2::ggsave()].
#'
#' @param fmt_width An integer number indicating the line width when formatting.
#'        It is used to set the option of `cli.width`. Default: `100`.
#'
#' @param verbose If `TRUE`, a progress bar will be shown for long-time export
#'        process. Default: `TRUE`.
#'
#' @return The full path of the file generated, invisibly.
#'
#' @examples
#' \dontrun{
#' export(wuge("张", "三"), "张三.png")
#'
#' export(name("张", max = 10), "张.xlsx")
#' }
#'
#' @export
export <- function(x, file, width = 10.5, height = 27, dpi = 300, ..., fmt_width = 100, verbose = TRUE) {
    UseMethod("export")
}

#' @export
export.default <- function(x, ...) {
    stop(sprintf("Did not know how to handle an object of class '%s'.", class(x)[1L]))
}

#' @export
export.WuGe <- function(x, file, width = 10.5, height = 27, dpi = 300, ..., fmt_width = 100) {
    if (nrow(x$xing) != 1L) stop("Currently, only one name can be exported at one time.")
    txt <- strsplit(format_wuge_single(x, width = 100), "\n", fixed = TRUE)
    txt[lengths(txt) == 0L] <- ""
    txt <- c("", unlist(txt), "")

    # NOTE: to make CRAN checks happy
    index <- text <- NULL
    p <- ggplot2::ggplot(data.frame(index = rev(seq_along(txt)), text = txt)) +
        ggplot2::geom_text(ggplot2::aes(0.02, index, label = text), vjust = 1, hjust = "left") +
        ggplot2::coord_cartesian(xlim = c(0, 1), expand = FALSE) +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "grey95", color = "black"),
            axis.title = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank()
        )

    ggplot2::ggsave(file, p, width = width, height = height, dpi = dpi, ...)
    invisible(normalizePath(file))
}

#' @export
export.WuGeName <- function(x, file, width = 10.5, height = 27, dpi = 300, ..., verbose = TRUE, fmt_width = 100) {
    if (length(unique(x$ming$index)) != 1L) stop("Currently, only one name can be exported at one time.")
    assert_flag(verbose)
    stopifnot(is.character(file) && length(file) == 1L && tools::file_ext(file) == "xlsx")

    wb <- openxlsx::createWorkbook()
    index <- split(x$score[, .SD, .SDcols = c("index", "id")], by = c("index", "id"))

    if (verbose) {
        cli::cli_progress_bar(
            total = length(index), clear = TRUE,
            format = "[{cli::pb_current}/{cli::pb_total}] | {cli::pb_percent} {cli::pb_bar} [Elapsed: {cli::pb_elapsed}]"
        )
    }
    for (ind in index) {
        sgl <- subset_wuge(x, ind$index, ind$id)

        sheet <- sprintf("%s%s",
            sgl$xing$character, paste0(sprintf("\u3010%s\u3011", sgl$ming$stroke_wuge), collapse = "")
        )

        if (verbose) cli::cli_progress_output(paste0("Working on '", sheet, "'", collapse = "\n"))
        if (nrow(sgl$ming) == 1L) {
            chars <- data.table::data.table(
                paste0(sgl$xing$character, collapse = ""),
                sgl$ming$character[[1L]]
            )
            data.table::setnames(chars, c("\u59d3", "\u540d"))
        } else {
            chars <- do.call(data.table::CJ, sgl$ming$character)
            data.table::setnames(chars, sprintf("\u7b2c%i\u5b57", seq_len(nrow(sgl$ming))))
            data.table::set(chars, NULL, "\u59d3", paste0(sgl$xing$character, collapse = ""))
            data.table::setcolorder(chars, "\u59d3")
        }
        data.table::set(chars, NULL, "\u59d3\u540d", do.call(paste0, chars))

        openxlsx::addWorksheet(wb, sheet)
        openxlsx::activeSheet(wb) <- sheet
        openxlsx::writeDataTable(wb, sheet, chars,
            withFilter = TRUE, bandedRows = TRUE, tableStyle = "TableStyleMedium2"
        )

        img <- export.WuGe(sgl, tempfile(fileext = ".png"),
            fmt_width = 100, width = width, height = height, dpi = dpi, ...
        )
        openxlsx::insertImage(wb, sheet, img, width = width, height = height,
            startRow = 1, startCol = ncol(chars) + 2
        )

        if (verbose) cli::cli_progress_update(1L)
    }

    openxlsx::activeSheet(wb) <- 1L
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    if (verbose) cli::cli_alert_success("Exporting process complete!")
    invisible(normalizePath(file))
}
