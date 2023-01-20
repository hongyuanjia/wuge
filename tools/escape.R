scripts <- list.files(here::here("R"))

for (src in scripts) {
    lines <- stringi::stri_read_lines(here::here("R", src))
    ind <- which(!stringi::stri_detect_regex(lines, "\\s*#") &
        (
            stringi::stri_detect_regex(lines, "\\p{Han}") |
            stringi::stri_detect_regex(lines, "[【】，。！“”‘’、]")
        )
    )
    if (length(ind)) {
        message(
            sprintf("Found Chinese characters in '%s' at line [%s]. Replacing...",
                src, paste0(ind, collapse = ", ")
            )
        )
        loc <- stringi::stri_locate_all_regex(lines[ind], "[【】，。！“”‘’、]|\\p{Han}")
        lines[ind] <- stringi::stri_sub_all_replace(lines[ind], loc,
            replacement = lapply(
                stringi::stri_sub_all(lines[ind], loc),
                stringi::stri_escape_unicode
            )
        )
        stringi::stri_write_lines(lines, here::here("R", src), sep = "\n")
    }
}
