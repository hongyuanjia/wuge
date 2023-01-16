source(here::here("tools/query.R"))
source(here::here("tools/format.R"))

# sources from other repos
DICT_SOURCE <- c(
    stconv        = "BYVoid/OpenCC/data/dictionary/STCharacters.txt",
    sancai        = "JakLiao/GoodGoodName/data/sancai.txt",
    shuli         = "whmnoe4j/Calendar/app/Console/Commands/ImportShuli.php",
    char          = "mapull/chinese-dictionary/data/character/char_base.json",
    char_common   = "mapull/chinese-dictionary/data/character/char_common.json",
    kangxi        = "fh250250/fortune/vendor/data/kangxi.csv",
    kangxi_wuxing = "sinshine/AI-name/data/wuxing.csv"
)

# local sources
DICT_SPECIAL <- c(
    sancai        = here::here("tools/data/special_sancai.csv")
)
DICT_LUCK <- c(
    base          = here::here("tools/data/luck_base.csv"),
    social        = here::here("tools/data/luck_social.csv"),
    health        = here::here("tools/data/luck_health.json")
)

init_sources <- function(types = NULL, update = FALSE) {
    if (is.null(types)) types <- names(DICT_SOURCE)

    if (any(invld <- !types %in% names(DICT_SOURCE))) {
        stop("Invalid source found: ", paste0("'", names(DICT_SOURCE)[invld], "'", collapse = ", "))
    }

    sources <- here::here("tools/data", basename(DICT_SOURCE))
    names(sources) <- names(DICT_SOURCE)

    if (any(!file.exists(sources))) update <- TRUE
    if (!update) return(sources[types])

    sources[types] <- vapply(DICT_SOURCE[types], FUN.VALUE = character(1L),
        function(src) {
            spl <- strsplit(src, "/", fixed = TRUE)[[1L]]
            init_file(
                dict = spl[[length(spl)]],
                dir = if (length(spl) == 3L) NULL else do.call(file.path, as.list(spl[-c(1:2, length(spl))])),
                repo = file.path(spl[1], spl[2]),
                new_name = spl[[length(spl)]],
                log = TRUE
            )
        }
    )
    sources[types]
}

path_dest <- function(dest) here::here("inst/extdata", dest)

gen_stconv <- function(update = FALSE) {
    src <- init_sources("stconv", update = update)
    data.table::fwrite(format_tbl_conv(src), path_dest("stconv.csv"))
}

gen_sancai <- function(update = FALSE) {
    src <- init_sources("sancai", update = update)
    sancai1 <- format_tbl_sancai(src)
    sancai2 <- query_all_sancai(force = update)

    sancai1[sancai2, on = "wuxing", `:=`(
        jixiong = i.jixiong, score = i.score, brief = i.brief, detail = i.detail
    )]

    data.table::setcolorder(sancai1, c("wuxing", "score", "mod1", "mod2",
        "jixiong", "brief", "description", "detail"))
    data.table::fwrite(sancai1, path_dest("sancai.csv"))
}

gen_shuli <- function(update = FALSE) {
    src <- init_sources("shuli", update = update)
    shuli1 <- format_tbl_shuli(src)
    shuli2 <- query_all_shuli(force = update)
    data.table::setnames(shuli1, c("desc_full", "desc"), c("description", "indication"))

    stopifnot(all(shuli1$num == shuli2$number))

    shuli1[shuli2, on = c("num" = "number"),
        c("brief", "jixiong", "short", "score", "description", "indication") := {
            same <- brief == i.brief
            brief[!same] <- sprintf("%s(%s)", brief[!same], i.brief[!same])

            list(
                brief = brief,
                jixiong = i.jixiong,
                short = i.short,
                score = i.score,
                description = sprintf("(1)%s||(2)%s", description, i.description),
                indication = sprintf("%s||%s", indication, i.fortune)
            )
        }
    ]
    data.table::setcolorder(shuli1,
        c("num", "score", "jixiong", "short", "brief", "indication",
            "foundation", "family", "health", "future", "fortune", "description"
        )
    )

    data.table::fwrite(shuli1, path_dest("shuli.csv"))
}

gen_char <- function(update = FALSE) {
    src <- init_sources("char", update = update)
    data.table::fwrite(format_tbl_char(src), path_dest("char.csv"))
}

gen_char_common <- function(update = FALSE) {
    src <- init_sources("char_common", update = update)
    data.table::fwrite(format_tbl_char_common(src), path_dest("char_common.csv"))
}

gen_kangxi <- function(update = FALSE) {
    src <- init_sources(c("kangxi", "kangxi_wuxing"), update = update)
    kangxi <- format_tbl_kangxi(src["kangxi"], src["kangxi_wuxing"])
    data.table::fwrite(kangxi, path_dest("kangxi.csv"))
}

gen_special <- function(update = FALSE) {
    data.table::fwrite(
        format_tbl_special_sancai(DICT_SPECIAL["sancai"]),
        path_dest("special_sancai.csv")
    )
}

gen_luck <- function(update = FALSE) {
    data.table::fwrite(
        format_tbl_luck_social(DICT_LUCK["social"]),
        path_dest("luck_social.csv")
    )

    career <- format_tbl_luck_career(DICT_LUCK["base"])
    health <- format_tbl_luck_health(DICT_LUCK["health"])

    data.table::fwrite(career$base, path_dest("luck_base.csv"))
    data.table::fwrite(career$success, path_dest("luck_success.csv"))

    data.table::fwrite(health$sancai, path_dest("luck_health_sancai.csv"))
    data.table::fwrite(health$desc, path_dest("luck_health_desc.csv"))

}

gen_dict <- function(types = NULL, update = FALSE) {
    all_types <- c(
        setdiff(names(DICT_SOURCE), "kangxi_wuxing"),
        "special", "luck"
    )

    if (is.null(types)) types <- all_types
    dicts <- types

    if ("special" %in% dicts) {
        dicts <- setdiff(c(dicts, sprintf("special_%s", names(DICT_SPECIAL))), "special")
    }

    if ("luck" %in% dicts) {
        dicts <- setdiff(c(dicts, sprintf("luck_%s", names(DICT_LUCK))), "luck")
    }

    if (any(invld <- !types %in% all_types)) {
        stop("Invalid type found: ", paste0("'", all_types[invld], "'", collapse = ", "))
    }

    dicts <- setNames(path_dest(paste0(dicts, ".csv")), dicts)

    for (type in types) {
        if (!file.exists(dicts[type])) update <- TRUE
        match.fun(sprintf("gen_%s", type))(update)
    }

    dicts
}
