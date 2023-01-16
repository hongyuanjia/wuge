str_extract <- function(x, pattern, ...) {
    regmatches(x, regexpr(pattern, x, ...))
}

str_extract_all <- function(x, pattern, ...) {
    regmatches(x, gregexpr(pattern, x, ...))
}

download_file <- function(file, dir, repo, new_name = file, branch = "master") {
    dest <- here::here(glue::glue("tools/data/{new_name}"))
    if (!file.exists(dest)) {
        message(glue::glue("Downloading file '{file}'..."))

        if (!dir.exists(dirname(dest))) {
            dir.create(dirname(dest), recursive = TRUE)
        }

        if (is.null(dir)) {
            path <- file
        } else {
            path <- paste(dir, file, sep = "/")
        }

        url <- glue::glue("https://raw.githubusercontent.com/{repo}/{branch}/{path}")
        if (download.file(url, dest, mode = "wb")) {
            stop(glue::glue("Failed to download '{file}'..."))
        }
    }

    dest
}

default_branch <- function(repo) {
    # get default branch
    gh::gh(glue::glue("/repos/{repo}"))$default_branch
}

check_sha <- function(file, dir, repo, branch = "master") {
    message(glue::glue("Retrieving GitHub SHA1 for '{file}'..."))
    endpoint <- glue::glue("/repos/{repo}/git/trees/{branch}")
    if (!is.null(dir)) {
        endpoint <- glue::glue("{endpoint}:{dir}")
    }

    gh <- gh::gh(endpoint)
    res <- purrr::keep(gh$tree, ~.$path == file)[[1L]]
    if (!length(res)) {
        stop(glue::glue("Could not find data of '{file}' from GitHub repo '{repo}'."))
    }
    res <- stats::setNames(list(res$sha), res$path)

    file_sha <- here::here("tools/meta.json")
    if (!file.exists(file_sha)) {
        jsonlite::write_json(res, file_sha, pretty = TRUE, auto_unbox = TRUE)
        return(FALSE)
    } else {
        old_sha <- jsonlite::read_json(file_sha)
        if (!is.null(old_sha[[file]]) && res[[file]] == old_sha[[file]]) {
            return(TRUE)
        } else {
            old_sha[[file]] <- res[[file]]
            jsonlite::write_json(old_sha, file_sha, pretty = TRUE, auto_unbox = TRUE)
            return(FALSE)
        }
    }
}

init_file <- function(dict, dir, repo, new_name = dict, log = TRUE) {
    branch <- default_branch(repo)
    f <- download_file(dict, dir, repo, new_name, branch)

    if (log && !check_sha(dict, dir, repo, branch)) {
        f <- download_file(dict, dir, repo, new_name, branch)
    }
    f
}

format_tbl_conv <- function(file = here::here("tools/data/STCharacters.txt")) {
    conv <- data.table::fread(
        file, sep = "\t", col.names = c("simplified", "traditional")
    )
    conv[, traditional := gsub(" ", "", traditional, fixed = TRUE)]

    conv
}

format_tbl_sancai <- function(file = here::here("tools/data/sancai.txt")) {
    lines <- data.table::fread(file, sep = NULL, header = FALSE)$V1
    grep("^[金木水火土]", lines, value = TRUE)
    i_start <- which(grepl("^[金木水火土]{3}\\s*[0-9]{3}[^0-9]*[0-9]{3}", lines))
    i_end <- which(grepl("【.+】", lines))

    wuxing <- substring(lines[i_start], 1L, 3L)

    mod <- lapply(
        str_extract_all(lines[i_start], "[0-9]"),
        function(x) c(paste0(x[1:3], collapse = ""), paste0(x[4:6], collapse = ""))
    )

    jixiong <- vapply(
        str_extract_all(lines[i_end], "(?<=【).+(?=】)", perl = TRUE),
        paste0, "", collapse = ""
    )

    desc <- unlist(Map(
        function(start, end) {
            txt <- paste0(lines[seq(start + 1L, end)], collapse = "")
            gsub("【.+】$", "", txt)
        },
        i_start, i_end
    ))

    sancai <- data.table::data.table(
        wuxing = wuxing,
        mod1 = vapply(mod, `[`, "", 1L),
        mod2 = vapply(mod, `[`, "", 2L),
        jixiong = gsub("凶多于吉", "凶多吉少", jixiong, fixed = TRUE),
        description = desc
    )

    # 金金火 is missing
    # ref: https://baike.baidu.com/item/三才数理
    patch <- data.table::data.table(
        wuxing = "金金火",
        mod1 = "773",
        mod2 = "784",
        jixiong = "大凶",
        description = "虽因勤奋可得一时之成功发展，但绝对的不安定，杂乱灾祸与变动殊多，必定早因不测之危祸或遭难或外伤或交通事故，或被人杀害或发狂杀人，而引致短寿凶终。"
    )
    sancai <- data.table::rbindlist(list(
        sancai[1:which(sancai$wuxing == "金金木")],
        patch,
        sancai[which(sancai$wuxing == "金金土"):nrow(sancai)]
    ))

    data.table::set(sancai, NULL, "score",
        with(sancai,
            data.table::fcase(
                jixiong == "大吉",     100L,
                jixiong == "吉",       95L,
                jixiong == "中吉",     85L,
                jixiong == "吉多于凶", 75L,
                jixiong == "吉凶参半", 60L,
                jixiong == "凶多吉少", 45L,
                jixiong == "大凶",     30L
            )
        )
    )

    sancai
}

format_tbl_shuli <- function(file = here::here("tools/data/ImportShuli.php")) {
    lines <- data.table::fread(file, sep = NULL, header = FALSE)$V1
    i_start <- which(grepl("^【[0-9]{1,2}[^0-9]{3,}】", lines))

    num <- as.integer(str_extract(lines[i_start], "(?<=【)[0-9]+", perl = TRUE))

    brief <- str_extract(lines[i_start], "(?<=（).+?(?=）)", perl = TRUE)

    desc <- gsub("^.+?）", "", lines[i_start], perl = TRUE)
    desc <- gsub("（[大半凶吉]+）$", "", desc, perl = TRUE)
    desc <- gsub("[，。\\.]$", "", trimws(desc))

    bad_or_good <- substring(lines[i_start + 1L], 5)

    clean_period <- function(x) {
        x <- gsub(".", "。", x, fixed = TRUE)
        x <- gsub("。{2,}", "。", x)
        x <- gsub(",$", "。", x)
        x
    }

    foundation <- clean_period(substring(lines[i_start + 2L], 5L))
    family <- clean_period(substring(lines[i_start + 3L], 5L))
    health <- clean_period(substring(lines[i_start + 4L], 5L))
    full <- clean_period(substring(lines[i_start + 5L], 5L))
    future <- clean_period(substring(lines[i_start + 6L], 5L))
    fortune <- clean_period(gsub("&$", "", substring(lines[i_start + 7L], 5L)))

    num_fortune <- data.table::data.table(
        num = num,
        brief = brief,
        desc = desc,
        jixiong = bad_or_good,
        foundation = foundation,
        family = family,
        health = health,
        future = future,
        fortune = fortune,
        desc_full = full
    )

    data.table::set(num_fortune, NULL, "score",
        with(num_fortune,
            data.table::fcase(
                jixiong == "大吉",     100L,
                jixiong == "吉",       90L,
                jixiong == "半吉",     80L,
                jixiong == "半吉半凶", 60L,
                jixiong == "凶",       40L,
                jixiong == "大凶",     30L
            )
        )
    )

    num_fortune
}

format_tbl_char <- function(file = here::here("tools/data/char_base.json")) {
    lst <- jsonlite::read_json(file)
    data.table::data.table(
        character = vapply(lst, `[[`, "", "char"),
        stroke = vapply(lst, `[[`, 1L, "strokes"),
        pinyin = lapply(lst, \(l) paste0(l[["pinyin"]], collapse = "|")),
        radical = vapply(lst, `[[`, "", "radicals")
    )
}

format_tbl_char_common <- function(file = here::here("tools/data/char_common.json")) {
    lst <- jsonlite::read_json(file)
    data.table::data.table(
        character = vapply(lst, `[[`, "", "char"),
        frequency = vapply(lst, `[[`, 1L, "frequency")
    )
}

format_tbl_kangxi <- function(file = here::here("tools/data/kangxi.csv"),
                              wuxing = here::here("tools/data/kangxi_wuxing.csv")) {
    dt <- data.table::fread(file)
    data.table::set(dt, NULL, "character",
        intToUtf8(strtoi(dt$codepoint, 10L), multiple = TRUE)
    )
    data.table::set(dt, NULL, "radical",
        intToUtf8(strtoi(dt$radical_codepoint, 10L), multiple = TRUE)
    )
    data.table::set(dt, NULL, c("codepoint", "radical_codepoint"), NULL)
    data.table::setnames(dt, c("strokes", "radical_strokes"), c("stroke", "radical_stroke"))
    data.table::setcolorder(dt, c("character", "stroke", "radical", "radical_stroke"))

    wuxing <- data.table::fread(wuxing)
    dt[wuxing, on = c("character" = "hanzi"), wuxing := i.wuxing]
    dt
}

format_tbl_luck_career <- function(file = here::here("tools/data/career.csv")) {
    dt <- data.table::fread(file)
    map <- data.table::data.table(
        num = c("1或2", "3或4", "5或6", "7或8", "9或0"),
        wuxing = c("木", "火", "土", "金", "水")
    )
    dt[map, on = c("人格" = "num"), `人格` := i.wuxing]
    dt[map, on = c("天格" = "num"), `天格` := i.wuxing]
    dt[map, on = c("地格" = "num"), `地格` := i.wuxing]

    success <- dt[, .(
        ren = `人格`, tian = `天格`,
        jixiong = str_extract(`成功运`, "(?<=^（).+(?=）)", perl = TRUE),
        description = str_extract(`成功运`, "(?<=）).+", perl = TRUE)
    )]

    base <- dt[, .(
        ren = `人格`, di = `地格`,
        jixiong = str_extract(`基础运`, "(?<=^（).+(?=）)", perl = TRUE),
        description = str_extract(`基础运`, "(?<=）).+", perl = TRUE)
    )]

    list(base = base, success = success)
}

format_tbl_luck_social <- function(file = here::here("tools/data/social.csv")) {
    dt <- data.table::fread(file)
    data.table::setnames(dt, c("ren", "wai", "jixiong", "description"))
    dt
}

format_tbl_special_sancai <- function(file = here::here("tools/data/special_sancai.csv")) {
    dt <- data.table::fread(file)
    data.table::setnames(dt, c("wuxing", "description"))
    dt
}

format_tbl_luck_health <- function(file = here::here("tools/data/health.json")) {
    df <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)

    desc <- data.table::data.table(
        index = seq.int(nrow(df)),
        description = gsub("$", "。", df$description)
    )
    index <- rep(desc$index, vapply(df$sancai, nrow, 1L))

    sancai <- data.table::rbindlist(df$sancai)
    data.table::setnames(sancai,
        c("天格", "地格", "人格"),
        c("tian", "ren", "di")
    )
    sancai[tian == "不论何数", tian := paste0(1:10, collapse = "或")]
    sancai <- data.table::data.table(
        index = index,
        tian = lapply(strsplit(sancai$tian, "或"), as.integer),
        ren = lapply(strsplit(sancai$ren, "或"), as.integer),
        di = lapply(strsplit(sancai$di, "或"), as.integer)
    )

    lst <- lapply(seq.int(nrow(sancai)),
        \(i) {
            do.call(
                data.table::CJ,
                list(tian = sancai$tian[[i]], ren = sancai$ren[[i]], di = sancai$di[[i]])
            )
        }
    )
    index <- rep(index, vapply(lst, nrow, 1L))
    sancai <- data.table::set(data.table::rbindlist(lst), NULL, "index", index)
    sancai[, c("tian", "ren", "di") := lapply(.SD, `%%`, 10L), .SDcols = -"index"]
    data.table::setcolorder(sancai, "index")

    list(sancai = sancai, desc = desc)
}
