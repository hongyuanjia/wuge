source(here::here("tools/query.R"))
source(here::here("tools/format.R"))

# get all shuli from http://ceshi.hanyunshi.com/xingming/
shuli <- query_all_shuli()

unlink(here::here("tools/meta.json"))

init_file("STCharacters.txt", "data/dictionary", "BYVoid/OpenCC") |>
    format_tbl_conv() |>
    data.table::fwrite(here::here("inst/extdata/stconv.csv"))

init_file("sancai.txt", "data", "JakLiao/GoodGoodName", "sancai.txt") |>
    format_tbl_sancai() |>
    data.table::fwrite(here::here("inst/extdata/sancai.csv"))

init_file("ImportShuli.php", "app/Console/Commands", "whmnoe4j/Calendar") |>
    format_tbl_wuge() |>
    data.table::fwrite(here::here("inst/extdata/shuli.csv"))

init_file("char_base.json", "data/character", "mapull/chinese-dictionary") |>
    format_tbl_char() |>
    data.table::fwrite(here::here("inst/extdata/char.csv"))

init_file("char_common.json", "data/character", "mapull/chinese-dictionary") |>
    format_tbl_char_common() |>
    data.table::fwrite(here::here("inst/extdata/char_common.csv"))

init_file("kangxi.csv", "vendor/data", "fh250250/fortune") |>
    format_tbl_kangxi() |>
    data.table::fwrite(here::here("inst/extdata/kangxi.csv"))

format_tbl_kangxi(
    init_file("kangxi.csv", "vendor/data", "fh250250/fortune"),
    init_file("wuxing.csv", "data", "sinshine/AI-name", "kangxi_wuxing.csv")
    ) |>
    data.table::fwrite(here::here("inst/extdata/kangxi.csv"))

format_tbl_special_sancai() %>%
    data.table::fwrite(here::here("inst/extdata/special_sancai.csv"))

format_tbl_social() %>%
    data.table::fwrite(here::here("inst/extdata/luck_social.csv"))

Map(data.table::fwrite,
    format_tbl_career(),
    here::here("inst/extdata", c("luck_base.csv", "luck_success.csv"))
)

Map(data.table::fwrite,
    format_tbl_health(),
    here::here("inst/extdata", c("luck_health_sancai.csv", "luck_health_desc.csv"))
)
