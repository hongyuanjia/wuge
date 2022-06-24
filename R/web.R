library(shiny)

ui <- shiny::fluidPage(
    tags$style(HTML('
        body {
            margin: 0px;
            font: 13px "Microsoft YaHei","微软雅黑","Microsoft JhengHei","宋体"
        }

        p {
            margin: 18px auto;
        }

        h1, h2, h3, h4, h5, h6 {
            margin: 0;
            padding: 0;
        }

        .wuge {
            width: 90%;
            overflow: hidden;
            padding-bottom: 8px;
            border-left: 1px solid #d9d9d9;
            border-right: 1px solid #d9d9d9;
            border-bottom: 1px solid #d9d9d9;
            max-height: 999999px;
        }

        .wuge .title {
            color: #222;
            font-size: 20px;
            text-align: center;
        }

        .wuge .title h2 {
            font-size: 26px;
            font-family: "微软雅黑", "Microsoft YaHei", "华文细黑";
            margin: 15px auto;
        }

        .wuge .inform {
            width: 90%;
            padding: 10px;
            background: #fff5ec;
            border: 1px solid #fcc;
            font-size: 16px;
            color: #706A6A;
            margin: 6px auto;
            overflow: hidden;
        }

        .wuge .inform form {
            width: 80%;
            margin: auto;
        }

        .wuge .inform .form-inline .form-control {
            display: block;
            margin-left: 10px;
            font-size: 18px;
        }

        .wuge .inform p {
            line-height: 180%;
            padding: 10px 0;
            margin: 0;
            font-size: 16px;
        }

        .wuge .inform .ipt {
            padding: 1px 0 1px 5px;
            height: 26px;
            line-height: 18px;
            border: 1px solid #d0ad9a;
            vertical-align: middle;
            background-color: #fff;
            font-size: 16px;
        }

        .wuge .content {
            font-size: 16px;
            padding: 10px 25px 10px 25px;
            line-height: 32px;
            color: #252525;
        }

        .read-content {
            overflow: hidden;
            width: 100%;
        }

        .wuge .content p {
            margin: 8px 0;
        }

        .read-content .item {
            border-style: solid;
            border-color: #a644a0;
            border-width: 1.5px;
            padding: 10px;
            margin: 10px;
        }

        .read-content .item .subitem {
            border-style: solid;
            border-color: #a41254;
            border-width: 1.5px;
            padding: 10px;
            margin: 10px;
        }

        .wuge .content h3 {
            margin: 14px 0;
            padding-left: 15px;
            line-height: 32px;
            border-left: 5px solid #a644a0;
            font-size: 18px;
            background: #F5F5F5;
        }

        .wuge .content h4 {
            margin: 14px 0;
            padding-left: 15px;
            line-height: 32px;
            border-left: 5px solid #a41254;
            font-size: 16px;
            background: #F5F5F5;
        }

        .wuge .content b {
            color: green
        }

        .sbutton {
            overflow: hidden;
            font-size: 14px;
            margin: 5px;
            padding: 7px 12px;
            font-weight: 700;
            border-radius: 5px;
            border: 0;
            text-align: center;
            text-decoration: none;
            background: #FF8A11;
            color: #fff;
            cursor: pointer;
            font: 14px Verdana,Arial,Tahoma;
            vertical-align: middle;
        }

        .center {
            text-align: center;
        }
    ')),

    # user input
    shiny::withTags(shiny::tagList(
        div(class = "wuge",
            div(class = "title", h2("姓名测试打分")),

            div(class = "inform center",

                form(
                    class = "form-inline",

                    div(
                        class = "form-group",

                        label(`for` = "xing", "姓"),
                        shiny::textInput("xing", label = NULL, value = ""),

                        label(`for` = "ming", "名"),
                        shiny::textInput("ming", label = NULL, value = "")
                    ),

                    shiny::actionButton("submit", label = "测试打分", class = "sbutton")
                ),

                span(style = "font-size: 10px; color: grey; text-align: left",
                    "提示：在计算笔画时需要采用繁体字，然后有些简体字对应多个繁体字型。",
                    "因此在简繁转换时仅采用数据库中第一个匹配结果。"
                )
            ),

            div(class = "content",

                div(class = "read-content",

                    shiny::htmlOutput("first", FALSE, p),

                    p(""),

                    table(
                        border = "0", cellpadding = "2", cellspacing = "0", width = "95%",
                        tbody(
                            tr(
                                shiny::htmlOutput("xingming-sum", FALSE, td, valign = "top"),
                                shiny::htmlOutput("wuge-sum", FALSE, td, style = "padding-left:20px;")
                            )
                        )
                    ),

                    p(""),

                    shiny::htmlOutput("score", FALSE),
                    shiny::htmlOutput("sancai", FALSE),
                    shiny::htmlOutput("shuli", FALSE),
                    shiny::htmlOutput("luck", FALSE)
                )
            )
        )
    ))
)

server <- function(input, output, session) {
    wuge_en_to_cn <- function(en) {
        data.table::fcase(
            en == "tian", "天格",
            en == "ren",  "人格",
            en == "di",   "地格",
            en == "wai",  "外格",
            en == "zong", "总格"
        )
    }
    wuge_desc <- c(
        tian = "[天格数是先祖遗传下来的，其数理对人的影响不大]",
        ren = "[人格数又称主运,是整个姓名的中心点，影响人一生的命运]",
        di = "[地格又称前运，是前半生的命运，会影响中年以前]",
        wai = "[外格又称灵运，主管命运之灵力、社交能力和智慧]",
        zong = "[又称后运，是后半生的命运，影响中年到老年]"
    )

    iv_rule_cn <- function(value) {
        if (anyNA(dict_char()[split_char(value), on = "character"]$stroke)) {
            "请输入中文"
        }
    }

    # input validation
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("xing", iv_rule_cn)
    iv$add_rule("ming", iv_rule_cn)
    iv$enable()

    wuge_data <- shiny::bindEvent(
        shiny::bindCache(
            shiny::reactive({
                shiny::req(input$xing, input$ming)
                shiny::req(iv$is_valid())
                get_wuge(input$xing, input$ming)
            }),
            input$xing, input$ming
        ),
        input$submit
    )

    output$first <- shiny::renderUI({
        shiny::tags$p(
            sprintf("%s，您好！以下是为您进行的姓名测试打分解析：",
                str_join(
                    str_join(wuge_data()$xing$character),
                    str_join(wuge_data()$ming$character)
                )
            )
        )
    })

    output$`xingming-sum` <- shiny::renderUI({
        style_xingming <- "font-size: 24px; font-weight: bold; color: #ff679a"
        shiny::withTags(shiny::tagList(
            div(class = "item",
                table(
                    tbody(
                        tr(
                            td("姓名："),
                            lapply(
                                c(wuge_data()$xing$character, wuge_data()$ming$character),
                                td,
                                style = style_xingming,
                                align = "center"
                            )
                        ),
                        tr(
                            td("繁体："),
                            lapply(
                                c(wuge_data()$xing$traditional, wuge_data()$ming$traditional),
                                td,
                                align = "center"
                            )
                        ),
                        tr(
                            td("拼音："),
                            lapply(
                                c(wuge_data()$xing$pinyin, wuge_data()$ming$pinyin),
                                td,
                                align = "center"
                            )
                        ),
                        tr(
                            td("笔画："),
                            lapply(
                                c(wuge_data()$xing$stroke_wuge, wuge_data()$ming$stroke_wuge),
                                td,
                                align = "center"
                            )
                        ),
                        tr(
                            td("五行："),
                            lapply(
                                get_stroke_wuxing(c(wuge_data()$xing$stroke_wuge, wuge_data()$ming$stroke_wuge)),
                                td,
                                align = "center"
                            )
                        )
                    )
                )
            )
        ))
    })

    output$`wuge-sum` <- shiny::renderUI({
        shuli <- wuge_data()$shuli
        sum <- sprintf(
            "%s -> %i（%s）",
            wuge_en_to_cn(shuli$wuge), shuli$num, shuli$wuxing
        )

        shiny::tags$div(class = "item",
            shiny::tagList(
                sum[1], shiny::tags$br(),
                sum[2], shiny::tags$br(),
                sum[3], shiny::tags$br(),
                sum[4], shiny::tags$br(),
                sum[5]
            )
        )
    })

    output$score <- shiny::renderUI({
        style_score <- "font-size: 24px; font-weight: bold; color: #ff679a"
        shiny::withTags(
            div(class = "item",
                h3("姓名打分："),
                div(
                    p(
                        "姓名三才评分：",
                        span(style = style_score, wuge_data()$score$score_sancai)
                    ),
                    p(
                        "姓名五格评分：",
                        span(style = style_score, wuge_data()$score$score_wuge)
                    ),
                    p(
                        "姓名综合评分：",
                        span(style = style_score, wuge_data()$score$score_total)
                    )
                )
            )
        )
    })

    output$sancai <- shiny::renderUI({
        shiny::withTags(
            div(class = "item",
                h3("姓名三才数理 解说："),
                p("三才配置：", b(wuge_data()$sancai$wuxing)),
                p("三才吉凶：", b(wuge_data()$sancai$jixiong)),
                p("三才解析：", wuge_data()$sancai$description)
            )
        )
    })

    output$shuli <- shiny::renderUI({
        shiny::withTags(
            div(class = "item",
                h3("姓名五格数理 解说："),

                wuge_data()$shuli[, by = "wuge", {
                    wuge_cn <- wuge_en_to_cn(wuge)

                    list(shiny::withTags(shiny::tagList(
                        div(class = "subitem",
                            h4(
                                sprintf("%s评分：", wuge_cn),
                                span(style = "color:red", sprintf("%s分", num)),
                                "，五行属",
                                span(style = "color:red", sprintf("%s，%s", wuxing, jixiong))
                            ),
                            span(wuge_desc[names(wuge_desc) == wuge],
                                style = "color: #6c6c6c; font-size: 10px;"
                            ),
                            p(b("概述："), brief),
                            p(b("寓意："), .SD$desc),
                            p(b("基业："), foundation),
                            p(b("家庭："), family),
                            p(b("健康："), health),
                            p(b("含义："), desc_full),
                            p(b("前途："), future),
                            p(b("财运："), fortune)
                        )
                    )))
                }]$V1
            )
        )
    })

    output$luck <- shiny::renderUI({
        shiny::withTags(
            div(class = "item",
                h3("姓名个性命运 解说："),

                div(class = "subitem",
                    h4(
                        "基础运 ",
                        span(style = "color:red", wuge_data()$luck$base$jixiong),
                    ),
                    p(
                        b(sprintf(
                            "人格与地格【%s%s】搭配",
                            wuge_data()$luck$base$ren,
                            wuge_data()$luck$base$di
                        )),
                        "：",
                        wuge_data()$luck$base$desc
                    )
                ),
                div(class = "subitem",
                    h4(
                        "成功运 ",
                        span(style = "color:red", wuge_data()$luck$success$jixiong),
                    ),
                    p(
                        b(sprintf(
                            "人格与天格【%s%s】搭配",
                            wuge_data()$luck$success$ren,
                            wuge_data()$luck$success$tian
                        )),
                        "：",
                        wuge_data()$luck$success$desc
                    )
                ),
                div(class = "subitem",
                    h4(
                        "社交运 ",
                        span(style = "color:red", wuge_data()$luck$social$jixiong),
                    ),
                    p(
                        b(sprintf(
                            "人格与外格【%i%i】搭配",
                            wuge_data()$luck$social$ren,
                            wuge_data()$luck$social$wai
                        )),
                        "：",
                        wuge_data()$luck$social$desc
                    )
                ),
                if (!is.na(wuge_data()$luck$health$description)) {
                    div(class = "subitem",
                        h4(
                            "健康运 ",
                            span(style = "color:red", "凶"),
                        ),
                        p(
                            b(sprintf(
                                    "人格、天格及地格【%i%i%i】搭配",
                                    wuge_data()$luck$health$ren,
                                    wuge_data()$luck$health$tian,
                                    wuge_data()$luck$health$di
                            )),
                            "：",
                            wuge_data()$luck$health$desc
                        )
                    )
                }
            )
        )
    })

    session$onSessionEnded(shiny::stopApp)
}

shinyApp(ui = ui, server = server)
