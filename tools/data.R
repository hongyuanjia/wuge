source(here::here("tools/gen.R"))
gen_dict(
    c(
        # 简体到繁体转换表
        # 数据来源：BYVoid/OpenCC
        "stconv",

        # 三才
        # 数据来源：JakLiao/GoodGoodName
        "sancai",

        # 五格数理
        # 数据来源：whmnoe4j/Calendar
        "shuli",

        # 汉字字典
        # 数据来源：mapull/chinese-dictionary
        "char",

        # 3500 常用汉字
        # 数据来源：mapull/chinese-dictionary
        "char_common",

        # 康熙字典
        # 数据来源：fh250250/fortune 和 sinshine/AI-name
        "kangxi",

        # 特殊三才(如'金金金')的解释
        "special",

        # 运势的解释，包括基础运、社交运、成功运、健康
        "luck"
    )
)
