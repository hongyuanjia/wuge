wuge
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# wuge 五格

<!-- badges: start -->
<!-- badges: end -->

给宝宝取名字可能是最让父母头大的一件事情。家中老一辈有的讲究根据
[“五格剖象法”](https://baike.baidu.com/item/五格剖象法)算出名字中每个字的笔画，
再根据宝宝生辰八字在五行强弱在名字上做文章弥补。对于大多数接受过现代化科学教育的
朋友们来说，一般对这种封建迷信是持怀疑态度的。但正所谓“宁可信其有，不可信其无”。
我家中长辈有比较信这个的，所以就在网上搜集各种资料。结果发现目前市面上提供这种功
能的网站都是收费的，写出了这个 {wuge} 包。

## Installation 安装

这个包还没有上传到 CRAN 上，目前可以使用如下方式安装：

``` r
remotes::install_github("hongyuanjia/wuge")
```

## Example 示例

### 分析名字

可使用 `wuge::wuge()`
对输入的名字按照五格剖象法进行分析。对于输入的汉字，默认会
将其转换成繁体字再计算。由于有时候简体转繁体的结果不一定准确，提供了`to_trad`选
项，并默认打开。将其关闭即可不进行简繁转换。这个时候你可以直接输入对应的繁体字。

`wuge::wuge()` 会生成一个 `WuGe` 的对象，其实就是一个
`list`，里面包含了诸如姓名
简单、繁体、拼音、五格数理、五格三才、运势等。默认会把所有信息分门别类打印到终端上。

``` r
wuge::wuge("张", "三")
```

    #> ══ 姓名 ════════════════════════════════════════════════════════════════ 张三 ══
    #> • 姓名：   张  三
    #> • 繁体：   張  三
    #> • 拼音：zhānɡ sān
    #> • 笔画：   11   3
    #> 
    #> ── 五格数理排盘 ──────────────────────────────────────────────────────── 张三 ──
    #>  【46.5 分】
    #> • 天格：12 -> 木 ->   凶
    #> • 人格：14 -> 火 ->   凶
    #> • 地格： 4 -> 火 ->   凶
    #> • 外格： 2 -> 木 -> 大凶
    #> • 总格：14 -> 火 ->   凶
    #> 
    #> ── 天格 12 的分析 ────────────────────────────────────────────────────── 张三 ──
    #>  【55 分】
    #> • 关键词：挖井无泉(掘井无泉)
    #> • 简述：无理之数，发展薄弱，虽生不足，难酬志向。
    #> • 数理暗示：
    #>   • 意志脆弱，家庭寂寞数
    #> ......

由于输出结果中的文字太多，有时候分享起来不方便。这时可以用
`wuge::export()` 将结 果输出成一张图片。

``` r
wuge::wuge("张", "三") |>
    wuge::export("张三.png")
```

<img src="https://github.com/hongyuanjia/wuge/blob/main/man/figures/wuge.png?raw=true" width="60%" />

### 获得名字笔画组

可使用 `wuge::name()`
计算出来满足要求的笔画组合及对应汉字。可输入的条件有：

- `num`: 名字的汉字个数
- `min`: 名字中汉字最少笔画数
- `max`: 名字中汉字最多笔画数
- `fixed`: 名字中某一个字的笔画数，如 `c(0, 4)`
  代表名字中第一个字不固定笔画，第 二个字必要是 4 画
- `shuli`: 五格数理的最低要求
- `sancai`: 五格三才的最低要求
- `common`: 是否只使用 3500 个常用汉字，默认开启

`wuge::name()` 会返回一个 `WuGeName` 的对象，也是一个 `list`，其结构与
`WuGe` 相似。 只不过里面包含了所有满足要求的汉字。

``` r
wuge::name("张", max = 10, shuli = "吉", sancai = "吉")
```

    #> ══ 姓名 ══════════════════════════════════════════════════════ 张【28】【24】 ══
    #> • 姓名：   张  *  *
    #> • 繁体：   張  *  *
    #> • 拼音：zhānɡ  *  *
    #> • 笔画：   11 28 24
    #> 
    #> ── 五格数理排盘 ────────────────────────────────────────────── 张【28】【24】 ──
    #>  【89.7 分】
    #> • 天格：12 -> 木 ->   凶
    #> • 人格：39 -> 水 ->   吉
    #> • 地格：52 -> 木 ->   吉
    #> • 外格：25 -> 土 ->   吉
    #> • 总格：63 -> 火 -> 大吉
    #> 
    #> ── 天格 12 的分析 ──────────────────────────────────────────── 张【28】【24】 ──
    #>  【55 分】
    #> • 关键词：挖井无泉(掘井无泉)
    #> • 简述：无理之数，发展薄弱，虽生不足，难酬志向。
    #> • 数理暗示：
    #>   • 意志脆弱，家庭寂寞数
    #> ......

与 `wuge::wuge()` 类似，也可以用 `wuge::export()` 将结果导出成一个
Excel，里面把
所有笔画组合及对应的解释都放到不同的工作表里了，方便和家人一起根据来选字。

``` r
wuge::name("张", max = 10, shuli = "吉", sancai = "吉") |>
    wuge::export("张.xlsx")
```

<img src="https://github.com/hongyuanjia/wuge/blob/main/man/figures/name.png?raw=true" width="60%" />

### 其他功能

除上述功能外，{wuge} 里还包含了一些其他小功能，比如：

- `sim2trad()`: 根据 [OpenCC](https://github.com/BYVoid/OpenCC)
  项目中的数据实现 了简单的简繁转换
- `pinyin()`: 获取输入汉字的所有可能拼音
- `stroke()`:
  获取输入汉字的笔画，注：这里的笔画是指汉语辞典中的笔画，并非进行五
  格计算时采用的笔画

## License 许可协议

本项目代码采用 GPLv3 协议 [![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

使用到了其他项目的数据：

- 简繁转换数据: [OpenCC](https://github.com/BYVoid/OpenCC)
  [![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

- 五格三才数据: [GoodGoodName](https://github.com/JakLiao/GoodGoodName)
  [![License:
  MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

- 五格数理数据:
  [whmnoe4j/Calendar](https://github.com/whmnoe4j/Calendar) [![License:
  MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

- 汉语拼音辞典:
  [mapull/chinese-dictionary](https://github.com/mapull/chinese-dictionary)
  [![License:
  MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

- 康熙辞典: [fh250250/fortune](https://github.com/fh250250/fortune)
  许可不详

- 汉字五行: [sinshine/AI-name](https://github.com/sinshine/AI-name)
  许可不详
