---
date: '2013-01-01 10:45:40'
layout: post
title: 你好，完备年，我等了24年了
categories:
- R
tags:
- R
- 记录
- 生活
- 学习

---
标题纯粹是标题党，写这篇日志的起子是如下这条状态（不知道是否是原创）：

> 刘万明 : 你好，2013！哥等了24年，你终于4个数字不同了

相信大家都明白这句话的意思（应该没有人从所在年份是3位数字的活过来吧）。
<!-- more -->

是的，我也等了24年了，不怕暴露年龄的说我确实是1988年出生的，很不好意思的说，1988年出生的应该是等这样的一年最长的80后。

好了，为了便于描述，先下个定义（私自下的，请轻拍）：

> 完备年：是指所在年份的四位数字是四个不同的阿拉伯数字的年份。

举例：2013年是完备年，因为2,0,1,3是四个不同的数字，而2012年就不是完备年，因为有两个2,只有三个不同的数字，所以不是完备年。

显然，该定义可以推广到5位数字或者更多，但是我相信看到这篇日志的应该活不到5位数字的时候，要是你活到了，麻烦你到我坟头烧次纸通知我一下。

好，我上面说了，1988年出生的应该是等这样的一年最长的80后。那么，我还可以这么说，如果从1900年算起，从出生需要等24年及以上才能遇到第一次完备年的同学都是在1988年出生的，换句话说，100多年以来（其实我可以很轻松的说，是200年以来甚至更多），连续都不是完备年的最长阶段就是从1988年-2012年这段时间！而且前看200年，后看200年，1988-2012依旧霸气的最长！！

 

废话少说，我们知道闰年很规律的出现，但是这里的完备年呢？

为了方便，我用R语言编了一个函数，你只需要输入起始年以及终止年，你就可以看到这期间的所有的完备年的年份（代码有需要优化的地方，请随时交流）。

```r
library(ggplot2)
co.year = function(from, to) {
    gap <- seq(from, to, by = 1)
    
    year.split <- strsplit(as.character(gap), "")
    year.matrix <- matrix(as.numeric(unlist(year.split)), ncol = 4, byrow = T)
    index <- apply(year.matrix, 1, function(x) length(unique(x)))
    the.year <- gap[index == 4]
    
    year.length <- length(the.year)
    return(list(the.years = the.year, how.many.years.you.will.meet = year.length))
}
```

 

比如：我是1988年出生的，现在是2013年，那么我活过的这段时间内有哪些年是完备年呢，它出现了几次呢？

直接输入：

```r
co.year(1988, 2088)
```

```
## $the.years
##  [1] 2013 2014 2015 2016 2017 2018 2019 2031 2034 2035 2036 2037 2038 2039
## [15] 2041 2043 2045 2046 2047 2048 2049 2051 2053 2054 2056 2057 2058 2059
## [29] 2061 2063 2064 2065 2067 2068 2069 2071 2073 2074 2075 2076 2078 2079
## [43] 2081 2083 2084 2085 2086 2087
## 
## $how.many.years.you.will.meet
## [1] 48
```


 

它表示这段时间（1988-2013），是完备年的只有2013年，这段时间只有一次完备年。好，再来看下1980年出生的最大80后的情况：

```r
co.year(1980, 2013)
```

```
## $the.years
## [1] 1980 1982 1983 1984 1985 1986 1987 2013
## 
## $how.many.years.you.will.meet
## [1] 8
```



 

很遗憾的是，88年前出生的80后肯定在1988年之前就遇到了完备年，而1988年及以后出生的娃儿都直到2013年才看到了完备年，遇到这种情况，不得不哆嗦的说一句：**“2013，你好！”** 

那么做到这里，我就不得不去看下，如果我有幸能活到八十岁，那会遇到多少次完备年？下一次我还用等24年或者更长时间才能遇到一次完备年吗？

 

很遗憾的说，即使我侥幸活100岁，也不会再需要再等24年及以上的时间碰到一次完备年了。

所以，如果2013年对于今天的我（1月1日）有何意义的话，那么我人生中这样的一个指标在2013年用完了。伤悲……

 

如果我能活到2068年（80岁）：

```r
co.year(1988, 2068)
```

```
## $the.years
##  [1] 2013 2014 2015 2016 2017 2018 2019 2031 2034 2035 2036 2037 2038 2039
## [15] 2041 2043 2045 2046 2047 2048 2049 2051 2053 2054 2056 2057 2058 2059
## [29] 2061 2063 2064 2065 2067 2068
## 
## $how.many.years.you.will.meet
## [1] 34
```


 

我会遇到34次完备年，很明显，都是在2013年之后。如果用图形表示就是这样：

```r
from <- 1988
to <- 2068
gap <- seq(from, to, by = 1)

year.split <- strsplit(as.character(gap), "")
year.matrix <- matrix(as.numeric(unlist(year.split)), ncol = 4, byrow = T)
index <- apply(year.matrix, 1, function(x) length(unique(x)))
the.year <- gap[index == 4]

plot.year <- data.frame(year = seq(from, to, by = 1), value = 0)
plot.year$value[index == 4] = 1

p <- ggplot(plot.year, aes(year, value))
p + geom_line(color = "red4", lwd = 2) + scale_x_continuous(breaks = seq(from, 
    to, by = 2), labels = seq(from, to, by = 2)) + scale_y_continuous(breaks = c(0, 
    1), labels = c("NO", "YES")) + theme(axis.text.x = element_text(colour = "black", 
    size = 7)) + labs(x = "年份", y = "是否是完备年", title = "从1988-2068的完备年")
```

![plot of chunk 802 ](figure/802 .png) 




 

很明显可以看到，真的不会有再等24年的机会了！！下次需要等的最长的就是2020-2030期间了了，才十年！！ 

 

那如果到2088年（100岁！）：

```r
co.year(1988, 2088)
```

```
## $the.years
##  [1] 2013 2014 2015 2016 2017 2018 2019 2031 2034 2035 2036 2037 2038 2039
## [15] 2041 2043 2045 2046 2047 2048 2049 2051 2053 2054 2056 2057 2058 2059
## [29] 2061 2063 2064 2065 2067 2068 2069 2071 2073 2074 2075 2076 2078 2079
## [43] 2081 2083 2084 2085 2086 2087
## 
## $how.many.years.you.will.meet
## [1] 48
```



我会遇到48次完备年（接近一半了啊！），图形表示：



```r
from <- 1988
to <- 2088
gap <- seq(from, to, by = 1)

year.split <- strsplit(as.character(gap), "")
year.matrix <- matrix(as.numeric(unlist(year.split)), ncol = 4, byrow = T)
index <- apply(year.matrix, 1, function(x) length(unique(x)))
the.year <- gap[index == 4]

plot.year <- data.frame(year = seq(from, to, by = 1), value = 0)
plot.year$value[index == 4] = 1

p <- ggplot(plot.year, aes(year, value))
p + geom_line(color = "red4", lwd = 2) + scale_x_continuous(breaks = seq(from, 
    to, by = 2), labels = seq(from, to, by = 2)) + scale_y_continuous(breaks = c(0, 
    1), labels = c("NO", "YES")) + theme(axis.text.x = element_text(colour = "black", 
    size = 7)) + labs(x = "年份", y = "是否是完备年", title = "从1988-2088的完备年")
```

![plot of chunk 1002 ](figure/1002 .png) 


 

 

 所以，站在2013年的第一天，我不得不再对2013年说一次：“你好，2013”，以及“完备年，你好，我等了你24年了，还好，这是我此生等你最长的一次了。”

