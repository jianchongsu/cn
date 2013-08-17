---
date: '2013-02-01 10:45:40'
layout: post
title: R函数介绍：merge()
categories:
- R
tags:
- R
- 记录
- 函数
- 数据处理

---

R函数介绍：merge()
========================================================

### 函数的用途
如果两个data frame 有相同的列或者行，你希望将这两个data frame通过他们相同的行或者列组合成一个data frame，新形成的这个data frame有前两个data frame的所有列的信息，只是两个相同的列融合为一个。
> 融合，合并也是merge单词的本义。
<!-- more -->

### 函数的参数

{% highlight r %}
merge(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all = FALSE, 
    all.x = all, all.y = all, sort = TRUE, ...)
{% endhighlight %}

常用的就是这些参数:
* x、y：就是两个需要融合合并的数据框；
* by：x和y相融合所需要的相同的列，列名一致；
* by.x,by.y：可能x和y中有1列内容是一样的，但是这两个列的列名不一致的时候，x和y融合以x中的by.x列和y中的by.y来融合；
* all:默认值为FALSE，即只显示x和y中融和列都有的部分，如果此列中x中有一行y中没有，则都不显示，同理，y中有一行x中没有，也不显示。将all=TRUE,将显示x和y的融合的所有行，缺失的用NA显示；
* all.x,all.y:all.x设为TRUE的时候，显示融合列中x的所有行，同理，all.y;
* sort:默认为TRUE，就是新形成的数据框中第一列的排序按照字母顺序或者数值顺序排序，可能与原数据框的顺序不一致，如果将all改成FALSE，即以x中列顺序为新数据框的列顺序；

### 实例分析
1、x与y融和列有相同的列名k1

{% highlight r %}
x <- data.frame(k1 = c("A", "B", "E", "D", "C"), names = c("Tukey", "Venables", 
    "Tierney", "Ripley", "Ripley"))
{% endhighlight %}
{% highlight r %}
y <- data.frame(k1 = c("A", "G", "C", "D", "B"), nationality = c("US", "Australia", 
    "US", "UK", "Australia"))
merge(x, y, by = "k1")
{% endhighlight %}

```
##   k1    names nationality
## 1  A    Tukey          US
## 2  B Venables   Australia
## 3  C   Ripley          US
## 4  D   Ripley          UK
```

可以看到，将两个数据框融合为一个。由于all默认为FALSE，所以只显示x中k1和y中k2都有的部分A，B，C，D行，而且sort默认为TRUE，所以新生成的数据框的第一列按照字母顺序排列；
而如果将sort改变：

{% highlight r %}
merge(x, y, by = "k1", sort = FALSE)
{% endhighlight %}

```
##   k1    names nationality
## 1  A    Tukey          US
## 2  B Venables   Australia
## 3  D   Ripley          UK
## 4  C   Ripley          US
```

可以看到是按照x中k1列的顺序排列。
接下来再看如果融和列的列名不一致：

{% highlight r %}
x <- data.frame(k1 = c("A", "B", "E", "D", "C"), names = c("Tukey", "Venables", 
    "Tierney", "Ripley", "Ripley"))

y <- data.frame(l1 = c("A", "G", "C", "D", "B"), nationality = c("US", "Australia", 
    "US", "UK", "Australia"))
merge(x, y, by = "k1")
{% endhighlight %}

```
## Error: 'by' must specify uniquely valid column(s)
```

{% highlight r %}
merge(x, y, by.x = "k1", by.y = "l1")
{% endhighlight %}

```
##   k1    names nationality
## 1  A    Tukey          US
## 2  B Venables   Australia
## 3  C   Ripley          US
## 4  D   Ripley          UK
```


最后看下all的变化：

{% highlight r %}
x <- data.frame(k1 = c("A", "B", "E", "D", "C"), names = c("Tukey", "Venables", 
    "Tierney", "Ripley", "Ripley"))

y <- data.frame(k1 = c("A", "G", "C", "D", "B"), nationality = c("US", "Australia", 
    "US", "UK", "Australia"))
merge(x, y, by = "k1")
{% endhighlight %}

```
##   k1    names nationality
## 1  A    Tukey          US
## 2  B Venables   Australia
## 3  C   Ripley          US
## 4  D   Ripley          UK
```

{% highlight r %}
merge(x, y, by = "k1", all = T)
{% endhighlight %}

```
##   k1    names nationality
## 1  A    Tukey          US
## 2  B Venables   Australia
## 3  C   Ripley          US
## 4  D   Ripley          UK
## 5  E  Tierney        <NA>
## 6  G     <NA>   Australia
```

{% highlight r %}
merge(x, y, by = "k1", all.x = T)
{% endhighlight %}

```
##   k1    names nationality
## 1  A    Tukey          US
## 2  B Venables   Australia
## 3  C   Ripley          US
## 4  D   Ripley          UK
## 5  E  Tierney        <NA>
```

从上面的代码和结果，相信大家可以清楚了all的功能。

### 后记
之后在**plyr包**的join()函数也可以实现merge()的功能，而且在处理速度上还更有优势，这里也介绍下join()

### 函数参数

{% highlight r %}
join(x, y, by = intersect(names(x), names(y)), type = "left", match = "all")
{% endhighlight %}

* x,y：为需要融合的两个数据框
* by：x和y相融合所需要的相同的列，列名一致；
* type:
  * inner: only rows with matching keys in both x and y
  * left: all rows in x, adding matching columns from y
  * right: all rows in y, adding matching columns from x
  * full: all rows in x with matching columns in y, then the rows of y that don’t match x.

* match:

### 实例分析


{% highlight r %}
library(plyr)
first <- ddply(baseball, "id", summarise, first = min(year))
system.time(b2 <- merge(baseball, first, by = "id", all.x = TRUE))
{% endhighlight %}

```
##    user  system elapsed 
##    0.67    0.00    0.71
```

```r
system.time(b3 <- join(baseball, first, by = "id"))
```

```
##    user  system elapsed 
##    0.14    0.00    0.14
```

{% highlight r %}
b2 <- arrange(b2, id, year, stint)
b3 <- arrange(b3, id, year, stint)
stopifnot(all.equal(b2, b3))
{% endhighlight %}

