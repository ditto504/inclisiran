inclisiran patient filter
================
LI, Yuxi, MD
2021/3/18

## 准备

在开始患者筛选之前，我们需要载入一些基础的R语言的包，用来进行数据清洗和处理。其中，tidyverse是一个系列包，里面包含了所有数据处理、数据表连接筛选的功能。

``` r
library(tidyverse)
#set date time style
options(scipen = 50)
```

## 患者队列筛选

### 基本筛选思路

客观的讲，ORION 18应该并不是一个入选很困难的研究。因为根据ORION
18研究的入选和排除标准，只要符合ASCVD，且LDL-C≥1.8mmol/L，且服用稳定的（≥30天）可耐受最大剂量他汀（无论是否包括依折麦布）就可以入选。或是高危ASCVD，LDL-C≥2.6mmol/L。因此，很自然的，我们想到有两大类患者可以利用EHR数据精准筛选患者。

1.  已经住院，确诊了ASCVD的患者，且在门诊随访中记录了LDL-C；

这一类患者的优势，在于临床情况明确，而且通常经过了住院治疗甚至手术的患者，后续依从性也会更好。但需要注意，根据排除标准，必须筛选那些至少在**3个月前**住院的患者。

2.  纯门诊，通过诊断与检验系统进行筛选、甚至开展实时的监测；

如果有条件，目前HIT的技术是可以实现，利用电子信息系统实时监测，并通过弹窗等方式提醒适合入选的患者的。但门诊的问题在于，很可能患者并不完全符合临床诊断的要求，而且**门诊的医嘱剂量，往往并不是患者真实服药的剂量。**

### 住院患者的筛选

#### 数据准备

首先，我们从`\data`里读入患者住院信息表；

``` r
in_patient <- read_csv('data/in_patient.csv')
```

``` r
head(in_patient)
```

<div class="kable-table">

| X1 | pid      | pflow    | in\_datetime        | out\_datetime       | dept   |
| -: | :------- | :------- | :------------------ | :------------------ | :----- |
|  1 | 0333a2e9 | 3df91585 | 3303-02-14 10:36:00 | 3303-02-21 08:40:50 | 心内科二病房 |
|  2 | 9e3b9f0e | 0d989e50 | 3124-01-25 20:55:49 | 3124-01-30 11:07:02 | 心内科一病房 |
|  3 | c35f2fda | 06da340d | 3698-04-10 16:33:12 | 3698-04-18 14:27:51 | 神经外科病房 |
|  4 | c35f2fda | b69aecbd | 3704-02-27 10:49:00 | 3704-03-02 08:19:11 | 心内科二病房 |
|  5 | c35f2fda | 3579fa0d | 3703-10-20 14:29:42 | 3703-10-30 08:41:02 | 心内科二病房 |
|  6 | 46cfd386 | 15d5f41d | 3720-04-23 14:48:00 | 3720-04-27 08:30:16 | 心内科一病房 |

</div>

然后，我们读取患者唯一识别码EMPI。以便于后续匹配其门诊检验数据信息。

``` r
empi<-read_csv("data/empi.csv")
```

``` r
head(empi)
```

<div class="kable-table">

| X1 | empi     | pid      | name | sex | birthday   |
| -: | :------- | :------- | :--- | :-- | :--------- |
|  1 | e22d2bb8 | 0333a2e9 | 张三   | 男   | 3221-09-25 |
|  2 | 424756a9 | 9e3b9f0e | 李四   | 男   | 3068-04-02 |
|  3 | e28aaed7 | c35f2fda | 王五   | 男   | 3644-04-21 |
|  4 | 2fe13316 | 46cfd386 | 赵六   | 男   | 3674-08-30 |
|  5 | 32bd69ef | 4bc06562 | 周七   | 男   | 3151-03-01 |
|  6 | bdf98ea4 | 8f0b5493 | 马八   | 男   | 3883-07-28 |

</div>

``` r
tmp<-
empi%>%
  select(pid,empi)
```

#### 末次住院

接下来，我们要选取每一位患者的**末次住院信息**，作为基线信息。之所以这样选择，是可以更特意的筛选出那些已经服用最大剂量他汀的患者。

这里面，我们利用了`arrange`（排序）以及`distinct`（唯一）的数据处理算法，并得到我们希望的患者列表。

并且，我们筛选了3个月前住院的患者。（由于时间隐私化的处理，具体的代码就不在这里展示了）

``` r
final<-
in_patient%>%
  left_join(tmp,by=c("pid"))%>%
  filter(!is.na(empi))%>%
  arrange(pid,desc(in_datetime))%>%
  distinct(empi,.keep_all = TRUE)%>%
  select(-X1)

final
```

<div class="kable-table">

| pid      | pflow    | in\_datetime        | out\_datetime       | dept   | empi     |
| :------- | :------- | :------------------ | :------------------ | :----- | :------- |
| 0333a2e9 | 3df91585 | 3303-02-14 10:36:00 | 3303-02-21 08:40:50 | 心内科二病房 | e22d2bb8 |
| 46cfd386 | 15d5f41d | 3720-04-23 14:48:00 | 3720-04-27 08:30:16 | 心内科一病房 | 2fe13316 |
| 4bc06562 | e472205a | 3219-06-29 11:16:00 | 3219-07-05 09:04:43 | 心内科二病房 | 32bd69ef |
| 8f0b5493 | 43c6069a | 3936-07-15 10:38:00 | 3936-07-18 08:51:28 | 心内科一病房 | bdf98ea4 |
| 9e3b9f0e | 0d989e50 | 3124-01-25 20:55:49 | 3124-01-30 11:07:02 | 心内科一病房 | 424756a9 |
| c35f2fda | b69aecbd | 3704-02-27 10:49:00 | 3704-03-02 08:19:11 | 心内科二病房 | e28aaed7 |

</div>

#### 末次化验

接下来，我们使用`EMPI`来匹配获得每位患者在全院系统（包括住院与门诊）的**末次**LDL-C化验数据。

``` r
lis_sample_in <- read_csv("data/lis_sample.csv")
```

``` r
lis_sample_in <- select(lis_sample_in, -X1)
```

``` r
head(lis_sample_in)
```

<div class="kable-table">

| sid              | pflow    | sampling\_time      | empi     |
| :--------------- | :------- | :------------------ | :------- |
| 32190629G0020251 | e472205a | 3219-06-29 11:52:59 | 32bd69ef |
| 32190629G0017007 | e472205a | 3219-06-29 11:52:50 | 32bd69ef |
| 32190629G0020616 | e472205a | 3219-06-29 11:52:46 | 32bd69ef |
| 32190701G0013903 | e472205a | 3219-07-01 09:12:02 | 32bd69ef |
| 32190701G0351138 | e472205a | 3219-06-29 11:52:56 | 32bd69ef |
| 32190701G0350285 | e472205a | 3219-06-29 11:52:42 | 32bd69ef |

</div>

从上面可以看到，虽然我们只有9例患者，但是包含了208份样本的化验数据。

接下来，我们读取具体的检验条目结果数据。

``` r
lis_result_in <- read_csv("data/lis_result.csv")
```

这里我们用表格形式展示一下化验结果的数据格式。

``` r
head(lis_result_in)
```

<div class="kable-table">

| X1 | sid              | ENGLISH\_NAME | CHINESE\_NAME | QUANTITATIVE\_RESULT | QUALITATIVE\_RESULT | TEST\_ITEM\_REFERENCE | TEST\_ITEM\_UNIT |
| -: | :--------------- | :------------ | :------------ | :------------------- | :------------------ | :-------------------- | :--------------- |
|  1 | 32200710G0014038 | eGFR          | 估算肾小球滤过率      | 70.982               | z                   | NA                    | ml/min/1.73㎡     |
|  2 | 32200710G0014038 | A/G           | 白球比值          | 1.49                 | z                   | 1.2-2.4               | NA               |
|  3 | 32200710G0014038 | AG            | 阴离子间隙         | 14.70                | z                   | NA                    | mmol/L           |
|  4 | 32200710G0014038 | ALB\*         | 白蛋白           | 46.8                 | z                   | 40-55                 | g/L              |
|  5 | 32200710G0014038 | ALP\*         | 碱性磷酸酶         | 82                   | z                   | 45-125                | IU/L             |
|  6 | 32200710G0014038 | ALT\*         | 谷丙转氨酶         | 8                    | l                   | 9-50                  | IU/L             |

</div>

##### LDL-C的标准化

考虑到不同医疗机构、不同的LIS（检验信息管理系统）对于LDL-C不一定使用统一的编码和术语表述，所以在使用EHR系统开展类似的研究工作时，一个很大的工作量是进行术语的标准化处理。比如，在这个例子中，我们可以使用关键字`低密度`来找出所有我们希望找到的检验条目。

``` r
lis_result_in%>%
  #filter(str_detect(tolower(ENGLISH_NAME),"ldl"))%>%
  filter(str_detect(CHINESE_NAME,"低密度"))%>%
  group_by(ENGLISH_NAME,CHINESE_NAME)%>%
  summarise(n=n())
```

<div class="kable-table">

| ENGLISH\_NAME | CHINESE\_NAME |  n |
| :------------ | :------------ | -: |
| LDL-C         | 低密度脂蛋白胆固醇     | 35 |

</div>

由于这里只展示了样例数据，所以输出的结果只有1条。但实际上，利用`低密度`作为关键词，还可能会搜出`极低密度胆固醇`等项目，我们需要配合临床知识将其进一步精确匹配与筛选。

``` r
tmp<-
lis_sample_in%>%
  filter(!is.na(empi))%>%
  semi_join(final,by=c("empi"))
```

之后，我们查看一下所有的LDL-C数据。

``` r
tmp<-
lis_result_in%>%
  filter(ENGLISH_NAME %in% c("LDL-C"))%>%
  left_join(tmp, by="sid")%>%
  select(empi,ENGLISH_NAME,QUANTITATIVE_RESULT,sampling_time)%>%
  filter(!is.na(empi))

tmp
```

<div class="kable-table">

| empi     | ENGLISH\_NAME | QUANTITATIVE\_RESULT | sampling\_time      |
| :------- | :------------ | :------------------- | :------------------ |
| 32bd69ef | LDL-C         | 1.49                 | 3220-07-10 07:51:27 |
| e28aaed7 | LDL-C         | 1.77                 | 3704-11-07 08:57:55 |
| e28aaed7 | LDL-C         | 1.15                 | 3704-12-28 09:09:18 |
| 2fe13316 | LDL-C         | 1.86                 | 3721-08-18 07:52:46 |
| bdf98ea4 | LDL-C         | 2.14                 | 3938-02-27 09:36:16 |
| e28aaed7 | LDL-C         | 1.68                 | 3703-12-24 10:30:38 |
| e28aaed7 | LDL-C         | 2.26                 | 3704-05-09 07:54:29 |
| e22d2bb8 | LDL-C         | 1.88                 | 3303-09-19 07:35:23 |
| 2fe13316 | LDL-C         | 4.36                 | 3720-04-09 06:22:17 |
| 2fe13316 | LDL-C         | 1.84                 | 3722-01-26 08:09:55 |
| 424756a9 | LDL-C         | 1.91                 | 3126-01-04 07:37:02 |
| e22d2bb8 | LDL-C         | 3.08                 | 3305-02-27 07:41:26 |
| 2fe13316 | LDL-C         | 2.74                 | 3720-04-16 06:19:58 |
| bdf98ea4 | LDL-C         | 3.51                 | 3936-07-15 13:49:06 |
| bdf98ea4 | LDL-C         | 1.67                 | 3937-05-28 09:53:17 |
| 32bd69ef | LDL-C         | 3.57                 | 3219-06-29 11:52:50 |
| 32bd69ef | LDL-C         | 1.95                 | 3219-08-12 07:47:47 |
| 32bd69ef | LDL-C         | 1.71                 | 3220-05-12 06:59:54 |
| e28aaed7 | LDL-C         | 1.90                 | 3703-10-20 14:45:19 |
| e28aaed7 | LDL-C         | 1.78                 | 3704-06-26 07:40:30 |
| e22d2bb8 | LDL-C         | 3.01                 | 3303-02-14 12:50:52 |
| 2fe13316 | LDL-C         | 1.87                 | 3721-11-10 08:15:27 |
| e28aaed7 | LDL-C         | 1.87                 | 3704-02-28 06:17:28 |
| e28aaed7 | LDL-C         | 1.93                 | 3704-09-27 08:57:07 |
| 2fe13316 | LDL-C         | 1.65                 | 3720-07-26 09:11:18 |
| 2fe13316 | LDL-C         | 2.35                 | 3720-04-24 06:04:52 |
| 2fe13316 | LDL-C         | 1.45                 | 3721-01-08 09:04:59 |
| 32bd69ef | LDL-C         | 1.50                 | 3219-11-08 07:57:15 |
| 32bd69ef | LDL-C         | 1.92                 | 3221-03-04 09:04:06 |
| 2fe13316 | LDL-C         | 5.66                 | 3720-04-03 06:37:15 |
| 2fe13316 | LDL-C         | 1.60                 | 3721-05-21 09:14:59 |
| bdf98ea4 | LDL-C         | 1.39                 | 3936-08-27 09:42:39 |
| e28aaed7 | LDL-C         | 2.38                 | 3706-03-10 09:20:36 |
| 2fe13316 | LDL-C         | 1.48                 | 3720-10-15 08:23:09 |
| 424756a9 | LDL-C         | 1.63                 | 3124-01-26 06:18:24 |

</div>

并利用`group_by`和`summarise`（分组及总结）功能，得出每一位患者**末次**的LDL-C结果及时间。

``` r
tmp<-
tmp%>%
  arrange(empi,sampling_time)%>%
  group_by(empi)%>%
  summarise(
    last_ldl_time=last(sampling_time),
    last_ldl=last(QUANTITATIVE_RESULT)
  )

tmp
```

<div class="kable-table">

| empi     | last\_ldl\_time     | last\_ldl |
| :------- | :------------------ | :-------- |
| 2fe13316 | 3722-01-26 08:09:55 | 1.84      |
| 32bd69ef | 3221-03-04 09:04:06 | 1.92      |
| 424756a9 | 3126-01-04 07:37:02 | 1.91      |
| bdf98ea4 | 3938-02-27 09:36:16 | 2.14      |
| e22d2bb8 | 3305-02-27 07:41:26 | 3.08      |
| e28aaed7 | 3706-03-10 09:20:36 | 2.38      |

</div>

#### 缺失比例

考虑到筛选的效率，我们可以在每一次小步骤进行过程中对关键变量的缺失比例进行评价。

``` r
final%>%
  left_join(tmp,by=c("empi"))%>%
  summarise_all(list(function(x) sum(is.na(x))/length(x)))
```

<div class="kable-table">

| pid | pflow | in\_datetime | out\_datetime | dept | empi | last\_ldl\_time | last\_ldl |
| --: | ----: | -----------: | ------------: | ---: | ---: | --------------: | --------: |
|   0 |     0 |            0 |             0 |    0 |    0 |               0 |         0 |

</div>

缺失比例满意，即可更新我们筛选的数据。

``` r
final<-
final%>%
  left_join(tmp,by=c("empi"))%>%
  filter(!is.na(last_ldl))
```

## 筛选1

根据我们上面提到的思路，首先我们来筛选那些已经住院，确诊了ASCVD的患者，且在门诊随访中记录了LDL-C的患者。

``` r
glimpse(final)
```

    ## Observations: 6
    ## Variables: 8
    ## $ pid           <chr> "0333a2e9", "46cfd386", "4bc06562", "8f0b5493", "9e3b9f…
    ## $ pflow         <chr> "3df91585", "15d5f41d", "e472205a", "43c6069a", "0d989e…
    ## $ in_datetime   <dttm> 3303-02-14 10:36:00, 3720-04-23 14:48:00, 3219-06-29 1…
    ## $ out_datetime  <dttm> 3303-02-21 08:40:50, 3720-04-27 08:30:16, 3219-07-05 0…
    ## $ dept          <chr> "心内科二病房", "心内科一病房", "心内科二病房", "心内科一病房", "心内科一病房", "心内科二…
    ## $ empi          <chr> "e22d2bb8", "2fe13316", "32bd69ef", "bdf98ea4", "424756…
    ## $ last_ldl_time <dttm> 3305-02-27 07:41:26, 3722-01-26 08:09:55, 3221-03-04 0…
    ## $ last_ldl      <chr> "3.08", "1.84", "1.92", "2.14", "1.91", "2.38"

下面的代码展示了如下的筛选条件：

1.  末次LDL-C\>1.8mmol/L；
2.  末次LDL-C检测时间在基线出院时间之后；

实际上我们在正式开展工作时候，还对患者的`院内诊断`、`介入手术`等信息进行了筛选，以确保其符合ASCVD的诊断标准。由于具体内容涉及更多院内隐私数据，故未在此处赘述。

``` r
#out<-
final%>%
  filter(last_ldl>1.8)%>%
  arrange(desc(last_ldl_time,last_ldl))%>%
  select(empi,pid,out_datetime,dept,last_ldl_time,last_ldl)%>%
  mutate(followup=as.Date(last_ldl_time)-as.Date(out_datetime))%>%
  filter(followup>0)%>%
  arrange(desc(last_ldl_time))%>%
  left_join(select(empi,empi,name,sex,birthday),by=c("empi"))%>%
  mutate(age=round(as.numeric(difftime(as.Date(last_ldl_time),birthday,units="days"))/365.25))%>%
  select(-birthday)
```

<div class="kable-table">

| empi     | pid      | out\_datetime       | dept   | last\_ldl\_time     | last\_ldl | followup | name | sex | age |
| :------- | :------- | :------------------ | :----- | :------------------ | :-------- | :------- | :--- | :-- | --: |
| bdf98ea4 | 8f0b5493 | 3936-07-18 08:51:28 | 心内科一病房 | 3938-02-27 09:36:16 | 2.14      | 589 days | 马八   | 男   |  55 |
| 2fe13316 | 46cfd386 | 3720-04-27 08:30:16 | 心内科一病房 | 3722-01-26 08:09:55 | 1.84      | 639 days | 赵六   | 男   |  47 |
| e28aaed7 | c35f2fda | 3704-03-02 08:19:11 | 心内科二病房 | 3706-03-10 09:20:36 | 2.38      | 738 days | 王五   | 男   |  62 |
| e22d2bb8 | 0333a2e9 | 3303-02-21 08:40:50 | 心内科二病房 | 3305-02-27 07:41:26 | 3.08      | 737 days | 张三   | 男   |  83 |
| 32bd69ef | 4bc06562 | 3219-07-05 09:04:43 | 心内科二病房 | 3221-03-04 09:04:06 | 1.92      | 608 days | 周七   | 男   |  70 |
| 424756a9 | 9e3b9f0e | 3124-01-30 11:07:02 | 心内科一病房 | 3126-01-04 07:37:02 | 1.91      | 705 days | 李四   | 男   |  58 |

</div>

最终，我们筛选出了6例患者。为了便于后续进一步精细化的筛选工作，我们也带出了他们的姓名、性别、年龄等信息。

事实上，哪怕没有药物医嘱的数据，经过这样一次筛选，我们已经可以大大提高找到目标受试者的概率了。因为后面唯一需要核实的，就是这部分患者目前的用药信息。我们可以将其导出，并交给负责的同事去进一步筛选。

## 筛选2

### 出院带药降脂方案

由于上面筛选出来的患者，可能有相当的比例并没有服用最大耐受剂量的他汀。所以我们还需要对这些患者基线出院时候的他汀剂量进行筛选。

首先我们读取医嘱数据。

``` r
meds <- read_csv('data/meds.csv',trim_ws = TRUE)
```

``` r
glimpse(meds)
```

    ## Observations: 41
    ## Variables: 7
    ## $ X1             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
    ## $ pflow          <chr> "e472205a", "e472205a", "e472205a", "e472205a", "e4722…
    ## $ ADVICE_CONTENT <chr> "拜阿司匹灵肠溶片||阿司匹林肠溶片", "硫酸氢氯吡格雷片", "波利特肠溶片||雷贝拉唑钠肠溶片", "…
    ## $ SPEC           <chr> "100mg*30片", "75mg x7片/盒(BLW)", "10mg*7片/盒", "20mg*7片/…
    ## $ DOSAGE         <dbl> 100.00, 75.00, 10.00, 40.00, 5.00, 6.25, 1000.00, 100.…
    ## $ DOSAGE_UNIT    <chr> "mg", "mg", "mg", "mg", "mg", "mg", "mg", "mg", "mg", …
    ## $ med_time       <dttm> 3219-07-04 18:06:44, 3219-07-04 18:06:44, 3219-07-04 …

这是经过处理后的医嘱数据的格式。

``` r
head(meds,20)
```

<div class="kable-table">

| X1 | pflow    | ADVICE\_CONTENT     | SPEC            |  DOSAGE | DOSAGE\_UNIT | med\_time           |
| -: | :------- | :------------------ | :-------------- | ------: | :----------- | :------------------ |
|  1 | e472205a | 拜阿司匹灵肠溶片||阿司匹林肠溶片   | 100mg\*30片      |  100.00 | mg           | 3219-07-04 18:06:44 |
|  2 | e472205a | 硫酸氢氯吡格雷片            | 75mg x7片/盒(BLW) |   75.00 | mg           | 3219-07-04 18:06:44 |
|  3 | e472205a | 波利特肠溶片||雷贝拉唑钠肠溶片    | 10mg\*7片/盒      |   10.00 | mg           | 3219-07-04 18:06:44 |
|  4 | e472205a | 阿托伐他汀钙片             | 20mg\*7片/盒(AL)  |   40.00 | mg           | 3219-07-04 18:06:44 |
|  5 | e472205a | 苯磺酸氨氯地平片            | 5mg\*7片/盒(LHX)  |    5.00 | mg           | 3219-07-04 18:06:44 |
|  6 | e472205a | 卡维地洛片||金络片          | 12.5MG\*14片/盒   |    6.25 | mg           | 3219-07-04 18:06:44 |
|  7 | e472205a | 补达秀缓释片||氯化钾缓释片      | 500MG x24片/盒    | 1000.00 | mg           | 3219-07-04 18:06:44 |
|  8 | b69aecbd | 拜阿司匹灵肠溶片||阿司匹林肠溶片   | 100mg\*30片      |  100.00 | mg           | 3704-03-01 11:24:27 |
|  9 | b69aecbd | 替格瑞洛片||倍林达片         | 90mg\*14片       |   90.00 | mg           | 3704-03-01 11:24:27 |
| 10 | b69aecbd | 倍他乐克缓释片||琥珀酸美托洛尔缓释片 | 47.5mg\*7片/盒    |   23.75 | mg           | 3704-03-01 11:24:27 |
| 11 | b69aecbd | 蒙诺片||福辛普利钠片         | 10MG x14片/盒     |    5.00 | mg           | 3704-03-01 11:24:27 |
| 12 | b69aecbd | 立普妥片||阿托伐他汀钙片       | 20MG\*7片        |   80.00 | mg           | 3704-03-01 11:24:27 |
| 13 | b69aecbd | 泮托拉唑钠肠溶片||潘妥洛克肠溶片   | 40MG x7片/盒      |   40.00 | mg           | 3704-03-01 11:24:27 |
| 14 | 15d5f41d | 硫酸氢氯吡格雷片            | 75mg x7片/盒(BLW) |   75.00 | mg           | 3720-04-26 10:03:14 |
| 15 | 15d5f41d | 阿托伐他汀钙片             | 20mg\*7片/盒(LPT) |   40.00 | mg           | 3720-04-26 10:03:14 |
| 16 | 15d5f41d | 拜阿司匹灵肠溶片||阿司匹林肠溶片   | 100mg\*30片      |  100.00 | mg           | 3720-04-26 10:03:14 |
| 17 | 15d5f41d | 阿卡波糖片||拜唐苹片         | 50MG\*30片 /盒    |  100.00 | mg           | 3720-04-26 10:03:14 |
| 18 | 15d5f41d | 依折麦布片||益适纯片         | 10mg\*5片/盒      |   10.00 | mg           | 3720-04-26 10:03:14 |
| 19 | 15d5f41d | 磷酸西格列汀片||捷诺维片       | 100mg\*14片/盒    |  100.00 | mg           | 3720-04-26 10:03:14 |
| 20 | 0d989e50 | 潘妥洛克肠溶片||泮托拉唑钠肠溶片   | 40MG x7片/盒      |   40.00 | mg           | 3124-01-29 10:49:25 |

</div>

### 他汀药物的标准化

与LDL-C类似，医嘱中也存在很多的**非标准数据**。我们基于此前的经验，对他汀药物进行了标准的分类处理。

``` r
tmp<-
meds%>%
  filter(str_detect(tolower(ADVICE_CONTENT),"(他汀)"))%>%
  arrange(pflow,med_time)%>%
  mutate(statin_type=case_when(
    str_detect(ADVICE_CONTENT,"阿托伐")~"阿托伐",
    str_detect(ADVICE_CONTENT,"瑞舒伐")~"瑞舒伐",
    str_detect(ADVICE_CONTENT,"普伐")~"普伐",
    str_detect(ADVICE_CONTENT,"辛伐")~"辛伐",
    str_detect(ADVICE_CONTENT,"氟伐")~"氟伐",
    str_detect(ADVICE_CONTENT,"匹伐")~"匹伐"
  ))%>%
  group_by(pflow)%>%
  summarise(
    statin_type=last(statin_type),
    dose=last(DOSAGE)
  )%>%
  ungroup()

tmp
```

<div class="kable-table">

| pflow    | statin\_type | dose |
| :------- | :----------- | ---: |
| 0d989e50 | 阿托伐          |   40 |
| 15d5f41d | 阿托伐          |   40 |
| 3df91585 | 阿托伐          |   40 |
| 43c6069a | 瑞舒伐          |   20 |
| b69aecbd | 阿托伐          |   80 |
| e472205a | 阿托伐          |   40 |

</div>

同样的，我们还看了依折麦布的使用情况。虽然这对于ORION 18并不是必要的。

``` r
tmp1<-
meds%>%
  filter(str_detect(tolower(ADVICE_CONTENT),"(依折麦布)"))%>%
  select(pflow,med_time)%>%
  mutate(emab=1)%>%
  group_by(pflow)%>%
  summarise(
    emab=max(emab)
  )%>%
  ungroup()

tmp1
```

<div class="kable-table">

| pflow    | emab |
| :------- | ---: |
| 15d5f41d |    1 |

</div>

### 缺失比例

``` r
final%>%
  left_join(tmp,by=c("pflow"))%>%
  left_join(tmp1,by=c("pflow"))%>%
  summarise_all(list(function(x) sum(is.na(x))/length(x)))
```

<div class="kable-table">

| pid | pflow | in\_datetime | out\_datetime | dept | empi | last\_ldl\_time | last\_ldl | statin\_type | dose |      emab |
| --: | ----: | -----------: | ------------: | ---: | ---: | --------------: | --------: | -----------: | ---: | --------: |
|   0 |     0 |            0 |             0 |    0 |    0 |               0 |         0 |            0 |    0 | 0.8333333 |

</div>

在保证满意的缺失比例前提下，我们再次更新数据。

``` r
final<-
final%>%
  left_join(tmp,by=c("pflow"))%>%
  left_join(tmp1,by=c("pflow"))%>%
  filter(!is.na(statin_type))
```

然后，我们就可以在刚才的基础上，进一步对用药数据进行筛选，找出那些已经服用**最大耐受剂量**他汀，依然出院后末次随访LDL-C\>1.8mmol/L的患者。

``` r
glimpse(final)
```

    ## Observations: 6
    ## Variables: 11
    ## $ pid           <chr> "0333a2e9", "46cfd386", "4bc06562", "8f0b5493", "9e3b9f…
    ## $ pflow         <chr> "3df91585", "15d5f41d", "e472205a", "43c6069a", "0d989e…
    ## $ in_datetime   <dttm> 3303-02-14 10:36:00, 3720-04-23 14:48:00, 3219-06-29 1…
    ## $ out_datetime  <dttm> 3303-02-21 08:40:50, 3720-04-27 08:30:16, 3219-07-05 0…
    ## $ dept          <chr> "心内科二病房", "心内科一病房", "心内科二病房", "心内科一病房", "心内科一病房", "心内科二…
    ## $ empi          <chr> "e22d2bb8", "2fe13316", "32bd69ef", "bdf98ea4", "424756…
    ## $ last_ldl_time <dttm> 3305-02-27 07:41:26, 3722-01-26 08:09:55, 3221-03-04 0…
    ## $ last_ldl      <chr> "3.08", "1.84", "1.92", "2.14", "1.91", "2.38"
    ## $ statin_type   <chr> "阿托伐", "阿托伐", "阿托伐", "瑞舒伐", "阿托伐", "阿托伐"
    ## $ dose          <dbl> 40, 40, 40, 20, 40, 80
    ## $ emab          <dbl> NA, 1, NA, NA, NA, NA

``` r
#out<-
final%>%
  #filter(!is.na(statin_type))%>%
  mutate(max=case_when(
    (statin_type=="阿托伐"&dose>=40)~"y",
    (statin_type=="瑞舒伐"&dose>=20)~"y",
    (statin_type=="辛伐"&dose>=20)~"y",
    (statin_type=="匹伐"&dose>=4)~"y"
  ))%>%
  filter(last_ldl>1.8&max=="y")%>%
  arrange(desc(last_ldl,last_ldl_time))%>%
  select(empi,pid,out_datetime,dept,last_ldl_time,last_ldl,statin_type,dose,emab,max)%>%
  mutate(followup=as.Date(last_ldl_time)-as.Date(out_datetime))%>%
  filter(followup>0)%>%
  arrange(desc(last_ldl_time))%>%
  left_join(select(empi,empi,name,sex,birthday),by=c("empi"))%>%
  mutate(age=round(as.numeric(difftime(as.Date(last_ldl_time),birthday,units="days"))/365.25))%>%
  select(-dept,-birthday,-max,-empi,-followup)
```

<div class="kable-table">

| pid      | out\_datetime       | last\_ldl\_time     | last\_ldl | statin\_type | dose | emab | name | sex | age |
| :------- | :------------------ | :------------------ | :-------- | :----------- | ---: | ---: | :--- | :-- | --: |
| 8f0b5493 | 3936-07-18 08:51:28 | 3938-02-27 09:36:16 | 2.14      | 瑞舒伐          |   20 |   NA | 马八   | 男   |  55 |
| 46cfd386 | 3720-04-27 08:30:16 | 3722-01-26 08:09:55 | 1.84      | 阿托伐          |   40 |    1 | 赵六   | 男   |  47 |
| c35f2fda | 3704-03-02 08:19:11 | 3706-03-10 09:20:36 | 2.38      | 阿托伐          |   80 |   NA | 王五   | 男   |  62 |
| 0333a2e9 | 3303-02-21 08:40:50 | 3305-02-27 07:41:26 | 3.08      | 阿托伐          |   40 |   NA | 张三   | 男   |  83 |
| 4bc06562 | 3219-07-05 09:04:43 | 3221-03-04 09:04:06 | 1.92      | 阿托伐          |   40 |   NA | 周七   | 男   |  70 |
| 9e3b9f0e | 3124-01-30 11:07:02 | 3126-01-04 07:37:02 | 1.91      | 阿托伐          |   40 |   NA | 李四   | 男   |  58 |

</div>

## 导出

最终，我们可以将数据导出并保存。

``` r
#write.csv(out,"data/inclisiran/out20210316.csv")
```

``` r
sessionInfo()
```

    ## R version 3.6.2 (2019-12-12)
    ## Platform: x86_64-redhat-linux-gnu (64-bit)
    ## Running under: CentOS Linux 8 (Core)
    ## 
    ## Matrix products: default
    ## BLAS/LAPACK: /usr/lib64/R/lib/libRblas.so
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] forcats_0.5.0   stringr_1.4.0   dplyr_0.8.5     purrr_0.3.3    
    ## [5] readr_1.3.1     tidyr_1.0.2     tibble_2.1.3    ggplot2_3.3.0  
    ## [9] tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.0.0 xfun_0.12        haven_2.2.0      lattice_0.20-38 
    ##  [5] colorspace_1.4-1 vctrs_0.2.4      generics_0.0.2   htmltools_0.4.0 
    ##  [9] yaml_2.2.1       utf8_1.1.4       rlang_0.4.6      pillar_1.4.3    
    ## [13] glue_1.4.0       withr_2.1.2      DBI_1.1.0        dbplyr_1.4.2    
    ## [17] modelr_0.1.6     readxl_1.3.1     lifecycle_0.2.0  munsell_0.5.0   
    ## [21] gtable_0.3.0     cellranger_1.1.0 rvest_0.3.5      evaluate_0.14   
    ## [25] knitr_1.28       fansi_0.4.1      highr_0.8        broom_0.5.5     
    ## [29] Rcpp_1.0.4.6     scales_1.1.0     backports_1.1.5  jsonlite_1.7.0  
    ## [33] fs_1.3.2         hms_0.5.3        digest_0.6.25    stringi_1.4.6   
    ## [37] grid_3.6.2       cli_2.0.2        tools_3.6.2      magrittr_1.5    
    ## [41] crayon_1.3.4     pkgconfig_2.0.3  xml2_1.2.5       reprex_0.3.0    
    ## [45] lubridate_1.7.4  assertthat_0.2.1 rmarkdown_2.1    httr_1.4.1      
    ## [49] rstudioapi_0.11  R6_2.4.1         nlme_3.1-142     compiler_3.6.2
