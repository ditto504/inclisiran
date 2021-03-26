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
glimpse(in_patient)
```

    ## Observations: 9
    ## Variables: 6
    ## $ X1           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9
    ## $ pid          <chr> "0333a2e9", "9e3b9f0e", "c35f2fda", "c35f2fda", "c35f2fd…
    ## $ pflow        <chr> "3df91585", "0d989e50", "06da340d", "b69aecbd", "3579fa0…
    ## $ in_datetime  <dttm> 3303-02-14 10:36:00, 3124-01-25 20:55:49, 3698-04-10 16…
    ## $ out_datetime <dttm> 3303-02-21 08:40:50, 3124-01-30 11:07:02, 3698-04-18 14…
    ## $ dept         <chr> "心内科二病房", "心内科一病房", "神经外科病房", "心内科二病房", "心内科二病房", "心内科一病…

然后，我们读取患者唯一识别码EMPI。以便于后续匹配其门诊检验数据信息。

``` r
empi<-read_csv("data/empi.csv")
```

``` r
glimpse(empi)
```

    ## Observations: 6
    ## Variables: 6
    ## $ X1       <dbl> 1, 2, 3, 4, 5, 6
    ## $ empi     <chr> "e22d2bb8", "424756a9", "e28aaed7", "2fe13316", "32bd69ef", …
    ## $ pid      <chr> "0333a2e9", "9e3b9f0e", "c35f2fda", "46cfd386", "4bc06562", …
    ## $ name     <chr> "张三", "李四", "王五", "赵六", "周七", "马八"
    ## $ sex      <chr> "男", "男", "男", "男", "男", "男"
    ## $ birthday <date> 3221-09-25, 3068-04-02, 3644-04-21, 3674-08-30, 3151-03-01,…

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

    ## # A tibble: 6 x 6
    ##   pid      pflow    in_datetime         out_datetime        dept         empi   
    ##   <chr>    <chr>    <dttm>              <dttm>              <chr>        <chr>  
    ## 1 0333a2e9 3df91585 3303-02-14 10:36:00 3303-02-21 08:40:50 心内科二病房 e22d2b…
    ## 2 46cfd386 15d5f41d 3720-04-23 14:48:00 3720-04-27 08:30:16 心内科一病房 2fe133…
    ## 3 4bc06562 e472205a 3219-06-29 11:16:00 3219-07-05 09:04:43 心内科二病房 32bd69…
    ## 4 8f0b5493 43c6069a 3936-07-15 10:38:00 3936-07-18 08:51:28 心内科一病房 bdf98e…
    ## 5 9e3b9f0e 0d989e50 3124-01-25 20:55:49 3124-01-30 11:07:02 心内科一病房 424756…
    ## 6 c35f2fda b69aecbd 3704-02-27 10:49:00 3704-03-02 08:19:11 心内科二病房 e28aae…

#### 末次化验

接下来，我们使用`EMPI`来匹配获得每位患者在全院系统（包括住院与门诊）的**末次**LDL-C化验数据。

``` r
lis_sample_in <- read_csv("data/lis_sample.csv")
```

``` r
lis_sample_in <- select(lis_sample_in, -X1)
```

``` r
glimpse(lis_sample_in)
```

    ## Observations: 208
    ## Variables: 4
    ## $ sid           <chr> "32190629G0020251", "32190629G0017007", "32190629G00206…
    ## $ pflow         <chr> "e472205a", "e472205a", "e472205a", "e472205a", "e47220…
    ## $ sampling_time <dttm> 3219-06-29 11:52:59, 3219-06-29 11:52:50, 3219-06-29 1…
    ## $ empi          <chr> "32bd69ef", "32bd69ef", "32bd69ef", "32bd69ef", "32bd69…

从上面可以看到，虽然我们只有9例患者，但是包含了208份样本的化验数据。

接下来，我们读取具体的检验条目结果数据。

``` r
lis_result_in <- read_csv("data/lis_result.csv")
```

``` r
glimpse(lis_result_in)
```

    ## Observations: 3,308
    ## Variables: 8
    ## $ X1                  <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15…
    ## $ sid                 <chr> "32200710G0014038", "32200710G0014038", "32200710…
    ## $ ENGLISH_NAME        <chr> "eGFR", "A/G", "AG", "ALB*", "ALP*", "ALT*", "AST…
    ## $ CHINESE_NAME        <chr> "估算肾小球滤过率", "白球比值", "阴离子间隙", "白蛋白", "碱性磷酸酶", "谷丙转…
    ## $ QUANTITATIVE_RESULT <chr> "70.982", "1.49", "14.70", "46.8", "82", "8", "17…
    ## $ QUALITATIVE_RESULT  <chr> "z", "z", "z", "z", "z", "l", "z", "z", "h", "z",…
    ## $ TEST_ITEM_REFERENCE <chr> NA, "1.2-2.4", NA, "40-55", "45-125", "9-50", "15…
    ## $ TEST_ITEM_UNIT      <chr> "ml/min/1.73㎡", NA, "mmol/L", "g/L", "IU/L", "IU/…

这里我们用表格形式展示一下化验结果的数据格式。

``` r
head(lis_result_in)
```

    ## # A tibble: 6 x 8
    ##      X1 sid   ENGLISH_NAME CHINESE_NAME QUANTITATIVE_RE… QUALITATIVE_RES…
    ##   <dbl> <chr> <chr>        <chr>        <chr>            <chr>           
    ## 1     1 3220… eGFR         估算肾小球滤过率… 70.982           z               
    ## 2     2 3220… A/G          白球比值     1.49             z               
    ## 3     3 3220… AG           阴离子间隙   14.70            z               
    ## 4     4 3220… ALB*         白蛋白       46.8             z               
    ## 5     5 3220… ALP*         碱性磷酸酶   82               z               
    ## 6     6 3220… ALT*         谷丙转氨酶   8                l               
    ## # … with 2 more variables: TEST_ITEM_REFERENCE <chr>, TEST_ITEM_UNIT <chr>

##### LDL-C的标准化

考虑到不同医疗机构、不同的LIS（检验信息管理系统）对于LDL-C不一定使用统一的编码和术语表述，所以在使用EHR系统开展类似的研究工作时，一个很大的工作量是进行术语的标准化处理。比如，在这个例子中，我们可以使用关键字`低密度`来找出所有我们希望找到的检验条目。

``` r
lis_result_in%>%
  #filter(str_detect(tolower(ENGLISH_NAME),"ldl"))%>%
  filter(str_detect(CHINESE_NAME,"低密度"))%>%
  group_by(ENGLISH_NAME,CHINESE_NAME)%>%
  summarise(n=n())
```

    ## # A tibble: 1 x 3
    ## # Groups:   ENGLISH_NAME [1]
    ##   ENGLISH_NAME CHINESE_NAME           n
    ##   <chr>        <chr>              <int>
    ## 1 LDL-C        低密度脂蛋白胆固醇    35

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

    ## # A tibble: 35 x 4
    ##    empi     ENGLISH_NAME QUANTITATIVE_RESULT sampling_time      
    ##    <chr>    <chr>        <chr>               <dttm>             
    ##  1 32bd69ef LDL-C        1.49                3220-07-10 07:51:27
    ##  2 e28aaed7 LDL-C        1.77                3704-11-07 08:57:55
    ##  3 e28aaed7 LDL-C        1.15                3704-12-28 09:09:18
    ##  4 2fe13316 LDL-C        1.86                3721-08-18 07:52:46
    ##  5 bdf98ea4 LDL-C        2.14                3938-02-27 09:36:16
    ##  6 e28aaed7 LDL-C        1.68                3703-12-24 10:30:38
    ##  7 e28aaed7 LDL-C        2.26                3704-05-09 07:54:29
    ##  8 e22d2bb8 LDL-C        1.88                3303-09-19 07:35:23
    ##  9 2fe13316 LDL-C        4.36                3720-04-09 06:22:17
    ## 10 2fe13316 LDL-C        1.84                3722-01-26 08:09:55
    ## # … with 25 more rows

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

    ## # A tibble: 6 x 3
    ##   empi     last_ldl_time       last_ldl
    ##   <chr>    <dttm>              <chr>   
    ## 1 2fe13316 3722-01-26 08:09:55 1.84    
    ## 2 32bd69ef 3221-03-04 09:04:06 1.92    
    ## 3 424756a9 3126-01-04 07:37:02 1.91    
    ## 4 bdf98ea4 3938-02-27 09:36:16 2.14    
    ## 5 e22d2bb8 3305-02-27 07:41:26 3.08    
    ## 6 e28aaed7 3706-03-10 09:20:36 2.38

#### 缺失比例

考虑到筛选的效率，我们可以在每一次小步骤进行过程中对关键变量的缺失比例进行评价。

``` r
final%>%
  left_join(tmp,by=c("empi"))%>%
  summarise_all(list(function(x) sum(is.na(x))/length(x)))
```

    ## # A tibble: 1 x 8
    ##     pid pflow in_datetime out_datetime  dept  empi last_ldl_time last_ldl
    ##   <dbl> <dbl>       <dbl>        <dbl> <dbl> <dbl>         <dbl>    <dbl>
    ## 1     0     0           0            0     0     0             0        0

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

    ## # A tibble: 6 x 10
    ##   empi  pid   out_datetime        dept  last_ldl_time       last_ldl followup
    ##   <chr> <chr> <dttm>              <chr> <dttm>              <chr>    <drtn>  
    ## 1 bdf9… 8f0b… 3936-07-18 08:51:28 心内科一… 3938-02-27 09:36:16 2.14     589 days
    ## 2 2fe1… 46cf… 3720-04-27 08:30:16 心内科一… 3722-01-26 08:09:55 1.84     639 days
    ## 3 e28a… c35f… 3704-03-02 08:19:11 心内科二… 3706-03-10 09:20:36 2.38     738 days
    ## 4 e22d… 0333… 3303-02-21 08:40:50 心内科二… 3305-02-27 07:41:26 3.08     737 days
    ## 5 32bd… 4bc0… 3219-07-05 09:04:43 心内科二… 3221-03-04 09:04:06 1.92     608 days
    ## 6 4247… 9e3b… 3124-01-30 11:07:02 心内科一… 3126-01-04 07:37:02 1.91     705 days
    ## # … with 3 more variables: name <chr>, sex <chr>, age <dbl>

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

    ## # A tibble: 20 x 7
    ##       X1 pflow  ADVICE_CONTENT     SPEC   DOSAGE DOSAGE_UNIT med_time           
    ##    <dbl> <chr>  <chr>              <chr>   <dbl> <chr>       <dttm>             
    ##  1     1 e4722… 拜阿司匹灵肠溶片||阿司匹林肠溶片… 100mg… 1.00e2 mg          3219-07-04 18:06:44
    ##  2     2 e4722… 硫酸氢氯吡格雷片   75mg … 7.50e1 mg          3219-07-04 18:06:44
    ##  3     3 e4722… 波利特肠溶片||雷贝拉唑钠肠溶片… 10mg*… 1.00e1 mg          3219-07-04 18:06:44
    ##  4     4 e4722… 阿托伐他汀钙片     20mg*… 4.00e1 mg          3219-07-04 18:06:44
    ##  5     5 e4722… 苯磺酸氨氯地平片   5mg*7… 5.00e0 mg          3219-07-04 18:06:44
    ##  6     6 e4722… 卡维地洛片||金络片 12.5M… 6.25e0 mg          3219-07-04 18:06:44
    ##  7     7 e4722… 补达秀缓释片||氯化钾缓释片… 500MG… 1.00e3 mg          3219-07-04 18:06:44
    ##  8     8 b69ae… 拜阿司匹灵肠溶片||阿司匹林肠溶片… 100mg… 1.00e2 mg          3704-03-01 11:24:27
    ##  9     9 b69ae… 替格瑞洛片||倍林达片… 90mg*… 9.00e1 mg          3704-03-01 11:24:27
    ## 10    10 b69ae… 倍他乐克缓释片||琥珀酸美托洛尔缓… 47.5m… 2.38e1 mg          3704-03-01 11:24:27
    ## 11    11 b69ae… 蒙诺片||福辛普利钠片… 10MG … 5.00e0 mg          3704-03-01 11:24:27
    ## 12    12 b69ae… 立普妥片||阿托伐他汀钙片… 20MG*… 8.00e1 mg          3704-03-01 11:24:27
    ## 13    13 b69ae… 泮托拉唑钠肠溶片||潘妥洛克肠溶片… 40MG … 4.00e1 mg          3704-03-01 11:24:27
    ## 14    14 15d5f… 硫酸氢氯吡格雷片   75mg … 7.50e1 mg          3720-04-26 10:03:14
    ## 15    15 15d5f… 阿托伐他汀钙片     20mg*… 4.00e1 mg          3720-04-26 10:03:14
    ## 16    16 15d5f… 拜阿司匹灵肠溶片||阿司匹林肠溶片… 100mg… 1.00e2 mg          3720-04-26 10:03:14
    ## 17    17 15d5f… 阿卡波糖片||拜唐苹片… 50MG*… 1.00e2 mg          3720-04-26 10:03:14
    ## 18    18 15d5f… 依折麦布片||益适纯片… 10mg*… 1.00e1 mg          3720-04-26 10:03:14
    ## 19    19 15d5f… 磷酸西格列汀片||捷诺维片… 100mg… 1.00e2 mg          3720-04-26 10:03:14
    ## 20    20 0d989… 潘妥洛克肠溶片||泮托拉唑钠肠溶片… 40MG … 4.00e1 mg          3124-01-29 10:49:25

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

    ## # A tibble: 6 x 3
    ##   pflow    statin_type  dose
    ##   <chr>    <chr>       <dbl>
    ## 1 0d989e50 阿托伐         40
    ## 2 15d5f41d 阿托伐         40
    ## 3 3df91585 阿托伐         40
    ## 4 43c6069a 瑞舒伐         20
    ## 5 b69aecbd 阿托伐         80
    ## 6 e472205a 阿托伐         40

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

    ## # A tibble: 1 x 2
    ##   pflow     emab
    ##   <chr>    <dbl>
    ## 1 15d5f41d     1

### 缺失比例

``` r
final%>%
  left_join(tmp,by=c("pflow"))%>%
  left_join(tmp1,by=c("pflow"))%>%
  summarise_all(list(function(x) sum(is.na(x))/length(x)))
```

    ## # A tibble: 1 x 11
    ##     pid pflow in_datetime out_datetime  dept  empi last_ldl_time last_ldl
    ##   <dbl> <dbl>       <dbl>        <dbl> <dbl> <dbl>         <dbl>    <dbl>
    ## 1     0     0           0            0     0     0             0        0
    ## # … with 3 more variables: statin_type <dbl>, dose <dbl>, emab <dbl>

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

    ## # A tibble: 6 x 10
    ##   pid   out_datetime        last_ldl_time       last_ldl statin_type  dose  emab
    ##   <chr> <dttm>              <dttm>              <chr>    <chr>       <dbl> <dbl>
    ## 1 8f0b… 3936-07-18 08:51:28 3938-02-27 09:36:16 2.14     瑞舒伐         20    NA
    ## 2 46cf… 3720-04-27 08:30:16 3722-01-26 08:09:55 1.84     阿托伐         40     1
    ## 3 c35f… 3704-03-02 08:19:11 3706-03-10 09:20:36 2.38     阿托伐         80    NA
    ## 4 0333… 3303-02-21 08:40:50 3305-02-27 07:41:26 3.08     阿托伐         40    NA
    ## 5 4bc0… 3219-07-05 09:04:43 3221-03-04 09:04:06 1.92     阿托伐         40    NA
    ## 6 9e3b… 3124-01-30 11:07:02 3126-01-04 07:37:02 1.91     阿托伐         40    NA
    ## # … with 3 more variables: name <chr>, sex <chr>, age <dbl>

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
    ## [25] knitr_1.28       fansi_0.4.1      broom_0.5.5      Rcpp_1.0.4.6    
    ## [29] scales_1.1.0     backports_1.1.5  jsonlite_1.7.0   fs_1.3.2        
    ## [33] hms_0.5.3        digest_0.6.25    stringi_1.4.6    grid_3.6.2      
    ## [37] cli_2.0.2        tools_3.6.2      magrittr_1.5     crayon_1.3.4    
    ## [41] pkgconfig_2.0.3  xml2_1.2.5       reprex_0.3.0     lubridate_1.7.4 
    ## [45] assertthat_0.2.1 rmarkdown_2.1    httr_1.4.1       rstudioapi_0.11 
    ## [49] R6_2.4.1         nlme_3.1-142     compiler_3.6.2
