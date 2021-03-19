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

2.  纯门诊，通过诊断与检验系统进行筛选、甚至实时监测；

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

``` r
head(in_patient)
```

    ## # A tibble: 6 x 6
    ##      X1 pid      pflow    in_datetime         out_datetime        dept        
    ##   <dbl> <chr>    <chr>    <dttm>              <dttm>              <chr>       
    ## 1     1 0333a2e9 3df91585 3303-02-14 10:36:00 3303-02-21 08:40:50 心内科二病房
    ## 2     2 9e3b9f0e 0d989e50 3124-01-25 20:55:49 3124-01-30 11:07:02 心内科一病房
    ## 3     3 c35f2fda 06da340d 3698-04-10 16:33:12 3698-04-18 14:27:51 神经外科病房
    ## 4     4 c35f2fda b69aecbd 3704-02-27 10:49:00 3704-03-02 08:19:11 心内科二病房
    ## 5     5 c35f2fda 3579fa0d 3703-10-20 14:29:42 3703-10-30 08:41:02 心内科二病房
    ## 6     6 46cfd386 15d5f41d 3720-04-23 14:48:00 3720-04-27 08:30:16 心内科一病房

然后，我们读取患者唯一识别码EMPI。

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
head(empi)
```

    ## # A tibble: 6 x 6
    ##      X1 empi     pid      name  sex   birthday  
    ##   <dbl> <chr>    <chr>    <chr> <chr> <date>    
    ## 1     1 e22d2bb8 0333a2e9 张三  男    3221-09-25
    ## 2     2 424756a9 9e3b9f0e 李四  男    3068-04-02
    ## 3     3 e28aaed7 c35f2fda 王五  男    3644-04-21
    ## 4     4 2fe13316 46cfd386 赵六  男    3674-08-30
    ## 5     5 32bd69ef 4bc06562 周七  男    3151-03-01
    ## 6     6 bdf98ea4 8f0b5493 马八  男    3883-07-28

``` r
tmp<-
empi%>%
  select(pid,empi)
```

#### 末次住院

接下来，我们要选取每一位患者的末次住院信息，作为基线信息。

这里面，我们利用了`arrange`（排序）以及`distinct`（唯一）的算法，并得到我们希望的患者列表。

``` r
final<-
in_patient%>%
  left_join(tmp,by=c("pid"))%>%
  filter(!is.na(empi))%>%
  arrange(pid,desc(in_datetime))%>%
  distinct(empi,.keep_all = TRUE)%>%
  select(-X1)
```

我们来看一下患者的时间跨度

``` r
summary(final$in_datetime)
```

    ##                  Min.               1st Qu.                Median 
    ## "3124-01-25 20:55:49" "3240-05-25 17:06:00" "3503-08-22 22:42:30" 
    ##                  Mean               3rd Qu.                  Max. 
    ## "3501-04-13 13:10:28" "3716-04-09 13:48:15" "3936-07-15 10:38:00"

#### 末次化验

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

``` r
head(lis_sample_in)
```

    ## # A tibble: 6 x 4
    ##   sid              pflow    sampling_time       empi    
    ##   <chr>            <chr>    <dttm>              <chr>   
    ## 1 32190629G0020251 e472205a 3219-06-29 11:52:59 32bd69ef
    ## 2 32190629G0017007 e472205a 3219-06-29 11:52:50 32bd69ef
    ## 3 32190629G0020616 e472205a 3219-06-29 11:52:46 32bd69ef
    ## 4 32190701G0013903 e472205a 3219-07-01 09:12:02 32bd69ef
    ## 5 32190701G0351138 e472205a 3219-06-29 11:52:56 32bd69ef
    ## 6 32190701G0350285 e472205a 3219-06-29 11:52:42 32bd69ef

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

``` r
tmp<-
lis_sample_in%>%
  filter(!is.na(empi))%>%
  semi_join(final,by=c("empi"))
```

``` r
tmp<-
lis_result_in%>%
  filter(ENGLISH_NAME %in% c("LDL-C"))%>%
  left_join(tmp, by="sid")%>%
  select(empi,ENGLISH_NAME,QUANTITATIVE_RESULT,sampling_time)%>%
  filter(!is.na(empi))
```

``` r
tmp<-
tmp%>%
  arrange(empi,sampling_time)%>%
  group_by(empi)%>%
  summarise(
    last_ldl_time=last(sampling_time),
    last_ldl=last(sampling_time)
  )
```

### 缺失比例

``` r
final%>%
  left_join(tmp,by=c("empi"))%>%
  summarise_all(list(function(x) sum(is.na(x))/length(x)))
```

    ## # A tibble: 1 x 8
    ##     pid pflow in_datetime out_datetime  dept  empi last_ldl_time last_ldl
    ##   <dbl> <dbl>       <dbl>        <dbl> <dbl> <dbl>         <dbl>    <dbl>
    ## 1     0     0           0            0     0     0             0        0

### 更新final数据

``` r
final<-
final%>%
  left_join(tmp,by=c("empi"))%>%
  filter(!is.na(last_ldl))
```

## 筛选1

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
    ## $ last_ldl      <dttm> 3305-02-27 07:41:26, 3722-01-26 08:09:55, 3221-03-04 0…

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
  mutate(age=round(as.numeric(difftime(as.Date(last_ldl_time),birthday,units="days"))/365.25))
```

    ## # A tibble: 6 x 11
    ##   empi  pid   out_datetime        dept  last_ldl_time       last_ldl           
    ##   <chr> <chr> <dttm>              <chr> <dttm>              <dttm>             
    ## 1 bdf9… 8f0b… 3936-07-18 08:51:28 心内科一… 3938-02-27 09:36:16 3938-02-27 09:36:16
    ## 2 2fe1… 46cf… 3720-04-27 08:30:16 心内科一… 3722-01-26 08:09:55 3722-01-26 08:09:55
    ## 3 e28a… c35f… 3704-03-02 08:19:11 心内科二… 3706-03-10 09:20:36 3706-03-10 09:20:36
    ## 4 e22d… 0333… 3303-02-21 08:40:50 心内科二… 3305-02-27 07:41:26 3305-02-27 07:41:26
    ## 5 32bd… 4bc0… 3219-07-05 09:04:43 心内科二… 3221-03-04 09:04:06 3221-03-04 09:04:06
    ## 6 4247… 9e3b… 3124-01-30 11:07:02 心内科一… 3126-01-04 07:37:02 3126-01-04 07:37:02
    ## # … with 5 more variables: followup <drtn>, name <chr>, sex <chr>,
    ## #   birthday <date>, age <dbl>

### 导出

``` r
#write.csv(out,"data/inclisiran/more20210316.csv")
```

## 出院带药降脂方案

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
```

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
```

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

### 更新final数据

``` r
final<-
final%>%
  left_join(tmp,by=c("pflow"))%>%
  left_join(tmp1,by=c("pflow"))%>%
  filter(!is.na(statin_type))
```

## 筛选2

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
    ## $ last_ldl      <dttm> 3305-02-27 07:41:26, 3722-01-26 08:09:55, 3221-03-04 0…
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
  select(-birthday)
```

    ## # A tibble: 6 x 14
    ##   empi  pid   out_datetime        dept  last_ldl_time       last_ldl           
    ##   <chr> <chr> <dttm>              <chr> <dttm>              <dttm>             
    ## 1 bdf9… 8f0b… 3936-07-18 08:51:28 心内科一… 3938-02-27 09:36:16 3938-02-27 09:36:16
    ## 2 2fe1… 46cf… 3720-04-27 08:30:16 心内科一… 3722-01-26 08:09:55 3722-01-26 08:09:55
    ## 3 e28a… c35f… 3704-03-02 08:19:11 心内科二… 3706-03-10 09:20:36 3706-03-10 09:20:36
    ## 4 e22d… 0333… 3303-02-21 08:40:50 心内科二… 3305-02-27 07:41:26 3305-02-27 07:41:26
    ## 5 32bd… 4bc0… 3219-07-05 09:04:43 心内科二… 3221-03-04 09:04:06 3221-03-04 09:04:06
    ## 6 4247… 9e3b… 3124-01-30 11:07:02 心内科一… 3126-01-04 07:37:02 3126-01-04 07:37:02
    ## # … with 8 more variables: statin_type <chr>, dose <dbl>, emab <dbl>,
    ## #   max <chr>, followup <drtn>, name <chr>, sex <chr>, age <dbl>

## 导出

``` r
#write.csv(out,"data/inclisiran/out20210316.csv")
```
