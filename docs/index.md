## 背景

Inclisiran以及ORION 18是正在开展的一项RCT研究。

作为整个研究PI所在的中心，北京大学第一医院也正在参与其中。而入选患者的筛选工作，就由我来负责。基于研究的入选和排除标准，我首先想到，能否利用电子病历（EHR）数据来更便捷、高效的进行患者的筛选和入组。

于是也有了这个Github项目。

## 说明

### 数据

这个项目中所有的示例数据，都包含在```/data```文件夹里。这个文件夹包含四个数据表，即dataframe，具体内容及包含变量如下：

- empi.csv

即患者唯一识别码数据表，**empi**是住院或门诊等所有系统中该患者的唯一识别码。而其中**pid**是患者住院电子病历数据的唯一标识。**name**是患者姓名，**sex**是性别，**birthday**是患者的出生日期。

**注意**：此处为了保护真实患者的隐私，此处提供的9例示例数据，虽然来源于真实EMR及我院心血管临床数据仓库，但已经对所有涉及隐私信息的数据进行了加密处理。尤其涉及到具体时间信息的数据，针对每个患者的所有数据库里涉及时间的变量，都进行了一个唯一随机数的时间漂移变量。这样做的好处，虽然我们无法根据某一次时间来定位该患者，但涉及到**年龄**这样的信息，我们依然可以利于**出生日期**与**当前时间**的时间差，来得到真实的年龄信息。这对于后续的数据分析是很有价值的。

- in_patient.csv

即患者住院信息表。其中**pflow**是此次入院的唯一标识，同一个患者，只会有唯一的**pid**与**empi**，但如果有多次住院，则会对应不同的**pflow**。

**in_datetime**与**out_datetime**分别是入院和出院的时间，如上文所述，也进行了随机数的漂移处理。**dept**是患者科室信息。

- lis_sample.csv

即患者检验项目表，包含住院和门诊检验信息。

- lis_result.csv

- meds.csv




You can use the [editor on GitHub](https://github.com/ditto504/inclisiran/edit/main/docs/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/ditto504/inclisiran/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.
