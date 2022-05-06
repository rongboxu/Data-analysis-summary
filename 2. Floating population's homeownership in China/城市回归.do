/*
目的：城市回归
作者：徐蓉波
修改时间：3.18
*/

*******不含房价******
use 城市尺度.dta, clear

gen lnaverGDP2=(lnaverGDP)^2

log using 城市回归结果.log, replace

correlate HR-lnaverGDP2 //相关关系很弱

reg HR cznum czhjperc //效果很差

gen czhjperc2=czhjperc*100
drop czhjperc

replace lnczhjperc=0
replace lnczhjperc=ln(czhjperc2) if ln(czhjperc2) >=1
replace lnczhjperc=0 if lnczhjperc==.

correlate HR lnczhjperc
correlate HR czhjperc2
correlate HR cznum
correlate HR averGDP

scatter HR cznum

********其他指标探索******
gen citylevel=0
replace citylevel=1 if CITY=="北京市" | CITY=="天津市" | CITY=="上海市" | CITY=="重庆市" ///
 | CITY=="大连市" | CITY=="宁波市" |  CITY=="青岛市" | CITY=="厦门市" | CITY=="深圳市" | ///
 CITY=="石家庄市" | CITY=="长春市" | CITY=="哈尔滨市" | CITY=="南京市" | CITY=="杭州市" | ///
 CITY=="太原市" | CITY=="呼和浩特市" | CITY=="合肥市" | CITY=="沈阳市" | CITY=="福州市" | ///
 CITY=="南昌市" | CITY=="济南市" | CITY=="郑州市" | CITY=="武汉市" | CITY=="长沙市" | ///
 CITY=="广州市" | CITY=="南宁市" | CITY=="海口市" | CITY=="成都市" | CITY=="贵阳市" | ///
 CITY=="昆明市" | CITY=="拉萨市" | CITY=="西安市" | CITY=="兰州市" | CITY=="西宁市" | ///
 CITY=="银川市" | CITY=="乌鲁木齐市" //接近一半的人都在这些城市
 
gen distr=1

save 城市尺度.dta, replace

************用一个新的文件******

gen distr=1
replace distr=2 if prov=="北京市"| prov=="河北省" | prov=="天津市" | prov=="海南省"| ///
 prov=="上海市" | prov=="江苏省" | prov=="浙江省" | prov=="福建省" | prov=="山东省" | prov=="广东省" 
replace distr=3 if prov=="山西省"| prov=="安徽省" | prov=="江西省" | prov=="河南省"| ///
 prov=="湖北省" | prov=="湖南省" 
replace distr=4 if prov=="内蒙古自治区"| prov=="广西壮族自治区" | prov=="重庆市" | prov=="四川省"| ///
 prov=="贵州省" | prov=="云南省" | prov=="西藏自治区" | prov=="陕西省" | prov=="甘肃省" | prov=="青海省"| ///
prov=="宁夏回族自治区" | prov=="新疆维吾尔自治区"  //东北是对照组

gen pir=housepr*prhousear/income
codebook pir

reg housera i.citylevel i.distr changz liudongper pir

reg housera i.citylevel i.distr changz liudongper

gen liudongper2=liudongper*100

codebook changz

hist changz, xtick(0(250)3250) kdenopts(kernel(gau)) bin(24) percent

gen changzfl=1
replace changzfl=2 if changz >=250.0
replace changzfl=3 if changz >=500.0
replace changzfl=4 if changz >=1000.0

reg housera i.citylevel i.distr i.changzfl liudongper

replace changzfl=3 if changzfl==4

reg housera i.distr ib3.changzfl liudongper

reg housera i.distr ib3.changzfl liudongper2 pir

gen liudongperfang=liudongper2^2

reg housera i.distr ib3.changzfl liudongper2 liudongperfang pir

reg housera i.distr ib3.changzfl liudongper2 liudongperfang

reg housera ib3.changzfl liudongper2 i.distr 
reg housera ib3.changzfl liudongper2 pir i.distr 

outreg2 using result.doc, append

save 城市尺度.dta, replace

*********探索共线性*******

reg housera i.citylevel i.distr ib1.changzfl liudongper2 
correlate housera citylevel 

use 城市尺度.dta
log using 城市回归结果.log, append

*****城市规模分类***
gen guimo=1
replace guimo=2 if changz>=50 
replace guimo=3 if changz>=100 
replace guimo=4 if changz>=500 
replace guimo=5 if changz>=1000

label define guimo_label 1 "小城市" 2 "中等城市" 3 "大城市" 4 "特大城市" 5 "超大城市"
label values guimo

tabulate housera guimo
bysort guimo: sum housera

********第二次回归尝试******
use 城市尺度.dta
reg housera ib1.guimo liudongperfang i.distr  //规模和流动人口占比都不显著

reg housera ib3.guimo i.distr

*******把地级市挑出来*****
sort citylevel prov

label define level_label 1 "直辖市和省会城市" 2 "地级市" 0 "地级地区和自治州、县级市"
label values citylevel level_label
codebook citylevel
tab citylevel prov

*******第二次回归尝试******
reg housera changz liudongper2 i.distr if citylevel==1 | citylevel==2 //只对地级及以上城市做
reg housera changz liudongper2 liudongperfang i.distr if citylevel==1 | citylevel==2 //加入流动人口占比平方

reg housera ib1.guimo i.distr ib2.citylevel if citylevel==1 | citylevel==2 
/* 用分类做的还不错 果然还是要针对城市来看 */
outreg2 using result.doc, replace

/* 行政等级分的再细一点 规模以谁为对照组 */
tab citylevel guimo //不考虑自治州的话 规模是1和2的就太少了 可以考虑合并 
gen citylevel2=citylevel
replace citylevel=3 if CITY=="北京市" | CITY=="天津市" | CITY=="上海市" | CITY=="重庆市" 
gen guimo2=guimo
replace guimo2=1 if guimo==2 //把原规模是1和2都赋值为1，这样就四个分类
tab citylevel2 guimo2

codebook citylevel //citylevel变成换之后的了！切记
label define level2_label 1 "省会城市" 2 "地级市" 0 "地级地区和自治州、县级市" 3 "直辖市"
label values citylevel level2_label

reg housera ib1.guimo2 i.distr ib2.citylevel if citylevel==1 | citylevel==2 | citylevel==3
outreg2 using result.doc, append
/* 规模变得不显著 还是用原来的 */
reg housera ib1.guimo i.distr ib2.citylevel if citylevel==1 | citylevel==2 | citylevel==3
outreg2 using result.doc, append

codebook distr
label define distr_label 1 "东北" 2 "东部" 3 "中部" 4 "西部" 
label value distr distr_label
/* 分地区做一下 */
tab distr guimo if citylevel==1 |citylevel==2 |citylevel==3 
codebook guimo2
reg housera ib2.guimo ib2.citylevel if (citylevel==1 | citylevel==2 | citylevel==3) /// 
& (distr==1) & (guimo==2 |guimo==3 |guimo==4) //东北没有规模是1和5的
outreg2 using result2.doc, replace
reg housera ib2.guimo ib2.citylevel if (citylevel==1 | citylevel==2 | citylevel==3) /// 
& (distr==2) & (guimo==2 |guimo==3 |guimo==4 |guimo==5) //东部没有规模是1的
outreg2 using result2.doc, append
reg housera ib3.guimo ib2.citylevel if (citylevel==1 | citylevel==2 | citylevel==3) /// 
& (distr==3) & (guimo==3 |guimo==4 |guimo==5) //中部没有规模是1和2的
outreg2 using result2.doc, append
reg housera ib1.guimo ib2.citylevel if (citylevel==1 | citylevel==2 | citylevel==3) /// 
& (distr==4) & (guimo==1 |guimo==2 |guimo==3 |guimo==4 |guimo==5) //西部规模都有
outreg2 using result2.doc, append
/* 分地区的结果显示，西部的城市间规模导致的自有率差异是显著的 其他效果都很差*/

/* 规模为1的做对照组还是不太好 数量太少了只有3个 选择中间值吧 */
reg housera ib3.guimo i.distr ib2.citylevel if citylevel==1 | citylevel==2 | citylevel==3
outreg2 using result.doc, append 
/* 结果显示和3有显著差异的是1和4 */

/* 把规模分的再细一点 */
gen guimo3=guimo
replace guimo3=guimo+1 if guimo>=3
replace guimo3=3 if changz>=100 & changz<=300
codebook guimo3
label define guimo3_label 1 "小城市" 2 "中等城市" 3 "Ⅱ型大城市" 4 "Ⅰ型大城市" 5 "特大城市" 6 "超大城市"
label values guimo3 guimo3_label

reg housera ib6.guimo3 i.distr ib2.citylevel if citylevel==1 | citylevel==2 | citylevel==3
/* 把规模分的再细一点 效果也不好 */

log close

*****描述*****
tab guimo if citylevel==1 | citylevel==2 | citylevel==3
tabstat guimo, by(citylevel) statistics(mean sd mi ma)
tabstat housera if citylevel==1 | citylevel==2 | citylevel==3, by(guimo) statistics(mean sd mi ma) 



******做省份的聚类分析*****************************
import excel "/Users/xrb/Desktop/Thesis/计算过程老excel版本.xls", sheet("3.7") cellrange(J1:K35) firstrow

use 省份聚类分析.dta, clear
log using 城市回归结果.log, append

***标准化****
egen sthouse=std(housera)

******第一类：划分聚类分析****
cluster kmeans sthouse, k(3)
cluster kmeans sthouse, k(4)
cluster kmeans sthouse, k(5)
sort _clus_3

******第二类：层次聚类分析****
cluster singlelinkage sthouse
cluster dendrogram
cluster generate type1=group(5)
sort type1

*****比较****
sort _clus_4
sum housera, detail

******再尝试*****
cluster completelinkage sthouse
cluster dendrogram
cluster generate type2=group(5)
sort type2

by _clus_4: sum housera

log close

******做城市的聚类分析*****************************
use 城市尺度.dta
log using 城市回归结果.log, append

egen sthouse=std(housera)
cluster kmeans sthouse, k(5)
cluster kmeans sthouse, k(5)
sort _clus_1

by _clus_1: sum housera, detail

cluster completelinkage sthouse
cluster generate type1=group(5)
sort type1
by type1: sum housera, detail

sum housera, detail

gen typen=type1
replace typen=4 if housera==25

******差异描述*****************************
use 城市尺度.dta
log using 城市回归结果.log, append

******重分行政等级*****
sort citylevel prov
codebook citylevel

label define level3_label 1 "省会城市" 2 "地级市" 0 "地区、自治州、盟" 3 "直辖市" 4 "县级行政单位"
label values citylevel level3_label
/*县级市包括省直辖和两个普通 */

by citylevel: sum housera, detail

log close

******导入重新计算的常住人口数据*****
use 城市尺度.dta
log using 城市回归结果.log, append

clear all

merge 1:1 CITY using "城区常住人口.dta"
sort _merge
sort citylevel prov
sort prov

by citylevel: sum cchangz, detail
codebook citylevel

******重分规模*****
sort cchangz citylevel
gen newguimo=. //缺失值默认是极大值
replace newguimo=6 if cchangz<.
replace newguimo=5 if cchangz<1000
replace newguimo=4 if cchangz<500 
replace newguimo=3 if cchangz<300 
replace newguimo=2 if cchangz<100 
replace newguimo=1 if cchangz<50

label define guimo3_label 1 "小城市" 2 "中等城市" 3 "Ⅱ型大城市" 4 "Ⅰ型大城市" 5 "特大城市" 6 "超大城市"
label values newguimo guimo3_label
sort newguimo prov

codebook newguimo
bysort newguimo: sum housera, detail

/* 规模是否只针对地级以上城市 剔除县级行政单位*/
sort newguimo citylevel //已经剔除
/*更改了吉林的问题 */


******对某些改变后的行政等级重算*****
use 城市尺度.dta
log using 城市回归结果.log, append

bysort citylevel: sum housera, detail

******自有率类型和x之间列联表*****
gen flhouse=1
replace flhouse=2 if housera>12
replace flhouse=3 if housera>24.5
replace flhouse=4 if housera>39.4
replace flhouse=5 if housera>58.5
tab flhouse
label define flhouse_label 1 "低住房自有率" 2 "较低住房自有率" 3 "中等住房自有率" ///
 4 "较高住房自有率" 5 "高住房自有率" 
label values flhouse flhouse_label

tabulate flhouse citylevel, col nofreq
tabulate flhouse newguimo, col nofreq 


******没搞清楚副省级的概念 重新弄行政等级*****
sort citylevel

label define level4_label 1 "副省级市" 2 "地级市" 0 "地区、自治州、盟" 3 "直辖市" 4 "县级行政单位" 5 "普通省会"
label values citylevel level4_label

bysort citylevel: sum housera, detail
tabulate flhouse citylevel, col nofreq

sort prov flhouse


******回归尝试——多分类******************************
log using 城市回归结果.log, append

codebook avegdp
gen lngdp=ln(avegdp)

mlogit flhouse ib1.newguimo ib2.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5, rrr baseoutcome(1)
/* 除了地区的结果都不好*/
mlogit flhouse ib3.newguimo ib1.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5, rrr baseoutcome(1)

gen lncz=ln(cchangz)
mlogit flhouse lncz ib1.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5, rrr baseoutcome(1)
/* 这个回归编号为4201 结果稍微好一点 lncz还可以*/

mlogit flhouse lncz ib5.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5, rrr baseoutcome(1)
/* 行政等级很不好 */

******行政等级citylevel3****
gen citylevel3=citylevel
replace citylevel3=1 if citylevel==3 //直辖市和副省级市合并 没有3

mlogit flhouse lncz ib1.citylevel3 lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5, rrr baseoutcome(1)


******回归尝试——线性******************************
reg housera lncz ib1.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5
/* 常住结果不好 但地级市显著比副省级市好*/

reg housera ib1.newguimo ib1.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5
/* 效果很不好*/

reg housera ib1.newguimo ib2.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5
/* 效果很不好*/

reg housera ib5.newguimo ib2.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5
reg housera ib6.newguimo ib2.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5
/* 效果很不好*/


gen lnhou=ln(housera)
reg lnhou lncz ib1.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5
reg lnhou ib6.newguimo ib2.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5


egen sthou=std(housera)
reg sthou lncz ib2.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5
/* 行政等级有结果*/
reg sthou ib3.newguimo ib2.citylevel lngdp ib2.distr if citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5



merge 1:m CITY using "2017年流动人口调查A卷169989记录.dta", gen(_m3) ///
keepusing(averbendi averhkids averhomein)

merge 1:1 CITY using "城市尺度占比基础数据.dta", gen(_m2) 


reg housera lncz ib2.citylevel lngdp ib2.distr mara averbendi averhkids ///
averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)
/* 不得不说r^2是挺高的 结果也挺好*/

reg housera ib1.newguimo ib5.citylevel ib2.distr mara averbendi averhkids ///
averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

bysort newguimo: sum housera, detail


duplicates drop CITY, force


******导入新数据******************************
 merge 1:1 CITY using "城市特征-公共服务.dta", gen(_m4)
 sort _m4
 
correlate avegdp housepr income cchangz juntea pritea junsch prisch hosbed unempl
correlate newguimo citylevel distr

reg housera lncz ib2.citylevel lngdp ib2.distr unempl housepr juntea ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

gen lninco=ln(income)
reg housera lncz ib2.citylevel lninco ib2.distr unempl housepr juntea ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

reg housera ib1.newguimo ib2.citylevel lninco ib2.distr unempl housepr juntea ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

gen newunempl=unempl*100
reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl housepr juntea ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

gen lnhousepr=ln(housepr)
reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl lnhousepr juntea ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)
/* 房价的对数不显著*/

gen newhousepr=housepr/1000
reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl newhousepr juntea ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl newhousepr prisch ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

gen averpri=sxqprisch/cchangz
sort averpri

correlate averpri avegdp housepr income cchangz juntea pritea junsch prisch hosbed unempl
correlate averpri housera

reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl newhousepr averpri ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

gen lncz2=lncz*lncz

reg housera lncz lncz2 ib2.citylevel lninco ib2.distr newunempl newhousepr averpri ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5)

twoway scatter housera lncz
twoway scatter housera averhomein

correlate income housera if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5)
twoway scatter housera income
correlate income housera if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) ///
& income>30000 //此时相关系数0.43
correlate income housera if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) ///
& income<30000 //相关系数不到0.1
twoway scatter housera lninco //对数的效果可能没原始好

reg housera ib1.newguimo ib2.citylevel income ib2.distr newunempl newhousepr averpri ///
mara averbendi averhomein edra fnra shebra if (citylevel==1 | ///
citylevel==2 |citylevel==3 |citylevel==5) //收入仍不显著



******对完整数据******************************
reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) //房价、家庭月收入居然不显著了
vif

correlate housera newguimo citylevel lninco distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) 




log using 城市回归结果.log, append

twoway scatter housera income 
correlate housera income if income>30000
correlate housera income if income<=30000
pwcorr housera income if income<=30000, sig
pwcorr housera income if income>30000, sig

twoway scatter housera housepr
pwcorr housera housepr, sig
pwcorr housera housepr if housepr>=10000, sig

sum housepr, detail
gen flhousepr=recode(housepr, 3000,4000,5000,6000,7000,35000)
bysort flhousepr:sum housera
sum housera

twoway scatter housera newunempl
pwcorr housera newunempl, sig
pwcorr housera newunempl if newunempl>=5, sig

twoway scatter housera averpri
pwcorr housera averpri, sig
pwcorr housera sxqprisch, sig
pwcorr housera hosbed, sig
twoway scatter housera sxqprisch
twoway scatter housera prisch


log using 城市回归结果.log, append

reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) //房价、家庭月收入居然不显著了

gen lninco2=lninco*lninco

reg housera ib1.newguimo ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) //加入可支配收入对数的平方 结果不错

reg housera lncz lncz2 ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) //加入常住人口对数及平方

outreg2 using "result0426", word replace 

reg housera lncz lncz2 ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) //只使用城市特征

outreg2 using "result0426", word append



log using 城市回归结果.log, append

scatter housera cchangz
scatter housera lncz
pwcorr housera lncz, sig





reg housera ib1.newguimo ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) //只使用城市特征 规模等级

reg housera ib1.newguimo ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli ///
if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5) //全部

outreg2 using "result0426", word replace



log using 城市回归结果.log, append

twoway (scatter housera income) (lfit housera income)

aaplot housera income, scheme(s2manual)
aaplot housera income, quadratic //平方

aaplot housera housepr, scheme(s2manual)
aaplot housera housepr, scheme(s2manual) quadratic
aaplot housera newhousepr, scheme(s2manual) quadratic

aaplot housera newunempl, scheme(s2manual) //r^2是0 可怕
aaplot housera newunempl, scheme(s2manual) quadratic

aaplot housera averpri, scheme(s2manual) 
aaplot housera averpri, scheme(s2manual) quadratic

sum income, detail

histogram housera, scheme(s2manual) width(5) percent kdenopts(kernel(gau))

kdensity housera, nograph generate(x dx) kernel(gau)
kdensity housera if citylevel==1, nograph generate(dx1) at(x) kernel(gau)
kdensity housera if citylevel==2, nograph generate(dx2) at(x) kernel(gau)
label var dx1 "副省级市"
label var dx2 "地级市"
line dx1 dx2 x
kdensity housera if citylevel==3, nograph generate(dx3) at(x) kernel(gau)
kdensity housera if citylevel==5, nograph generate(dx4) at(x) kernel(gau)
label var dx3 "直辖市"
label var dx4 "普通省会城市"
line dx3 dx1 dx4 dx2 x, scheme(s2manual)




log using 城市回归结果.log, append
logout, save(des1) excel replace: 

sum income newhousepr newunempl averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
fnra ksra averyear ///
shebra avershxinli ///
if citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5 //不行 因为sum不能识别if

tabstat income newhousepr newunempl averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
fnra ksra averyear ///
shebra avershxinli ///
if citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5, ///
s(mean min max) f(%10.2f) c(s) //这个可以！！！！

tabstat housera if (citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5), ///
s(mean min max) f(%10.2f) c(s) //这个可以！！！！

tabstat housera, s(mean min max) f(%10.2f) c(s) //这个可以！！！！

tabulate newguimo if citylevel==1 | citylevel==2 |citylevel==3 |citylevel==5
tabulate citylevel 

tabstat housera income newhousepr newunempl averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
fnra ksra averyear ///
shebra avershxinli, ///
s(mean min max) f(%10.2f) c(s) //这个可以！！！！

tabulate newguimo 
tabulate citylevel
tabulate distr




reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl newhousepr averpri 
 //只使用城市特征 规模等级 加入可支配收入平方会导致共线性 
 
pwcorr housera newguimo citylevel lninco distr newunempl newhousepr averpri 
// 规模、房价、可支配收入之间相关性比较强
pwcorr lninco newhousepr

reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl averpri 
outreg2 using "result0503", word replace
reg housera ib1.newguimo ib2.citylevel ib2.distr newhousepr newunempl averpri 
outreg2 using "result0503", word append



reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli 
//全部 
//加入可支配收入平方会导致共线性


reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli 

pwcorr housera newguimo citylevel lninco distr newunempl averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli 

reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl averpri ///
mara averbendi  averhkids ///
averhomein edra sfra ///
ksra averhkids fnra ///
shebra avershxinli 
// 本地家庭成员和未成年子女相关系数较高 去掉一个会让地区系数变化 去掉跨省也是 why？
outreg2 using "result0503", word append

reg housera ib1.newguimo ib2.citylevel lninco ib2.distr newunempl averpri ///
mara  averhkids 
averhomein edra sfra 

ksra averhkids fnra ///
shebra avershxinli 

//地区系数 似乎是本地家庭成员的锅
pwcorr averbendi averhkids, sig





log using 城市回归结果.log, append

reg housera ib1.newguimo ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli 
//平方项依然加
outreg2 using "result0504", word replace

reg housera ib1.newguimo ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri 
outreg2 using "result0504", word replace

outreg2 using "result0504", sideway dec(3) word replace


reg housera ib1.newguimo ib2.citylevel lninco lninco2 ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli 



gen citylevel5=citylevel
replace citylevel5=2 if citylevel==5

reg housera ib1.newguimo ib2.citylevel5 lninco lninco2 ib2.distr newunempl newhousepr averpri 
outreg2 using "result0504", sideway bdec(3) sdec(2) word replace

reg housera ib1.newguimo ib2.citylevel5 lninco lninco2 ib2.distr newunempl newhousepr averpri ///
mara averbendi averhkids ///
averhomein edra sfra ///
ksra averyear fnra ///
shebra avershxinli 

outreg2 using "result0504", sideway bdec(3) sdec(2) word append


tabulate citylevel5
label define citylevel5_label 1 "副省级市" 2 "地级市" 3 "直辖市" 
label values citylevel5 citylevel5_label 

label define level5_label 1 "副省级市" 2 "地级市" 3 "直辖市" 0 "地区、自治州、盟" 4 "县级行政单位"
label values citylevel5 level5_label 

bysort citylevel5: sum housera, detail


tabstat housera, by(citylevel5) statistics(mean sd mi ma)

tabulate citylevel5 flhouse, row nofreq

gen flpri=1
replace flpri=2 if averpri>=1
replace flpri=3 if averpri>=2
replace flpri=4 if averpri>=5


tabstat housera, by(flpri) statistics(mean sd mi ma)

gen flunem=1
replace flunem=2 if newunempl>=3
replace flunem=3 if newunempl>=6
replace flunem=4 if newunempl>=10

tabstat housera, by(flunem) statistics(mean sd mi ma)


gen flincome=recode(income, 20000,21000,22000,23000,24000,25000,26000,27000, ///
28000,29000,30000,31000,32000,33000,34000,35000,36000,37000,38000,39000,40000, ///
41000,42000,43000,44000,45000,46000,47000,48000,49000,50000,53000)

bysort flincome: sum housera




