/****************
	目的：初步处理、数据描述分析
	作者：徐蓉波
	最后修改：2020.3.16
****************/

use 2017年流动人口调查A卷169989记录.dta

codebook Q308

drop if Q308==4
drop if Q308==5
drop if Q308==6  //这三步将自购的三类刨除

sum Q103, detail

drop if Q308==1
drop if Q308==3
drop if Q308==7
drop if Q308==8
drop if Q308==9
drop if Q308==10   //只保留租住私房

mean(Q103) if Q308==1 //单位、雇主房不包括就业场所
hist Q103 if Q308==1, bin(15) percent addl


********换标签过程***********
*给性别换标签
label define sex_label 1 "男" 2 "女"
label values q101b1 q101b2 q101b3 q101b4 q101b5 q101b6 q101b7 q101b8 q101b9 q101b10 sex_label

*教育
label drop edu_label
label define edu_label 1 "未上过学" 2 "小学" 3 "初中" 4 "高中/中专" 5 "大专" 6 "本科" 7 "研究生"
label values q101e1 q101e2 q101e3 q101e4 q101e5 q101e6 q101e7 q101e8 q101e9 q101e10 edu_label

*户口
label define hukou_label 1 "农业" 2 "非农业" 3 "农转居" 4 "非农转居" 5 "居民" 6 "其他"
label values q101f1 q101f2 q101f3 q101f4 q101f5 q101f6 q101f7 q101f8 q101f9 q101f10 hukou_label

save "2017A卷169989经处理", replace 

*********分析***************
clear all
log using 初始结果.log, replace
use "2017A卷169989经处理.dta", clear 

//每类住房性质的住房支出情况
sort Q308
by Q308: sum Q103, detail

//住房支出为0的占比
by Q308: count if Q103==0
tab1 Q308

//看包吃包住和住房类型的关系
tab1 Q102 //Q102B是包住折合的钱
sort Q308
by Q308: count if Q102B !=0 & Q102B !=.  //每种住房类型里包住情况的数量

//探究自建房为什么支出为0的占多数
sort C7
by C7: count if Q308==9
sort C1
by C1: count if Q308==9
tab1 C1  //看省份的自建房 
sort C2
by C2: count if Q308==9 & C1=="宁夏回族自治区" //看自建房在宁夏各个城市的分布 但这个结果很乱
by C2: count if Q308==9 & C1=="新疆维吾尔自治区"

by Q308: count if C1=="新疆生产建设兵团"
codebook C2  //调查城市数

//将自建房、借住房、其他非正规居所和三类自购剔除
keep if Q308==1 | Q308==21 | Q308==22 | Q308==3 | Q308==8  

//下面开始看y和x的关系 
scatter Q103 Q105
sum Q105, detail 
count if Q105<0 | Q105==0  
codebook Q105  //月收入有一个缺失值

log close


**********改变处理过的数据**********
//3.6号修改 

drop if Q308==9 //将自建房刨除

//按照之前举措把标签替换/////////////

clear all
use "2017A卷169989经处理.dta", clear

//研究范围里住房性质的占比【图1】
graph pie, over(Q308) plabel(_all percent, gap(10) form(%7.1f) size(*1.3))  ///
legend(row(2) size(*0.75)) ///
pie(1, c(ebblue)) pie(2, c(eltgreen)) pie(3, c(erose)) pie(4, c(erose*0.7)) ///
pie(5, c(edkblue*1.2)) pie(6, c(eltblue)) pie(7, c(emidblue*0.8)) 

clear all
use 2017年流动人口调查A卷169989记录.dta

//全体样本里住房性质的占比【图2】
graph pie, over(Q308) plabel(_all percent, gap(10) form(%7.1f) size(*1.1)) ///
legend(row(3) size(*0.7)) ///
pie(1, c(ebblue)) pie(2, c(eltgreen)) pie(3, c(orange*0.8)) pie(4, c(orange)) ///
pie(5, c(sand*1.2)) pie(6, c(erose)) pie(7, c(erose*0.7)) pie(8, c(sand))  ///
pie(9, c(edkblue*1.2)) pie(10, c(eltblue)) pie(11, c(emidblue*0.8)) 


log close

log using 初始结果.log, append

//计算家庭月收入和月支出均值
drop mincom-mpay
total Q100 //总个体数
gen minco=Q105/368935
total minco  //求出人均月收入
gen mpay=Q104/368935
total mpay //求出人均月支出
gen mhouse=Q103/368935
total mhouse //人均月住房支出


**************描述性分析二*****************
//3.7修改
log using 初始结果.log, append

sum Q104 Q103, detail
hist Q104, percent
graph box Q104
hist Q103, percent
graph box Q103  //箱图的效果都不好 离群值太多 

//重新计算支出的统计量 也就是同住人数
ta Q100
gen x1=1 if q101k1==1 
gen x2=1 if q101k2==1 
gen x3=1 if q101k3==1 
gen x4=1 if q101k4==1 
gen x5=1 if q101k5==1 
gen x6=1 if q101k6==1 
gen x7=1 if q101k7==1 
gen x8=1 if q101k8==1 
gen x9=1 if q101k9==1 
gen x10=1 if q101k10==1 
ta q101k2
ta x2
drop hbendi
egen hbendi=rsum(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
label var minco "人均月收入"
label var mpay "人均月支出"
label var mhouse "人均月住房支出"
label var hbendi "家庭同住人数/在本地居住人数"
total hbendi
replace mpay=Q104/298603
replace mhouse=Q103/298603
total mpay mhouse

//住房支出的空间结构
save 2017A卷169989经处理.dta

gen a1=1 if Q308==1 | Q308==21 | Q308==22 | Q308==3 | Q308==7 | Q308==8 | Q308==10
sort C1
by C1: count if a1==1
ta C1
sum C1, detail //算每省的租住房率

save 2017年流动人口调查A卷169989记录.dta
log close


*************描述性分析3***********
use 2017年流动人口调查A卷169989记录.dta, clear

log using 初始结果.log, append

histogram Q104, percent
histogram Q103, percent

sum Q104, detail
sum Q103, detail

save 2017年流动人口调查A卷169989记录.dta

clear all

use "2017A卷169989经处理.dta", clear

clear all

log close

