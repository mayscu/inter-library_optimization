library(data.table)

test_data<-data.frame(lend_loc=testdata[,2],college=testdata[ ,7],coll_loc=testdata[ ,4],book_np=testdata[ ,5],stringsAsFactors = FALSE)
#导入原始馆藏数据

college_correct<-data.table(college_name=各学院人数[ ,1],college_weight=各学院人数[ ,2])
setkey(college_correct,"学院")
#导入学院的修正值

collection_pattern<-data.table(book_no=中图分类法_简表_详表_[ ,1],book_name=中图分类法_简表_详表_[ ,2])
setkey(collection_pattern,"book_no.A")
#导入馆藏的完整分类表,用data.table包设置主键

#collection_pattern[ ,1]<-as.character(collection_pattern[ ,1])
#collection_pattern[ ,2]<-as.character(collection_pattern[ ,2])
library(stringr)

#match_str<-as.character(collection_correct[ ,1])
#apply(collection_pattern,2,str_detect,pattern=c("A11","A12"))
#a<-mapply(str_detect,string=collection_pattern[ ,1],pattern=collection_correct[ ,1])

collection_correct<-data.table(book_no=总数据[ ,1],book_correct=总数据[ ,3])
#导入筛选后的修正值表
setkey(collection_correct,"book_no.分类号")
collection_processed<-collection_pattern[collection_correct,nomatch=NA]
#筛选后的修正表
match_str<-data.frame(x=collection_processed$book_no.A,y=collection_processed$`book_name.马克思主义、列宁主义、毛泽东思想、邓小平理论`,stringsAsFactors = FALSE)


multi_replace<-function(raw_strs,patterns){
#patterns是匹配条件表,raw_strs是待处理的文本
 library("stringr")
 for(i in 1:nrow(patterns)){
      raw_strs[str_detect(raw_strs,patterns[i,1])]<-patterns[i,2]

 } 
      
        raw_strs
       #输出处理后的字符串
}

test_data[ ,4]<-multi_replace(test_data[ ,4],match_str)
#替换书目

collection_loc<-data.frame(name=c("炳麟","敬文","北区","本部","阳澄湖"),location=c("炳麟图书馆","敬文图书馆","北区图书馆","本部图书馆","阳澄湖图书馆"),stringsAsFactors = FALSE)
#馆藏地替换规则

test_data[ ,1]<-multi_replace(test_data[ ,1],collection_loc)
test_data[ ,3]<-multi_replace(test_data[ ,3],collection_loc)
#替换馆藏地
test_data_table<-data.table(test_data)
setkey(test_data_table,"读者单位")
tdata<-test_data_table[college_correct,nomatch=NA]
#有的读者单位也需要通过内联过滤掉

