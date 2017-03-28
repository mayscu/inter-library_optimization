import_collection<-function(the_file,ll,c,cl,bn){
  imported_data<-data.frame(lend_loc=the_file[,ll],读者单位=the_file[ ,c],
                        coll_loc=the_file[ ,cl],索书号=the_file[ ,bn],
                        stringsAsFactors = FALSE)
  #指定相应字段所对应的行号，返回导入后的数据,
  #包括借阅地点、馆藏地点、学院、和索书号
  
  # 
  imported_data<-na.omit(imported_data)
  
  collection_loc_pattern<-data.frame(name=c("炳麟","敬文","北区","本部","阳澄湖","G学报","王健","数科院","^[0-9]"),location=c("炳麟图书馆","敬文图书馆","北区图书馆","本部图书馆","阳澄湖图书馆","本部图书馆","本部图书馆","本部图书馆","本部图书馆"),stringsAsFactors = FALSE)
  #馆藏地替换规则
  imported_data[ ,1]<-multi_substitute(imported_data[ ,1],collection_loc_pattern)
  imported_data[ ,3]<-multi_substitute(imported_data[ ,3],collection_loc_pattern)
  #替换馆藏地
  imported_data[,2]<-multi_substitute(imported_data[,2],as.data.frame(college_pattern))
  #替换学院名称
  b<-data.table(imported_data,key=names(imported_data)[2])
  a<-data.table(college_correct[1],key="学院")
  imported_data<-b[a,nomatch=0]
  imported_data<-as.data.frame(na.omit(imported_data))
  #用替换后的数据和修正表连接过滤掉多余的读者单位
  
  imported_data[ ,4]<-multi_substitute(imported_data[ ,4],collection_pattern)
  #替换书目
  
  b<-data.table(imported_data,key=names(imported_data)[4])
  a<-data.table(collection_filtered,key=names(collection_filtered)[2])
  imported_data<-b[a,nomatch=NA]
  imported_data<-as.data.frame(na.omit(imported_data))
  
  incidence_mat<-create_incidence_matrix(imported_data[,c(2,4)],
                                    college_correct[ ,1],
                                    collection_filtered[,2])
  #imported_data
  incidence_mat
}
incidence_matrix<-import_collection(X2014[1:40000,],13,3,15,12)
visual_data<-import_collection(X2013_2016_6苏大通借通还数据,1,7,3,5)
