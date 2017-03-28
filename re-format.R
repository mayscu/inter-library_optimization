library(stringr)
library(igraph)
library(dplyr)
library(data.table)


import_collection<-function(the_file,ll,c,cl,bn){
  imported_data<-data.frame(lend_loc=the_file[,ll],college=the_file[ ,c],
                            coll_loc=the_file[ ,cl],book_np=the_file[ ,bn],
                            stringsAsFactors = FALSE)
  imported_data
#指定相应字段所对应的行号，返回导入后的数据,
  #包括借阅地点、馆藏地点、学院、和索书号
}
# 
imported_data<-import_collection(X2013_2016_6苏大通借通还数据,2,7,4,5)
imported_data<-na.omit(imported_data)

college_correct<-data.frame(college_name=各学院人数[ ,1],college_weight=各学院人数[ ,2],stringsAsFactors = FALSE)
#导入学院的修正值

collection_pattern<-data.frame(book_no=中图分类法_简表_详表_[ ,1],book_name=中图分类法_简表_详表_[ ,2],stringsAsFactors = FALSE)
#导入分类号映射条件

collection_correct<-data.table(book_no=总数据[ ,1],book_correct=总数据[ ,3])
#导入简化后的修正值表

df_join<-function(df1,df2){
  #对两个数据框进行连接，并默认使用第一个字段作为主键,以df1为基准进行连接
  df1<-data.table(df1,key=names(df1)[1])
  df2<-data.table(df2,key=names(df2)[1])
  df1<-df1[df2,nomatch=NA]
  as.data.frame(df1)
}


collection_filtered<-df_join(collection_pattern,collection_correct)
collection_filtered<-na.omit(collection_filtered)
#将馆藏修正值和简化后的修正表形成要实用的修正表

multi_substitute<-function(raw_strs,patterns){
  #patterns是匹配条件表,raw_strs是待处理的文本
  library("stringr")
  for(i in 1:nrow(patterns)){
    raw_strs[str_detect(raw_strs,patterns[i,1])]<-patterns[i,2]
    
  } 
  
  raw_strs
  #输出处理后的字符串
}
collection_loc_pattern<-data.frame(name=c("炳麟","敬文","北区","本部","阳澄湖","G学报","王健","数科院"),location=c("炳麟图书馆","敬文图书馆","北区图书馆","本部图书馆","阳澄湖图书馆","本部图书馆","本部图书馆","本部图书馆"),stringsAsFactors = FALSE)
#馆藏地替换规则
imported_data[ ,1]<-multi_substitute(imported_data[ ,1],collection_loc_pattern)
imported_data[ ,3]<-multi_substitute(imported_data[ ,3],collection_loc_pattern)
#替换馆藏地
imported_data[,2]<-multi_substitute(imported_data[,2],as.data.frame(college_pattern))
#替换学院名称
b<-data.table(imported_data,key="读者单位")
a<-data.table(college_correct[1],key="学院")
imported_data<-b[a,nomatch=0]
imported_data<-as.data.frame(na.omit(imported_data))
#用替换后的数据和修正表连接过滤掉多余的读者单位

collection_pattern<-collection_filtered[,1:2]
collection_pattern[617,]<-c("^[0-9△]","others")
#添加额外匹配项
imported_data[ ,4]<-multi_substitute(imported_data[ ,4],collection_pattern)
#替换书目

b<-data.table(imported_data,key="索书号")
a<-data.table(collection_filtered,key=names(collection_filtered)[2])
imported_data<-b[a,nomatch=NA]
imported_data<-as.data.frame(na.omit(imported_data))
#######################################以上为预处理环节

create_incidence_matrix<-function(two_dim_table,rname,cname){
  #导入二维表和相关矩阵的行列信息(作为数据框输入)
  mat<-matrix(0,nrow=nrow(as.data.frame(rname)),ncol = nrow(as.data.frame(cname)))
  dimnames(mat)<-list(as.vector(rname,mode="character"),as.vector(cname,mode="character"))
  for(i in 1:nrow(two_dim_table)){
    if (is.na(two_dim_table[i, 1]) | is.na(two_dim_table[i, 2])){
    #if (two_dim_table[i, 1] %in% rname | two_dim_table[i, 2]){
      next
    }
    else{
     # if(sum(apply(rname,1,str_detect,pattern=two_dim_table[i,1]))&sum(apply(cname,1,str_detect,pattern=two_dim_table[i,1])))
     #   {next}
      tryCatch({
      mat[two_dim_table[i, 1], two_dim_table[i, 2]] <- mat[two_dim_table[i, 1], two_dim_table[i, 2]]+1
      },
      error=function(e){""}
      )
      }
  }
  mat
}

incidence_matrix<-create_incidence_matrix(imported_data[,c(2,4)],
                        college_correct[ ,1],
                        collection_filtered[,2])
#生成相关矩阵

is.all.zero<-function(v){
  sum(v)==0
}#判断某列是否全为0

#incidence_mat_simplified<-incidence_mat[ ,-(apply(incidence_mat,2,is.all.zero))]
#防止下标乱掉，且没被借阅的书很少，就先不处理

correct_incidence_matrix<-function(mat,correct_row,correct_col,s){
  #correct_row是行标的修正值表，s是修正显著程度
  # the_row<-as.data.frame(dimnames(mat)[1],stringsAsFactors = FALSE)
  # the_col<-as.data.frame(dimnames(mat)[2],stringsAsFactors = FALSE)
  # #读取行标和列标
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      mat[i,j]<-as.numeric(correct_row[i,2])*as.numeric(correct_col[j,2])*s*as.numeric(mat[i,j])
      #i是行标，j是列标，分别根据修正表读取行和列的修正值并相乘
    }
  }
  mat
}
incidence_matrix<-import_collection(X2014[1:40000,],13,3,15,12)

incidence_mat_corrected<-correct_incidence_matrix(incidence_matrix,
                                                  college_correct,collection_filtered[ ,2:3],1)
#对矩阵进行修正


sum_books<-data.frame(books=collection_filtered$马克思主义.列宁主义.毛泽东思想.邓小平理论,value=apply(incidence_mat_corrected,2,sum))
#统计每本书的值

sum_colleges<-data.frame(college=college_correct$学院,value=rowSums(incidence_mat_corrected))
#统计每个学院的度

compute_W<-function(mat,sum_colleges,sum_books){
  W<-matrix(0,nrow = ncol(mat),ncol = ncol(mat))
  dimnames(W)<-list(as.vector(sum_books$books,mode="character"),as.vector(sum_books$books,mode="character"))
  #W是资源分配矩阵，indi_mat是关联矩阵
  for(i in 1:nrow(W)){#行标变化
    for(j in 1:ncol(W)){#列标变化
      if(sum_books[j,2]==0){
        next
      }
      temp<-0
      for(r in 1:nrow(mat)){#遍历所有读者
        if(sum_colleges[r,2]==0){
          next#跳过借阅量为0的读者
        }
        temp<-temp+((mat[r,i]*mat[r,j])/sum_colleges[r,2])
      }
      W[i,j]<-(temp/sum_books[j,2])
    }
  }
  W
}#运行的也太慢了

W_matrix<-compute_W(incidence_mat_corrected,sum_colleges,sum_books)

compute_f<-function(mat_W,mat_corrected){
  #f是输出目标矩阵,mat_Raw是修正后的相关矩阵
  f<-mat_corrected
  f[,]<-0
  for(r in 1:nrow(mat_corrected)){
    #读者
    for(i in 1:nrow(mat_W)){
      temp<-0
      #行标
      for(j in 1:ncol(mat_W)){
        #列标
        temp<-temp+mat_W[i,j]*mat_corrected[r,j]
      }
      f[r,i]<-temp
    }
  }
  f
}
f_matrix<-compute_f(W_matrix,incidence_mat_corrected)
#计算出所有的f评分

compute_delta<-function(f_matrix){
  delta_mat<-f_matrix
  delta_mat[,]<-0
  for(b in 1:ncol(f_matrix)){
    #针对每本书，计算每个读者的delta值
    sigma_f<-sum(f_matrix[ ,b])
    #统计第b列的所有f值之和
    if(sigma_f==0){next}
    #忽略没有被借过的书
    for(r in 1:nrow(f_matrix)){
      delta_mat[r,b]<-f_matrix[r,b]/sigma_f
    }
  }
  delta_mat
}

delta_matrix<-compute_delta(f_matrix)

campus<-list(炳麟图书馆=c("社会学院","政治与公共管理学院","教育学院","金螳螂建筑学院","材料与化学化工学部","纳米科学技术学院","艺术学院","音乐学院","药学院","唐文治书院","文学院","凤凰传媒学院"),
                  本部图书馆=c("王健法学院","外国语学院","数学科学学院","物理与光电能源学部","计算机科学与技术学院","电子信息学院","机电工程学院","护理学院","敬文书院"),
                  敬文图书馆=c("东吴商学院","体育学院"),
                  北区图书馆=c("沙钢钢铁学院","基础医学与生物科学学院","放射医学与防护学院","公共卫生学院","纺织与服装工程学院"),
                  阳澄湖图书馆="城市与轨道交通学院")
#设置校区集合

delta_BL<-delta_matrix[as.vector(campus$炳麟图书馆,mode="character"),]
delta_BB<-delta_matrix[as.vector(campus$本部图书馆,mode="character"),]
delta_BQ<-delta_matrix[as.vector(campus$北区图书馆,mode="character"),]
delta_DQ<-delta_matrix[as.vector(campus$敬文图书馆,mode="character"),]
delta_YCH<-matrix(delta_matrix[as.vector(campus$阳澄湖图书馆,mode="character"),],nrow=1)
#对每个校区取DELTA的子集

Distribute_A<-function(mat_delta,mat_v){
  #输入delta矩阵和分类馆藏量数据框，得到mat_a作为分配结果，mat_v是馆藏量
  mat_a<-mat_delta
  mat_a[ , ]<-0
  for(i in 1:nrow(mat_delta)){
    for(j in 1:ncol(mat_delta)){
      mat_a[i,j]<-as.integer((mat_delta[i,j])*as.numeric(mat_v[j,2]))
      #注意数据类型，要把字符转化成数量型才能参与运算；结果转化成整形，以四舍五入
    }
  }
  mat_a
}
collection_sums<-data.frame(书籍=dimnames(incidence_matrix)[[2]],馆藏量=colSums(incidence_matrix))

distribution_A_BB<-Distribute_A(delta_BB,collection_sums)
distribution_A_BL<-Distribute_A(delta_BL,collection_sums)
distribution_A_BQ<-Distribute_A(delta_BQ,collection_sums)
distribution_A_DQ<-Distribute_A(delta_DQ,collection_sums)
distribution_A_YCH<-Distribute_A(delta_YCH,collection_sums)

sum_distribute<-function(distribute_matrix){
  sum_distribute_matrix<-colSums(distribute_matrix)
  sum_distribute_matrix
}
sum_distribution_A_BB<-sum_distribute(distribution_A_BB)
sum_distribution_A_BL<-sum_distribute(distribution_A_BL)
sum_distribution_A_BQ<-sum_distribute(distribution_A_BQ)
sum_distribution_A_DQ<-sum_distribute(distribution_A_DQ)
sum_distribution_A_YCH<-sum_distribute(distribution_A_YCH)

write.table(delta_matrix,"delta矩阵.csv")


