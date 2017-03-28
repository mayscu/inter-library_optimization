a<-matrix(0,nrow=29,ncol=524)
dimnames(a)<-list(data.frame(college_correct)[ ,1],match_str[ ,2])
#建立一个行名为学院，列名为书的29X523矩阵
tdata<-data.frame(data.frame(tdata)[ ,2],data.frame(tdata)[ ,4],stringsAsFactors = FALSE)

create_adjacentMatrix <- function(input_matrix, tdTable) {
  for (i in 1:nrow(tdTable)) {
    if (is.na(tdTable[i, 1]) | is.na(tdTable[i, 2])){
      next
    }
    else{
      input_matrix[tdTable[i, 1], tdTable[i, 2]] <- input_matrix[tdTable[i, 1], tdTable[i, 2]]+1
    }
  }
  input_matrix
}
#构建邻接矩阵
output<-create_adjacentMatrix(a,tdata)

  fat_mat<-output[ ,apply(output,2,is.all.zero)]
#移除未被借阅的书

is.all.zero<-function(v){
  sum(v)==0
}
#判断某个向量中的所有元素是否都为0

adjacentMatrix_correct<-function(input_matrix,correct_row,correct_col,s){
  #correct_row是行标的修正值表，n是修正显著程度
  for(i in 1:nrow(input_matrix)){
    for(j in 1:ncol(input_matrix)){
      input_matrix[i,j]<-correct_row[i,2]*s*correct_col[j,2]*s*input_matrix[i,j]
    #i是行标，j是列标，分别根据修正表读取行和列的修正值并相乘
      }
  }
  input_matrix
}

output<-adjacentMatrix_correct(output,data.frame(college_correct),data.frame(collection_processed)[ ,2:3],10)
#对矩阵进行修正

d_b<-data.frame(book=match_str[ ,2],value=apply(output,2,sum))
#统计每本书的度

d_r<-data.frame(college=college_match_rule[ ,1],value=apply(output,1,sum))
#统计每个读者的度

mat_W<-matrix(0,nrow=524,ncol=524)
dimnames(mat_W)<-list(match_str[ ,2],match_str[ ,2])
#构建524X524的资源分配矩阵w

compute_W<-function(W,d_r,d_b,indi_mat){
  #W是资源分配矩阵，indi_mat是关联矩阵
  for(i in 1:nrow(W)){#行标变化
    for(j in 1:ncol(W)){#列标变化
      if(d_b[j,2]==0){
        next
      }
      temp<-0
      for(r in 1:nrow(indi_mat)){#遍历所有读者
        if(d_r[r,2]==0){
          next#跳过借阅量为0的读者
        }
        temp<-temp+((indi_mat[r,i]*indi_mat[r,j])/d_r[r,2])
      }
      W[i,j]<-(temp/d_b[j,2])
    }
  }
  W
}#运行的也太慢了

mat_W_done<-compute_W(mat_W,d_r,d_b,output)

f_mat<-output[ ,!apply(output,2,is.all.zero)]
f_mat<-0
#构建借阅倾向矩阵并初始化
compute_f<-function(W,L,f){
  #f是输出目标矩阵
  for(r in 1:nrow(L)){
    #读者
  for(i in 1:nrow(W)){
    temp<-0
    #行标
    for(j in 1:ncol(W)){
      #列标
      temp<-temp+W[i,j]*L[r,j]
    }
    f[r,i]<-temp
  }
  }
  f
}
f_done<-compute_f(mat_W_done,output,output)
#计算出所有的f评分

compute_delta<-function(f,delta_mat){
    for(b in 1:ncol(f)){
     #针对每本书，计算每个读者的delta值
      sigma_f<-sum(f[ ,b])
      #统计第b列的所有f值之和
      if(sigma_f==0){next}
      #忽略没有被借过的书
      for(r in 1:nrow(f)){
        delta_mat[r,b]<-f[r,b]/sigma_f
      }
     }
  delta_mat
  }

delta_done<-compute_delta(f_done,output)

campus<-list(炳麟图书馆=c("社会学院","政治与公共管理学院","教育学院","金螳螂建筑学院","材料与化学化工学部","纳米科学技术学院","艺术学院","音乐学院","药学院","唐文治书院","文学院","凤凰传媒学院"),
                  本部图书馆=c("王健法学院","外国语学院","数学科学学院","物理与光电能源学部","计算机科学与技术学院","电子信息学院","机电工程学院","护理学院","敬文书院"),
                  敬文图书馆=c("东吴商学院","体育学院"),
                  北区图书馆=c("沙钢钢铁学院","基础医学与生物科学学院","放射医学与防护学院","公共卫生学院"),
                  阳澄湖图书馆=c("纺织与服装工程学院","城市与轨道交通学院"))
#设置校区集合


delta_BL<-delta_done[as.vector(campus$炳麟图书馆,mode="character"),]
delta_BB<-delta_done[as.vector(campus$本部图书馆,mode="character"),]
delta_BQ<-delta_done[as.vector(campus$北区图书馆,mode="character"),]
delta_DQ<-delta_done[as.vector(campus$敬文图书馆,mode="character"),]
delta_YCH<-delta_done[as.vector(campus$阳澄湖图书馆,mode="character"),]

multi_match<-function(df1,df2){
  #对两个数据框进行连接，并默认使用第一个字段作为主键,以df1为基准进行连接
  df1<-data.table(df1,key=names(df1)[1])
  df2<-data.table(df2,key=names(df2)[1])
  df1<-df1[df2,nomatch=NA]
  as.data.frame(df1)
}

a<-multi_match(总数据,collection_processed)
collection_sums<-a[c(4,2)]
collection_sums[524,1]<-d_b[524,1]
collection_sums[is.na(collection_sums)]<-0
#不能直接删掉na，会导致下标不同，所以把NA改成0
collection_sums<-na.omit(collection_sums)
#保证行列数一致

Distribute_A<-function(mat_delta,mat_v){
  #输入delta矩阵和分类馆藏量数据框，得到mat_a作为分配结果
  mat_a<-mat_delta
  mat_a[,]<-0
  for(i in 1:nrow(mat_delta)){
    for(j in 1:ncol(mat_delta)){
  mat_a[i,j]<-as.integer((mat_delta[i,j])*as.numeric(mat_v[j,2]))
  #注意数据类型，要把字符转化成数量型才能参与运算；结果转化成整形，以四舍五入
    }
  }
  mat_a
}

distribution_A_BB<-Distribute_A(delta_BB,collection_sums)
distribution_A_BL<-Distribute_A(delta_BL,collection_sums)
distribution_A_BQ<-Distribute_A(delta_BQ,collection_sums)
distribution_A_DQ<-Distribute_A(delta_DQ,collection_sums)
distribution_A_YCH<-Distribute_A(delta_YCH,collection_sums)