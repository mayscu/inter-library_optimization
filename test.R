import_test_mat<-function(the_file,ll,c,cl,bn){
  test_data<-data.frame(lend_loc=the_file[,ll],读者单位=the_file[ ,c],
                            coll_loc=the_file[ ,cl],索书号=the_file[ ,bn],
                            stringsAsFactors = FALSE)
  #指定相应字段所对应的行号，返回导入后的数据,
  #包括借阅地点、馆藏地点、学院、和索书号

# 
test_data<-na.omit(test_data)

collection_loc_pattern<-data.frame(name=c("炳麟","敬文","北区","本部","阳澄湖","G学报","王健","数科院","^[0-9]"),location=c("炳麟图书馆","敬文图书馆","北区图书馆","本部图书馆","阳澄湖图书馆","本部图书馆","本部图书馆","本部图书馆","本部图书馆"),stringsAsFactors = FALSE)
#馆藏地替换规则
test_data[ ,1]<-multi_substitute(test_data[ ,1],collection_loc_pattern)
test_data[ ,3]<-multi_substitute(test_data[ ,3],collection_loc_pattern)
#替换馆藏地
test_data[,2]<-multi_substitute(test_data[,2],as.data.frame(college_pattern))
#替换学院名称
b<-data.table(test_data,key=names(test_data)[2])
a<-data.table(college_correct[1],key="学院")
test_data<-b[a,nomatch=0]
test_data<-as.data.frame(na.omit(test_data))
#用替换后的数据和修正表连接过滤掉多余的读者单位

test_data[ ,4]<-multi_substitute(test_data[ ,4],collection_pattern)
#替换书目

b<-data.table(test_data,key=names(test_data)[4])
a<-data.table(collection_filtered,key=names(collection_filtered)[2])
test_data<-b[a,nomatch=NA]
test_data<-as.data.frame(na.omit(test_data))

test_mat<-create_incidence_matrix(test_data[,c(2,4)],
                                       college_correct[ ,1],
                                       collection_filtered[,2])
test_mat
}
test_matrix<-import_test_mat(X2013_2016_6苏大通借通还数据,2,7,4,5)
#自动导入测试数据集并生成相关矩阵

compute_fill_rate<-function(test_matrix){
  test_BL<-test_matrix[as.vector(campus$炳麟图书馆,mode="character"),]
  test_BB<-test_matrix[as.vector(campus$本部图书馆,mode="character"),]
  test_BQ<-test_matrix[as.vector(campus$北区图书馆,mode="character"),]
  test_DQ<-test_matrix[as.vector(campus$敬文图书馆,mode="character"),]
  test_YCH<-t(as.matrix(test_matrix[as.vector(campus$阳澄湖图书馆,mode="character"),],nrow=1))
  dimnames(test_YCH)[[1]][1]<-"城市与轨道交通学院"
  #针对只有一行的阳澄湖校区要进行额外设置
  #划分子矩阵
  fr_values<-data.frame(college=dimnames(test_matrix)[[1]],fr=0,stringsAsFactors = FALSE)
  #建立FR表
  compute_fill_rate_campus<-function(test_XX,distribution_A_XX){
    for(i in 1:nrow(test_XX)){
      temp<-vector(mode="numeric",length = ncol(test_XX))
      for(j in 1:ncol(test_XX)){
        temp_sum<-as.numeric(distribution_A_XX[i,j])
        #temp_sum<-as.numeric(colSums(distribution_A_XX,2)[[j]])
        #相应校区书j的馆藏数目
        if(test_XX[i,j]==0){
          next}
        temp[j]<-as.numeric(temp_sum/test_XX[i,j])
      }
      #fr_values[dimnames(test_BB)[[1]][i],2]<-ave(temp)
      fr_values[fr_values$college==dimnames(test_XX)[[1]][i],2]<-ave(na.omit(temp[temp!=0]))[1]
      #向父环境中存取，此时compute_fill_rate_campus的父环境是compute_fill_rate
      #移除na值才能求均值
      #对所有书的涵盖率求均值
      
      assign("fr_values",fr_values,envir=parent.env(environment()))
    }
  }
  #对每个校区分别计算fr值并存入fr表中
  
  
    compute_fill_rate_campus(test_BB,distribution_A_BB)
    compute_fill_rate_campus(test_BL,distribution_A_BL)
    compute_fill_rate_campus(test_BQ,distribution_A_BQ)
    compute_fill_rate_campus(test_DQ,distribution_A_DQ)
    compute_fill_rate_campus(test_YCH,distribution_A_YCH)
    fr_values
}

fr_values<-compute_fill_rate(test_matrix)
fr_log<-ave(na.omit(log(fr_values[,2])))
debug(compute_fill_rate)

test_matrix<-import_test_mat(X2013[1:5000,],13,3,15,12)
distribution_A<-Distribute_A(delta_matrix,collection_sums)
compute_fill_rate_2<-function(test_matrix){
  #针对每一个学院的每一个分类分别计算满足率
  distribution_A<-Distribute_A(delta_matrix,collection_sums)
  fr_matrix<-test_matrix
  fr_matrix[,]<-0
  for(i in 1:nrow(fr_matrix)){
    for(j in 1:ncol(fr_matrix)){
      tryCatch({
        if(distribution_A[i,j]!=0&test_matrix[i,j]!=0){
        fr_matrix[i,j]<-as.numeric(distribution_A[i,j])/as.numeric(test_matrix[i,j])}
      },
        error=function(e){fr_matrix[i,j]<-0.1 },
        warnings=function(e){fr_matrix[i,j]<-0.1 }
      )
    }
  }
  # fr_matrix_BL<-fr_matrix[as.vector(campus$炳麟图书馆,mode="character"),]
  # fr_matrix_BB<-fr_matrix[as.vector(campus$本部图书馆,mode="character"),]
  # fr_matrix_BQ<-fr_matrix[as.vector(campus$北区图书馆,mode="character"),]
  # fr_matrix_DQ<-fr_matrix[as.vector(campus$敬文图书馆,mode="character"),]
  # fr_matrix_YCH<-t(as.matrix(test_matrix[as.vector(campus$阳澄湖图书馆,mode="character"),],nrow=1))
  # dimnames(fr_matrix_YCH)[[1]][1]<-"城市与轨道交通学院"
  
  fr_martrix_dataframe<-function(fr_matrix){
    df<-data.frame(书籍="",fr=0,学院="",stringsAsFactors = FALSE)
    for(i in 1:nrow(fr_matrix)){
      for(j in 1:ncol(fr_matrix)){
        temp<-data.frame(书籍=dimnames(fr_matrix)[[2]][j],
                           fr=fr_matrix[i,j],
                           学院=dimnames(fr_matrix)[[1]][i])
        df<-rbind(df,temp)
        
      }
    }
    df
  }
  fr_df<-fr_martrix_dataframe(fr_matrix)
  fr_df#把fr矩阵铺平成数据框
}
fr_df_BB<-fr_df[as.vector(campus$炳麟图书馆,mode="character"),]
(a<-qplot(学院,fr,data=fr_df,geom="boxplot",alpha=I(1/20),
        color=学院))

#对每个学院的需求满足情况进行可视化
fr_df[,2]<-log(fr_df[,2])
fr_df_BL<-merge(fr_df,as.data.frame(campus$炳麟图书馆,mode="character")
               ,by.x = "学院",by.y="campus$炳麟图书馆",all.x = FALSE)
fr_df_BB<-merge(fr_df,as.data.frame(campus$本部图书馆,mode="character")
                ,by.x = "学院",by.y="campus$本部图书馆",all.x = FALSE)
fr_df_DQ<-merge(fr_df,as.data.frame(campus$敬文图书馆,mode="character")
                ,by.x = "学院",by.y="campus$敬文图书馆",all.x = FALSE)
fr_df_BQ<-merge(fr_df,as.data.frame(campus$北区图书馆,mode="character")
                ,by.x = "学院",by.y="campus$北区图书馆",all.x = FALSE)
fr_df_YCH<-merge(fr_df,as.data.frame(campus$阳澄湖图书馆,mode="character")
                ,by.x = "学院",by.y="campus$阳澄湖图书馆",all.x = FALSE)
#按校区切分fr_df


(BL<-qplot(学院,fr,data=fr_df_BL,geom="boxplot",alpha=I(1/20),
        color=学院,font-family="微软雅黑"))+
(BB<-qplot(学院,fr,data=fr_df_BB,geom="boxplot",alpha=I(1/20),
             color=学院))


RColorBrewer::display.brewer.all()

qplot(学院,fr,data=fr_df_BB,geom="boxplot",alpha=I(1/20),
        color=学院)+
  theme(axis.text.x=element_text(angle=60,size=5,family = "宋体"),
        axis.title.x=element_text(family="宋体"))
the_theme<-theme(axis.text.x=element_text(angel=90))
BL+theme(axis.text.x=element_text(angel=90))
theme_set(the_theme)

BB<-ggplot(fr_df_BB,aes(学院,log(fr),main="本部图书馆"),ylim=c(0,8))+
  labs(title = "本部图书馆")+
  theme(axis.text.x=element_text(angle=60,size=5,family = "宋体"),
                                  axis.title.x=NULL,
                                  plot.title=element_text(family="宋体"))+
  geom_boxplot(aes(colour=学院))+ 
  theme(legend.position="none")+xlab(NULL)+ylab("log(fr)")

BL<-ggplot(fr_df_BL,aes(学院,log(fr)),ylim=c(0,8))+
  labs(title = "炳麟图书馆")+
  theme(axis.text.x=element_text(angle=60,size=5,family = "宋体"),
        axis.title.x=NULL,plot.title=element_text(family="宋体"))+
  geom_boxplot(aes(colour=学院))+
  theme(legend.position="none")+xlab(NULL)+ylab("log(fr)")

BQ<-ggplot(fr_df_BQ,aes(学院,log(fr)),ylim=c(0,8))+
  labs(title = "北区图书馆")+
  theme(axis.text.x=element_text(angle=60,size=5,family = "宋体"),
        axis.title.x=NULL,plot.title=element_text(family="宋体"))+
  geom_boxplot(aes(colour=学院))+
  theme(legend.position="none")+xlab(NULL)+ylab("log(fr)")

DQ<-ggplot(fr_df_DQ,aes(学院,log(fr),main="敬文图书馆"),ylim=c(0,8))+
  labs(title = "敬文图书馆")+
  theme(axis.text.x=element_text(angle=60,size=5,family = "宋体"),plot.title=element_text(family="宋体"))+
  geom_boxplot(aes(colour=学院))+
  theme(legend.position="none")+xlab(NULL)+ylab("log(fr)")

YCH<-ggplot(fr_df_YCH,aes(学院,log(fr)))+
  labs(title = "阳澄湖图书馆")+
  theme(axis.text.x=element_text(angle=60,size=5,family = "宋体"),
        axis.title.x=NULL,plot.title=element_text(family="宋体"))+
  geom_boxplot(aes(colour=学院))+
  theme(legend.position="none")+xlab(NULL)+ylab("log(fr)")
YCH+scale_y_discrete(limits=c(0,8))
#抛弃掉limits以外的值
# YCH<-YCH+coord_cartesian(ylim=c(0,8),expand=FALSE)
# BL<-BL+coord_cartesian(ylim=c(0,8),expand=FALSE)
# BB<-BB+coord_cartesian(ylim=c(0,8),expand=FALSE)
# BQ<-BQ+coord_cartesian(ylim=c(0,8),expand=FALSE)
# DQ<-DQ+coord_cartesian(ylim=c(0,8),expand=FALSE)
#控制坐标轴的显示范围，统一坐标轴

library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(3,2)))
vplayout<-function(x,y){
  viewport(layout.pos.row=x,layout.pos.col=y)}
print(BL,vp=vplayout(1,1))
print(BB,vp=vplayout(1,2))
print(DQ,vp=vplayout(2,2))
print(BQ,vp=vplayout(2,1))
print(YCH,vp=vplayout(3,1))

#分割显示五个校区的数据
fr_df<-fr_df[fr_df[,3]!="唐文治书院",]
fr_df<-fr_df[fr_df[,3]!="敬文书院",]
fr_df<-fr_df[fr_df[,2]!=0,]
nrow(fr_df[fr_df[,2]>1,])/nrow(fr_df)
ave(fr_df[,2])
# boxplot_setting<- data.frame(
#   x=fr_df$学院,
#   y=fr_df$fr,
#   y0 = min(y),
#   y25 = quantile(y, 0.3),
#   y50 = median(y),
#   y75 = quantile(y, 0.7),
#   y100 = max(y)
# )
# ggplot(na.omit(boxplot_setting),aes(x,y))+
#   theme(axis.text.x=element_text(angle=60,size=8,family = "宋体"),
#         axis.title.x=NULL)+
#   geom_boxplot(aes(colour=x,ymin=y0,lower=y25,middle=y50,upper=y75,ymax=y100),stat="identity")+
#   theme(legend.position="none")+xlab(NULL)+ylab("log(fr)")

ggplot(na.omit(fr_df),aes(学院,log(fr)))+
  theme(axis.text.x=element_text(angle=60,size=8,family = "宋体"),
        axis.title.x=NULL)+
  geom_boxplot(aes(colour=学院))+
  theme(legend.position="none")+xlab(NULL)+ylab("log(fr)")


# ##绘制distribution的马赛克图
# install.packages("vcd")
# library(vcd)
# a<-data.frame(书籍=dimnames(distribution_A_DQ)[[2]],
#                 数量=colSums(distribution_A_DQ))
# mosaic(~书籍+数量,data=a)
# View()
# library(ggplot2)
# qplot(y=数量,data=a,color=factor(书籍))+
#   theme(legend.position="none",text=element_text(family = "宋体"))+coord_polar()+geom_bar()
# 
# ggplot(a,aes(x=factor(书籍),fill=factor(书籍),xlab=NA))+geom_bar(width = 1)+theme(legend.position="none",text=element_text(family = "宋体"))+coord_polar()+xlab(NA)
#     

14889/15081

write.csv(a,file = ".../Desktop/")
write.table(import_data,"import_data.csv",sep=",",row.names=FALSE)
write.table(import_data,"correct_incidence_matrix.csv",sep=",",row.names=FALSE)
write.table(collection_pattern,"collection_pattern.csv",sep=",",row.names=FALSE)
write.table(collection_correct,"collection_correct.csv",sep=",",row.names=FALSE)
write.table(college_correct,"college_correct.csv",sep=",",row.names=FALSE)
write.table(college_pattern,"college_pattern.csv",sep=",",row.names=FALSE)
write.table(distribution_A_BB,"distribution_A_BB.csv",sep=",",row.names=FALSE)
write.table(distribution_A_BL,"distribution_A_BL.csv",sep=",",row.names=FALSE)
write.table(distribution_A_BQ,"distribution_A_BQ.csv",sep=",",row.names=FALSE)
write.table(distribution_A_DQ,"distribution_A_DQ.csv",sep=",",row.names=FALSE)
write.table(distribution_A_YCH,"distribution_A_YCH.csv",sep=",",row.names=FALSE)
write.table(fr_matrix_YCH,"distribution_A_YCH.csv",sep=",",row.names=FALSE)
test_data
test_data
