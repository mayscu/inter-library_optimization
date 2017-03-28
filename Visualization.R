




incidence_mat_simplified[is.na(incidence_mat_simplified)]<-0
#移除NA值
incidence_mat_simplified<-incidence_matrix[rowSums(incidence_matrix)!=0,colSums(incidence_matrix)!=0]

graph_from_adjacency_matrix(incidence_mat_simplified,mode=c("directed"),weighted = "借阅量")
g2<-graph_from_incidence_matrix(incidence_mat_simplified,directed = FALSE,weighted ="weight",
                                multiple = FALSE)
g2<-graph_from_incidence_matrix(incidence_matrix,directed = FALSE,weighted ="weight",
                                multiple = FALSE)
#从关联矩阵导入网络关系
g1<-graph_from_data_frame(tdata,directed = TRUE)
#从矩阵导入网络关系
plot.igraph(g1,vertex.size=4,vertex.label=NA,
            edge.arrow.size=0.1,vertex.color=rainbow(15),
            mark.groups = list(c(1,2,3,4,5),c(6,7,8)),
            mark.col = rainbow(10),layout=layout.sugiyama
)
#绘制一个普通的图
is.bipartite(g1)
tkplot(g1)
groups(g1)

plot.igraph(g2,
            vertex.label=NA,vertex.size=4,edge.color=3
)
#绘制二部图
aaa<-layout_as_bipartite(g2,layout=layout_aaa,types=NULL,hgap=30,vgap=100)
#定义二部图的定义形式

campus_graph<-list(炳麟图书馆=c("社会学院","政治与公共管理学院","教育学院","金螳螂建筑学院","材料与化学化工学部","纳米科学技术学院","艺术学院","音乐学院","药学院","文学院","凤凰传媒学院"),
                  本部图书馆=c("王健法学院","外国语学院","数学科学学院","物理与光电能源学部","计算机科学与技术学院","电子信息学院","机电工程学院","护理学院"),
                  敬文图书馆=c("东吴商学院","体育学院"),
                  北区图书馆=c("沙钢钢铁学院","基础医学与生物科学学院","放射医学与防护学院","公共卫生学院","纺织与服装工程学院"),
                  阳澄湖图书馆="城市与轨道交通学院")

layout_aaa
plot(g2, layout = layout_as_bipartite,mark.groups=campus_graph,
     vertex.color=c("green","cyan")[V(g2)$type+1],vertex.label=NA,
     vertex.shape=c("circle","square"),
     vertex.size=2)

write.table(incidence_mat_simplified,file="hehehe",sep=",")
walktrap.community(g2)


com=edge.betweenness(g2)
V(g2)$sg=com$membership
V(g2)$name[V(g2)$type==FALSE]


edge.width=E(g2)$weight
#边宽度与权重建立映射
E(g2)$curved<-0.2
#改变节点弧度
E(g2)$color=rgb(119,136,153,80,maxColorValue = 255)
V(g2)$color=c(rgb(72,61,139,80,maxColorValue = 255),rgb(176,196,222,80,maxColorValue = 255))[as.numeric(V(g2)$type)+1]
#根据节点type属性改变颜色
V(g2)$shape=c("square","circle")[as.numeric(V(g2)$type)+1]
#改变节点形状
#V(g2)$size[1:29]=degree(g2)[1:29]/ave(degree(g2)[1:29])[1] 引入平均值来归一化权重
V(g2)$size[1:29]=degree(g2)[1:29]/10
V(g2)$size[-(1:29)]=degree(g2)[-(1:29)]
plot(g2,layout=layout.grid,
               vertex.size=V(g2)$size,
                vertex.color=V(g2)$color,
               edge.width=0.5*(E(g2)$weight)/(ave(E(g2)$weight)[1]),
                 #mark.groups=campus_graph,
                edge.arrow.size=0.08,edge.color=E(g2)$color,
               vertex.frame.color=NA,margin=rep(0,4),vertex.label=V(g2)$name,
               vertex.label.cex=0.6, vertex.label.family="微软雅黑"
     )

g4<-graph_from_adjacency_matrix(
  visual_matrix_campus,mode=c("directed"),
  weighted = "借阅量")
#画校区馆藏流动图
plot(g4)
edge.width=E(g2)$weight/ave()
#边宽度与权重建立映射
E(g4)$curved<-0.2
#改变节点弧度
E(g4)$color=rgb(51,0,0,95,maxColorValue = 255)
V(g4)$color=c(rgb(153,204,255,80,maxColorValue = 255),
              rgb(153,204,204,80,maxColorValue = 255),
              rgb(153,153,204,80,maxColorValue = 255),
              rgb(153,102,102,80,maxColorValue = 255),
              rgb(153,102,153,80,maxColorValue = 255))
V(g4)$size=degree(g4)*5
E(g4)$width=3*E(g4)$借阅量/ave(E(g4)$借阅量)[1]
E(g4)$width[E(g4)$width<1]<-E(g4)$width[E(g4)$width<1]*5
m


plot(g4,layout=layout_nicely,
     vertex.size=V(g4)$size,
     vertex.color=V(g4)$color,
     edge.width=E(g4)$width,
     edge.label=E(g4)$借阅量,
     edge.label.color=rgb(0,0,51,maxColorValue = 255),
     edge.label.cex=.8,
     edge.label.family="微软雅黑",
     #mark.groups=campus_graph,
     edge.arrow.size=0.8*E(g4)$借阅量/ave(E(g4)$借阅量)[1],edge.color=E(g4)$color,
     vertex.frame.color=NA,margin=rep(0,4),vertex.label=V(g4)$name,
     vertex.label.cex=1, vertex.label.family="微软雅黑"
)

distribution_A_campus<-matrix(0,nrow=5,ncol=520)
dimnames(distribution_A_campus)[[2]]<-dimnames(distribution_A)[[2]]
dimnames(distribution_A_campus)[[1]]<-c("炳麟图书馆","本部图书馆","敬文图书馆","北区图书馆","阳澄湖图书馆")
distribution_A_campus[1,]<-colSums(distribution_A_BL)
distribution_A_campus[2,]<-colSums(distribution_A_BB)
distribution_A_campus[3,]<-colSums(distribution_A_DQ)
distribution_A_campus[4,]<-colSums(distribution_A_BQ)
distribution_A_campus[5,]<-colSums(distribution_A_YCH)

distribution_A_campus<-distribution_A_campus[,colSums(distribution_A_campus)!=0]



g4<-graph_from_incidence_matrix(
  distribution_A_campus,directed = FALSE,weighted ="馆藏量",
                                multiple = FALSE)

V(g4)$color=c(rgb(153,204,255,90,maxColorValue = 255),
              rgb(153,204,204,90,maxColorValue = 255),
              rgb(153,153,204,90,maxColorValue = 255),
              rgb(153,102,102,90,maxColorValue = 255)
              ,rgb(153,102,153,90,maxColorValue = 255))
V(g4)$color[1:5]<-rainbow(5)
V(g4)$size=log(degree(g4))+1
V(g4)$size[1:5]=log(rowSums(distribution_A_campus))
V(g4)$size[6:265]<-log(colSums(distribution_A_campus))
E(g4)$width=log(E(g4)$馆藏量)+1
E(g4)$color=rgb(119,136,153,30,maxColorValue = 255)
E(g4)[E(g4)$馆藏量>300]$color=rgb(25,25,112,80,maxColorValue = 255)
E(g4)$curved<-0.2
#改变节点弧度
V(g4)$name[1:5]<-c("炳麟","本部","敬文","北区","阳澄湖")
V(g4)$label.color<-rgb(119,136,153,95,maxColorValue = 255)
V(g4)$label.color[1:5]<-rgb(70,130,180,maxColorValue = 255)
V(g4)$label.size<-0.6
V(g4)$label.size[1:5]<-1
V(g4)$label.distance<-0.1
V(g4)$label.distance[1:5]<--0.4

V(g4)$shape=c("square","circle")[as.numeric(V(g4)$type)+1]
plot(g4,layout=g4$layout,
     vertex.size=V(g4)$size,
     vertex.color=V(g4)$color,
     # edge.width=E(g4)$width,
     #mark.groups=campus_graph,
     edge.arrow.size=E(g4)$馆藏量,edge.color=E(g4)$color,
     vertex.frame.color=NA,vertex.label=V(g4)$name,
     vertex.label.cex=V(g4)$label.size, vertex.label.color=V(g4)$label.color,vertex.label.family="微软雅黑",
     vertex.label.dist=V(g4)$label.distance,
     margin=0
)
E(g4)[2]

g4$layout<-layout_as_bipartite(g4,types=NULL,maxiter = 10000)
g4$layout[1:5]<-c(2000,3000,4000,5000,6000)
g4$layout<-layout.grid(g4)
g4$layout
