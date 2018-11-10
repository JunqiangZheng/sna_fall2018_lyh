###############################################################################################
#数据处理
###############################################################################################
    
    ###读入原始数据，选择前两列###
#all<- read.csv("/Users/fanouyang/Desktop/sna_2018fall/all_data.csv")
#all<-all %>% select(vert1_id,vert2_id)  

    ###利用reshape2库中的dcast函数，把行列形式转成交互矩阵，wide型数据###
#library(reshape2)
#all_matrix<-all %>% dcast(vert1_id~vert2_id)
#write.csv(all_matrix,file = "/Users/fanouyang/Desktop/sna_2018fall/all_matrix.csv") 
    ###写出交互矩阵数据，再重新读入，现已有交互矩阵文件，直接读入###
all_matrix<- read.csv("D:/zju/课程/欧阳/slides/all_matrix.csv",row.names=1)
    ###利用melt函数，把矩阵转化成行列形式，long型数据###
#all_edge <- melt(all_matrix)
#write.csv(all_edge,file = "/Users/fanouyang/Desktop/sna_2018fall/all_edge.csv") 
    ###写出再读入，已有文件所以直接读入，同上###
all_edge<- read.csv("D:/zju/课程/欧阳/slides/all_edge.csv")

###############################################################################################
#network level
###############################################################################################

library(sna)
    ###使用SNA包的gplot网络可视化函数在network层级进行处理###
    ###参数：矩阵数据，有向图，显示vertex顶点标签，标签字符大小character expansion，顶点大小，顶点颜色###
    ###尝试了其他显示修改
gplot(all_matrix,gmode="digraph", displaylabels=TRUE,label.cex=0.7,label.pos=5,
      vertex.cex=2.5,vertex.col="cornflowerblue",edge.len=FALSE)
    ###把矩阵转化成网络，下面对网络进行操作###
overallnet=network(all_matrix)

###############################################################################################
#node level
###############################################################################################
   
    ###计算in/outdegree，分别是我贡献和我收到的交互信息###
id<-degree(overallnet,gmode="digraph",cmode="indegree")
od<-degree(overallnet,gmode="digraph",cmode="outdegree")

    ###计算betweenness，对不同小组的连接能力，rescale默认为假，若为真则分数总和为1###
bet1=betweenness(overallnet,rescale=T)
bet2=betweenness(overallnet)

    ###计算closeness，与其他人的紧密关系，散播和得到消息的能力高，rescale同上###
clo1=closeness(overallnet,rescale=T)
clo2=closeness(overallnet)

    ###计算eigenvector，中心顶点间的比重关系，越高则连接其他顶点的可能性越大，rescale同上###
eig1=evcent(overallnet,rescale=T)
eig2=evcent(overallnet)

    ###使用SNA包的gplot网络可视化函数在network层级对node等进行处理###
    ###参数：数据，顶点大小，无向图
    ###      顶点标签不加框线，标签大小，位置不偏移，颜色
    ###      顶点颜色基于in/degree设置，模式RGB，边线颜色
    ###      标签内容是网络顶点名，边线宽度基于矩阵设置
    ###      模式基于fruchtermanreingold的gplot.layout函数设置
gplot(overallnet, vertex.cex=(id+od)^0.5/2, gmode="graph",
      boxed.labels=FALSE,label.cex=0.7, label.pos=5, label.col="grey17",
      vertex.col=rgb((id+od)/max(id+od),0,(id+od)/max(id+od)),edge.col="grey17",
      label=network.vertex.names(overallnet),edge.lwd=all_matrix/2,mode = "fruchtermanreingold")

    ###使用SNA包的plot.sociomatrix颜色强度绘制函数对network进行处理，只要有交互就会有显示###
    ###参数：数据，不标记对角线，标题，可选标签扩展因子，Y/X的比例
plot.sociomatrix(overallnet, diaglab = FALSE, 
                 main = "The overall interaction", cex.lab = 0.4, asp = 0.5)


    ###自定义函数：将网络节点的in/outdegree等五个参数建立二维列表并排序
    ###按照网络节点对象和参数值进行排列
    ###cbind取值到矩阵、列表等，%v%从矩阵、列表取特定值/列
node.centrality<-function(list.net){
  lapply(list.net,
         function(z){
           x<-z
           ###计算五个参数，并命名行列两个维度的名称
           central.nodes<-cbind(degree(x,cmode="indegree"), degree(x,cmode="outdegree"),evcent(x,rescale=T),betweenness(x,rescale=T),
                                closeness(x,cmode="directed",rescale=T))
           colnames(central.nodes)<-c("idegree","odegree","eigen","betweenness","closeness")
           rownames(central.nodes)<-z%v%"vertex.names"
           ###对二维列表的每一列进行排序
           #o1<-order(central.nodes[,1],decreasing =TRUE)
           #o2<-order(central.nodes[,2],decreasing =TRUE)
           #o3<-order(central.nodes[,3],decreasing =TRUE)
           #o4<-order(central.nodes[,4],decreasing =TRUE)
           #o5<-order(central.nodes[,5],decreasing =TRUE)
           #list(ranking=central.nodes,order=cbind(o1,o2,o3,o4,o5))
         })
}
    ###使用自定义函数验证
nc<-node.centrality(list(overallnet))
#nc

###############################################################################################
#graph level
###############################################################################################
    
    ###SNA包centralization函数计算集中化程度，结果返回一个中心值
centralization(overallnet,degree)
centralization(overallnet,degree,cmode="outdegree")
centralization(overallnet,degree,cmode="indegree")
centralization(overallnet, betweenness)
centralization(overallnet, closeness)
centralization(overallnet, evcent)
    ###计算网络大小、密度、交互度，平均交互度，入向和出向的平均
network.size(overallnet)            #size
gden(overallnet,mode="graph")       #density
degree(overallnet)                  #all degreee
mean(degree(overallnet))            #average degree
sum(id)/20                          #average in degree
sum(od)/20                          #average out degree

    ###计算网络中信息传递的程度，先计算数量，后评估关系
    ###dyad.census函数结果分三类：相互（A给B并B给A），不对称（A给B或B给A），空（无关）
    ###此处是66，62，62
dyad.census(overallnet)
    ###network.dyadcount函数结果表示网络中二元组的数量，可以指定对象X，na.omit真则忽略边缘方向
    ###此处针对overallnet整个网络=66+62+62=190，双向380，未忽略
network.dyadcount(overallnet, na.omit = F)
    ###network.edgecount函数计算整个网络的连接线，此处194条边线，未忽略
network.edgecount(overallnet, na.omit = F)
    
    ###计算整个网络的二元互易性，可以理解为邻居关联程度，单独个体有邻居为1，无邻居为0
    ###此处计算整体结果，结果必属于(0,1]，只有空图或全互联为1
    ###几种具体测量方式：默认dyadic
    #---edgewise= num((a,b)==(b,a))/all
    #---dyadic= num(边界)/非空
    #---dyadic.nonnull= 相互/非空，此处=66/66+62=0.515625
grecip(overallnet, measure = "edgewise")
grecip(overallnet, measure = "dyadic")
grecip(overallnet, measure = "dyadic.nonnull")

    ###transitivity
    ###计算整个网络的三元信息传递性，分为强传递和弱传递（默认弱），丢弃缺省三元组（a/b/c=null）
    #---弱a-> b-> c => a-> c。单向推出
    #---强a-> b-> c <=> a-> c。双向等价
gtrans(overallnet)

    ###hierarchy
    ###计算整个网络的等级分数,整体不对称程度越大，等级层次结构就越多
    #---reciprocity方式=1-grecip分数（默认dyadic）
    #---krackhardt方式，如非对称非空对象之间无往返路径则值=1，else=0，累计分数判断等级
hierarchy(overallnet, measure = "reciprocity")
hierarchy(overallnet, measure = "krackhardt")


    ###弱连接的方式计算整体的组件成份数（图的数目），这里只有一张图，当然是1
components(overallnet,connected="weak")
    ###计算Krackhardt推荐的connectedness图之间的连接程度，g值默认为全部数据，一张图=1
connectedness(overallnet, g=NULL)

    ###geodist计算所有节点之间的路径和数量
    #---如果节点间没有路径用inf.replace的值代替
    #---count.paths为真则结果输出路径数量矩阵
    #---predecessors为真则结果包含前导节点
    #---ignore.eval为真则忽略边缘路径信息，一般为TRUE
geodist(overallnet, inf.replace=Inf, count.paths=TRUE, predecessors=FALSE,ignore.eval=TRUE)
geo=geodist(overallnet)
    ###此处会返回counts和gdist两个矩阵，针对geo中gdist矩阵求最大路径，看整体维度
max(geo$gdist)  #diameter

    ###自定义函数计算整个网络中两个节点间路径的平均长度=总路径数/总二元组关系数
    ###注意无向图二元组数量要2倍
    #---choose(network.size(net),2)=190，整个网络中选择2个节点间的有向图directed关系数量
averagePathLength<-function(net){
  if(!is.network(net)){stop("Not a Network")}
  gd<-geodist(net)
  if(net%n%"directed"){
    return((1/choose(network.size(net),2))*sum(gd$gdist))
  }
  (1/(2*choose(network.size(net),2)))*sum(gd$gdist)
}
    ###计算例子中平均路径长度
averagePathLength(overallnet)

    ###自定义函数计算整体维度的最大路径数
diameter<-function(net){
  gd<-geodist(net)
  max(gd$gdist)
}
    ###计算例子中最大路径
diameter(overallnet)


###############################################################################################
#other package
###############################################################################################

########### you can use tnet package to calculate those metrics ##########

########## igraph format analysis ##########
##### this is a good example http://kateto.net/networks-r-igraph #######
library(igraph)

# final edge list 
all_edge=read.csv("/Users/fanouyang/Desktop/sna_2018fall/all_edge.csv") 

# create igraph from the edge list
all_igraph <- graph_from_data_frame(d=all_edge, directed=T)
# cheeck node and edge
E(all_igraph)     
V(all_igraph)
# plot not pretty,  need further revise it later
plot(all_igraph, edge.arrow.size=.1)
## node-level analysis
igraph::degree(all_igraph)
igraph::degree(all_igraph,mode="out")
igraph::betweenness(all_igraph)
igraph::closeness(all_igraph, mode="in")
igraph::closeness(all_igraph, mode="out")
igraph::closeness(all_igraph, mode="all")
igraph::eigen_centrality(all_igraph)
## network level analysis
igraph::diameter(all_igraph, directed = TRUE)
dyad_census(all_igraph)
distances(all_igraph)
shortest_paths(all_igraph, 5)
centr_degree(all_igraph)$centralization
centr_clo(all_igraph, mode="all")$centralization
centr_eigen(all_igraph, directed=FALSE)$centralization

#### use visNetwork package########
# plot you should customize it later
library(visNetwork)
visIgraph(all_igraph, idToLabel = TRUE, layout = "layout_nicely",
          physics = FALSE, smooth = T)



