# 基于R的大学图书馆分馆资源优化算法

这里是我本科毕业论文《基于二部图匹配的通借通还馆藏优化研究-以苏州大学图书馆为例》中所用到的代码。数据集方面因为是图书馆那里给的数据，涉及隐私不便公开。本文同样发表在了期刊[图书情报工作](http://kns.cnki.net/KCMS/detail/detail.aspx?dbcode=CJFQ&dbname=CJFDLAST2018&filename=TSQB201719015&v=MjkwNzMzcVRyV00xRnJDVVJMT2ZiK2R0RmlEa1ViN0JNVDdhYkxHNEg5Yk5wbzlFWVlSOGVYMUx1eFlTN0RoMVQ=)当中，其核心思路基于周涛的NBI算法（*ZHOU T, REN J, MEDO M, et al. Bipartite network projection and personal recommendation[J]. Phys Rev E, 2007, 76(4): 46-115.）*。 思路偏向于pagerank，最开始本想用类似运筹学的方法去做，但无奈知识不够无法深入这个领域。

代码文件其实没有经过特别细致的整理，当时处于刚入门状态，很多代码也没有注意规范性和可读性、复用性，基本都是一次性代码，但是确实是我第一个独立解决的问题，放在这里聊胜于无。
