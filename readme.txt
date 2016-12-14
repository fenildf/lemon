LEMON语法分析生成器
豆瓣链接: https://book.douban.com/subject/1954135/

本书说了代码基于sqlite-2.8.17.tar.gz ，其实经过我本人的严格对比，本书其实是基于sqlite-3.1.2.tar.gz，这是个大坑。

sqlite-3.1.2.tar.gz代码下载网址: http://pkgs.fedoraproject.org/repo/pkgs/sqlite/sqlite-3.1.2.tar.gz/d9c52489aa95d618842bf97778c17f04/

lemon.c 跟 lempar.c是从sqlite-3.1.2.tar.gz 代码中取得的。

0、修改lemon.c 的3358行为:out = file_open(lemp,".cpp","wb"); #支持c++

1、gcc -o lemon lemon.c #编译得到lemon可执行文件

2、./lemon example4.y -c  #得到example4.cpp、example4.h、example4.out三个文件

3、剩下看CMakeLists.txt文件