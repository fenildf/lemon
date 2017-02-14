LEMON语法分析生成器
豆瓣链接: https://book.douban.com/subject/1954135/

本书说了代码基于sqlite-2.8.17.tar.gz ，其实经过我本人的严格对比，本书其实是基于sqlite-3.1.2.tar.gz，这是个大坑。

sqlite-3.1.2.tar.gz代码下载网址: http://pkgs.fedoraproject.org/repo/pkgs/sqlite/sqlite-3.1.2.tar.gz/d9c52489aa95d618842bf97778c17f04/

lemon.c 跟 lempar.c是从sqlite-3.1.2.tar.gz 代码中取得的。

parse.y就是sqlite-3.1.2.tar.gz 中针对SQL写的语法文件。

0、修改lemon.c 的3358行为:out = file_open(lemp,".cpp","wb"); #支持c++

1、gcc -o lemon lemon.c #编译得到lemon可执行文件

2、./lemon example4.y -c  #得到example4.cpp、example4.h、example4.out三个文件

3、剩下看CMakeLists.txt文件

4、// LEMON有下述20个特殊的申明符号。每一个特殊符号都以"%"字符为首,后面跟着由小写字母和下划线组成的字符串
%code            %default_destructor        %default_type
%destructor      %extra_argument            %include
%left            %name                      %nonassoc
%parse_accept    %parse_failure             %right
%stack_overflow  %stack_size                %start_symbol
%syntax_error    %token_destructor          %token_prefix
%token_type      %type
语法文件.y 中允许出现的20个以%开头的特殊指定符,在lemon数据结构中居然有16个与之相关,而不出现在lemon数据结构中的仅仅为%left %right %nonassoc %ifdef-%endif 跟 %ifndef-%endif

5、语法分析的5个基本数据结构:
symbol符号(包括终结符,非终结符)
rule(产生式)
config(项目,产生式的变形==LEMON在.out文件中用*号代替了黑点)
state(状态)
action(动作)

还有全局变量lemon 跟 存放符号名称的xla结构体

6、LEMON有两个特殊符号,一个是$ 另一个是error,这两个符号不出现在.y语法文件中,是LEMON程序为进行语法分析而自行"硬填"进去的符号。
x1a 装备字符串,相当于名字name的存储区域。Strsafe()相关函数
x2a 装备符号,Symbol_new()函数,包括终结符,也包括非终结符
x3a 装备状态state
x4a 装备项目config

7、词法扫描 过程


