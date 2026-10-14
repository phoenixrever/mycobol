# COBOL安装
### 1.安装WSL
>https://learn.microsoft.com/zh-cn/windows/wsl/install

### 2.安装Ubuntu
默认是安装Ubuntu 20.04 LTS，安装完成后，打开Ubuntu终端，输入以下命令更新软件包：
```bash
sudo apt update
sudo apt upgrade
```
可以从windows商店安装其他版本的linux,比如debit

### 3.安装GnuCOBOL
```bash
sudo apt install gnucobol

cobc --version # 查看安装的版本
which cobc # 查看编译器是否可用：
``` 

### 4.安装VSCode插件
1.COBOL插件  
2.COBOL Theme插件   

### 5.编译与运行
在WSL 终端中，进入到cobol文件所在的目录，执行以下命令：
```bash
cd lessions/ch01/
cobc -x helloworld.cbl -o bin/helloworld # 编译 -x 生成可执行文件
./helloworld # 运行
``` 

### 6.cobol插件代码补全变大写风格
```json
"coboleditor.intellisense_style": "uppercase"
```