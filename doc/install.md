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

## 4.安装VSCode插件
1.IBM Z Open Editor 
2.COBOL Theme插件   


Windows 和 Ubuntu 是两个独立的运行环境：

- Windows：`C:\Program Files\Java\jdk-21`
- Ubuntu：需要自己的 Linux 版本 JDK 21。

所以 Windows 上执行 `java -version` 成功，并不能证明 Ubuntu 上也有 Java。

### 1. 在 Ubuntu 安装 Java 21

打开 VS Code 连接 Ubuntu 后的终端，执行：

```
sudo apt update
sudo apt install -y openjdk-21-jdk
```

检查：

```
java -version
```

查看 Java 实际路径：

```
readlink -f "$(which java)"
```

通常会得到类似：

```
/usr/lib/jvm/java-21-openjdk-amd64/bin/java
```

那么 JAVA_HOME 就是：

```
/usr/lib/jvm/java-21-openjdk-amd64
```

## 2. 配置 Ubuntu 环境变量

执行：

```
echo 'export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64' >> ~/.bashrc
echo 'export PATH=$JAVA_HOME/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
```

检查：

```
echo $JAVA_HOME
java -version
```

### 3. 配置 IBM Z Open Editor

在 VS Code 的 Remote Ubuntu 窗口中，打开 Settings JSON，配置：

```
{
    "zopeneditor.JAVA_HOME": "/usr/lib/jvm/java-21-openjdk-amd64"
}
```

注意：这里使用 Ubuntu 路径，不是 Windows 路径。

"[cobol]": {
    "editor.formatOnSave": true,
    "files.insertFinalNewline": true
}

### 4. 重启插件

按 `Ctrl + Shift + P`：

1. 执行 `Developer: Reload Window`
2. 等待 IBM Z Open Editor 启动。
3. 检查 Java 配置错误是否消失。

> Zowe profile 这个提示的意思是：IBM Z Open Editor 没有找到 Zowe Profile，因此无法连接 IBM z/OS 大型机。 不需要安装


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