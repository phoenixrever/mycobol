      *===============================================
      *           COBOL 程序结构介绍
      *===============================================
      * 1. IDENTIFICATION DIVISION.
      *    - 程序标识部分，声明程序名称等。
      *
      * 2. ENVIRONMENT DIVISION.
      *    - 环境部分，描述程序运行环境。
      *
      * 3. DATA DIVISION.
      *    - 数据部分，定义变量和存储区域。
      *
      * 4. PROCEDURE DIVISION.
      *    - 程序逻辑部分，包含段（Paragraph）和节（Section）。
      *===============================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HELLOWORLD".
       AUTHOR.     チヨウ殿.
      *This is a comment in COBOL
      *Columns 8-11 A Margin
      *Columns 12-72 B Margin
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
      *===============================================
      *- 01：表示最顶层的主变量（主记录）
      *- 05、10、15：表示子变量，组成主变量的部分
      *- 数字间隔通常是 5，但不是必须，可以用 01、02、03，只是习惯上用间隔 5，方便以后插入新变量。
      *
      *- 等级号是固定范围的数字，常用的是：01、02、03、...、49、66、77、88 等
      *- 01 是顶层变量
      *- 66 是条件名，常用来定义条件变量
      *- 77 是独立变量，不属于任何结构
      *- 88 是条件值，用于定义布尔类型的条件（True/False）

      
      *01 VAR-A   PIC A(5).    * 只能是5个字母字符
      *01 VAR-X   PIC X(5).    * 5个任意字符（数字、字母、符号均可）
      *01 VAR-N   PIC 9(5).    * 5位数字，如 12345
      *01 VAR-S   PIC S9(5).   * 带符号的5位数字，如 -12345
      *01 VAR-D   PIC 9(3)V99. * 数值，3位整数和2位小数，比如 123.45
      *===============================================
       WORKING-STORAGE SECTION.
       01 NAME.
           05 FIRST-NAME PIC X(10) VALUE "チヨウ".
           05 LAST-NAME  PIC X(10) VALUE "殿".
       01 HELLO-MESSAGE PIC X(50) VALUE "Hello World! チヨウ殿".
       PROCEDURE DIVISION.
      *PERFORM 0100-START-HERE. 不写0100-START-HERE 会有警告，顺序执行没关系
      *0100-START-HERE.
           DISPLAY NAME.
           DISPLAY HELLO-MESSAGE.
           STOP RUN.
       END PROGRAM HELLOWORLD.
