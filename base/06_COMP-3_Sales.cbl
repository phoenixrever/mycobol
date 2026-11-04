       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SALES.
       AUTHOR.       チョウ殿.
      ***************************************************************
      *  This program reads a file containing sales person yearly   *
      *   sales information and prints a report.                    *
      ***************************************************************

      *===============================================
      * 环境说明部，描述程序运行环境和文件
       ENVIRONMENT DIVISION.
      * 配置节，用于描述程序的源计算机和目标计算机等环境信息。
       CONFIGURATION SECTION.
      * 历史上用于声明编译程序所使用的计算机类型。 现在不需要写
       SOURCE-COMPUTER.
      * 历史上用于描述程序运行目标环境。现在不需要写
       OBJECT-COMPUTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALESFILE ASSIGN TO    "data/SALES.DAT"
                            ORGANIZATION LINE SEQUENTIAL.
           SELECT PRINT-FILE ASSIGN TO    "report/SALESREPORT.DAT".

      *===============================================
      *  DATA DIVISION：数据说明部，定义数据结构和变量
       DATA DIVISION.
       FILE SECTION.

      * 输入文件描述
       FD SALESFILE.

      * 输入文件记录结构
       01 SALESDETAILS.
      * 结束标志
          88 ENDOFSALES                       VALUE HIGH-VALUES.
      * 销售员编号
       05 SALESPERSON-ID           PIC 9(5).
      * 销售员姓名
          05 SALESPERSON-NAME.
             10 LASTNAME           PIC X(20).
             10 FIRSTNAME          PIC X(20).
      * 区域
          05 REGION                PIC X(5).
      * 年销售额
          05 YEARLYSALES           PIC 9(6).
      * 性别
          05 GENDER                PIC X.

      * 输出文件描述
       FD PRINT-FILE.

      * 输出文件记录结构
       01 PRINT-LINE               PIC X(132).

      *===============================================
      *- 01：表示最顶层的主变量（主记录）
      *- 02-49：代表基本数据项，通常是主变量的直接子变量
      *- 05、10、15：表示子变量，组成主变量的部分
      *- 数字间隔通常是 5，但不是必须，可以用 01、02、03，只是习惯上用间隔 5，方便以后插入新变量。
      *
      *- 等级号是固定范围的数字，常用的是：01、02、03、...、49、66、77、88 等
      *- 01 是顶层变量
      *- 66 通常用于定义数据重定义（REDEFINES），允许同一块内存以不同的方式解释
      *- 77 是独立变量，不属于任何结构
      *- 88 是条件值，用于定义布尔类型的条件（True/False）
      *
      *  01：顶层数据项，通常顶格写。
      *  05：子项，通常缩进 3 格或 4 格
      *
      *01 VAR-A   PIC A(5).    * 只能是5个字母字符
      *01 VAR-X   PIC X(5).    * 5个任意字符（数字、字母、符号均可）
      *01 VAR-N   PIC 9(5).    * 5位数字，如 12345
      *01 VAR-S   PIC S9(5).   * 带符号的5位数字，如 -12345
      *01 VAR-D   PIC 9(3)V99. * 数值，3位整数和2位小数，比如 123.45
      *===============================================

      * 工作存储区，定义中间变量
       WORKING-STORAGE SECTION.
      *=========================COMP-3======================
      * 9(10) COMP-3 表示使用压缩格式存储。packed decimal（压缩十进制）
      * 是一种将两个十进制数字存储在一个字节中的方式，节省存储空间。
      * 低位字节的最后一个半字节用于存储符号（正或负）。
      * 例如，数字 21544 ：
      * byte 1: 0010 0001 (2, 1)
      * byte 2: 0101 0100 (5, 4)
      * byte 3: 0100 1111 (4, C)  C表示正数，D表示负数，F表示无符号正数
      *===============================================
      * 总销售额
       01 WS-FILEDS.
          05 WS-TOTAL-SALES        PIC 9(10) COMP-3.
          05 WS-COMMISSION-RATE    PIC V99    VALUE 0.03.
          05 WS-COMMISSION-AMOUNT  PIC 9(10) COMP-3.
      * 各区域销售额
       01 WS-REGION-SALES. 
          05 WS-EAST               PIC 9(7)   VALUE ZEROES.
          05 WS-WEST               PIC 9(7)   VALUE ZEROES.
          05 WS-NORTH              PIC 9(7)   VALUE ZEROES.
          05 WS-SOUTH              PIC 9(7)   VALUE ZEROES.

      * FILLER作用就是在输出时插入指定数量的空格，让报表的各个字段对齐、格式美观。
      * 报表标题行
       01 HEADING-LINE.
          05 FILLER                PIC X(5)   VALUE SPACES.
          05 FILLER                PIC X(16)  VALUE
                'SALESPERSON NAME'.
          05 FILLER                PIC X(29)  VALUE SPACES.
          05 FILLER                PIC X(6)   VALUE 'REGION'.
          05 FILLER                PIC X(10)  VALUE
                SPACES.
          05 FILLER                PIC X(12)  VALUE
                'YEARLY SALES'.
          05 FILLER                PIC X(73)  VALUE
                SPACES.

      * 报表明细行
       01 DETAIL-LINE.
          05 FILLER                PIC X(5)   VALUE
                SPACES.
          05 DET-SALESPERSON-NAME  PIC X(40).
          05 FILLER                PIC X(5)   VALUE
                SPACES.
          05 DET-REGION            PIC X(5).
          05 FILLER                PIC X(10)  VALUE
                SPACES.
          05 DET-YEARLYSALES       PIC X(12).
          05 FILLER                PIC X(40)  VALUE
                SPACES.

      * 报表总计行
       01 TOTAL-LINE.
          05 FILLER                PIC X(5)   VALUE
                SPACES.
          05 FILLER                PIC X(16)  VALUE
                SPACES.
          05 FILLER                PIC X(10)  VALUE
                SPACES.
          05 FILLER                PIC X(6)   VALUE
                SPACES.
          05 FILLER                PIC X(10)  VALUE
                SPACES.
          05 TOTAL-YRLY-SALES      PIC X(12).
          05 FILLER                PIC X(73)  VALUE
                SPACES.

      *===============================================
      *  PROCEDURE DIVISION：过程部，程序主逻辑
       PROCEDURE DIVISION.
           PERFORM 0050-OPEN-FILE.

      * 打开文件段落
       0050-OPEN-FILE.
           OPEN INPUT SALESFILE.
           OPEN OUTPUT PRINT-FILE.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0200-STOP-RUN.

      * 处理所有销售记录
       0100-PROCESS-RECORDS.

           PERFORM 0110-WRITE-HEADING-LINE.
           READ SALESFILE
           AT END
              SET ENDOFSALES TO TRUE
           END-READ.
           PERFORM UNTIL ENDOFSALES
                   ADD YEARLYSALES TO WS-TOTAL-SALES
                   MOVE SALESPERSON-NAME TO DET-SALESPERSON-NAME
                   MOVE REGION TO DET-REGION
                   MOVE YEARLYSALES TO DET-YEARLYSALES
                   COMPUTE WS-COMMISSION-AMOUNT = YEARLYSALES *
                      WS-COMMISSION-RATE
                   DISPLAY FIRSTNAME,
                           " ",
                           LASTNAME,
                           " earned $",
                           WS-COMMISSION-AMOUNT,
                           " in commission."
                   PERFORM 0120-WRITE-DETAIL-LINE

                   READ SALESFILE
                   AT END
                      SET ENDOFSALES TO TRUE
                   END-READ
           END-PERFORM.
      * 写总计行
           PERFORM 0130-WRITE-TOTAL-LINE. 

      * 写标题行
       0110-WRITE-HEADING-LINE.
           MOVE HEADING-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.

      * 写明细行
       0120-WRITE-DETAIL-LINE.
           MOVE DETAIL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

      * 写总计行
       0130-WRITE-TOTAL-LINE.
           MOVE WS-TOTAL-SALES TO TOTAL-YRLY-SALES.
           MOVE TOTAL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

      * 关闭文件并结束程序
       0200-STOP-RUN.
           CLOSE SALESFILE.
           CLOSE PRINT-FILE.
           STOP RUN.
          END PROGRAM SALES.
