IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES.           * 程序名称
       AUTHOR.     PEGGY FISHER.    * 作者
      ***************************************************************
      *  This program reads a file containing sales person yearly   *
      *   sales information and prints a report.                    *
      ***************************************************************

*===============================================
*  ENVIRONMENT DIVISION：环境说明部，描述程序运行环境和文件
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.       * 配置节（一般很少用）
       SOURCE-COMPUTER.             * 源计算机（可省略）
       OBJECT-COMPUTER.             * 目标计算机（可省略）

       INPUT-OUTPUT SECTION.        * 输入输出节
       FILE-CONTROL.                * 文件控制段，定义文件与物理文件名的对应关系
            SELECT SALESFILE ASSIGN TO "SALES.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.   * 按行顺序读取
            SELECT PRINT-FILE ASSIGN TO "SALESREPORT.DAT".

*===============================================
*  DATA DIVISION：数据说明部，定义数据结构和变量
       DATA DIVISION.
       FILE SECTION.                * 文件节，定义文件结构

* 输入文件描述
       FD SALESFILE.

* 输入文件记录结构
       01 SALESDETAILS.
            88 ENDOFSALES VALUE HIGH-VALUES.    * 结束标志
            05 SALESPERSON-ID       PIC 9(5).   * 销售员编号
            05 SALESPERSON-NAME.                * 销售员姓名
                10 LASTNAME         PIC X(20).  * 姓
                10 FIRSTNAME        PIC X(20).  * 名
            05 REGION               PIC X(5).   * 区域
            05 YEARLYSALES          PIC 9(6).   * 年销售额
            05 GENDER               PIC X.      * 性别

      * 输出文件描述
        FD PRINT-FILE.

      * 输出文件记录结构
        01  PRINT-LINE              PIC X(132). * 一行输出

      * 工作存储区，定义中间变量
        WORKING-STORAGE SECTION.
      * 9(10).-> 9(10) COMP-3 表示使用压缩格式存储。
        01   WS-TOTAL-SALES         PIC 9(10) COMP-3.   * 总销售额

        01   WS-REGION-SALES.                           * 各区域销售额
             05 WS-EAST             PIC 9(7) VALUE ZEROES.
             05 WS-WEST             PIC 9(7) VALUE ZEROES.
             05 WS-NORTH            PIC 9(7) VALUE ZEROES.
             05 WS-SOUTH            PIC 9(7) VALUE ZEROES.

      * FILLER作用就是在输出时插入指定数量的空格，让报表的各个字段对齐、格式美观。
      * 报表标题行
        01  HEADING-LINE.
            05 FILLER              PIC X(5) VALUE SPACES.         * 占位用，不存储实际数据，只用于格式对齐
            05 FILLER              PIC X(16) VALUE 'SALESPERSON NAME'. * 标题文本
            05 FILLER              PIC X(29) VALUE SPACES.        * 占位用，控制输出格式
            05 FILLER              PIC X(6)  VALUE 'REGION'.      * 标题文本
            05 FILLER              PIC X(10) VALUE SPACES.        * 占位用，控制输出格式
            05 FILLER              PIC X(12) VALUE 'YEARLY SALES'.* 标题文本
            05 FILLER              PIC X(73) VALUE SPACES.        * 占位用，控制输出格式

      * 报表明细行
        01  DETAIL-LINE.
            05 FILLER               PIC X(5)  VALUE SPACES.       * 占位用，控制输出格式
            05 DET-SALESPERSON-NAME PIC X(40).                    * 销售员姓名
            05 FILLER               PIC X(5)  VALUE SPACES.       * 占位用，控制输出格式
            05 DET-REGION           PIC X(5).                     * 区域
            05 FILLER               PIC X(10)  VALUE SPACES.      * 占位用，控制输出格式
            05 DET-YEARLYSALES      PIC X(12).                    * 年销售额
            05 FILLER               PIC X(40)  VALUE SPACES.      * 占位用，控制输出格式
      
      * 报表总计行
        01  TOTAL-LINE.
            05 FILLER               PIC X(5)   VALUE SPACES.      * 占位用，控制输出格式
            05 FILLER               PIC X(16)  VALUE SPACES.      * 占位用，控制输出格式
            05 FILLER               PIC X(10)  VALUE SPACES.      * 占位用，控制输出格式
            05 FILLER               PIC X(6)   VALUE SPACES.      * 占位用，控制输出格式
            05 FILLER               PIC X(10)  VALUE SPACES.      * 占位用，控制输出格式
            05 TOTAL-YRLY-SALES     PIC X(12).                    * 总销售额
            05 FILLER               PIC X(73)  VALUE SPACES.      * 占位用，控制输出格式

      *===============================================
      *  PROCEDURE DIVISION：过程部，程序主逻辑
        PROCEDURE DIVISION.

      * 打开文件段落
        0050-OPEN-FILE.
           OPEN INPUT SALESFILE.           * 打开输入文件
           OPEN OUTPUT PRINT-FILE.         * 打开输出文件
           PERFORM 0100-PROCESS-RECORDS.   * 处理记录
           PERFORM 0200-STOP-RUN.          * 结束处理

      * 处理所有销售记录
        0100-PROCESS-RECORDS.

           PERFORM 0110-WRITE-HEADING-LINE.   * 写标题行
           READ SALESFILE
                AT END SET ENDOFSALES TO TRUE
                END-READ.
           PERFORM UNTIL ENDOFSALES
            ADD YEARLYSALES TO WS-TOTAL-SALES         * 累加总销售额
            MOVE SALESPERSON-NAME TO DET-SALESPERSON-NAME
            MOVE REGION TO DET-REGION
            MOVE YEARLYSALES TO DET-YEARLYSALES
            PERFORM 0120-WRITE-DETAIL-LINE           * 写明细行

            READ SALESFILE
            AT END SET ENDOFSALES TO TRUE
            END-READ
           END-PERFORM.
           PERFORM 0130-WRITE-TOTAL-LINE.            * 写总计行
      
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
