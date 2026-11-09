       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SALESDATAVALIDATION.
       AUTHOR.       チョウ殿.
      ***************************************************************
      *
      *  如果一个文件前面5行validate 都出错的话就不用继续往下再验证了      
      *  这个时候可以使用 GOTO 语句         
            
      *  和C语言的 goto 类似，GO TO 语句用于无条件跳转到指定的段落或标签。
      *  GO TO 语句的使用应谨慎，过度使用可能导致代码难以维护和理解。
      *  在编程中尽量避免使用 GO TO
      *  使用 PERFORM 或其他控制结构来实现逻辑跳转。
      
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
           SELECT SALESFILE ASSIGN TO    "data/ERROR-SALES.DAT"
                            ORGANIZATION LINE SEQUENTIAL.
           SELECT NEWSALESFILE ASSIGN TO "report/NEWSALESFILE.DAT"
                    ORGANIZATION LINE SEQUENTIAL.
           SELECT ERROR-REPORT ASSIGN TO "report/SALESERRORS.DAT"
                    ORGANIZATION LINE SEQUENTIAL.

      *===============================================
      *  DATA DIVISION：数据说明部，定义数据结构和变量ERROR-
       DATA DIVISION.
       FILE SECTION.

      * 输入文件描述
       FD SALESFILE
      * 固定長：每レコードの長さが一定。
      * 可変長：各レコードの長さが異なり、最小値と最大値を指定する。
      * RECORD IS VARYING IN SIZE で可変長レコードを定義する。
      * RECORD CONTAINS を省略した場合、
      * 01レベルのレコード定義から長さを決定する。
      * 複数の01レコード定義も最大と最小の長さを決定する。
            RECORD CONTAINS 57 CHARACTERS.
      * 输入文件记录结构
       01 SALESDETAILS.
      * 结束标志
          88 ENDOFSALES                       VALUE HIGH-VALUES
           .
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
             88 VALID-GENDER                  VALUE 'M'
                                       , 'F', 'U'.
      * 输出文件描述
       FD NEWSALESFILE
           RECORD CONTAINS 57 CHARACTERS.
      * 输出文件记录结构
       01 VALID-SALES-RECORD       PIC X(57).
       
       FD ERROR-REPORT.
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
      * 当前时间数据
       01 WS-DATE.
          10 WS-YEAR               PIC 9(2).
          10 WS-MONTH              PIC 9(2).
          10 WS-DAY                PIC 9(2).
       
       01 WS-FIELDS.
          05 WS-BAD-RECORDS        PIC 9(5).

       01 ERROR-DESCRIPTIONS.
          05 NON-NUMERIC-EMP-ID    PIC X(40)  VALUE
                "EMPLOYEE ID IS NOT NUMERIC".
          05 NON-NUMERIC-SALES     PIC X(40)  VALUE
                "SALE AMOUNT IS NOT NUMERIC".
          05 MISSING-LNAME         PIC X(40)  VALUE
                "LAST NAME IS MISSING".
          05 MISSING-FNAME         PIC X(40)  VALUE
                "FIRST NAME IS MISSING".
          05 INVALID-NAME          PIC X(40)  VALUE
                "INVALID NAME".
          05 INVALID-REGION        PIC X(40)  VALUE
                "INVALID REGION".
          05 INVALID-GENDER        PIC X(40)  VALUE
                "INVALID GENDER SHOULD BE M, F OR U".
       01 VALID-RECORD-SWITCH      PIC X(3).
          88 VALID-RECORD                     VALUE "YES".
                
      * FILLER作用就是在输出时插入指定数量的空格，让报表的各个字段对齐、格式美观。
      * 报表标题行
       01 HEADING-LINE.
          05 FILLER                PIC X(2)   VALUE 'ID'.
          05 FILLER                PIC X(6)   VALUE SPACES.
          05 FILLER                PIC X(16)  VALUE
                'SALESPERSON NAME'.
          05 FILLER                PIC X(24)  VALUE SPACES.
          05 FILLER                PIC X(6)   VALUE 'REGION'.
          05 FILLER                PIC X(4)   VALUE SPACES.
          05 FILLER                PIC X(12)  VALUE
                'YEARLY SALES'.
          05 FILLER                PIC X(2)   VALUE SPACES.
          05 FILLER                PIC X(6)   VALUE 'GENDER'.
          05 FILLER                PIC X(5)   VALUE SPACES.
          05 FILLER                PIC X(40)  VALUE
                'ERROR MESSAGE'.
             
       01 HEADING-LINE-1.
          05 FILLER                PIC X(100) VALUE SPACES.
          05 HEAD-MM               PIC 9(2).
          05 FILLER                PIC X      VALUE '/'.
          05 HEAD-DD               PIC 9(2).
          05 FILLER                PIC X      VALUE '/'.
          05 HEAD-YY               PIC 9(2).

       01 HEADING-LINE-2.
          05 FILLER                PIC X(5)   VALUE '-----'.
          05 FILLER                PIC X(3)   VALUE SPACES.
          05 FILLER                PIC X(10)  VALUE
                '----------'.
          05 FILLER                PIC X(10)  VALUE SPACES.
          05 FILLER                PIC X(10)  VALUE
                '----------'.
          05 FILLER                PIC X(10)  VALUE SPACES.
          05 FILLER                PIC X(8)   VALUE
                '--------'.
          05 FILLER                PIC X(2)   VALUE SPACES.
          05 FILLER                PIC X(11)  VALUE
                '----------'.
          05 FILLER                PIC X(14)  VALUE SPACES.    
          05 FILLER                PIC X(40)  VALUE
                '-----------------'.    
          05 FILLER                PIC X(20)  VALUE SPACES.    
            
      * 报表明细行
       01 DETAIL-LINE.
          05 DET-SALES-ID          PIC X(5).
          05 FILLER                PIC X(3)   VALUE SPACES.
          05 DET-SALESPERSON-NAME  PIC X(40).
          05 DET-REGION            PIC X(5).
          05 FILLER                PIC X(5)   VALUE SPACES.
      * 连续多个 $ 不代表显示多个美元符号，而是让 $ 根据数字长度浮动。
      * 最右侧的 9 保证至少显示一位数字。123456　⇒ $123,456,789
          05 DET-YEARLYSALES       PIC X(12).
          05 FILLER                PIC X(5)   VALUE SPACES.
          05 DET-GENDER            PIC X.
          05 FILLER                PIC X(7)   VALUE SPACES.
          05 DET-ERROR-MSG         PIC X(40).

      *===============================================
      *  PROCEDURE DIVISION：过程部，程序主逻辑
       PROCEDURE DIVISION.
           PERFORM 0050-OPEN-FILE.

      *============================================================*
      * ACCEPT FROM DATE
      *============================================================*
      * 使用 ACCEPT 语句获取系统当前日期。
      * 返回格式：YYMMDD（6位）。
      * YY：年份后两位。
      * 示例：
      * 01 WS-DATE PIC 9(6).
      * ACCEPT WS-DATE FROM DATE.
      *============================================================*
      * FUNCTION CURRENT-DATE
      *============================================================*
      * 使用 COBOL 内置函数获取当前日期和时间。
      * 返回格式：YYYYMMDDhhmmsscc±hhmm（共21位）。
      * YYYY：四位年份。
      * MM：月份（01～12）。
      * DD：日期（01～31）。
      * hh：小时（00～23）。
      * mm：分钟（00～59）。
      * ss：秒（00～59）。
      * cc：百分之一秒。
      * ±hhmm：与 UTC 的时差。
      * 例如：2026110821234500+0900。
      * 表示 2026年11月08日 21:23:45.00，
      * 时区为 UTC+09:00。
      * 只需要年月日时，可以截取返回值的前8位。
      *
      * 示例：
      * 01 WS-CURRENT-DATE PIC X(21).
      * 01 WS-DATE         PIC X(8).
      * MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
      * MOVE WS-CURRENT-DATE(1:8) TO WS-DATE.
      *============================================================*
       0050-OPEN-FILE.
           OPEN INPUT SALESFILE.
           OPEN OUTPUT NEWSALESFILE, ERROR-REPORT.
           INITIALIZE WS-DATE
           ACCEPT WS-DATE FROM DATE.
           MOVE WS-YEAR TO HEAD-YY.
           MOVE WS-MONTH TO HEAD-MM.
           MOVE WS-DAY TO HEAD-DD.
           PERFORM 0090-READ-SALES-RECORD.
           PERFORM 0110-WRITE-HEADING-LINE.
           PERFORM 0100-PROCESS-RECORDS UNTIL ENDOFSALES
           PERFORM 0200-STOP-RUN.
      * 读取一条记录 
      * 如果文件为空，第一次 READ 就设置 ENDOFSALES，不会进入处理循环
      * AT END 不是说读到结束 而是当读取不到记录时，执行 AT END 语句块
       0090-READ-SALES-RECORD.
           READ SALESFILE
           AT END
              SET ENDOFSALES TO TRUE
           END-READ.

       0100-PROCESS-RECORDS.
           MOVE 'YES' TO VALID-RECORD-SWITCH.
           PERFORM 0150-VALIDATE-SALES.
      *    IF VALID-RECORD
      *       MOVE SALESDETAILS TO VALID-SALES-RECORD
      *       WRITE VALID-SALES-RECORD
      *    END-IF.
           IF VALID-RECORD
              WRITE VALID-SALES-RECORD FROM SALESDETAILS
           ELSE
              ADD 1 TO WS-BAD-RECORDS
           END-IF.
      *    这边先执行 ADD，再判断，才能是5条记录
           DISPLAY "WS-BAD-RECORDS:", WS-BAD-RECORDS
           IF WS-BAD-RECORDS >= 5
              DISPLAY "Too many bad records, stopping processing."
              GO TO 0200-STOP-RUN
           END-IF.
      *    读取下一条记录
           PERFORM 0090-READ-SALES-RECORD.

       0110-WRITE-HEADING-LINE.
      * 等价于 MOVE HEADING-LINE-1 TO PRINT-LINE WRITE PRINT-LINE
           WRITE PRINT-LINE FROM HEADING-LINE-1
              AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADING-LINE
              AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADING-LINE-2
              AFTER ADVANCING 1 LINE.
      * MOVE SPACES，清空 PRINT-LINE 之前残留内容。     
           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE.

      * 这边可以 用 THRU 来合并 title
      * 0150 也可以合并 至少语义不同 没必要
       0150-VALIDATE-SALES.
           PERFORM 0150-PRINT-FILE-INFO.
      *    PERFORM 0160-VALIDATE-ID.
      *    PERFORM 0165-VALIDATE-NAME.
      *    PERFORM 0170-VALIDATE-REGION.
      *    PERFORM 0175-VALIDATE-SALES.
      *    PERFORM 0180-VALIDATE-GENDER.
           PERFORM 0160-VALIDATE-ID THRU 0180-VALIDATE-GENDER.

       0150-PRINT-FILE-INFO.
           MOVE SALESPERSON-ID TO DET-SALES-ID.
           MOVE SALESPERSON-NAME TO DET-SALESPERSON-NAME.
           MOVE REGION TO DET-REGION.
           MOVE YEARLYSALES TO DET-YEARLYSALES.
           MOVE GENDER TO DET-GENDER.

       0160-VALIDATE-ID.
      * 也可以写作 IF SALESPERSON-ID NOT NUMERIC THEN
           IF SALESPERSON-ID IS NOT NUMERIC
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE NON-NUMERIC-EMP-ID TO DET-ERROR-MSG
              PERFORM 0190-WRITE-ERROR-LINE
           END-IF.

       0165-VALIDATE-NAME.
           IF LASTNAME = SPACES
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE MISSING-LNAME TO DET-ERROR-MSG
              PERFORM 0190-WRITE-ERROR-LINE
           ELSE
              IF LASTNAME IS NOT ALPHABETIC
                 MOVE 'NO' TO VALID-RECORD-SWITCH
                 MOVE INVALID-NAME TO DET-ERROR-MSG
                 PERFORM 0190-WRITE-ERROR-LINE
              END-IF
           END-IF.
           IF FIRSTNAME = SPACES
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE MISSING-FNAME TO DET-ERROR-MSG
              PERFORM 0190-WRITE-ERROR-LINE
           ELSE
              IF FIRSTNAME IS NOT ALPHABETIC
                 MOVE 'NO' TO VALID-RECORD-SWITCH
                 MOVE INVALID-NAME TO DET-ERROR-MSG
                 PERFORM 0190-WRITE-ERROR-LINE
              END-IF
           END-IF.

       0170-VALIDATE-REGION.
      * 下面的 IF 语句可以写成 OR 连接的形式，也可以写成 NOT AND 连接的形式。
      *  CONTINUE 或者 NEXT SENTENCE 表示继续执行下一条语句，不做任何操作。
      *    IF REGION = 'EAST' OR REGION = 'WEST'
      *       OR REGION = 'NORTH' OR REGION = 'SOUTH'
      *       CONTINUE
      *    ELSE
      *       MOVE 'NO' TO VALID-RECORD-SWITCH
      *       MOVE INVALID-REGION TO DET-ERROR-MSG
      *       PERFORM 0190-WRITE-ERROR-LINE
      *    END-IF.
           
           IF REGION NOT = 'EAST' AND REGION NOT = 'WEST'
              AND REGION NOT = 'NORTH' AND REGION NOT = 'SOUTH'
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE INVALID-REGION TO DET-ERROR-MSG
              PERFORM 0190-WRITE-ERROR-LINE
           END-IF.

       0175-VALIDATE-SALES.
           IF YEARLYSALES IS NOT NUMERIC
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE NON-NUMERIC-SALES TO DET-ERROR-MSG
              PERFORM 0190-WRITE-ERROR-LINE
           END-IF.

       0180-VALIDATE-GENDER.
       
           IF NOT VALID-GENDER
              MOVE 'NO' TO VALID-RECORD-SWITCH
              MOVE INVALID-GENDER TO DET-ERROR-MSG
              PERFORM 0190-WRITE-ERROR-LINE
           END-IF.
      
       0190-WRITE-ERROR-LINE.
      *    MOVE SALESPERSON-ID TO DET-SALES-ID
      *    MOVE SALESPERSON-NAME TO DET-SALESPERSON-NAME
      *    MOVE REGION TO DET-REGION
      *    MOVE YEARLYSALES TO DET-YEARLYSALES
      *    MOVE GENDER TO DET-GENDER
           WRITE PRINT-LINE FROM DETAIL-LINE
              AFTER ADVANCING 1 LINE.

      * 关闭文件并结束程序
       0200-STOP-RUN.
           CLOSE SALESFILE.
           CLOSE NEWSALESFILE, ERROR-REPORT.
           STOP RUN.

       END PROGRAM SALESDATAVALIDATION.
