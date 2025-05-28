      *===============================================
      *           DIVISION 和 SECTION 结构关系详解
      *===============================================
      * COBOL 程序结构分为多层：
      * 
      * 1. DIVISION（部）：最高层级的结构单位，表示程序的主要部分。
      *    常见的有：
      *      - IDENTIFICATION DIVISION.   程序标识部，说明程序名称、作者等信息
      *      - ENVIRONMENT DIVISION.      环境说明部，描述程序运行环境和文件
      *      - DATA DIVISION.             数据说明部，定义数据结构和变量
      *      - PROCEDURE DIVISION.        过程部，包含程序的具体逻辑和操作
      *
      * 2. SECTION（节）：DIVISION 的子结构，用于进一步细分和组织代码。
      *    常见于 ENVIRONMENT DIVISION 和 DATA DIVISION。
      *    例如：
      *      - ENVIRONMENT DIVISION.
      *          CONFIGURATION SECTION.      配置节（一般很少用）
      *          INPUT-OUTPUT SECTION.       输入输出节（定义文件控制）
      *      - DATA DIVISION.
      *          WORKING-STORAGE SECTION.    工作存储节（定义变量）
      *          FILE SECTION.               文件节（定义文件结构）
      *
      * 3. PARAGRAPH（段落）：SECTION 的子结构，或直接属于 DIVISION。
      *    例如 FILE-CONTROL.、PROCEDURE DIVISION 里的 0100-PROCESS-RECORDS.
      *
      * 4. 语句：段落中的具体执行语句，如 MOVE、COMPUTE、DISPLAY 等。
      *
      * 结构示例：
      *   ENVIRONMENT DIVISION.        <== 部
      *       INPUT-OUTPUT SECTION.    <== 节
      *           FILE-CONTROL.        <== 段落
      *   DATA DIVISION.
      *       FILE SECTION.
      *           FD ...               <== 文件描述
      *       WORKING-STORAGE SECTION.
      *           01 ...               <== 变量定义
      *   PROCEDURE DIVISION.
      *       0100-...                 <== 段落
      *           ...                  <== 语句
      *===============================================

      * 程序标识部
       IDENTIFICATION DIVISION.           
       PROGRAM-ID. "ENHANCEDBMICALCULATOR".
       AUTHOR.     PEGGY FISHER.
      * This program reads input from a file

      * 环境说明部
       ENVIRONMENT DIVISION.              

      * 输入输出节
       INPUT-OUTPUT SECTION.              
      * FILE-CONTROL 表示文件控制部分（相当于创建文件变量），定义了程序将使用的文件。
       FILE-CONTROL.
            SELECT BMI-FILE ASSIGN TO "BMI-INPUT.DAT"
      * ORGANIZATION IS LINE SEQUENTIAL 表示按文本行顺序读取     
               ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PRINT-FILE ASSIGN TO "BMI-REPORT.DAT".

      * 数据说明部
       DATA DIVISION.                     

      * FILE SECTION 定义了程序使用的文件结构和数据格式。
       FILE SECTION.

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

      * 输入文件描述
       FD BMI-FILE.
      * 文件记录结构
       01 BMIDETAILS.                   
           88 ENDOFBMI VALUE HIGH-VALUES.  
           05 PERSON-NAME.
              10 LASTNAME      PIC X(20).
              10 FIRSTNAME     PIC X(20).
           05 HEIGHT-INCHES     PIC 999.
           05 WEIGHT            PIC 999.

      * 输出文件描述
       FD PRINT-FILE.

       01 PRINT-LINE        PIC X(132).  

      * 工作存储节，定义变量
       WORKING-STORAGE SECTION.          
       01 WS. 
           05 WS-BMI           PIC 99V99.   

      * 报表标题行
       01  HEADING-LINE.                
           05 FILLER            PIC X(5) VALUE SPACES.
           05 FILLER            PIC X(40) VALUE 'NAME'.
           05 FILLER            PIC X(22) VALUE 'HEIGHT IN INCHES'.
           05 FILLER            PIC X(30) VALUE 'WEIGHT IN POUNDS'.
           05 FILLER            PIC X(12) VALUE 'BMI'.
           05 FILLER            PIC X(22) VALUE SPACES.

      * 报表明细行
       01  DETAIL-LINE.                  
           05 FILLER           PIC X(5)  VALUE SPACES.
           05 DET-NAME         PIC X(40).
           05 FILLER           PIC X(5)  VALUE SPACES.
           05 DET-HEIGHT       PIC X(5).
           05 FILLER           PIC X(20)  VALUE SPACES.
           05 DET-WEIGHT       PIC X(12).
           05 FILLER           PIC X(10)  VALUE SPACES.
           05 DET-BMI          PIC 999.99.
           05 FILLER           PIC X VALUE '%'.
         
      * 过程部，程序主逻辑
       PROCEDURE DIVISION.                

      * 打开文件段落
       0050-OPEN-FILE.                    
           OPEN INPUT BMI-FILE.
           OPEN OUTPUT PRINT-FILE.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0400-STOP-RUN.

      * 处理记录段落
       0100-PROCESS-RECORDS.             

           PERFORM 0300-WRITE-HEADING-LINE.
      *    This is a priming read of the data file
           READ BMI-FILE
                AT END SET ENDOFBMI TO TRUE
                END-READ.
           PERFORM 0200-CALCULATE-BMI UNTIL ENDOFBMI. 

      * 计算 BMI 段落
       0200-CALCULATE-BMI.               
           COMPUTE WS-BMI = WEIGHT * 703 / (HEIGHT-INCHES * 
            HEIGHT-INCHES).
           MOVE PERSON-NAME TO DET-NAME.
           MOVE HEIGHT-INCHES TO DET-HEIGHT.
           MOVE WEIGHT TO DET-WEIGHT.
           MOVE WS-BMI TO DET-BMI.
           PERFORM 0320-WRITE-DETAIL-LINE.
           READ BMI-FILE
              AT END SET ENDOFBMI TO TRUE
           END-READ.

      * 写标题行段落
       0300-WRITE-HEADING-LINE.          
           MOVE HEADING-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.

      * 写明细行段落
       0320-WRITE-DETAIL-LINE.           
           MOVE DETAIL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

      * 结束处理段落
       0400-STOP-RUN.                    
          CLOSE BMI-FILE.
          CLOSE PRINT-FILE.

       STOP RUN.
       END PROGRAM ENHANCEDBMICALCULATOR.
