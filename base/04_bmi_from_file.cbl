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
       AUTHOR.     チョウ殿.
      * This program reads input from a file

      * 环境说明部
       ENVIRONMENT DIVISION.              

      * 输入输出节
       INPUT-OUTPUT SECTION.              
      * FILE-CONTROL 表示文件控制部分（相当于创建文件变量），定义了程序将使用的文件。
       FILE-CONTROL.
      * ASSIGN TO 表示将文件变量与外部文件关联起来，指定外部文件的名称。
      * 相对路径按程序的当前工作目录解析；从项目根目录运行时，
      * 输入文件位于 DATA/BMI-INPUT.DAT。
      * 注意这边路径是相对base，我在base 里面运行 bin/04_bmi_form_file 
      * - 而不是相对于源代码文件的路径。
      * 固定宽度读取时，程序按字节位置切字段，按字节数补空格，而不是按字符数
            SELECT BMI-FILE ASSIGN TO "data/BMI-INPUT.DAT"
      * ORGANIZATION IS LINE SEQUENTIAL 表示按文本行顺序读取     
      * 其他 的组织方式包括：
      * - ORGANIZATION IS SEQUENTIAL：顺序文件，按记录顺序访问
      * - ORGANIZATION IS INDEXED：索引文件，按索引访问记录
      * - ORGANIZATION IS RELATIVE：相对文件，按相对记录号访问
               ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PRINT-FILE ASSIGN TO "report/BMI-REPORT.DAT".

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

      * FD 表示文件描述部分，定义文件记录在内存中的结构和字段格式。
       FD BMI-FILE.
      * 每次 READ 成功时，输入记录会放入 BMIDETAILS。
       01 BMIDETAILS.                   
           05 PERSON-NAME.
              10 LASTNAME      PIC X(20).
              10 FIRSTNAME     PIC X(20).
           05 HEIGHT-CM        PIC 9(4)V99.
           05 WEIGHT-KG        PIC 9(4)V99.

      * 输出文件描述
       FD PRINT-FILE.
      * 输出记录长度设为 160，足以容纳包含 BMI-RESULT 的明细行。
       01 PRINT-LINE        PIC X(160).

      * 工作存储节：定义程序运行期间使用的状态和计算变量。
       WORKING-STORAGE SECTION. 
      *===============================================
      * WS 自己起的变量名，通常是 Working Storage（工作存储区） 的缩写。 
      * WS-EOF 与输入记录分开保存，避免把业务数据当作文件状态。
      * 88 ENDOFBMI 88 级条件名不额外占用存储空间
      * SET ENDOFBMI TO TRUE 相当于 MOVE 'Y' TO WS-EOF。
      * 输出内容 ws：
      * N00000
      * - WS-EOF 是 N，占 1 个字符。
      * - WS-BMI 是 00000，占 5 个数字字符。
      * - V 是隐含小数点，不占存储空间。
      * 
      * 同级字段通常使用相同的层级编号
      *
      *=============================================== 
       01  WS. 
      * 初始值 N 约定表示 Not End 尚未遇到文件末尾；读取到文件末尾时改为 Y。
           05 WS-EOF           PIC X VALUE 'N'.
              88 ENDOFBMI      VALUE 'Y'.
      * BMI 通常有三位整数和两位小数，例如 24.22。
           05 WS-BMI           PIC 999V99.

      * 报表标题行 通过多个字段控制每个标题的位置和宽度。它本身不会换行
      * FILLER 表示该字段没有名字，通常用于占位或不需要引用的字段。
       01  HEADING-LINE.                
           05 FILLER            PIC X(5) VALUE SPACES.
           05 FILLER            PIC X(40) VALUE 'NAME'.
           05 FILLER            PIC X(22) VALUE 'HEIGHT IN CM'.
           05 FILLER            PIC X(30) VALUE 'WEIGHT IN KG'.
           05 FILLER            PIC X(12) VALUE 'BMI'.
           05 FILLER            PIC X(22) VALUE SPACES.
           05 FILLER            PIC X(20) VALUE 'BMI RESULT'.

      * 报表明细行
       01  DETAIL-LINE.                  
           05 FILLER           PIC X(5)  VALUE SPACES.
           05 DET-NAME         PIC X(40).
           05 FILLER           PIC X(5)  VALUE SPACES.
      * 数值编辑字段用于在报表中显示小数点和千位分隔符。
      * Z 表示如果数字为零则显示为空格，9 表示显示数字，V 表示隐含小数点。
      * , 数字达到千位时显示逗号，例如 1,234.56。
      * 9 必须显示一位数字
      * Z,ZZ9.99 表示4位整数、2位小数，前导零显示为空格，小数点后两位。
           05 DET-HEIGHT       PIC Z,ZZ9.99.
           05 FILLER           PIC X(20)  VALUE SPACES.
           05 DET-WEIGHT-KG    PIC Z,ZZ9.99.
           05 FILLER           PIC X(10)  VALUE SPACES.
           05 DET-BMI          PIC 999.99.
           05 FILLER           PIC X VALUE '%'.
           05 FILLER           PIC X(20)  VALUE SPACES.
           05 BMI-RESULT       PIC X(20) VALUE SPACE.
         
      * 过程部：按“打开文件 -> 处理记录 -> 关闭文件”的顺序执行。
      * 句点结束主流程语句，STOP RUN 确保所有段落执行完后正常退出。
       PROCEDURE DIVISION.                
           PERFORM 0050-OPEN-FILE
           PERFORM 0100-PROCESS-RECORDS
           PERFORM 0400-CLOSE-FILES
           STOP RUN.

      * 打开输入文件及报表输出文件。
      * 该段落只负责 OPEN，不在这里启动处理或结束程序，
      * 以便主流程能清楚地控制各步骤的执行顺序。
       0050-OPEN-FILE.                    
           OPEN INPUT BMI-FILE.
           OPEN OUTPUT PRINT-FILE.

      * 输出报表标题，然后逐条读取并处理输入记录。
      * 每次循环只执行一次 READ：
      * - AT END：设置独立的 EOF 条件，下一轮循环前结束循环；
      * - NOT AT END：当前 BMIDETAILS 有效，立即计算并输出明细。
      * 空输入文件也会输出标题，然后在第一次 READ 的 AT END 退出。
       0100-PROCESS-RECORDS.             
           PERFORM 0300-WRITE-HEADING-LINE.
           PERFORM UNTIL ENDOFBMI
               READ BMI-FILE
                   AT END
                       SET ENDOFBMI TO TRUE
                   NOT AT END
                       PERFORM 0200-CALCULATE-BMI
               END-READ
           END-PERFORM.

      * 根据当前成功读取的记录计算并输出 BMI。
      * 输入字段使用厘米和公斤，因此公制公式为：
      * BMI = 体重(kg) * 10000 / 身高(cm)的平方。
      * 身高为零时公式会发生除零错误，因此报告提示并跳过该条记录。
      * 本段落不再执行 READ；所有文件读取统一由 0100 段落控制。
       0200-CALCULATE-BMI.               
           IF HEIGHT-CM = ZERO
               DISPLAY "ZERO HEIGHT; RECORD SKIPPED."
           ELSE
      * ROUNDED 使计算结果按目标字段的小数位数进行四舍五入。
               COMPUTE WS-BMI ROUNDED =
                   WEIGHT-KG * 10000 /
                   (HEIGHT-CM * HEIGHT-CM)
               MOVE PERSON-NAME TO DET-NAME
               MOVE HEIGHT-CM TO DET-HEIGHT
               MOVE WEIGHT-KG TO DET-WEIGHT-KG
               MOVE WS-BMI TO DET-BMI
               PERFORM 0250-CALCULATE-RESULT
               PERFORM 0320-WRITE-DETAIL-LINE
           END-IF.

       0250-CALCULATE-RESULT.
      * 使用未编辑的数值 WS-BMI 做比较；DET-BMI 是用于显示的编辑字段。
           IF WS-BMI < 18.5 THEN
               MOVE "Underweight" TO BMI-RESULT
           ELSE IF WS-BMI < 24.9 THEN
               MOVE "Normal weight" TO BMI-RESULT
           ELSE IF WS-BMI < 29.9 THEN
               MOVE "Overweight" TO BMI-RESULT
           ELSE
               MOVE "Obesity" TO BMI-RESULT
           END-IF. 
           
      * 将标题记录写入报表文件，并在标题后额外写一行空白行。
      * PRINT-LINE COBOL的 WRITE 通常要求写入文件记录 FD 中定义的记录名，
      * - 不能写另一个WORKING-STORAGE里的数据项。
      * AFTER ADVANCING 表示先推进，再写入 LINE 表示推进的单位是行
      * BEFORE ADVANCING 表示写入后再推进。
      * 实际执行顺序：先空一行，再写入标题。
       0300-WRITE-HEADING-LINE.          
           MOVE HEADING-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
      * 把 PRINT-LINE 的 160 个字符全部设为空格。
           MOVE SPACES TO PRINT-LINE.
      * 把这条全为空格的记录写入输出文件，在标题下方形成空白行。这里不换行
           WRITE PRINT-LINE.

      * 将已填充的 DETAIL-LINE 写入报表文件。
       0320-WRITE-DETAIL-LINE.           
           MOVE DETAIL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

      * 关闭所有已打开的文件。
      * STOP RUN 放在主流程中，而不是放在本段落里，
      * 这样段落职责单一，PERFORM 返回行为也更清晰。
       0400-CLOSE-FILES.                    
           CLOSE BMI-FILE.
           CLOSE PRINT-FILE.

       END PROGRAM ENHANCEDBMICALCULATOR.
