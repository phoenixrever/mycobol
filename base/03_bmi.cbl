       IDENTIFICATION DIVISION.
       PROGRAM-ID. INPUT-OUT.
       AUTHOR. チヨウ殿.
      * This is a comment in COBOL
      * This program calculates BMI based on user input for weight and 
      * - height.
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01 VAR-A   PIC A(5).    * 只能是5个字母字符
      *01 VAR-X   PIC X(5).    * 5个任意字符（数字、字母、符号均可）
      *01 VAR-N   PIC 9(5).    * 5位数字，如 12345 或者 99999
      *01 VAR-S   PIC S9(5).   * 带符号的5位数字，如 -12345
      *01 VAR-D   PIC 9(3)V99. * 数值，3位整数和2位小数，比如 123.45
      *===============================================
       01 HEIGHT-CM           PIC 9(4)V99 VALUE 0.
       01 WEIGHT-KG           PIC 9(4)V99 VALUE 0.
       01 HEIGHT-M            PIC 9(2)V999 VALUE 0.
       01 HEIGHT-SQUARE       PIC 9(4)V9999 VALUE 0.
       01 BMI                 PIC 9(2)V99 VALUE 0.
       01 BMI-RESULT          PIC X(20) VALUE SPACE.  
   
       PROCEDURE DIVISION.
       PERFORM 0100-START-HERE.
       0100-START-HERE.
           DISPLAY "请输入身高（单位：厘米）：".
           ACCEPT HEIGHT-CM

           DISPLAY "请输入体重（单位：公斤）：".
           ACCEPT WEIGHT-KG

      * BMI 计算公式：BMI = 体重(kg) / (身高(m) * 身高(m))       
           COMPUTE HEIGHT-M = HEIGHT-CM / 100
           COMPUTE HEIGHT-SQUARE = HEIGHT-M * HEIGHT-M
           COMPUTE BMI = WEIGHT-KG / HEIGHT-SQUARE
      * COMPUTE BMI = WEIGHT-KG / ((HEIGHT-CM / 100) * 
      * - (HEIGHT-CM / 100))

           IF BMI < 18.5 THEN
               MOVE "Underweight" TO BMI-RESULT
           ELSE IF BMI < 24.9 THEN
               MOVE "Normal weight" TO BMI-RESULT
           ELSE IF BMI < 29.9 THEN
               MOVE "Overweight" TO BMI-RESULT
           ELSE
               MOVE "Obesity" TO BMI-RESULT
           END-IF.
           
           DISPLAY "Your BMI is: " BMI.
           DISPLAY "BMI Result: " BMI-RESULT.
           STOP RUN.
       END PROGRAM INPUT-OUT.
       