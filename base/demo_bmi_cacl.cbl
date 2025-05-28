       IDENTIFICATION DIVISION.
       PROGRAM-ID. INPUT-OUT.
       AUTHOR. チヨウ殿.
      * This is a comment in COBOL
      * This program calculates BMI based on user input for weight and 
      * - height.
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 HEIGHT-CM           PIC 9(4)V99 VALUE 0.
       01 WEIGHT-KG           PIC 9(4)V99 VALUE 0.
       01 HEIGHT-M            PIC 9(2)V999 VALUE 0.
       01 HEIGHT-SQUARE       PIC 9(4)V9999 VALUE 0.
       01 BMI                 PIC 9(2)V99 VALUE 0.
       01 BMI-RESULT          PIC X(20) VALUE SPACE.  
   
       PROCEDURE DIVISION.
           DISPLAY "请输入身高（单位：厘米）：".
           ACCEPT HEIGHT-CM

           DISPLAY "请输入体重（单位：公斤）：".
           ACCEPT WEIGHT-KG

      * BMI 计算公式：BMI = 体重(kg) / (身高(m) * 身高(m))       
           COMPUTE HEIGHT-M = HEIGHT-CM / 100
           COMPUTE HEIGHT-SQUARE = HEIGHT-M * HEIGHT-M
           COMPUTE BMI = WEIGHT-KG / HEIGHT-SQUARE
      *    COMPUTE BMI = WEIGHT-KG / ((HEIGHT-CM / 100) * 
      *    - (HEIGHT-CM / 100))

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
       