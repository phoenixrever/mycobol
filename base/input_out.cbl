       IDENTIFICATION DIVISION.
       PROGRAM-ID. INPUT-OUT.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-INPUT        PIC X(50).
       PROCEDURE DIVISION.
           DISPLAY "input will error when you use long japanses : ".
           ACCEPT USER-INPUT.
           DISPLAY "output will error use long japanses: " USER-INPUT.
      *单词或标识符不能跨行，否则会触发警告。
      *建议使用多次 DISPLAY 或 将字符串分行
      *-    USER-INPUT 
      *    DISPLAY USER-INPUT *使用多次 DISPLAY
           STOP RUN.
       END PROGRAM INPUT-OUT.
       