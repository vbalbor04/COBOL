      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CorteC1.
       ENVIRONMENT DIVISION.

      *------------------------------------------------

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARTICULOS
           ASSIGN TO "DATOS_Articulos.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FSTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD ARTICULOS.
       01  REGISTRO-ARTICULOS.
           03 FD-ARTICULO    PIC X(10).
           03 FD-MARCA       PIC X(10).
           03 FD-TALLE       PIC X(10).
           03 FD-COLOR       PIC X(10).

      *------------------------------------------------

       WORKING-STORAGE SECTION.
       01  WS-FIN                PIC X(1).
           88 SI-FIN             VALUE 'S'.
           88 NO-FIN             VALUE 'N'.

       77  WS-FSTATUS            PIC X(2).
       77  WS-TALLE              PIC X(10).
       77  WS-CONTA-TALLES       PIC 9(05).
       77  WS-CONTA-REG          PIC 9(05).

      *------------------------------------------------

       PROCEDURE DIVISION.
           PERFORM 100-INICIO THRU F-100-INICIO.
           PERFORM 200-PROCESO THRU F-200-PROCESO UNTIL SI-FIN.
           PERFORM 300-FINAL THRU F-300-FINAL.
           STOP RUN.

      *------------------------------------------------
       100-INICIO.

           SET NO-FIN TO TRUE.
           MOVE 0 TO WS-CONTA-REG.

           OPEN INPUT ARTICULOS.
      *    DISPLAY 'WS-FSTATUS OPEN: ' WS-FSTATUS.

           PERFORM LEER-ARCHIVO THRU F-LEER-ARCHIVO.
      *    DISPLAY 'WS-FSTATUS PRIMER READ: ' WS-FSTATUS.
           IF SI-FIN
              DISPLAY 'EL ARCHIVO ESTA VACIO'
           END-IF.

       F-100-INICIO.
           EXIT.

       200-PROCESO.
           MOVE FD-TALLE TO WS-TALLE.
           MOVE ZEROES TO WS-CONTA-TALLES.
           PERFORM 210-CORTE-TALLE THRU F-210-CORTE-TALLE
               UNTIL SI-FIN OR FD-TALLE NOT EQUAL WS-TALLE.
           DISPLAY 'HAY ' WS-CONTA-TALLES ' DEL TALLE: '
                   WS-TALLE.

       F-200-PROCESO.
           EXIT.


       300-FINAL.
           CLOSE ARTICULOS.
           DISPLAY 'WS-FSTATUS CLOSE: ' WS-FSTATUS.
       F-300-FINAL.
           EXIT.
      *------------------------------------------------

       210-CORTE-TALLE.
           ADD 1 TO WS-CONTA-TALLES.
           PERFORM LEER-ARCHIVO THRU F-LEER-ARCHIVO.
       F-210-CORTE-TALLE.
           EXIT.

      *------------------------------------------------

       LEER-ARCHIVO.
           READ ARTICULOS NEXT
               AT END
                   SET SI-FIN TO TRUE
      *             DISPLAY 'FIN: ' WS-FIN
               NOT AT END
                   ADD 1 TO WS-CONTA-REG
      *              DISPLAY WS-CONTA-REG ' * '
      *                      FD-ARTICULO ' * '
      *                      FD-MARCA    ' * '
      *                      FD-TALLE    ' * '
      *                      FD-COLOR    ' * '
      *                      WS-FIN
           END-READ.

       F-LEER-ARCHIVO.
           EXIT.

      *------------------------------------------------
