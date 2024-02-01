       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTFECHA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 DIA PIC 99.
       77 MES PIC 99.
       77 A�O PIC 9999.
       77 F-DIA PIC 99.
       77 F-MES PIC 99.
       77 F-A�O PIC 9999.
       77 DIA-MIN PIC 99 VALUE 01.
       77 DIA-MAX PIC 99.
       77 MES-MIN PIC 99 VALUE 01.
       77 MES-MAX PIC 99 VALUE 12.
       77 A�O-MIN PIC 9999.
       77 A�O-MAX PIC 9999.
       77 NOMBREMES PIC x(10).
       77 RESTO pic 999.
       77 RESULTADO pic 999.
       77 BISIESTO PIC 9 VALUE 4.
       77 CONTADOR-DIA PIC 9 VALUE 0.
       77 CONTADOR-MES PIC 9 VALUE 0.
       77 CONTADOR-A�O PIC 9 VALUE 0.
       01 C-CONTROL PIC X.
           88 CONTROL-SI VALUE "S".
           88 CONTROL-NO VALUE "N".
       LINKAGE SECTION.
      * INICIO DEL AREA DE COMUNICACION / COPY
       COPY "COPY-ACF.cpy".
      * FIN DEL AREA DE COMUNICACION / COPY

       PROCEDURE DIVISION USING AreaDeComunicacionFecha.
       MAIN-PROCEDURE.
           MOVE 00 TO Valor
           MOVE "FECHA CARGADA CORRECTAMENTE" TO Desc-retorno
           EVALUATE Opcion
               WHEN 1
                   MOVE 1900 TO A�O-MIN
                   MOVE 2040 TO A�O-MAX
                   PERFORM ValidarIngresoA�o
                   PERFORM ValidarIngresoMes
                   PERFORM ValidarIngresoDia
                   PERFORM FormatearFecha
                   PERFORM MoverCampos
                   PERFORM FinalizarRutina
               WHEN 2
                   MOVE F-I-DIA TO DIA
                   MOVE F-I-MES TO MES
                   MOVE F-I-A�O TO A�O
                   PERFORM FormatearFecha
                   PERFORM MoverCampos
                   PERFORM FinalizarRutina
               WHEN 3
                   MOVE 2000 TO A�O-MIN
                   MOVE 2023 TO A�O-MAX
                   PERFORM ValidarIngresoA�o
                   PERFORM ValidarIngresoMes
                   PERFORM FormatearFecha
                   PERFORM MoverCampos
                   PERFORM FinalizarRutina
               WHEN OTHER
                   MOVE 10 TO Valor
                   MOVE "OPCION INVALIDA" TO Desc-retorno
                   PERFORM FinalizarRutina
           END-EVALUATE.
       FinalizarRutina.
           GOBACK.

       ValidarIngresoA�o.
           MOVE "A" TO C-CONTROL
           PERFORM INGRESAR-A�O UNTIL CONTROL-SI.

           INGRESAR-A�O.
           DISPLAY "INGRESE UN A�O VALIDO ENTRE "A�O-MIN" Y "A�O-MAX
           ACCEPT A�O
           IF A�O > A�O-MAX OR A�O < A�O-MIN OR A�O IS NOT NUMERIC THEN
               ADD 1 TO CONTADOR-A�O
               MOVE "N" TO C-CONTROL
           ELSE
               MOVE "S" TO C-CONTROL
           END-IF
           IF CONTADOR-A�O >=5 THEN
               MOVE "S" TO C-CONTROL
           END-IF.

       ValidarIngresoMes.
           MOVE "A" TO C-CONTROL
           PERFORM INGRESAR-MES UNTIL CONTROL-SI.

           INGRESAR-MES.
           DISPLAY "INGRESE UN MES VALIDO (ENTRE 1 Y 12)"
           ACCEPT MES
           IF MES > MES-MAX OR MES < MES-MIN OR MES IS NOT NUMERIC THEN
               ADD 1 TO CONTADOR-A�O
               MOVE "N" TO C-CONTROL
           ELSE
               MOVE "S" TO C-CONTROL
           END-IF
           IF CONTADOR-MES >=5 THEN
               MOVE "S" TO C-CONTROL
           END-IF.


       ValidarIngresoDia.
       PERFORM EVALUAR-MES
       MOVE "A" TO C-CONTROL
       PERFORM VALIDAR-DIA UNTIL CONTROL-SI.
       VALIDAR-DIA.
           DISPLAY "INGRESE UN DIA VALIDO ENTRE "DIA-MIN" Y "DIA-MAX
           ACCEPT DIA
           IF DIA > DIA-MAX OR DIA < DIA-MIN OR DIA IS NOT NUMERIC THEN
               ADD 1 TO CONTADOR-A�O
               MOVE "N" TO C-CONTROL
           ELSE
               MOVE "S" TO C-CONTROL
           END-IF
           IF CONTADOR-DIA >=5 THEN
               MOVE "S" TO C-CONTROL
           END-IF.

       EVALUAR-MES.
      * DE PASO ASIGNAMOS NOMBRE AL MES
           EVALUATE MES
           WHEN 01
               MOVE "ENERO" TO NOMBREMES
               MOVE 31 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 02
               MOVE "FEBRERO" TO NOMBREMES
               DIVIDE BISIESTO INTO A�O GIVING RESULTADO REMAINDER RESTO
               IF RESTO = 0 THEN
                   MOVE 29 TO DIA-MAX
      *             PERFORM INGRESAR-DIA
               ELSE
                   MOVE 28 TO DIA-MAX
      *             PERFORM INGRESAR-DIA
               END-IF
           WHEN 03
               MOVE "MARZO" TO NOMBREMES
               MOVE 31 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 04
               MOVE "ABRIL" TO NOMBREMES
               MOVE 30 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 05
               MOVE "MAYO" TO NOMBREMES
               MOVE 31 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 06
               MOVE "JUNIO" TO NOMBREMES
               MOVE 30 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 07
               MOVE "JULIO" TO NOMBREMES
               MOVE 31 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 08
               MOVE "AGOSTO" TO NOMBREMES
               MOVE 31 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 09
               MOVE "SEPTIEMBRE" TO NOMBREMES
               MOVE 30 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 10
               MOVE "OCTUBRE" TO NOMBREMES
               MOVE 31 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 11
               MOVE "NOVIEMBRE" TO NOMBREMES
               MOVE 30 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           WHEN 12
               MOVE "DICIEMBRE" TO NOMBREMES
               MOVE 31 TO DIA-MAX
      *         PERFORM INGRESAR-DIA
           END-EVALUATE.

       FormatearFecha.
           PERFORM EVALUAR-MES
           MOVE DIA TO F-DIA
           MOVE MES TO F-MES
           MOVE A�O TO F-A�O.

      *ASIGNAR VALORES A FORMATOS
       MoverCampos.
      *FORMATO 1
       MOVE F-DIA TO F1-S-DIA
       MOVE F-MES TO F1-S-MES
       MOVE F-A�O TO F1-S-A�O
      *FORMATO 2
       MOVE F-DIA TO F2-S-DIA
       MOVE F-MES TO F2-S-MES
       MOVE F-A�O TO F2-S-A�O
      *FORMATO 3
       MOVE F-DIA TO F3-S-DIA
       MOVE F-MES TO F3-S-MES
       MOVE F-A�O TO F3-S-A�O
      *FORMATO 4
       MOVE F-DIA TO F4-S-DIA
       MOVE F-MES TO F4-S-MES
       MOVE F-A�O TO F4-S-A�O
      *FORMATO 5
       MOVE F-DIA TO F5-S-DIA
       MOVE NOMBREMES TO F5-S-MES
       MOVE F-A�O TO F5-S-A�O.
      *FORMATO 6
       MOVE F-MES TO F6-S-MES
       MOVE F-A�O TO F6-S-A�O.


       END PROGRAM RUTFECHA.
