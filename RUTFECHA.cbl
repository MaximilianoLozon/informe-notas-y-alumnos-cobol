       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTFECHA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 DIA PIC 99.
       77 MES PIC 99.
       77 AÑO PIC 9999.
       77 F-DIA PIC 99.
       77 F-MES PIC 99.
       77 F-AÑO PIC 9999.
       77 DIA-MIN PIC 99 VALUE 01.
       77 DIA-MAX PIC 99.
       77 MES-MIN PIC 99 VALUE 01.
       77 MES-MAX PIC 99 VALUE 12.
       77 AÑO-MIN PIC 9999.
       77 AÑO-MAX PIC 9999.
       77 NOMBREMES PIC x(10).
       77 RESTO pic 999.
       77 RESULTADO pic 999.
       77 BISIESTO PIC 9 VALUE 4.
       77 CONTADOR-DIA PIC 9 VALUE 0.
       77 CONTADOR-MES PIC 9 VALUE 0.
       77 CONTADOR-AÑO PIC 9 VALUE 0.
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
                   MOVE 1900 TO AÑO-MIN
                   MOVE 2040 TO AÑO-MAX
                   PERFORM ValidarIngresoAño
                   PERFORM ValidarIngresoMes
                   PERFORM ValidarIngresoDia
                   PERFORM FormatearFecha
                   PERFORM MoverCampos
                   PERFORM FinalizarRutina
               WHEN 2
                   MOVE F-I-DIA TO DIA
                   MOVE F-I-MES TO MES
                   MOVE F-I-AÑO TO AÑO
                   PERFORM FormatearFecha
                   PERFORM MoverCampos
                   PERFORM FinalizarRutina
               WHEN 3
                   MOVE 2000 TO AÑO-MIN
                   MOVE 2023 TO AÑO-MAX
                   PERFORM ValidarIngresoAño
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

       ValidarIngresoAño.
           MOVE "A" TO C-CONTROL
           PERFORM INGRESAR-AÑO UNTIL CONTROL-SI.

           INGRESAR-AÑO.
           DISPLAY "INGRESE UN AÑO VALIDO ENTRE "AÑO-MIN" Y "AÑO-MAX
           ACCEPT AÑO
           IF AÑO > AÑO-MAX OR AÑO < AÑO-MIN OR AÑO IS NOT NUMERIC THEN
               ADD 1 TO CONTADOR-AÑO
               MOVE "N" TO C-CONTROL
           ELSE
               MOVE "S" TO C-CONTROL
           END-IF
           IF CONTADOR-AÑO >=5 THEN
               MOVE "S" TO C-CONTROL
           END-IF.

       ValidarIngresoMes.
           MOVE "A" TO C-CONTROL
           PERFORM INGRESAR-MES UNTIL CONTROL-SI.

           INGRESAR-MES.
           DISPLAY "INGRESE UN MES VALIDO (ENTRE 1 Y 12)"
           ACCEPT MES
           IF MES > MES-MAX OR MES < MES-MIN OR MES IS NOT NUMERIC THEN
               ADD 1 TO CONTADOR-AÑO
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
               ADD 1 TO CONTADOR-AÑO
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
               DIVIDE BISIESTO INTO AÑO GIVING RESULTADO REMAINDER RESTO
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
           MOVE AÑO TO F-AÑO.

      *ASIGNAR VALORES A FORMATOS
       MoverCampos.
      *FORMATO 1
       MOVE F-DIA TO F1-S-DIA
       MOVE F-MES TO F1-S-MES
       MOVE F-AÑO TO F1-S-AÑO
      *FORMATO 2
       MOVE F-DIA TO F2-S-DIA
       MOVE F-MES TO F2-S-MES
       MOVE F-AÑO TO F2-S-AÑO
      *FORMATO 3
       MOVE F-DIA TO F3-S-DIA
       MOVE F-MES TO F3-S-MES
       MOVE F-AÑO TO F3-S-AÑO
      *FORMATO 4
       MOVE F-DIA TO F4-S-DIA
       MOVE F-MES TO F4-S-MES
       MOVE F-AÑO TO F4-S-AÑO
      *FORMATO 5
       MOVE F-DIA TO F5-S-DIA
       MOVE NOMBREMES TO F5-S-MES
       MOVE F-AÑO TO F5-S-AÑO.
      *FORMATO 6
       MOVE F-MES TO F6-S-MES
       MOVE F-AÑO TO F6-S-AÑO.


       END PROGRAM RUTFECHA.
