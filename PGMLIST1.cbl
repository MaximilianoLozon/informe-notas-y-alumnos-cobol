       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ALUMNOS ASSIGN TO "ALUMNOS.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS-ALU.
       SELECT NOTAS ASSIGN TO "NOTAS.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS-NOT.

       DATA DIVISION.
       FILE SECTION.
       FD ALUMNOS.
           01 ALUMNOS-REGISTRO.
               05 ALU-NRO-ALUMNO PIC 9(4).
               05 ALU-NOMBRE PIC X(23).
               05 ALU-NRO-PAIS PIC 999.
               05 ALU-ANIO PIC 9(4).
               05 ALU-MES PIC 99.

       FD NOTAS.
           01 NOTAS-ALUMNOS.
               05 NOT-NRO-ALUMNO PIC 9(4).
               05 NOT-NRO-MATERIA PIC 99.
               05 NOT-ANIO PIC 9(4).
               05 NOT-MES PIC 99.
               05 NOT-NOTA PIC 99.

       WORKING-STORAGE SECTION.
       01 WS-FS-ALU PIC XX.
           88 WS-FS-ALU-OK VALUE "00".
           88 WS-FS-ALU-NO VALUE "10".

       01 WS-FS-NOT PIC XX.
           88 WS-FS-NOT-OK VALUE "00".
           88 WS-FS-NOT-NO VALUE "10".

       01 WS-ANIO-IN PIC 9(4).
       01 WS-MES-IN PIC 99.

       01 CODIGO-ALUMNO PIC 9999.

       01 WS-ESPERAR-ENTER PIC X VALUE SPACE.

       01 INF-FECHA-INICIO PIC X(10).
       01 INF-FECHA-CORTE PIC X(6).
       01 INF-NOMBRE-ALUMNO PIC X(23).
       01 INF-NACIONALIDAD-ALUMNO PIC X(20).
       01 INF-NOMBRE-MATERIA PIC X(25).
       01 INF-MES PIC 99.
       01 INF-MATERIA-PROMEDIO PIC 99 VALUE 07.

       01 CORTE-1 PIC X.
           88 CORTE-1-TRUE VALUE "Y".
           88 CORTE-1-FALSE VALUE "N".

       01 CORTE-2 PIC X.
           88 CORTE-2-TRUE VALUE "Y".
           88 CORTE-2-FALSE VALUE "N".
      *rutinas
       01 RUTFECHA PIC X(8) VALUE "RUTFECHA".
       01 RUTMATER PIC X(8) VALUE "RUTMATER".
       01 RUTPAIS PIC X(8) VALUE "RUTPAIS".
      * AREA DE COMUNICACION RUTINAS
       COPY "COPY-ACF.cpy".
       COPY "COPY-ACN.cpy".
       COPY "COPY-ACM.cpy".

      * AREA DE PROCEDIMIENTOS
       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           PERFORM CARGAR-TABLAS-DE-RUTINAS
           MOVE "N" TO CORTE-1
           PERFORM 1000-INGRESAR-FECHA
           PERFORM 1001-CONVERTIR-FECHA
           PERFORM 1002-MOSTRAR-FECHA
           PERFORM 1003-ESPERAR-ENTER
           PERFORM 1004-ABRIR-ARCHIVOS
           PERFORM 2000-MOSTRAR-INFO UNTIL CORTE-1-TRUE
           PERFORM 4000-CERRAR-ARCHIVOS
           STOP RUN.

       1000-INGRESAR-FECHA.
           CALL "RUTFECHA" USING "3" F6-S-MES F6-S-A�O.

       1001-CONVERTIR-FECHA.
           MOVE F6-S-A�O TO WS-ANIO-IN
           MOVE F6-S-MES TO WS-MES-IN.

       1002-MOSTRAR-FECHA.
           MOVE WS-ANIO-IN TO INF-FECHA-INICIO.

       1003-ESPERAR-ENTER.
           DISPLAY "Presione ENTER para continuar" LINE 24 COLUMN 1
           ACCEPT WS-ESPERAR-ENTER.

       1004-ABRIR-ARCHIVOS.
           OPEN INPUT ALUMNOS
           IF NOT WS-FS-ALU-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO" LINE 23 COLUMN 1
               DISPLAY "FILE STATUS " WS-FS-ALU LINE 24 COLUMN 1
           END-IF.

       2000-MOSTRAR-INFO.
           PERFORM 2100-EXTRAE-DATO
           IF (WS-ANIO-IN * 100 + WS-MES-IN <= ALU-ANIO * 100 + ALU-MES)
           AND CORTE-1-FALSE
               MOVE ALU-NOMBRE TO INF-NOMBRE-ALUMNO
               MOVE ALU-NRO-ALUMNO TO CODIGO-ALUMNO
               PERFORM OBTENER-NOMBRE-NACIONALIDAD
               DISPLAY INF-NOMBRE-ALUMNO
               MOVE ALU-ANIO TO INF-FECHA-INICIO
               PERFORM 2200-MOSTRAR-NOTAS UNTIL CORTE-2-TRUE
           END-IF.

       2100-EXTRAE-DATO.
           READ ALUMNOS NEXT RECORD AT END
           MOVE "Y" TO CORTE-1.

       2200-MOSTRAR-NOTAS.
           PERFORM 2210-EXTRAE-DATO-NOTA
           MOVE NOT-NRO-MATERIA TO Codigo-materia
           PERFORM OBTENER-NOMBRE-MATERIA
           MOVE NOT-MES TO INF-MES
           DISPLAY INF-NOMBRE-MATERIA
           IF (WS-ANIO-IN * 100 + WS-MES-IN <= NOT-ANIO * 100 + NOT-MES)
           AND NOT WS-FS-NOT-OK
               ADD NOT-NOTA TO INF-MATERIA-PROMEDIO
           END-IF.

       2210-EXTRAE-DATO-NOTA.
           READ NOTAS NEXT RECORD AT END
           MOVE "Y" TO CORTE-2.

       CARGAR-TABLAS-DE-RUTINAS.
           INITIALIZE AreaDeComunicacionPais
           MOVE 1 TO Opcion-pais
           CALL RUTPAIS USING AreaDeComunicacionPais.

       OBTENER-NOMBRE-NACIONALIDAD.
           INITIALIZE AreaDeComunicacionPais
           MOVE 2 TO Opcion-pais
           MOVE ALU-NRO-PAIS TO Codigo-pais
           CALL RUTPAIS USING AreaDeComunicacionPais
           MOVE NOMBRE-pais TO INF-NACIONALIDAD-ALUMNO.

       OBTENER-NOMBRE-MATERIA.
           INITIALIZE AreaDeComunicacionMateria
           MOVE 2 TO Opcion-materia
           MOVE NOT-NRO-MATERIA TO Codigo-materia
           CALL RUTMATER USING AreaDeComunicacionMateria
           MOVE NOMBRE-MATERIA TO INF-NOMBRE-MATERIA.

       4000-CERRAR-ARCHIVOS.
           CLOSE ALUMNOS
           IF NOT WS-FS-ALU-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO" LINE 23 COLUMN 1
               DISPLAY "FILE STATUS " WS-FS-ALU LINE 24 COLUMN 1
           END-IF.
