       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTMATER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MATERIAS             ASSIGN TO "MATERIAS.dat"
                                       ORGANIZATION IS LINE SEQUENTIAL
                                           FILE STATUS IS WS-FS-MAT.
       DATA DIVISION.
       FILE SECTION.
       FD MATERIAS.
           01 MATERIAS-REGISTRO.
              05 MAT-CODIGO PIC ZZ.
              05 MAT-NOMBRE PIC X(25).

       WORKING-STORAGE SECTION.
       01 WS-FS-MAT PIC XX.
           88 WS-FS-MAT-OK VALUE "00".
           88 WS-FS-MAT-NO VALUE "10".
       01 TABLA-MATERIAS.
           05 MATERIA OCCURS 10 TIMES.
               10 WS-MAT-COD PIC 99.
               10 WS-MAT-NOMBRE PIC X(25).
       77 INDICE PIC 99.
       LINKAGE SECTION.
       COPY "COPY-ACM.cpy".
      * AREA DE PROCEDIMIENTOS
       PROCEDURE DIVISION USING AreaDeComunicacionMateria.
       1000-INICIO.
           EVALUATE Opcion-materia
               WHEN 1
               PERFORM 2000-PROCEDIMIENTO-CREA-TABLA
               WHEN 2
               PERFORM 3000-PROCEDIMIENTO-BUSCA-CODIGO
               WHEN OTHER
               MOVE "30" TO RET-COD
               MOVE "OPCION INVALIDA" TO DESC-ERROR
           END-EVALUATE.
       PERFORM 4000-FINAL.

       4000-FINAL.
           GOBACK.

       2000-PROCEDIMIENTO-CREA-TABLA.
       PERFORM 2100-ABRIR-ARCHIVO
           PERFORM 2200-CARGAR-TABLA VARYING INDICE FROM 1 BY 1
           UNTIL INDICE > 8
           PERFORM 4000-CERRAR-ARCHIVO.

           2100-ABRIR-ARCHIVO.
               OPEN INPUT MATERIAS.
               IF NOT WS-FS-MAT-OK THEN
                   MOVE "10" TO RET-COD
                   MOVE "ERROR AL ABRIR ARCHIVO" TO DESC-ERROR
               END-IF.

           2200-CARGAR-TABLA.
               PERFORM 2210-EXTRAE-DATO
               MOVE MAT-CODIGO TO WS-MAT-COD(INDICE)
               MOVE MAT-NOMBRE TO WS-MAT-NOMBRE(INDICE)
               MOVE "CARGA EXITOSA" TO DESC-ERROR
               MOVE "00" TO RET-COD.

           2210-EXTRAE-DATO.
               READ MATERIAS NEXT RECORD AT END
               IF NOT WS-FS-MAT-OK THEN
                   MOVE "10" TO RET-COD
                   MOVE "ERROR AL CARGAR REGISTRO" TO DESC-ERROR
               END-IF.
           4000-CERRAR-ARCHIVO.
               CLOSE MATERIAS.
               IF NOT WS-FS-MAT-OK THEN
                   MOVE "10" TO RET-COD
                   MOVE "ERROR AL CERRAR ARCHIVO" TO DESC-ERROR
               END-IF.

       3000-PROCEDIMIENTO-BUSCA-CODIGO.
           MOVE 0 TO INDICE
               PERFORM VARYING INDICE FROM 1 BY 1
               UNTIL INDICE > 8 OR RET-COD = "00"
                   IF Codigo-materia = WS-MAT-COD(INDICE) THEN
                       MOVE WS-MAT-NOMBRE(INDICE) TO NOMBRE-MATERIA
                       MOVE "00" TO RET-COD
                   ELSE
                       MOVE "20" TO RET-COD
                   END-IF
               END-PERFORM
           IF RET-COD = "00" THEN
               MOVE "CODIGO ENCONTRADO" TO DESC-ERROR
           ELSE
               MOVE "CODIGO NO ENCONTRADO" TO DESC-ERROR
           END-IF.
       END PROGRAM RUTMATER.
