       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTPAIS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT NACIONALIDAD            ASSIGN TO "NACIONALIDAD.dat"
                                       ORGANIZATION IS LINE SEQUENTIAL
                                           FILE STATUS IS WS-FS-NAC.
       DATA DIVISION.
       FILE SECTION.
       FD NACIONALIDAD.
           01 NACIONALIDAD-REGISTRO.
              05 NAC-CODIGO PIC 999.
              05 NAC-NOMBRE PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-FS-NAC PIC XX.
           88 WS-FS-NAC-OK VALUE "00".
           88 WS-FS-NAC-NO VALUE "10".
       01 TABLA-PAIS.
           05 PAIS OCCURS 10 TIMES.
               10 WS-NAC-COD PIC 999.
               10 WS-NAC-NOMBRE PIC X(20).
       77 INDICE PIC 99.
       LINKAGE SECTION.
       COPY "COPY-ACN.cpy".
      * AREA DE PROCEDIMIENTOS
       PROCEDURE DIVISION USING AreaDeComunicacionPais.
       1000-INICIO.
           EVALUATE Opcion-pais
               WHEN 1
               PERFORM 2000-PROCEDIMIENTO-CREA-TABLA
               WHEN 2
               PERFORM 3000-PROCEDIMIENTO-BUSCA-CODIGO
               WHEN OTHER
               MOVE "30" TO RET-COD-2
               MOVE "OPCION INVALIDA" TO DESC-ERROR-2
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
               OPEN INPUT NACIONALIDAD.
               IF NOT WS-FS-NAC-OK THEN
                   MOVE "10" TO RET-COD-2
                   MOVE "ERROR AL ABRIR ARCHIVO" TO DESC-ERROR-2
               END-IF.

           2200-CARGAR-TABLA.
               PERFORM 2210-EXTRAE-DATO
               MOVE NAC-CODIGO TO WS-NAC-COD(INDICE)
               MOVE NAC-NOMBRE TO WS-NAC-NOMBRE(INDICE)
               MOVE "CARGA EXITOSA" TO DESC-ERROR-2
               MOVE "00" TO RET-COD-2.

           2210-EXTRAE-DATO.
               READ NACIONALIDAD NEXT RECORD AT END
               IF NOT WS-FS-NAC-OK THEN
                   MOVE "10" TO RET-COD-2
                   MOVE "ERROR AL CARGAR REGISTRO" TO DESC-ERROR-2
               END-IF.
           4000-CERRAR-ARCHIVO.
               CLOSE NACIONALIDAD.
               IF NOT WS-FS-NAC-OK THEN
                   MOVE "10" TO RET-COD-2
                   MOVE "ERROR AL CERRAR ARCHIVO" TO DESC-ERROR-2
               END-IF.

       3000-PROCEDIMIENTO-BUSCA-CODIGO.
           MOVE 0 TO INDICE
           PERFORM VARYING INDICE FROM 1 BY 1
           UNTIL INDICE > 10 OR RET-COD-2 = "00"
               IF Codigo-pais = WS-NAC-COD(INDICE) THEN
                   MOVE WS-NAC-NOMBRE(INDICE) TO NOMBRE-pais
                   MOVE "00" TO RET-COD-2
               ELSE
                   MOVE "20" TO RET-COD-2
               END-IF
           END-PERFORM
           IF RET-COD-2 = "00" THEN
               MOVE "CODIGO ENCONTRADO" TO DESC-ERROR-2
           ELSE
               MOVE "CODIGO NO ENCONTRADO" TO DESC-ERROR-2
           END-IF.
       END PROGRAM RUTPAIS.
