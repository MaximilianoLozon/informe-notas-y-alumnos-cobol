       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGENMATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * OPCIONAL EVITA ERROR DE COMPILACION SI EL ARCHIVO NO EXISTE
       SELECT MATERIAS             ASSIGN TO "MATERIAS.dat"
                                       ORGANIZATION IS LINE SEQUENTIAL
                                           FILE STATUS IS WS-FS-MAT.

       DATA DIVISION.
       FILE SECTION.
       FD MATERIAS.
           01 MATERIAS-REGISTRO.
              05 MAT-CODIGO PIC 99.
              05 MAT-NOMBRE PIC X(25).

       WORKING-STORAGE SECTION.
      *  WorkingStorage-FileStatus-Nacionalidad.
       01 WS-FS-MAT PIC XX.
           88 WS-FS-MAT-OK VALUE "00".
           88 WS-FS-MAT-NO VALUE "10".
       77 WS-MAT-COD PIC ZZ VALUE"00".
       77 WS-MAT-NOMBRE PIC X(25) VALUE SPACES.
       01 WS-CONTROL PIC XX.
           88 WS-CONTROL-OK VALUE "SI".
           88 WS-CONTROL-NO VALUE "NO".

       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           PERFORM 1000-ABRIR-ARCHIVO
           PERFORM 2000-PROCES-INFO
           UNTIL WS-CONTROL-NO OR WS-FS-MAT-NO
           PERFORM 3000-CERRAR-ARCHIVO
           STOP RUN.

           1000-ABRIR-ARCHIVO.
               OPEN OUTPUT MATERIAS.
               IF NOT WS-FS-MAT-OK THEN
                   DISPLAY "ERROR AL ABRIR ARCHIVO"
                   DISPLAY "FILE STATUS " WS-FS-MAT
               END-IF.
           2000-PROCES-INFO.
               DISPLAY "INGRESA CODIGO DE LA MATERIA"
               ACCEPT WS-MAT-COD
               DISPLAY "INGRESA NOMBRE DE LA MATERIA"
               ACCEPT WS-MAT-NOMBRE
               MOVE WS-MAT-COD TO MAT-CODIGO
               MOVE WS-MAT-NOMBRE TO MAT-NOMBRE
               PERFORM 2100-GUARDAR-INFO
               DISPLAY "INGRESAR OTRO CAMPO? SI/NO" ACCEPT WS-CONTROL.

           2100-GUARDAR-INFO.
               WRITE MATERIAS-REGISTRO
               IF NOT WS-FS-MAT-OK THEN
                   DISPLAY "ERROR AL CARGAR REGISTRO"
                   DISPLAY "FILE STATUS " WS-FS-MAT
               END-IF.
           3000-CERRAR-ARCHIVO.
               CLOSE MATERIAS.
               IF NOT WS-FS-MAT-OK THEN
                   DISPLAY "ERROR AL CERRAR ARCHIVO"
                   DISPLAY "FILE STATUS " WS-FS-MAT
               END-IF.

       END PROGRAM PGENMATE.
