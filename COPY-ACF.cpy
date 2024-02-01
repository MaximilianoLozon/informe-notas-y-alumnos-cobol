      ******************************************************************
      * Author: MAXIMILIANO LOZON
      * Purpose: AREA DE COMUNICACION PARA LA RUTINA FECHA
       01 AreaDeComunicacionFecha.
           05 DatoDeEntrada.
               10 Opcion  PIC 9.
               10 FechaIngreso.
                   15 F-I-DIA PIC 99.
                   15 F-I-MES PIC 99.
                   15 F-I-AÑO PIC 9999.
           05 DatoDeSalida.
               10 FechaFormato1.
                   15 F1-S-DIA PIC 99.
                   15 F1-S-MES PIC 99.
                   15 F1-S-AÑO PIC 9999.
               10 FechaFormato2.
                   15 F2-S-AÑO PIC 9999.
                   15 F2-S-MES PIC 99.
                   15 F2-S-DIA PIC 99.
               10 FechaFormato3.
                   15 F3-S-DIA PIC 99.
                   15 FILLER PIC X value "/".
                   15 F3-S-MES PIC 99.
                   15 FILLER PIC X value "/".
                   15 F3-S-AÑO PIC 9999.
               10 FechaFormato4.
                   15 F4-S-DIA PIC 99.
                   15 FILLER PIC X value "-".
                   15 F4-S-MES PIC 99.
                   15 FILLER PIC X value "-".
                   15 F4-S-AÑO PIC 9999..
               10 FechaFormato5.
                   15 F5-S-DIA PIC 99.
                   15 FILLER PIC XXXX value " DE ".
                   15 F5-S-MES PIC X(9).
                   15 FILLER PIC XXXX value " DE ".
                   15 F5-S-AÑO PIC 9999.
               10 FechaFormato6.
                   15 F6-S-MES PIC 99.
                   15 FILLER PIC X value "/".
                   15 F6-S-AÑO PIC 9999.
           05 DatoDeRetorno.
               10 Valor PIC 99.
               10 Desc-retorno PIC X(50).
      ******************************************************************
