XLIBRARY libc.dylib libc.so.6

BEGIN-STRUCTURE TIMEVAL
    FIELD: TV_SEC
    FIELD: TV_USEC
END-STRUCTURE

BEGIN-STRUCTURE TIMEZONE
    LFIELD: TZ_MINUTESWEST
    LFIELD: TZ_DSTTIME
END-STRUCTURE

FUNCTION: gettimeofday ( *tp *tz -- $status )

: TIMEOFDAY
    HERE TIMEVAL TIMEZONE + 0 FILL
    HERE HERE TIMEVAL +
    {: TP TZ :}
    TP TZ GETTIMEOFDAY
    0= IF
        ." Time = " TP TV_SEC ? ." . " TP TV_USEC ? CR
        ." TZ = " TZ TZ_MINUTESWEST L@ . TZ TZ_DSTTIME L@ IF ." (DST)" THEN CR
    ELSE
        ." FAIL" CR
    THEN
;

BEGIN-STRUCTURE TMS
    LFIELD: TM_SEC
    LFIELD: TM_MIN
    LFIELD: TM_HOUR
    LFIELD: TM_MDAY
    LFIELD: TM_MON
    LFIELD: TM_YEAR
    LFIELD: TM_WDAY
    LFIELD: TM_YDAY
    LFIELD: TM_ISDST
     FIELD: TM_GMTOFF
     FIELD: TM_ZONE                     \ pointer (char*)
END-STRUCTURE

FUNCTION: time ( *tloc -- $$time )
FUNCTION: localtime_r ( *clock *tm -- *tm )

\ Ugly but it works
: DAYOFWEEK
    CASE
        0 OF ." Sunday" ENDOF
        1 OF ." Monday" ENDOF
        2 OF ." Tuesday" ENDOF
        3 OF ." Wednesday" ENDOF
        4 OF ." Thursday" ENDOF
        5 OF ." Friday" ENDOF
        6 OF ." Saturday" ENDOF
    ENDCASE
;

\ Ugly but it works
: MONTH
    CASE
         0 OF ." January" ENDOF
         1 OF ." February" ENDOF
         2 OF ." March" ENDOF
         3 OF ." April" ENDOF
         4 OF ." May" ENDOF
         5 OF ." June" ENDOF
         6 OF ." July" ENDOF
         7 OF ." August" ENDOF
         8 OF ." September" ENDOF
         9 OF ." October" ENDOF
        10 OF ." November" ENDOF
        11 OF ." December" ENDOF
    ENDCASE
;

: LOCALTIME
    HERE 1 CELLS TMS + 0 FILL
    HERE HERE 1 CELLS +
    {: TLOC TM :}
    TLOC TIME DROP                      \ Clock is stored in tloc, don't need return value
    TLOC TM LOCALTIME_R DROP            \ Structure is passed in, don't need return value
    ." Local time is "
    TM TM_WDAY L@ DAYOFWEEK ." , " TM TM_MDAY L@ . TM TM_MON L@ MONTH BL EMIT
    TM TM_YEAR L@ 1900 + 0 <# # # # # #> TYPE BL EMIT
    TM TM_HOUR L@ 0 <# # # #> TYPE [CHAR] : EMIT  TM TM_MIN L@ 0 <# # # #> TYPE [CHAR] : EMIT
    TM TM_SEC L@ 0 <# # # #> TYPE BL EMIT
    TM TM_ZONE P@ BEGIN DUP C@ DUP WHILE EMIT 1+ REPEAT 2DROP
    CR
;
