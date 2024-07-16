xlibrary libc.dylib libc.so.6

begin-structure timeval
    field: tv_sec
    field: tv_usec
end-structure

begin-structure timezone
    lfield: tz_minuteswest
    lfield: tz_dsttime
end-structure

function: gettimeofday ( *tp *tz -- $status )

: timeofday
    here timeval timezone + 0 fill
    here here timeval +
    {: tp tz :}
    tp tz gettimeofday
    0= if
        ." Time = " tp tv_sec ? ." . " tp tv_usec ? cr
        ." TZ = " tz tz_minuteswest l@ . tz tz_dsttime l@ if ." (DST)" then cr
    else
        ." Fail" cr
    then
;

begin-structure tms
    lfield: tm_sec
    lfield: tm_min
    lfield: tm_hour
    lfield: tm_mday
    lfield: tm_mon
    lfield: tm_year
    lfield: tm_wday
    lfield: tm_yday
    lfield: tm_isdst
     field: tm_gmtoff
     field: tm_zone                     \ pointer (char*)
end-structure

function: time ( *tloc -- $$time )
function: localtime_r ( *clock *tm -- *tm )

\ Ugly but it works
: dayofweek
    case
        0 of ." Sunday" endof
        1 of ." Monday" endof
        2 of ." Tuesday" endof
        3 of ." Wednesday" endof
        4 of ." Thursday" endof
        5 of ." Friday" endof
        6 of ." Saturday" endof
    endcase
;

\ Ugly but it works
: month
    case
         0 of ." January" endof
         1 of ." February" endof
         2 of ." March" endof
         3 of ." April" endof
         4 of ." May" endof
         5 of ." June" endof
         6 of ." July" endof
         7 of ." August" endof
         8 of ." September" endof
         9 of ." October" endof
        10 of ." November" endof
        11 of ." December" endof
    endcase
;

: localtime
    here 1 cells tms + 0 fill
    here here 1 cells +
    {: tloc tm :}
    tloc time drop                      \ Clock is stored in tloc, don't need return value
    tloc tm localtime_r drop            \ Structure is passed in, don't need return value
    ." Local time is "
    tm tm_wday l@ dayofweek ." , " tm tm_mday l@ . tm tm_mon l@ month bl emit
    tm tm_year l@ 1900 + 0 <# # # # # #> type bl emit
    tm tm_hour l@ 0 <# # # #> type [char] : emit  tm tm_min l@ 0 <# # # #> type [char] : emit
    tm tm_sec l@ 0 <# # # #> type bl emit
    tm tm_zone p@ begin dup c@ dup while emit 1+ repeat 2drop
    cr
;
