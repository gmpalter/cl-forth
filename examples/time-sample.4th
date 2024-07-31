\ -*- Syntax: Forth -*-
\ 
\ Copyright (c) 2024 Gary Palter
\ 
\ Licensed under the MIT License;
\ you may not use this file except in compliance with the License.
\ You may obtain a copy of the License at
\ 
\   https://opensource.org/license/mit

\ Example: Using CL-Forth's foreign function interface (FFI) facility

\ Functions are defined in libc on macOS and Linux
xlibrary libc.dylib libc.so.6

begin-structure timeval
    field: tv_sec                       \ seconds since January 1, 1970
    field: tv_usec                      \ microseconds in the current second
end-structure

begin-structure timezone
    lfield: tz_minuteswest              \ minutes west of UTC
    lfield: tz_dsttime                  \ non-zero if Daylight Saving Time is in effect
end-structure

\ Arguments are pointers to a timeval structure and a timezone structure which are filled with current time
\ Return value is zero for success, non-zero for failure
function: gettimeofday ( *tp *tz -- $status )

: timeofday
    here timeval timezone + 0 fill      \ Zero out space for timeval and timezone structures
    here here timeval +
    {: tp tz :}                         \ tp is address of timeval structure, tz is address of timezone structure
    tp tz gettimeofday
    0= if
        \ Success, display contents of both structures
        ." Time = " tp tv_sec @ 0 <# #S #> type [char] . emit tp tv_usec @ 0 <# # # # # # # #> type cr
        ." TZ = " tz tz_minuteswest l@ . tz tz_dsttime l@ if ." (DST)" then cr
    else
        ." Fail" cr
    then
;

begin-structure tm
    lfield: tm_sec
    lfield: tm_min
    lfield: tm_hour
    lfield: tm_mday                     \ day of month
    lfield: tm_mon                      \ 1 = January, 2 = February, etc.
    lfield: tm_year                     \ year - 1900
    lfield: tm_wday                     \ 0 = Sunday, 1 = Monday, etc.
    lfield: tm_yday                     \ day of year (zero-based)
    lfield: tm_isdst                    \ non-zero if Daylight Saving Time in effect
     field: tm_gmtoff                   \ offset from UTC in seconds (positive is East)
     field: tm_zone                     \ pointer to timezone as a C string
end-structure

\ Argument is a pointer to a time_t (int64_t) where the current time is stored
\ Return value is also the current time
function: time ( *tloc -- $$time )

\ Arguments are a pointer to a time_t to be decoded and a tm structure which is filled in
\ Return value is also a pointer to the tm structure
function: localtime_r ( *clock *tm -- *tm )

\ Naive but it works
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

\ Naive but it works
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
    here 1 cells tm + 0 fill            \ Zero out space for a time_t value and tm structure
    here here 1 cells +
    {: tloc tm :}                       \ tloc is address of time_t, tm is address of tm structure
    tloc time drop                      \ Clock is stored in tloc, don't need return value
    tloc tm localtime_r drop            \ Structure is passed in, don't need return value
    \ Display local time in format similar to the Unix date command
    ." Local time is "
    tm tm_wday l@ dayofweek ." , " tm tm_mday l@ . tm tm_mon l@ month bl emit
    \ Print year as YYYY
    tm tm_year l@ 1900 + 0 <# # # # # #> type bl emit
    \ Print time of day as HH:MM:SS
    tm tm_hour l@ 0 <# # # #> type [char] : emit tm tm_min l@ 0 <# # # #> type [char] : emit
    tm tm_sec l@ 0 <# # # #> type bl emit
    \ tm_zone is a pointer to a C string -- Print characters until we reach the terminating NUL
    tm tm_zone p@ begin dup c@ dup while emit 1+ repeat 2drop
    cr
;
