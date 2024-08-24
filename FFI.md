## CL-Forth Foreign Function Interface (FFI)

CL-Forth includes a foreign function interface (FFI) loosely based on the External Library Interface in
[SwiftForth](https://www.forth.com/swiftforth/).

_TO BE CONTINUED_

<!--

LIBRARY libname
XLIBRARY libname1 ... libnameN
FUNCTION: name ( params -- return )
GLOBAL: name
CALLBACK: name ( params -- return )
AS name
[OPTIONAL]
.LIBS
.IMPORTS

The form of a parameter list is

   ( params -- return )

The name given to each parameter and the return value is for documentation purposes only as all parameters
are taken from the data stack or floating-point stack and the return value is placed on the data or floating-point stack.

However, prefix character(s) determine the type of a parameter or the return value.
If no prefix is present, the parameter or return value is a 64-bit signed integer

| Prefix | CFFI type | Interpretation |
| --- | --- | --- |
| `*` | `:pointer` | An address of data either in one of Forth's data spaces or the foreign data space |
| `$` | `:int32` | 32-bit signed integer value taken/pushed from/to the data stack |
| `$u` | `:uint32` | 32-bit unsigned integer value taken/pushed from/to the data stack |
| `$$` | `:int64` | 64-bit signed integer value taken/pushed from/to the data stack |
| `$$u` | `:uint64` | 64-bit unsigned integer value taken/pushed from/to the data stack |
| `%` | `:single` | Single precision floating point value taken/pushed from/to the floating-point stack |
| `%%` | `:double` | Double precision floating point value taken/pushed from/to the floating-point stack |

BEGIN-NAMED-STRUCTURE
WFIELD:
LFIELD:
word access (W@, UW@, W!, W,)
longword access (L@, UL@, L!, L,)
pointer access (P@, P!)

-->

### FFI Examples

The file [time-sample.4th](examples/time-sample.4th) uses `xlibrary` and `function:` to define  `gettimeofday`, `time`, and
`localtime_r` words which invoke the corresponding C functions. It then uses those words and appropriate structure definitions
to define two words, `timeofday` and `localtime`, to print the results of calling those functions.

``` forth
? (forth:run)
CL-Forth Version 1.3
Running under Clozure Common Lisp Version 1.12.2 (v1.12.2-102-gdd595bb6) DarwinX8664
include examples/time-sample.4th
OK.
timeofday 
Time = 1722479563.358060
TZ = 300 (DST)
OK.
localtime
Local time is Wednesday, 31 July 2024 22:32:49 EDT
OK.
bye
```
