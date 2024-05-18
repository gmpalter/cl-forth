# CL-Forth

Common Lisp implementation of the Forth 2012 Standard, CL-Forth


## License

_TO BE DETERMINED_


## Usage

CL-Forth is defined as an ASDF system. To load CL-Forth into Lisp

``` lisp
(asdf:load-system '#:cl-forth)
```

You can run the [Forth 2012 Test Suite](https://github.com/gerryjackson/forth2012-test-suite.git)

``` lisp
(asdf:test-system '#:cl-forth)
```

To start the CL-Forth interpreter loop

``` lisp
(forth:run)
```


## Implementation

_TO BE SUPPLIED_



## Missing Words

CL-Forth does not implement the optional Block word set.

CL-Forth does not implement the optional Extended-Character word set.

CL-Forth does not implement `KEY` which is part of the Core word set.

The following words that are part of the optional Facility and Facility extensions word set are not implemented.

| | | | | |
| --- | --- | --- | --- | --- |
| `AT-XY` | `KEY?` | `PAGE` | `EKEY` | `EKEY>CHAR` |
| `EKEY>FKEY` | `EKEY?` | `EMIT?` | `K-ALT-MASK` | `K-CTRL-MASK` |
| `K-DELETE` | `K-DOWN` | `K-END` | `K-F1` | `K-F10` |
| `K-F11` | `K-F12` | `K-F2` | `K-F3` | `K-F4` |
| `K-F5 `| `K-F6` | `K-F7` | `K-K8` | `K-F9` |
| `K-HOME` | `K-INSERT` | `K-LEFT` | `K-NEXT` | `K-PRIOR` |
| `K-RIGHT` | `K-SHIFT-MASK` | `K-UP` |

  
## Additional Words

These words are defined as "Common Usage" in the [Forth Programmer's Manual, 3rd Edition](https://www.forth.com/forth-books/).

| | | | | |
| --- | --- | --- | --- | --- |
| `,"` | `2+` | `2-` | `C+!` | `CONTEXT` |
| `CURRENT` | `CVARIABLE` | `M-` | `M/` | `NOT` |
| `NUMBER` |`NUMBER?` | `VOCABULARY` |

These words are defined by [SwiftForth](https://www.forth.com/swiftforth/).

| | | | | |
| --- | --- | --- | --- | --- |
| `-?` | `EMPTY` | `GILD` | `OFF` | `ON` |
| `OPTIONAL` | `SILENT` | `VERBOSE` | `WARNING` | `\\` |
| `{` |

These additional words are defined.

| | |
| --- | --- |
| `.SF` | Display the contents of the floating-point stack |
| `.SR` | Display the contents of the return stack |
|  `ALL-WORDS` | Display all words in all word lists in the search order |
| `BREAK` | Enter a Lisp break loop |
| `INLINEABLE` | Mark that the most recent definition's code may be inlined |
| `NOTINTERPRETED` | Mark that the most recent definition must only appear in definitions |
| `RELOAD` | Reload a predefined definition |
| `REMOVE` | Erase a single word |
| `SHOW-CODE` | Controls whether completing a definition shows the generated code |

  
## Native Code Support

CL-Forth implements `CODE` and `;CODE` to allow the definition of words written in Lisp rather than Forth. The terminator for
the Lisp code block is `;ENDCODE`.

Here is an example of using native code.

``` c
\ ( c-addr1 u - c-addr2 u)
\ Converts the string at C-ADDR1 U to uppercase and leaves the result in transient space at C-ADDR2 U.
CODE UPCASE
  (let ((count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "Count to UPCASE must be positive"))
    (multiple-value-bind (data offset)
        (memory-decode-address memory address)
      (let* ((original (forth-string-to-native data offset count))
             (upcased (string-upcase original))
             (string-space (reserve-string-space memory))
             (address (transient-space-base-address memory string-space)))
        (ensure-transient-space-holds memory string-space count)
        (multiple-value-bind (data offset)
            (memory-decode-address memory address)
          (native-into-forth-string upcased data offset)
          (seal-transient-space memory string-space)
          (stack-push data-stack address)
          (stack-push data-stack count)))))
;ENDCODE
```

## Supported Platforms

CL-Forth was initially implemented using CCL.

It has been updated to work with SBCL. However, at present, the word `RESIZE-FILE` will always return an error indication.

It compiles with LispWorks but does not work. (I.e., it crashes running the Forth test suite.)

