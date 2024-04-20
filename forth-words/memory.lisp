(in-package #:forth)

;;; Memory-Allocation words as defined in Section 14 of the Forth 2012 specification

(define-word allocate-memory (:word "ALLOCATE")
  "( u – a-addr ior )"
  "allocate U address units of contiguous data space. The data-space pointer is unaffected by this operation."
  "The initial content of the allocated space is undefined."
  "If the allocation succeeds, A-ADDR is the aligned starting address of the allocated space and IOR is zero."
  "If the operation fails, A-ADDR does not represent a valid address and IOR is the implementation-defined I/O result code"
  (let ((count (cell-unsigned (stack-pop data-stack))))
    (multiple-value-bind (addr ior)
        (allocate-native-memory memory count)
      (stack-push data-stack addr)
      (stack-push data-stack ior))))

(define-word free-memory (:word "FREE")
  "( a-addr – ior )"
  "Return the contiguous region of data space indicated by A-ADDR to the system for later allocation. A-ADDR shall indicate"
  "a region of data space that was previously obtained by ALLOCATE or RESIZE. The data-space pointer is unaffected by this"
  "operation.  the operation succeeds, IOR is zero. If the operation fails, ior is the implementation-defined I/O result code"
  (let ((address (stack-pop data-stack)))
    (stack-push data-stack (free-native-memory memory address))))

(define-word resize-memory (:word "RESIZE")
  "( a-addr1 u – a-addr2 ior )"
  "Change the allocation of the contiguous data space starting at the address A-ADDR1, previously allocated by ALLOCATE"
  "or RESIZE, to U address units. U may be either larger or smaller than the current size of the region. The data-space"
  "pointer is unaffected by this operation."
  "If the operation succeeds, A-ADDR2 is the aligned starting address of U address units of allocated memory and IOR is zero."
  "A-ADDR2 may be, but need not be, the same as A-ADDR1. If they are not the same, the values contained in the region at"
  "A-ADDR1 are copied to A-ADDR2, up to the minimum size of either of the two regions. If they are the same, the values"
  "contained in the region are preserved to the minimum of U or the original size. If A-ADDR2 is not the same as A-ADDR1,"
  "the region of memory at A-ADDR1 is returned to the system according to the operation of FREE."
  "If the operation fails, A-ADDR2 equals A-ADDR1, the region of memory at A-ADDR1 is unaffected, and IOR is"
  "the implementation-defined I/O result code"
  (let ((count (cell-unsigned (stack-pop data-stack)))
        (addr1 (stack-pop data-stack)))
    (multiple-value-bind (addr2 ior)
        (resize-native-memory memory addr1 count)
      (stack-push data-stack addr2)
      (stack-push data-stack ior))))
