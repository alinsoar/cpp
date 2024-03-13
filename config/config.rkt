;; -*- mode:scheme ; buffer-read-only:nil -*-

;;; number of bits for smallest object that is not a bit-field (byte)
(define __CHAR_BIT                      8)

;;; Minimum and maximum values a `signed char' can hold.
(define __SCHAR_MIN                     -128)
(define __SCHAR_MAX                     127)

;;; Maximum value an `unsigned char' can hold.  (Minimum is 0.)
(define __UCHAR_MAX                     255)

;;; Minimum and maximum values a `char' can hold.
(define __CHAR_MIN                      __SCHAR_MIN)
(define __CHAR_MAX                      __SCHAR_MAX)

;; maximum number of bytes in a multibyte character, for any supported locale
(define MB_LEN_MAX                     1)

;;; Minimum and maximum values a `signed short int' can hold.
(define __SHRT_MIN                      -32768)
(define __SHRT_MAX                      32767)

;;; Maximum value an `unsigned short int' can hold.  (Minimum is 0.)
(define __USHRT_MAX                     65535)

;;; Minimum and maximum values a `signed int' can hold.
(define __INT_MAX                       2147483647)
(define __INT_MIN                       (- (- __INT_MAX) 1))

;;; Maximum value an `unsigned int' can hold.  (Minimum is 0.)
(define __UINT_MAX                      4294967295)

;;; Minimum and maximum values a `signed long int' can hold.
;; (define __LONG_MAX                      9223372036854775807)
(define __LONG_MAX                      2147483647)
(define __LONG_MIN                      (- (- __LONG_MAX) 1))

;;; Maximum value an `unsigned long int' can hold.  (Minimum is 0.)
;; (define __ULONG_MAX                     18446744073709551615)
(define __ULONG_MAX                     4294967295)

;;; Minimum and maximum values a `signed long long int' can hold.
(define __LLONG_MAX                     9223372036854775807)
(define __LLONG_MIN                     (- (- __LLONG_MAX) 1))

;;; Maximum value an `unsigned long long int' can hold.  (Minimum is 0.)

;;      __ULLONG_MAX                    18446744073709551615
(define __ULLONG_MAX                    (+ (* __LLONG_MAX 2) 1))



(define SYSTEM-HEADERS-SEARCH-PATH
  (map string->path
       '(
         "/mnt/LITTLE-COMPUTER/include-test"
         "/usr/include/"
         ;; "/usr/include/linux" ;  this would require extensions of the grammar
         "/mnt/work/LITTLE-COMPUTER/SIM/sema/tema"
         "/mnt/work/LITTLE-COMPUTER-DATED/SIM/cc/incl/gtk-3.0/"
         "/usr/include/glib-2.0/"
         "/usr/lib/x86_64-linux-gnu/glib-2.0/include/"
         "/usr/include/pango-1.0"
         "/usr/include/cairo/"
         "/usr/include/gdk-pixbuf-2.0"
         "/usr/include/atk-1.0"
         )))


