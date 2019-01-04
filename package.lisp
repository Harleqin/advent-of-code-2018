(in-package #:cl-user)

(defpackage #:aoc-2018
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre
        #:for
        #:split-sequence)
  (:export #:array-flat-view
           #:defenum
           #:dovector
           #:download
           #:factorize
           #:frequencies
           #:read-integers
           #:sort-by
           #:strcat))
