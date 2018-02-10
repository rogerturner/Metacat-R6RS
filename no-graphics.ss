;;=============================================================================
;; Copyright (c) 2016 Roger Turner
;; This file is part of Metacat, which is (c) 1999, 2003 by James B. Marshall
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================

(define pause (lambda z #f))
(define make (lambda z #f))
(define outline-box (lambda z #f))
(define arrowhead (lambda z #f))
(define <rgb> #f)
(define swl-font (lambda z #f))
(define sans-serif #f)
(define %small-group-arrowhead-length% #f)
(define %group-arrowhead-angle% #f)
(define *control-panel* (lambda z #f))

(define swl:sync-display (lambda z #f))

(define swl:version "1.3")

(define setup-gui (lambda z #f))

(define solid-box (lambda z #f))
(define group-graphics (lambda z #f))
(define bridge-graphics (lambda z #f))
(define draw-group-grope (lambda z #f))
(define draw-bridge-grope (lambda z #f))
(define new-bridge-label-number (lambda z #f))
(define urgency-color (lambda z #f))
(define make-group-pexp (lambda z #f))
(define make-bridge-pexp (lambda z #f))
(define initialize-rule-graphics (lambda z #f))

(define *fg-color* #f)
(define %default-fg-color% #f)
