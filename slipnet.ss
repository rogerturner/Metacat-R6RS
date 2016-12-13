;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
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

(define %max-activation% 100)
(define %workspace-activation% 100)
(define %full-activation-threshold% 50)

(define make-slipnode
  (lambda (name-symbol short-name conceptual-depth)
    (let* ((activation 0)
           (activation-buffer 0)
           (frozen? #f)
           (changed-frozen? #f)
           (rate-of-decay #f)
           (intrinsic-link-length 0)
           (shrunk-link-length 0)
           (top-down-codelet-types '())
           (incoming-links '())
           (category-links '())
           (instance-links '())
           (property-links '())
           (lateral-links '())
           (lateral-sliplinks '())
           (links-labeled-by-node '())
           (descriptor-predicate? (lambda (object) #f))
           (full-lowercase-name
            (string-downcase
             (string-suffix (symbol->string name-symbol) 6)))
           (full-uppercase-name (string-upcase full-lowercase-name))
           (graphics-coord #f)
           (graphics-label-coord #f))
      (lambda msg
       (let ((self (first msg)))
         (record-case (rest msg)
           (object-type () 'slipnode)
           (get-name-symbol () name-symbol)
           (get-lowercase-name () full-lowercase-name)
           (get-uppercase-name () full-uppercase-name)
           (get-short-name () short-name)
           (get-CM-short-name ()
             (if (eq? self plato-letter-category)
    ;; This avoids occasional graphics problems with overlapping CMs:
              "LettCtgy"
              (tell self 'get-short-name)))
      ;; This is solely for the purposes of (say <slipnode>):
           (print-name () short-name)
           (print () (printf "Slipnode \"~a\"~%" short-name))
           (draw-activation-graphics (slipnet-window)
             (tell slipnet-window 'draw-activation
              graphics-coord activation frozen?))
           (reset ()
                  (set! activation 0)
             (set! activation-buffer 0)
             (set! frozen? #f)
             (set! changed-frozen? #f)
             (set! rate-of-decay
              ($1- (expt (% conceptual-depth) (/ %update-cycle-length% 15))))
             'done)
           (get-graphics-coord () graphics-coord)
           (get-graphics-label-coord () graphics-label-coord)
           (set-graphics-coord (coord) (set! graphics-coord coord) 'done)
           (set-graphics-label-coord (coord) (set! graphics-label-coord coord) 'done)
           (get-conceptual-depth () conceptual-depth)
           (frozen? () frozen?)
           (get-activation () activation)
           (get-intrinsic-link-length () intrinsic-link-length)
           (get-shrunk-link-length () shrunk-link-length)
           (get-incoming-links () incoming-links)
           (get-category-links () category-links)
           (get-lateral-links () lateral-links)
           (get-lateral-sliplinks () lateral-sliplinks)
           (get-property-links () property-links)
           (get-links-labeled-by-node () links-labeled-by-node)
           (get-degree-of-assoc ()
             ($100- (if (fully-active? self) shrunk-link-length intrinsic-link-length)))

           (category? () (not (null? instance-links)))
           (instance? () (not (null? category-links)))

           (get-category ()
             (if (null? category-links)
              #f
              (tell (first category-links) 'get-to-node)))

           (get-outgoing-links ()
             (append category-links instance-links property-links
              lateral-links lateral-sliplinks))

           (get-instance-nodes ()
             (tell-all instance-links 'get-to-node))

           (get-similar-property-links ()
             (filter (lambda (link)
                      (prob? (temp-adjusted-probability
                              (% (tell link 'get-degree-of-assoc)))))
              property-links))

           (get-related-node (relation)
             (if (eq? relation plato-identity)
              self
              (let ((related-nodes
                     (tell-all
                       (filter
                         (lambda (link) (eq? (tell link 'get-label-node) relation))
                         (tell self 'get-outgoing-links))
                       'get-to-node)))
                (cond
                  ((null? related-nodes) #f)
                  ((null? (rest related-nodes)) (first related-nodes))
                  (else (select
                         (lambda (node)
                           (eq? (tell node 'get-category) (tell self 'get-category)))
                         related-nodes))))))

           (freeze ()
             (set! frozen? #t)
             (set! changed-frozen? #t)
             'done)
           (unfreeze ()
             (set! frozen? #f)
             (set! changed-frozen? #t)
             'done)
           (clamp (new-value)
             (monitor-slipnode-activation-change self activation new-value)
             (set! activation new-value)
             (set! activation-buffer 0)
             (set! frozen? #t)
             (set! changed-frozen? #t)
             'done)
           (set-activation (new-value)
             (if* (not frozen?)
              (set! activation new-value)
              (set! activation-buffer 0))
             'done)
           (update-activation (new-value)
             (if* (not frozen?)
              (monitor-slipnode-activation-change self activation new-value)
              (set! activation new-value)
              (set! activation-buffer 0))
             'done)
           (increment-activation-buffer (delta)
             (if* (not frozen?)
              (set! activation-buffer (+ activation-buffer delta)))
             'done)
           (decrement-activation-buffer (delta)
             (if* (not frozen?)
              (set! activation-buffer (- activation-buffer delta)))
             'done)
           (flush-activation-buffer ()
             (let ((new-value (min %max-activation% (+ activation activation-buffer))))
              (monitor-slipnode-activation-change self activation new-value)
              (set! activation new-value)
              (set! activation-buffer 0))
             'done)
           (activate-from-workspace ()
             (tell self 'increment-activation-buffer %workspace-activation%)
             'done)
           (decay-activation ()
             (let ((decay-amount (round (* rate-of-decay activation))))
              (tell self 'decrement-activation-buffer decay-amount))
             'done)
           (spread-activation ()
             (for* each link in (tell self 'get-outgoing-links) do
              (let* ((to-node (tell link 'get-to-node))
                     (association (tell link 'get-intrinsic-degree-of-assoc))
                     (spread-amount
                      (round (* (/ %update-cycle-length% 15)
                              (% association)
                              activation))))
                (tell to-node 'increment-activation-buffer spread-amount)))
             'done)

           (set-intrinsic-link-length (new-value)
             (set! intrinsic-link-length new-value)
             (set! shrunk-link-length (round ($40% new-value)))
             'done)

           (define-descriptor-predicate (new-procedure)
             (set! descriptor-predicate? new-procedure)
             'done)

           (possible-descriptor? (object)
             (descriptor-predicate? object))

           (description-possible? (object)
             (not (null? (tell self 'get-possible-descriptors object))))

           (get-possible-descriptors (object)
             (filter-meth (tell-all instance-links 'get-to-node)
              'possible-descriptor? object))

           (set-top-down-codelet-types codelet-types
             (set! top-down-codelet-types codelet-types)
             'done)

           (attempt-to-post-top-down-codelets ()
             (if* (above-threshold? self)
              (let ((urgency (* (% conceptual-depth) activation)))
                (for* each codelet-type in top-down-codelet-types do
                  (stochastic-if* (post-codelet-probability codelet-type)
                    (repeat* (num-of-codelets-to-post codelet-type) times
      ;; Scope for top-down codelets posted by
      ;; active slipnodes is entire workspace:
                     (tell *coderack* 'add-deferred-codelet
                       (tell codelet-type 'make-codelet
                         urgency self *workspace*)))))))
             'done)

           (add-to-incoming-links (new-link)
             (set! incoming-links (cons new-link incoming-links))
             'done)

           (add-to-outgoing-links (link-type new-link)
             (case link-type
              ((category) (set! category-links (cons new-link category-links)))
              ((instance) (set! instance-links (cons new-link instance-links)))
              ((property) (set! property-links (cons new-link property-links)))
              ((lateral) (set! lateral-links (cons new-link lateral-links)))
              ( (lateral-sliplink)
                (set! lateral-sliplinks (cons new-link lateral-sliplinks))))
             'done)

           (add-to-links-labeled-by-node (link)
             (set! links-labeled-by-node (cons link links-labeled-by-node))
             'done)

      ;; slippage <desc1>==<label>==><desc2> is applicable to <node> iff:
      ;;
      ;; (1) <desc1> = <node>
      ;;   In this case the slipped node is <desc2>
      ;;
      ;; (2) <node> has a sliplink with label
      ;;   In this case the slipped node is the node related to <node> by <label>
      ;;   (probability is a function of sliplink's current degree of association)
      ;; Example:   a => b
      ;;            |
      ;;            z => ?
      ;; first=(opp)=>last slippage applied to <successor> node
      ;; should sometimes cause a slippage to <predecessor>.

           (apply-slippages (slippages sliplog)
             (cond
              ((null? slippages) self)
              ((eq? (tell (first slippages) 'get-descriptor1) self)
               (tell sliplog 'applied (first slippages))
               (tell (first slippages) 'get-descriptor2))
              (else
      ;; See if a coattail slippage can be made:
                (let ((label (tell (first slippages) 'get-label)))
                  (if (or (not (exists? label))
                       (eq? (tell (first slippages) 'get-CM-type)
                            (tell self 'get-category)))
                    (tell self 'apply-slippages (rest slippages) sliplog)
                    (let ((sliplink (select-meth lateral-sliplinks 'labeled? label)))
                     (if (and (exists? sliplink)
                              (prob? (coattail-slippage-probability
                                      (first slippages) label self sliplink)))
                       (let ((node2 (tell self 'get-related-node label)))
                         (tell sliplog 'coattail self label node2 (first slippages))
                         node2)
                       (tell self 'apply-slippages (rest slippages) sliplog))))))))

           (else (delegate msg base-object))))))))


(define coattail-slippage-probability
  (lambda (inducing-slippage inducing-slippage-label node sliplink)
    (% (tell sliplink 'get-degree-of-assoc))))


(define get-label
  (lambda (from-node to-node)
    (if (eq? from-node to-node)
      plato-identity
      (let ((link (select
                   (lambda (link) (eq? (tell link 'get-from-node) from-node))
                   (tell to-node 'get-incoming-links))))
       (if (exists? link)
         (tell link 'get-label-node)
         #f)))))


(define relationship-between
  (lambda (nodes)
    (if (all-exist? nodes)
      (let ((relations (adjacency-map get-label nodes)))
       (if (and (all-exist? relations) (all-same? relations))
         (first relations)
         #f))
      #f)))


(define make-slipnet-link
  (lambda (from-node to-node link-type)
    (let ((label-node #f)
          (fixed-length? #f)
          (link-length 0)
          (print-name
            (format "~a->~a"
              (tell from-node 'get-lowercase-name)
              (tell to-node 'get-lowercase-name))))
      (lambda msg
       (let ((self (first msg)))
         (record-case (rest msg)
           (object-type () 'slipnet-link)
           (print-name () print-name)
           (print ()
             (printf "Sliplink ~a~n" print-name))
           (get-link-type () link-type)
           (get-from-node () from-node)
           (get-to-node () to-node)
           (get-label-node () label-node)
           (get-link-length () link-length)
           (get-intrinsic-degree-of-assoc ()
             (if fixed-length?
              ($100- link-length)
              ($100- (tell label-node 'get-intrinsic-link-length))))
           (get-degree-of-assoc ()
             (if fixed-length?
              ($100- link-length)
              (if (fully-active? label-node)
                ($100- (tell label-node 'get-shrunk-link-length))
                ($100- (tell label-node 'get-intrinsic-link-length)))))
           (labeled? (node) (eq? label-node node))
           (set-label-node (node)
             (set! label-node node)
             (tell label-node 'add-to-links-labeled-by-node self)
             'done)
           (set-link-length (new-length)
             (set! link-length new-length)
             (set! fixed-length? #t)
             'done)
           (else (delegate msg base-object))))))))


(define related?
  (lambda (node1 node2)
    (or (eq? node1 node2) (linked? node1 node2))))


(define linked?
  (lambda (node1 node2)
    (member? node2 (tell-all (tell node1 'get-outgoing-links) 'get-to-node))))


(define slip-linked?
  (lambda (node1 node2)
    (member? node2
      (tell-all (tell node1 'get-lateral-sliplinks) 'get-to-node))))


(define update-slipnet-activations
  (lambda ()
    (for* each theme in (tell *themespace* 'get-all-active-themes) do
      (tell theme 'spread-activation-to-slipnet))
    (for* each node in *slipnet-nodes* do
      (tell node 'decay-activation))
    (for* each node in (filter fully-active? *slipnet-nodes*) do
      (tell node 'spread-activation))
    (for* each node in *slipnet-nodes* do
      (tell node 'flush-activation-buffer))
    (for* each node in (filter partially-active? *slipnet-nodes*) do
      (stochastic-if* (^3 (% (tell node 'get-activation)))
       (tell node 'update-activation %max-activation%)))))


(define fully-active?
  (lambda (node)
    (= (tell node 'get-activation) %max-activation%)))


(define above-threshold?
  (lambda (node)
    (>= (tell node 'get-activation) %full-activation-threshold%)))


(define partially-active?
  (lambda (node)
    (and (above-threshold? node) (not (fully-active? node)))))

;;----------------------------------------------------------------------------------
;; Slipnet Nodes

(define plato-a (make-slipnode 'plato-a "a" 10))
(define plato-b (make-slipnode 'plato-b "b" 10))
(define plato-c (make-slipnode 'plato-c "c" 10))
(define plato-d (make-slipnode 'plato-d "d" 10))
(define plato-e (make-slipnode 'plato-e "e" 10))
(define plato-f (make-slipnode 'plato-f "f" 10))
(define plato-g (make-slipnode 'plato-g "g" 10))
(define plato-h (make-slipnode 'plato-h "h" 10))
(define plato-i (make-slipnode 'plato-i "i" 10))
(define plato-j (make-slipnode 'plato-j "j" 10))
(define plato-k (make-slipnode 'plato-k "k" 10))
(define plato-l (make-slipnode 'plato-l "l" 10))
(define plato-m (make-slipnode 'plato-m "m" 10))
(define plato-n (make-slipnode 'plato-n "n" 10))
(define plato-o (make-slipnode 'plato-o "o" 10))
(define plato-p (make-slipnode 'plato-p "p" 10))
(define plato-q (make-slipnode 'plato-q "q" 10))
(define plato-r (make-slipnode 'plato-r "r" 10))
(define plato-s (make-slipnode 'plato-s "s" 10))
(define plato-t (make-slipnode 'plato-t "t" 10))
(define plato-u (make-slipnode 'plato-u "u" 10))
(define plato-v (make-slipnode 'plato-v "v" 10))
(define plato-w (make-slipnode 'plato-w "w" 10))
(define plato-x (make-slipnode 'plato-x "x" 10))
(define plato-y (make-slipnode 'plato-y "y" 10))
(define plato-z (make-slipnode 'plato-z "z" 10))
(define plato-one (make-slipnode 'plato-one "one" 30))
(define plato-two (make-slipnode 'plato-two "two" 30))
(define plato-three (make-slipnode 'plato-three "three" 30))
(define plato-four (make-slipnode 'plato-four "four" 30))
(define plato-five (make-slipnode 'plato-five "five" 30))
(define plato-leftmost (make-slipnode 'plato-leftmost "lmost" 40))
(define plato-rightmost (make-slipnode 'plato-rightmost "rmost" 40))
(define plato-middle (make-slipnode 'plato-middle "middle" 40))
(define plato-single (make-slipnode 'plato-single "single" 40))
(define plato-whole (make-slipnode 'plato-whole "whole" 40))
(define plato-alphabetic-first (make-slipnode 'plato-alphabetic-first "first" 60))
(define plato-alphabetic-last (make-slipnode 'plato-alphabetic-last "last" 60))
(define plato-left (make-slipnode 'plato-left "left" 40))
(define plato-right (make-slipnode 'plato-right "right" 40))
(define plato-predecessor (make-slipnode 'plato-predecessor "pred" 50))
(define plato-successor (make-slipnode 'plato-successor "succ" 50))
(define plato-sameness (make-slipnode 'plato-sameness "same" 80))
(define plato-predgrp (make-slipnode 'plato-predgrp "predgrp" 50))
(define plato-succgrp (make-slipnode 'plato-succgrp "succgrp" 50))
(define plato-samegrp (make-slipnode 'plato-samegrp "samegrp" 80))
(define plato-identity (make-slipnode 'plato-identity "Identity" 90))
(define plato-opposite (make-slipnode 'plato-opposite "Opposite" 90))
(define plato-letter (make-slipnode 'plato-letter "letter" 20))
(define plato-group (make-slipnode 'plato-group "group" 80))
(define plato-letter-category (make-slipnode 'plato-letter-category "LetterCtgy" 30))
(define plato-string-position-category (make-slipnode 'plato-string-position-category "StringPos" 70))
(define plato-alphabetic-position-category (make-slipnode 'plato-alphabetic-position-category "AlphaPos" 80))
(define plato-direction-category (make-slipnode 'plato-direction-category "Direction" 70))
(define plato-bond-category (make-slipnode 'plato-bond-category "BondCtgy" 80))
(define plato-group-category (make-slipnode 'plato-group-category "GroupCtgy" 80))
(define plato-length (make-slipnode 'plato-length "Length" 60))
(define plato-object-category (make-slipnode 'plato-object-category "ObjectCtgy" 90))
(define plato-bond-facet (make-slipnode 'plato-bond-facet "BondFacet" 90))

(define *slipnet-nodes*
  (list plato-a plato-b plato-c plato-d plato-e plato-f plato-g plato-h plato-i
        plato-j plato-k plato-l plato-m plato-n plato-o plato-p plato-q plato-r
        plato-s plato-t plato-u plato-v plato-w plato-x plato-y plato-z plato-one
        plato-two plato-three plato-four plato-five plato-leftmost plato-rightmost
        plato-middle plato-single plato-whole plato-alphabetic-first
        plato-alphabetic-last plato-left plato-right plato-predecessor
        plato-successor plato-sameness plato-predgrp plato-succgrp plato-samegrp
        plato-identity plato-opposite plato-letter plato-group plato-letter-category
        plato-string-position-category plato-alphabetic-position-category
        plato-direction-category plato-bond-category plato-group-category
        plato-length plato-object-category plato-bond-facet))

(define *slipnet-letters*
  (list plato-a plato-b plato-c plato-d plato-e plato-f plato-g plato-h plato-i
   plato-j plato-k plato-l plato-m plato-n plato-o plato-p plato-q plato-r
   plato-s plato-t plato-u plato-v plato-w plato-x plato-y plato-z))

(define *slipnet-numbers*  
  (list plato-one plato-two plato-three plato-four plato-five))

(define *top-down-slipnodes*
  (list plato-left plato-right plato-predecessor plato-successor
   plato-sameness plato-predgrp plato-succgrp plato-samegrp
   plato-string-position-category plato-alphabetic-position-category
   plato-length))

(define *initially-clamped-slipnodes*
  (list plato-letter-category plato-string-position-category))

(define platonic-letter?
  (lambda (node)
    (member? node *slipnet-letters*)))

(define platonic-number?
  (lambda (node)
    (member? node *slipnet-numbers*)))

(define number->platonic-number
  (lambda (n)
    (if (> n (length *slipnet-numbers*))
      #f
      (nth (- n 1) *slipnet-numbers*))))

(define platonic-number->number
  (lambda (node)
    (add1 (list-index *slipnet-numbers* node))))

(define platonic-relation?
  (lambda (node)
    (or (eq? node plato-identity)
     (eq? node plato-opposite)
     (eq? node plato-predecessor)
     (eq? node plato-successor))))

(define platonic-literal?
  (compose not platonic-relation?))

(define inverse
  (lambda (node)
    (cond
     ((eq? node plato-identity) plato-identity)
     ((exists? node) (tell node 'get-related-node plato-opposite))
     (else #f))))


;; Attach top-down codelet-types to top-down slipnodes.  This can only
;; be done after *codelet-types* has been defined.

(for* each node in (list plato-left plato-right) do
  (tell node 'set-top-down-codelet-types
    top-down-bond-scout:direction
    top-down-group-scout:direction))

(for* each node in (list plato-predecessor plato-successor plato-sameness) do
  (tell node 'set-top-down-codelet-types
    top-down-bond-scout:category))

(for* each node in (list plato-predgrp plato-succgrp plato-samegrp) do
  (tell node 'set-top-down-codelet-types
    top-down-group-scout:category))

(for* each node in (list plato-string-position-category
                    plato-alphabetic-position-category
                    plato-length) do
  (tell node 'set-top-down-codelet-types
    top-down-description-scout))

;; Set intrinsic-link-length values for the slipnodes
;; that serve as label nodes for certain slipnet links.

(tell plato-predecessor 'set-intrinsic-link-length 60)
(tell plato-successor 'set-intrinsic-link-length 60)
(tell plato-sameness 'set-intrinsic-link-length 0)
(tell plato-identity 'set-intrinsic-link-length 0)
(tell plato-opposite 'set-intrinsic-link-length 80)

;; Define possible-descriptor? predicate for slipnodes
;; that can be used as descriptors for objects.

(tell plato-one 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 1))))

(tell plato-two 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 2))))

(tell plato-three 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 3))))

(tell plato-four 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 4))))

(tell plato-five 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 5))))

(tell plato-leftmost 'define-descriptor-predicate
  (lambda (object)
    (and (not (tell object 'string-spanning-group?))
         (tell object 'leftmost-in-string?))))

(tell plato-rightmost 'define-descriptor-predicate
  (lambda (object)
    (and (not (tell object 'string-spanning-group?))
         (tell object 'rightmost-in-string?))))

(tell plato-middle 'define-descriptor-predicate
  (lambda (object)
    (tell object 'middle-in-string?)))

(tell plato-single 'define-descriptor-predicate
  (lambda (object)
    (and (letter? object) (tell object 'spans-whole-string?))))

(tell plato-whole 'define-descriptor-predicate
  (lambda (object)
    (tell object 'string-spanning-group?)))

(tell plato-alphabetic-first 'define-descriptor-predicate
  (lambda (object)
    (eq? (tell object 'get-descriptor-for plato-letter-category) plato-a)))

(tell plato-alphabetic-last 'define-descriptor-predicate
  (lambda (object)
    (eq? (tell object 'get-descriptor-for plato-letter-category) plato-z)))

(tell plato-letter 'define-descriptor-predicate letter?)

(tell plato-group 'define-descriptor-predicate group?)
        
;;----------------------------------------------------------------------------------
;; Slipnet Links

(define establish-link-type-length
  (lambda (from-node to-node link-type length)
    (let ((new-link (make-slipnet-link from-node to-node link-type)))
      (tell from-node 'add-to-outgoing-links link-type new-link)
      (tell to-node 'add-to-incoming-links new-link)
      (tell new-link 'set-link-length length)
      new-link)))

(define establish-link-type-label
  (lambda (from-node to-node link-type label)
    (let ((new-link (make-slipnet-link from-node to-node link-type)))
      (tell from-node 'add-to-outgoing-links link-type new-link)
      (tell to-node 'add-to-incoming-links new-link)
      (tell new-link 'set-label-node label)
      new-link)))

(define instance-link-97
  (lambda (from-node to-node)
    (establish-link-type-length from-node to-node 'instance 97)))

(define instance-link-100
  (lambda (from-node to-node)
    (establish-link-type-length from-node to-node 'instance 100)))

(define category-link-0
  (lambda (from-node to-node)
    (establish-link-type-length from-node to-node 'category 0)))

(define category-link-diff
  (lambda (from-node to-node)
    (establish-link-type-length from-node to-node 'category
      (- (cd to-node) (cd from-node)))))
  
(define lateral-link-label
  (lambda (from-node to-node label)
    (establish-link-type-label from-node to-node 'lateral label)))

(define lateral-sliplink-label
  (lambda (from-node to-node label)
    (establish-link-type-label from-node to-node 'lateral-sliplink label)))

(define lateral-link-length
  (lambda (from-node to-node length)
    (establish-link-type-length from-node to-node 'lateral length)))

(define lateral-sliplink-length
  (lambda (from-node to-node length)
    (establish-link-type-length from-node to-node 'lateral-sliplink length)))

(define lateral-link-length-label
  (lambda (from-node to-node length label)
    (let ((new-link (make-slipnet-link from-node to-node 'lateral)))
      (tell from-node 'add-to-outgoing-links 'lateral new-link)
      (tell to-node 'add-to-incoming-links new-link)
      (tell new-link 'set-link-length length)
      (tell new-link 'set-label-node label)
      new-link)))


;;  SUCCESSOR and PREDECESSOR links

(define a-b_link (lateral-link-label plato-a plato-b plato-successor))
(define b-c_link (lateral-link-label plato-b plato-c plato-successor))
(define c-d_link (lateral-link-label plato-c plato-d plato-successor))
(define d-e_link (lateral-link-label plato-d plato-e plato-successor))
(define e-f_link (lateral-link-label plato-e plato-f plato-successor))
(define f-g_link (lateral-link-label plato-f plato-g plato-successor))
(define g-h_link (lateral-link-label plato-g plato-h plato-successor))
(define h-i_link (lateral-link-label plato-h plato-i plato-successor))
(define i-j_link (lateral-link-label plato-i plato-j plato-successor))
(define j-k_link (lateral-link-label plato-j plato-k plato-successor))
(define k-l_link (lateral-link-label plato-k plato-l plato-successor))
(define l-m_link (lateral-link-label plato-l plato-m plato-successor))
(define m-n_link (lateral-link-label plato-m plato-n plato-successor))
(define n-o_link (lateral-link-label plato-n plato-o plato-successor))
(define o-p_link (lateral-link-label plato-o plato-p plato-successor))
(define p-q_link (lateral-link-label plato-p plato-q plato-successor))
(define q-r_link (lateral-link-label plato-q plato-r plato-successor))
(define r-s_link (lateral-link-label plato-r plato-s plato-successor))
(define s-t_link (lateral-link-label plato-s plato-t plato-successor))
(define t-u_link (lateral-link-label plato-t plato-u plato-successor))
(define u-v_link (lateral-link-label plato-u plato-v plato-successor))
(define v-w_link (lateral-link-label plato-v plato-w plato-successor))
(define w-x_link (lateral-link-label plato-w plato-x plato-successor))
(define x-y_link (lateral-link-label plato-x plato-y plato-successor))
(define y-z_link (lateral-link-label plato-y plato-z plato-successor))

(define b-a_link (lateral-link-label plato-b plato-a plato-predecessor))
(define c-b_link (lateral-link-label plato-c plato-b plato-predecessor))
(define d-c_link (lateral-link-label plato-d plato-c plato-predecessor))
(define e-d_link (lateral-link-label plato-e plato-d plato-predecessor))
(define f-e_link (lateral-link-label plato-f plato-e plato-predecessor))
(define g-f_link (lateral-link-label plato-g plato-f plato-predecessor))
(define h-g_link (lateral-link-label plato-h plato-g plato-predecessor))
(define i-h_link (lateral-link-label plato-i plato-h plato-predecessor))
(define j-i_link (lateral-link-label plato-j plato-i plato-predecessor))
(define k-j_link (lateral-link-label plato-k plato-j plato-predecessor))
(define l-k_link (lateral-link-label plato-l plato-k plato-predecessor))
(define m-l_link (lateral-link-label plato-m plato-l plato-predecessor))
(define n-m_link (lateral-link-label plato-n plato-m plato-predecessor))
(define o-n_link (lateral-link-label plato-o plato-n plato-predecessor))
(define p-o_link (lateral-link-label plato-p plato-o plato-predecessor))
(define q-p_link (lateral-link-label plato-q plato-p plato-predecessor))
(define r-q_link (lateral-link-label plato-r plato-q plato-predecessor))
(define s-r_link (lateral-link-label plato-s plato-r plato-predecessor))
(define t-s_link (lateral-link-label plato-t plato-s plato-predecessor))
(define u-t_link (lateral-link-label plato-u plato-t plato-predecessor))
(define v-u_link (lateral-link-label plato-v plato-u plato-predecessor))
(define w-v_link (lateral-link-label plato-w plato-v plato-predecessor))
(define x-w_link (lateral-link-label plato-x plato-w plato-predecessor))
(define y-x_link (lateral-link-label plato-y plato-x plato-predecessor))
(define z-y_link (lateral-link-label plato-z plato-y plato-predecessor))

(define one-two_link (lateral-link-label plato-one plato-two plato-successor))
(define two-three_link (lateral-link-label plato-two plato-three plato-successor))
(define three-four_link (lateral-link-label plato-three plato-four plato-successor))
(define four-five_link (lateral-link-label plato-four plato-five plato-successor))

(define five-four_link (lateral-link-label plato-five plato-four plato-predecessor))
(define four-three_link (lateral-link-label plato-four plato-three plato-predecessor))
(define three-two_link (lateral-link-label plato-three plato-two plato-predecessor))
(define two-one_link (lateral-link-label plato-two plato-one plato-predecessor))



;;  LETTER-CATEGORY links

(define letter-category-a_link (instance-link-97 plato-letter-category plato-a))
(define letter-category-b_link (instance-link-97 plato-letter-category plato-b))
(define letter-category-c_link (instance-link-97 plato-letter-category plato-c))
(define letter-category-d_link (instance-link-97 plato-letter-category plato-d))
(define letter-category-e_link (instance-link-97 plato-letter-category plato-e))
(define letter-category-f_link (instance-link-97 plato-letter-category plato-f))
(define letter-category-g_link (instance-link-97 plato-letter-category plato-g))
(define letter-category-h_link (instance-link-97 plato-letter-category plato-h))
(define letter-category-i_link (instance-link-97 plato-letter-category plato-i))
(define letter-category-j_link (instance-link-97 plato-letter-category plato-j))
(define letter-category-k_link (instance-link-97 plato-letter-category plato-k))
(define letter-category-l_link (instance-link-97 plato-letter-category plato-l))
(define letter-category-m_link (instance-link-97 plato-letter-category plato-m))
(define letter-category-n_link (instance-link-97 plato-letter-category plato-n))
(define letter-category-o_link (instance-link-97 plato-letter-category plato-o))
(define letter-category-p_link (instance-link-97 plato-letter-category plato-p))
(define letter-category-q_link (instance-link-97 plato-letter-category plato-q))
(define letter-category-r_link (instance-link-97 plato-letter-category plato-r))
(define letter-category-s_link (instance-link-97 plato-letter-category plato-s))
(define letter-category-t_link (instance-link-97 plato-letter-category plato-t))
(define letter-category-u_link (instance-link-97 plato-letter-category plato-u))
(define letter-category-v_link (instance-link-97 plato-letter-category plato-v))
(define letter-category-w_link (instance-link-97 plato-letter-category plato-w))
(define letter-category-x_link (instance-link-97 plato-letter-category plato-x))
(define letter-category-y_link (instance-link-97 plato-letter-category plato-y))
(define letter-category-z_link (instance-link-97 plato-letter-category plato-z))

(define a-letter-category_link (category-link-0 plato-a plato-letter-category))
(define b-letter-category_link (category-link-0 plato-b plato-letter-category))
(define c-letter-category_link (category-link-0 plato-c plato-letter-category))
(define d-letter-category_link (category-link-0 plato-d plato-letter-category))
(define e-letter-category_link (category-link-0 plato-e plato-letter-category))
(define f-letter-category_link (category-link-0 plato-f plato-letter-category))
(define g-letter-category_link (category-link-0 plato-g plato-letter-category))
(define h-letter-category_link (category-link-0 plato-h plato-letter-category))
(define i-letter-category_link (category-link-0 plato-i plato-letter-category))
(define j-letter-category_link (category-link-0 plato-j plato-letter-category))
(define k-letter-category_link (category-link-0 plato-k plato-letter-category))
(define l-letter-category_link (category-link-0 plato-l plato-letter-category))
(define m-letter-category_link (category-link-0 plato-m plato-letter-category))
(define n-letter-category_link (category-link-0 plato-n plato-letter-category))
(define o-letter-category_link (category-link-0 plato-o plato-letter-category))
(define p-letter-category_link (category-link-0 plato-p plato-letter-category))
(define q-letter-category_link (category-link-0 plato-q plato-letter-category))
(define r-letter-category_link (category-link-0 plato-r plato-letter-category))
(define s-letter-category_link (category-link-0 plato-s plato-letter-category))
(define t-letter-category_link (category-link-0 plato-t plato-letter-category))
(define u-letter-category_link (category-link-0 plato-u plato-letter-category))
(define v-letter-category_link (category-link-0 plato-v plato-letter-category))
(define w-letter-category_link (category-link-0 plato-w plato-letter-category))
(define x-letter-category_link (category-link-0 plato-x plato-letter-category))
(define y-letter-category_link (category-link-0 plato-y plato-letter-category))
(define z-letter-category_link (category-link-0 plato-z plato-letter-category))

(let ((letter-category-depth (cd plato-letter-category)))
  (for* each letter in *slipnet-letters* do
    (tell (first (tell letter 'get-category-links))
      'set-link-length (- letter-category-depth (cd letter)))))

(define samegrp-letter-category_link 
  (lateral-link-length plato-samegrp plato-letter-category 50))



;;  LENGTH links

(define length-one_link   (instance-link-100 plato-length plato-one))
(define length-two_link   (instance-link-100 plato-length plato-two))
(define length-three_link (instance-link-100 plato-length plato-three))
(define length-four_link  (instance-link-100 plato-length plato-four))
(define length-five_link  (instance-link-100 plato-length plato-five))

(define one-length_link   (category-link-0 plato-one plato-length))
(define two-length_link   (category-link-0 plato-two plato-length))
(define three-length_link (category-link-0 plato-three plato-length))
(define four-length_link  (category-link-0 plato-four plato-length))
(define five-length_link  (category-link-0 plato-five plato-length))

(let ((length-depth (cd plato-length)))
  (for* each number in *slipnet-numbers* do
    (tell (first (tell number 'get-category-links))
      'set-link-length (- length-depth (cd number)))))

(define predgrp-length_link 
  (lateral-link-length plato-predgrp plato-length 95))
(define succgrp-length_link 
  (lateral-link-length plato-succgrp plato-length 95))
(define samegrp-length_link 
  (lateral-link-length plato-samegrp plato-length 95))



;;  OPPOSITE links

(define alphabetic-first-alphabetic-last_link
  (lateral-sliplink-label plato-alphabetic-first plato-alphabetic-last plato-opposite))
(define alphabetic-last-alphabetic-first_link
  (lateral-sliplink-label plato-alphabetic-last plato-alphabetic-first plato-opposite))

(define leftmost-rightmost_link
  (lateral-sliplink-label plato-leftmost plato-rightmost plato-opposite))
(define rightmost-leftmost_link
  (lateral-sliplink-label plato-rightmost plato-leftmost plato-opposite))

(define left-right_link
  (lateral-sliplink-label plato-left plato-right plato-opposite))
(define right-left_link
  (lateral-sliplink-label plato-right plato-left plato-opposite))

(define successor-predecessor_link
  (lateral-sliplink-label plato-successor plato-predecessor plato-opposite))
(define predecessor-successor_link
  (lateral-sliplink-label plato-predecessor plato-successor plato-opposite))

(define predgrp-succgrp_link
  (lateral-sliplink-label plato-predgrp plato-succgrp plato-opposite))
(define succgrp-predgrp_link
  (lateral-sliplink-label plato-succgrp plato-predgrp plato-opposite))




;;  PROPERTY links

(define a-alphabetic-first_link 
  (establish-link-type-length plato-a plato-alphabetic-first 'property 75))

(define z-alphabetic-last_link 
  (establish-link-type-length plato-z plato-alphabetic-last 'property 75))


;;  OBJECT-CATEGORY links

(define object-category-letter_link (instance-link-100 plato-object-category plato-letter))
(define letter-object-category_link (category-link-diff plato-letter plato-object-category))

(define object-category-group_link (instance-link-100 plato-object-category plato-group))
(define group-object-category_link (category-link-diff plato-group plato-object-category))


;;  STRING-POSITION-CATEGORY links

(define string-position-category-leftmost_link 
  (instance-link-100 plato-string-position-category plato-leftmost))
(define leftmost-string-position-category_link
  (category-link-diff plato-leftmost plato-string-position-category))

(define string-position-category-rightmost_link 
  (instance-link-100 plato-string-position-category plato-rightmost))
(define rightmost-string-position-category_link
  (category-link-diff plato-rightmost plato-string-position-category))

(define string-position-category-middle_link 
  (instance-link-100 plato-string-position-category plato-middle))
(define middle-string-position-category_link
  (category-link-diff plato-middle plato-string-position-category))

(define string-position-category-single_link 
  (instance-link-100 plato-string-position-category plato-single))
(define single-string-position-category_link
  (category-link-diff plato-single plato-string-position-category))

(define string-position-category-whole_link 
  (instance-link-100 plato-string-position-category plato-whole))
(define whole-string-position-category_link
  (category-link-diff plato-whole plato-string-position-category))



;;  ALPHABETIC-POSITION-CATEGORY links

(define alphabetic-position-category-alphabetic-first_link 
  (instance-link-100 plato-alphabetic-position-category plato-alphabetic-first))
(define alphabetic-first-alphabetic-position-category_link
  (category-link-diff plato-alphabetic-first plato-alphabetic-position-category))

(define alphabetic-position-category-alphabetic-last_link 
  (instance-link-100 plato-alphabetic-position-category plato-alphabetic-last))
(define alphabetic-last-alphabetic-position-category_link
  (category-link-diff plato-alphabetic-last plato-alphabetic-position-category))


;;  DIRECTION-CATEGORY links

(define direction-category-left_link
  (instance-link-100 plato-direction-category plato-left))
(define left-direction-category_link
  (category-link-diff plato-left plato-direction-category))

(define direction-category-right_link
  (instance-link-100 plato-direction-category plato-right))
(define right-direction-category_link
  (category-link-diff plato-right plato-direction-category))


;;  BOND-CATEGORY links

(define bond-category-predecessor_link
  (instance-link-100 plato-bond-category plato-predecessor))
(define predecessor-bond-category_link
  (category-link-diff plato-predecessor plato-bond-category))

(define bond-category-successor_link
  (instance-link-100 plato-bond-category plato-successor))
(define successor-bond-category_link
  (category-link-diff plato-successor plato-bond-category))

(define bond-category-sameness_link
  (instance-link-100 plato-bond-category plato-sameness))
(define sameness-bond-category_link
  (category-link-diff plato-sameness plato-bond-category))


;;  GROUP-CATEGORY links

(define group-category-predgrp_link
  (instance-link-100 plato-group-category plato-predgrp))
(define predgrp-group-category_link
  (category-link-diff plato-predgrp plato-group-category))

(define group-category-succgrp_link
  (instance-link-100 plato-group-category plato-succgrp))
(define succgrp-group-category_link
  (category-link-diff plato-succgrp plato-group-category))

(define group-category-samegrp_link
  (instance-link-100 plato-group-category plato-samegrp))
(define samegrp-group-category_link
  (category-link-diff plato-samegrp plato-group-category))


;;  ASSOCIATED GROUP links

(define sameness-samegrp_link
  (lateral-link-length-label plato-sameness plato-samegrp 30 plato-group-category))
(define successor-succgrp_link
  (lateral-link-length-label plato-successor plato-succgrp 60 plato-group-category))
(define predecessor-predgrp_link
  (lateral-link-length-label plato-predecessor plato-predgrp 60 plato-group-category))



;;  ASSOCIATED BOND-CATEGORY links

(define samegrp-sameness_link
  (lateral-link-length-label plato-samegrp plato-sameness 90 plato-bond-category))
(define succgrp-successor_link
  (lateral-link-length-label plato-succgrp plato-successor 90 plato-bond-category))
(define predgrp-predecessor_link
  (lateral-link-length-label plato-predgrp plato-predecessor 90 plato-bond-category))



;;  BOND-FACET links

(define bond-facet-letter-category_link
  (instance-link-100 plato-bond-facet plato-letter-category))
(define letter-category-bond-facet_link
  (category-link-diff plato-letter-category plato-bond-facet))

(define bond-facet-length_link
  (instance-link-100 plato-bond-facet plato-length))
(define length-bond-facet_link
  (category-link-diff plato-length plato-bond-facet))


;;  LETTER-CATEGORY-LENGTH links

(define letter-category-length_link
  (lateral-sliplink-length plato-letter-category plato-length 95))
(define length-letter-category_link
  (lateral-sliplink-length plato-length plato-letter-category 95))


;;  LETTER-GROUP links

(define letter-group_link
  (lateral-sliplink-length plato-letter plato-group 90))
(define group-letter_link
  (lateral-sliplink-length plato-group plato-letter 90))



;;  DIRECTION-POSITION, DIRECTION-NEIGHBOR, and POSITION-NEIGHBOR links

;; Copycat conceptual problem:  For abc -> ...; cba -> ?, if the a's in the
;; initial and target strings get described as "first" ("alphabetic-first", that is),
;; then a first=>first mapping is almost certain for any bridge between them.
;; However, this makes an a->a bridge incompatible with a symmetric c->c
;; bridge because the CMs {rmost=>lmost, first=>first} are incompatible.
;; This helps out in the xyz problem, but it is a big hindrence here.  Really these
;; CMs should be simply "non-supporting", rather than "incompatible".  We want
;; {rmost=>lmost, right=>right} to be incompatible, {rmost=>lmost, first=>first}
;; to be non-supporting but compatible, and {rmost=>lmost, first=>last} or
;; {rmost=>lmost, left=>right} to be supporting and compatible.  Unfortunately,
;; given the way links between these nodes are originally defined in the Slipnet,
;; this isn't possible.  The following is designed to remedy this.  This may change
;; the behavior of the program significantly, at least on problems like xyz.
;; Now {first=>first, lmost=>rmost} will be neither supporting-CMs nor
;; incompatible CMs.  However, {first=>last, lmost=>rmost} will still be supporting,
;; and {lmost=>rmost, left=>left} will still be incompatible, as they should be:

(define leftmost-left_link
  (lateral-link-length-label plato-leftmost plato-left 90 plato-identity))
(define left-leftmost_link
  (lateral-link-length-label plato-left plato-leftmost 90 plato-identity))

(define leftmost-right_link
  (lateral-link-length-label plato-leftmost plato-right 100 plato-opposite))
(define right-leftmost_link
  (lateral-link-length-label plato-right plato-leftmost 100 plato-opposite))

(define rightmost-left_link
  (lateral-link-length-label plato-rightmost plato-left 100 plato-opposite))
(define left-rightmost_link
  (lateral-link-length-label plato-left plato-rightmost 100 plato-opposite))

(define rightmost-right_link
  (lateral-link-length-label plato-rightmost plato-right 90 plato-identity))
(define right-rightmost_link
  (lateral-link-length-label plato-right plato-rightmost 90 plato-identity))

(define alphabetic-first-leftmost_link
  (lateral-link-length plato-alphabetic-first plato-leftmost 100))
(define leftmost-alphabetic-first_link
  (lateral-link-length plato-leftmost plato-alphabetic-first 100))

(define alphabetic-first-rightmost_link
  (lateral-link-length plato-alphabetic-first plato-rightmost 100))
(define rightmost-alphabetic-first_link
  (lateral-link-length plato-rightmost plato-alphabetic-first 100))

(define alphabetic-last-leftmost_link
  (lateral-link-length plato-alphabetic-last plato-leftmost 100))
(define leftmost-alphabetic-last_link
  (lateral-link-length plato-leftmost plato-alphabetic-last 100))

(define alphabetic-last-rightmost_link
  (lateral-link-length plato-alphabetic-last plato-rightmost 100))
(define rightmost-alphabetic-last_link
  (lateral-link-length plato-rightmost plato-alphabetic-last 100))




;;  OTHER LINKS

(define single-whole_link
  (lateral-sliplink-length plato-single plato-whole 90))
(define whole-single_link
  (lateral-sliplink-length plato-whole plato-single 90))
