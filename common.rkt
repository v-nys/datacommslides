#lang at-exp racket
(require
  racket/provide
  reprovide/reprovide
  scribble/srcdoc
  slideshow
  (only-in racket/gui/base make-color)
  (only-in browser/external send-url)
  (for-doc scribble/manual))

;; generally useful libraries
(reprovide
 slideshow/staged-slide
 pict/conditional)

;; color palette
(define (mix c1 c2)
  (make-color
   (round (/ (+ (send c1 red) (send c2 red)) 2))
   (round (/ (+ (send c1 green) (send c2 green)) 2))
   (round (/ (+ (send c1 blue) (send c2 blue)) 2))))
(define transparent (make-color 255 255 255 0))
(define owl-red (make-color 255 92 168))
(define owl-green (make-color 90 168 0))
(define owl-blue (make-color 0 152 233))
(define owl-yellow (make-color 242 147 24))
(define owl-violet (mix owl-red owl-blue))
(define owl-brown (mix owl-red owl-green))
(define owl-orange (mix owl-red owl-yellow))
(define owl-cyan (mix owl-blue owl-green))
(define light-gray (make-color 210 210 210))
(define v-light-gray (make-color 225 225 225))
(provide
 transparent
 (matching-identifiers-out
  #rx"^owl-[a-z]+$"
  (all-defined-out)))
(define warning-color (make-parameter owl-red))
(define link-color (make-parameter owl-cyan))
(define accent-color (make-parameter owl-yellow))
(define example-color (make-parameter owl-blue))
(define tip-color (make-parameter owl-green))
(provide warning-color link-color accent-color)

;; boxes
(define (content-box header . elements)
  (let ([paragraph (apply para elements #:fill? #t #:decode? #t #:width (- (current-para-width) m-width))])
    (vc-append
     ;; the top bar
     (lc-superimpose
      (filled-rectangle
       (current-para-width)
       (+ (/ m-height 2)
          (pict-height header))
       #:color light-gray
       #:draw-border? #f)
      (hc-append
       (blank (/ m-width 2) m-height)
       header))
     ;; the body
     (cc-superimpose
      (filled-rectangle
       (current-para-width)
       (+ (pict-height paragraph) (/ m-height 2))
       #:draw-border? #f
       #:color v-light-gray)
      paragraph))))

(define (warning-box . elements)
  (content-box
   (colorize
    (bt "Opgelet!")
    (warning-color))
   elements))
(provide
 (proc-doc/names
  warning-box
  ((flat-rec-contract elem/c (or/c string? pict? (listof elem/c))) ... . -> .  pict?)
  (e ...)
  @{Draws the supplied arguments @racket[e ...] inside a box indicating a warning.}))

(define (tip-box . elements)
  (content-box
   (colorize
    (bt "Tip:")
    (tip-color))
   elements))
(provide
 (proc-doc/names
  tip-box
  ((flat-rec-contract elem/c (or/c string? pict? (listof elem/c))) ... . -> .  pict?)
  (e ...)
  @{Draws the supplied arguments @racket[e ...] inside a box indicating a tip.}))

(define (question-box . elements)
  (content-box
   (colorize
    (bt "Doordenker:")
    (accent-color))
   elements))
(provide
 (proc-doc/names
  question-box
  ((flat-rec-contract elem/c (or/c string? pict? (listof elem/c))) ... . -> .  pict?)
  (e ...)
  @{Draws the supplied arguments @racket[e ...] inside a box indicating a question.}))

;; text formatting functions
(define (highlight text) (colorize (t text) (accent-color)))
(define emph it)
(define work it)
(define (term tm) ; TODO: derive a list of terms per presentation from this
  (colorize (bt tm) (accent-color)))
(define (placeholder p)
  (colorize (t p) (link-color)))
(define (nbtt str) (text str 'modern (current-font-size)))
(provide emph work term placeholder nbtt highlight)

;; generally useful functionality
(define standard-m (t "m"))
(define m-height (pict-height standard-m))
(define m-width (pict-width standard-m))

(define (clickable-link pict-or-str address)
  (clickback
   (if (string? pict-or-str)
       (colorize (t pict-or-str) (link-color))
       pict-or-str)
   (λ ()
     (send-url address))))
(provide
 (proc-doc/names
  clickable-link
  (->
   (or/c pict? string?)
   string?
   pict?)
  (ps url)
  @{Turns a @racket{pict?} or @racket{string?} @racket[ps] into a @racket{pict?} which, when clicked, opens a browser window for @racket[url].}))

(define (qa txt q a)
  (hbl-append
   (t txt)
   (colorize
    (text "?" (cons 'superscript (current-main-font)) (current-font-size))
    (accent-color))))
(provide qa)

(define (subsubitem . elems)
  (apply subitem elems #:width (- (current-para-width) (* 4 (current-gap-size))) #:bullet (t "-")))
(provide subsubitem)