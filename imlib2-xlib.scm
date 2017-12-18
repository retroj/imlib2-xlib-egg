;; Copyright 2017 John J Foerch. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module imlib2-xlib
  (imlib-context-set-display
   imlib-context-disconnect-display
   imlib-context-set-visual
   imlib-context-set-colormap
   imlib-context-set-drawable
   imlib-context-set-mask
   imlib-context-get-display
   imlib-context-get-visual
   imlib-context-get-colormap
   imlib-context-get-drawable
   imlib-context-get-mask
   imlib-get-visual-depth
   imlib-get-best-visual
   imlib-pixmap-and-mask?
   imlib-pixmap-and-mask-pixmap
   imlib-pixmap-and-mask-mask
   imlib-render-pixmaps-for-whole-image
   imlib-render-pixmaps-for-whole-image-at-size
   imlib-render-image-on-drawable
   imlib-render-image-on-drawable-at-size
   imlib-render-image-part-on-drawable-at-size
   imlib-render-get-pixel-color
   imlib-create-image-from-drawable
   imlib-create-image-from-ximage
   imlib-create-scaled-image-from-drawable
   imlib-copy-drawable-to-image
   imlib-render-image-on-drawable-skewed
   imlib-render-image-on-drawable-at-angle)

(import chicken scheme foreign)

(use foreigners
     imlib2)

(declare (disable-interrupts))

(foreign-declare "#include <Imlib2.h>") 


;;
;; Datatypes
;;

(define-foreign-type XID unsigned-long)
(define-foreign-type Colormap XID)
(define-foreign-type DATA32 unsigned-int)
(define-foreign-type Display c-pointer)
(define-foreign-type Drawable XID)
(define-foreign-type Imlib_Image c-pointer)
(define-foreign-type Pixmap XID)
(define-foreign-type Visual c-pointer)
(define-foreign-type XImage c-pointer)

(define-foreign-type cbool char
  (lambda (b) (integer->char (if b 1 0)))
  (lambda (c) (if (eqv? #\null c) #f #t)))


;;
;;
;;

(define (imlib2-xlib-error loc msg . args)
  (abort (make-composite-condition
	   (make-property-condition 'exn
				    'location loc
				    'message msg
				    'arguments args)
	   (make-property-condition 'imlib2-xlib))))

(define (assert-image img loc . args)
  (when (not (image-ptr img))
    (imlib2-xlib-error loc "Invalid image parameter" args)))

(define-syntax define/img
  (syntax-rules ()
    ((define/img (func img arg ...)
       body ...)
     (define (func img arg ...)
       (assert-image img (quote func) arg ...)
       (%imlib-context-set-image (image-ptr img))
       body ...))))

(define %imlib-context-set-image
  (foreign-lambda void imlib_context_set_image Imlib_Image))


;;
;; Context setting
;;

(define imlib-context-set-display
  (foreign-lambda void imlib_context_set_display Display))

(define imlib-context-disconnect-display
  (foreign-lambda void imlib_context_disconnect_display))

(define imlib-context-set-visual
  (foreign-lambda void imlib_context_set_visual Visual))

(define imlib-context-set-colormap
  (foreign-lambda void imlib_context_set_colormap Colormap))

(define imlib-context-set-drawable
  (foreign-lambda void imlib_context_set_drawable Drawable))

(define imlib-context-set-mask
  (foreign-lambda void imlib_context_set_mask Pixmap))


;;
;; Context getting
;;

(define imlib-context-get-display
  (foreign-lambda Display imlib_context_get_display))

(define imlib-context-get-visual
  (foreign-lambda Visual imlib_context_get_visual))

(define imlib-context-get-colormap
  (foreign-lambda Colormap imlib_context_get_colormap))

(define imlib-context-get-drawable
  (foreign-lambda Drawable imlib_context_get_drawable))

(define imlib-context-get-mask
  (foreign-lambda Pixmap imlib_context_get_mask))

(define imlib-get-visual-depth
  (foreign-lambda int imlib_get_visual_depth Display Visual))

(define (imlib-get-best-visual display screen)
  (let-location ((depth int))
    (let ((visual
           ((foreign-lambda Visual imlib_get_best_visual Display int (c-pointer int))
            display screen (location depth))))
      (values visual depth))))


;;
;; Rendering functions
;;

(define-record-type :imlib-pixmap-and-mask
  (%make-imlib-pixmap-and-mask pixmap mask)
  imlib-pixmap-and-mask?
  (pixmap imlib-pixmap-and-mask-pixmap)
  (mask imlib-pixmap-and-mask-mask))

(define %imlib-free-pixmap-and-mask
  (foreign-lambda void imlib_free_pixmap_and_mask Pixmap))

(define (free-imlib-pixmap-and-mask! ipm)
  (%imlib-free-pixmap-and-mask (imlib-pixmap-and-mask-pixmap ipm))
  (%imlib-free-pixmap-and-mask (imlib-pixmap-and-mask-mask ipm)))

(define (make-imlib-pixmap-and-mask pixmap mask)
  (let ((ipm (%make-imlib-pixmap-and-mask pixmap mask)))
    (set-finalizer! ipm free-imlib-pixmap-and-mask!)
    ipm))

(define/img (imlib-render-pixmaps-for-whole-image img)
  (let-location ((pixmap Pixmap)
                 (mask Pixmap))
    ((foreign-lambda void imlib_render_pixmaps_for_whole_image
                     (c-pointer Pixmap) (c-pointer Pixmap))
     (location pixmap) (location mask))
    (make-imlib-pixmap-and-mask pixmap mask)))

(define/img (imlib-render-pixmaps-for-whole-image-at-size img width height)
  (let-location ((pixmap Pixmap)
                 (mask Pixmap))
    ((foreign-lambda void imlib_render_pixmaps_for_whole_image_at_size
                     (c-pointer Pixmap) (c-pointer Pixmap) int int)
     (location pixmap) (location mask) width height)
    (make-imlib-pixmap-and-mask pixmap mask)))

(define/img (imlib-render-image-on-drawable img x y)
  ((foreign-lambda void imlib_render_image_on_drawable int int)
   x y))

(define/img (imlib-render-image-on-drawable-at-size img x y width height)
  ((foreign-lambda void imlib_render_image_on_drawable_at_size int int int int)
   x y width height))

(define/img (imlib-render-image-part-on-drawable-at-size img source-x source-y
                                                         source-width source-height
                                                         x y width height)
  ((foreign-lambda void imlib_render_image_part_on_drawable_at_size
                   int int int int int int int int)
   source-x source-y source-width source-height x y width height))

(define imlib-render-get-pixel-color
  (foreign-lambda DATA32 imlib_render_get_pixel_color))



;;
;; Creation functions
;;

(define (imlib-create-image-from-drawable mask x y width height need-to-grab-x?)
  (set-finalizer!
   (make-image
    ((foreign-lambda Imlib_Image imlib_create_image_from_drawable
                     Pixmap int int int int cbool)
     mask x y width height need-to-grab-x?))
   gc-collect-image))

(define (imlib-create-image-from-ximage ximage mask x y width height need-to-grab-x?)
  (set-finalizer!
   (make-image
    ((foreign-lambda Imlib_Image imlib_create_image_from_ximage
                     XImage XImage int int int int cbool)
     ximage mask x y width height need-to-grab-x?))
   gc-collect-image))

(define (imlib-create-scaled-image-from-drawable mask source-x source-y
                                                 source-width source-height
                                                 dest-width dest-height
                                                 need-to-grab-x?
                                                 get-mask-from-shape?)
  (set-finalizer!
   (make-image
    ((foreign-lambda Imlib_Image imlib_create_scaled_image_from_drawable
                     Pixmap int int int int int int cbool cbool)
     mask source-x source-y source-width source-height
     dest-width dest-height need-to-grab-x? get-mask-from-shape?))
   gc-collect-image))

(define/img (imlib-copy-drawable-to-image img mask x y width height
                                          dest-x dest-y need-to-grab-x?)
  ((foreign-lambda cbool imlib_copy_drawable_to_image
                   Pixmap int int int int int int cbool)
   mask x y width height dest-x dest-y need-to-grab-x?))


;;
;; Rotation / skewing
;;

(define/img (imlib-render-image-on-drawable-skewed img source-x source-y
                                                   source-width source-height
                                                   dest-x dest-y
                                                   h-angle-x h-angle-y
                                                   v-angle-x v-angle-y)
  ((foreign-lambda void imlib_render_image_on_drawable_skewed
                   int int int int int int int int int int)
   source-x source-y source-width source-height dest-x dest-y
   h-angle-x h-angle-y v-angle-x v-angle-y))

(define/img (imlib-render-image-on-drawable-at-angle img source-x source-y
                                                     source-width source-height
                                                     dest-x dest-y
                                                     angle-x angle-y)
  ((foreign-lambda void imlib_render_image_on_drawable_at_angle
                   int int int int int int int int)
   source-x source-y source-width source-height dest-x dest-y angle-x angle-y))

)
