;directive -lstdc++ samples/tgaimage.cpp samples/stub.cpp -DHEAP_SIZE=4000000

(define tgaimage_new_w_h_bpp (cproc 'TGAImage* "tgaimage_new_w_h_bpp"))
(define tgaimage_delete (cproc 'void "tgaimage_delete"))
(define tgaimage_write_tga_file (cproc 'bool "tgaimage_write_tga_file"))
(define tgaimage_flip_vertically (cproc 'bool "tgaimage_flip_vertically"))
(define tgaimage_set_r_g_b (cproc 'bool "tgaimage_set_r_g_b"))

(define line
  (lambda (x0 y0 x1 y1 image color)
    (do ((t 0.0 (fl+ t 0.01)))
        ((fl<= 1.0 t) #t)
      (let ([x (fixnum (fl+ (fl* (flonum x0) (fl- 1.0 t)) (fl* (flonum x1) t)))]
            [y (fixnum (fl+ (fl* (flonum y0) (fl- 1.0 t)) (fl* (flonum y1) t)))])
        (tgaimage_set_r_g_b image x y (vector-ref color 0) (vector-ref color 1) (vector-ref color 2))))))

(let ([image (tgaimage_new_w_h_bpp 100 100 3)])
  (line 13 20 80 40 image '#(255 255 255))
  (tgaimage_flip_vertically image)
  (tgaimage_write_tga_file image "output.tga"))
