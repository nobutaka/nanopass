;directive -lstdc++ samples/tgaimage.cpp samples/geometry.cpp samples/model.cpp samples/stub.cpp -DHEAP_SIZE=4000000

(define tgaimage_new_w_h_bpp (cproc 'TGAImage* "tgaimage_new_w_h_bpp"))
(define tgaimage_delete (cproc 'void "tgaimage_delete"))
(define tgaimage_write_tga_file (cproc 'bool "tgaimage_write_tga_file"))
(define tgaimage_flip_vertically (cproc 'bool "tgaimage_flip_vertically"))
(define tgaimage_set_r_g_b (cproc 'bool "tgaimage_set_r_g_b"))

(define model_new (cproc 'Model* "model_new"))
(define model_delete (cproc 'void "model_delete"))
(define model_nfaces (cproc 'int "model_nfaces"))
(define model_vert
  (let ([vert (cproc 'float "model_vert")])
    (lambda (model i)
      (vector (vert model i 0) (vert model i 1) (vert model i 2)))))
(define model_face
  (let ([face (cproc 'int "model_face")])
    (lambda (model i)
      (vector (face model i 0) (face model i 1) (face model i 2)))))

(define line
  (lambda (x0 y0 x1 y1 image color)
    (let ([steep #f])
      (if (fx< (fxabs (fx- x0 x1)) (fxabs (fx- y0 y1)))
          (begin (swap! x0 y0)
                 (swap! x1 y1)
                 (set! steep #t))
          #f)
      (if (fx< x1 x0)
          (begin (swap! x0 x1)
                 (swap! y0 y1))
          #f)
      (let ([dx (fx- x1 x0)]
            [dy (fx- y1 y0)])
        (let ([derror2 (fx* (fxabs dy) 2)]
              [error2 0]
              [y y0])
          (do ((x x0 (fx+ x 1)))
              ((fx< x1 x) #t)
            (if steep
                (tgaimage_set_r_g_b image y x (vector-ref color 0) (vector-ref color 1) (vector-ref color 2))
                (tgaimage_set_r_g_b image x y (vector-ref color 0) (vector-ref color 1) (vector-ref color 2)))
            (set! error2 (fx+ error2 derror2))
            (if (fx< dx error2)
                (begin (set! y (fx+ y (if (fx< y0 y1) 1 -1)))
                       (set! error2 (fx- error2 (fx* dx 2))))
                #f)))))))

(let ([model (model_new "samples/obj/african_head/african_head.obj")]
      [image (tgaimage_new_w_h_bpp 800 800 3)])
  (do ((i 0 (fx+ i 1)))
      ((fx<= (model_nfaces model) i) #t)
    (let ([face (model_face model i)])
      (do ((j 0 (fx+ j 1)))
          ((fx<= 3 j) #t)
        (let ([v0 (model_vert model (vector-ref face j))]
              [v1 (model_vert model (vector-ref face (modulo (fx+ j 1) 3)))])
          (let ([x0 (fixnum (fl/ (fl* (fl+ (vector-ref v0 0) 1.0) 800.0) 2.0))]
                [y0 (fixnum (fl/ (fl* (fl+ (vector-ref v0 1) 1.0) 800.0) 2.0))]
                [x1 (fixnum (fl/ (fl* (fl+ (vector-ref v1 0) 1.0) 800.0) 2.0))]
                [y1 (fixnum (fl/ (fl* (fl+ (vector-ref v1 1) 1.0) 800.0) 2.0))])
            (line x0 y0 x1 y1 image '#(255 255 255)))))))
  (tgaimage_flip_vertically image)
  (tgaimage_write_tga_file image "output.tga" #t)
  (tgaimage_delete image)
  (model_delete model))
