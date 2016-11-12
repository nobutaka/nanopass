;directive -lstdc++ samples/tgaimage.cpp samples/stub.cpp -DHEAP_SIZE=4000000
(define tgaimage_new_w_h_bpp (cproc 'TGAImage* "tgaimage_new_w_h_bpp"))
(define tgaimage_delete (cproc 'void "tgaimage_delete"))
(define tgaimage_write_tga_file (cproc 'bool "tgaimage_write_tga_file"))
(define tgaimage_flip_vertically (cproc 'bool "tgaimage_flip_vertically"))
(define tgaimage_set_r_g_b (cproc 'bool "tgaimage_set_r_g_b"))

(let ([image (tgaimage_new_w_h_bpp 100 100 3)])
  (tgaimage_set_r_g_b image 10 10 255 0 0)
  (tgaimage_set_r_g_b image 10 11 0 255 0)
  (tgaimage_set_r_g_b image 10 12 0 0 255)
  (tgaimage_flip_vertically image)
  (tgaimage_write_tga_file image "output.tga"))
