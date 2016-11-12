#include "tgaimage.h"

extern "C" TGAImage *tgaimage_new_w_h_bpp(int w, int h, int bpp)
{
    return new TGAImage(w, h, bpp);
}

extern "C" void tgaimage_delete(TGAImage *image)
{
    delete image;
}

extern "C" bool tgaimage_write_tga_file(TGAImage *image, const char *filename, bool rle)
{
    return image->write_tga_file(filename, rle);
}

extern "C" bool tgaimage_flip_vertically(TGAImage *image)
{
    return image->flip_vertically();
}

extern "C" bool tgaimage_set_r_g_b(TGAImage *image, int x, int y, unsigned char R, unsigned char G, unsigned char B)
{
    return image->set(x, y, TGAColor(R, G, B));
}
