#include "tgaimage.h"
#include "model.h"

extern "C"
{

TGAImage *tgaimage_new_w_h_bpp(int w, int h, int bpp)
{
    return new TGAImage(w, h, bpp);
}

void tgaimage_delete(TGAImage *image)
{
    delete image;
}

bool tgaimage_write_tga_file(TGAImage *image, const char *filename, bool rle)
{
    return image->write_tga_file(filename, rle);
}

bool tgaimage_flip_vertically(TGAImage *image)
{
    return image->flip_vertically();
}

bool tgaimage_set_r_g_b(TGAImage *image, int x, int y, unsigned char R, unsigned char G, unsigned char B)
{
    return image->set(x, y, TGAColor(R, G, B));
}

Model *model_new(const char *filename)
{
    return new Model(filename);
}

void model_delete(Model *model)
{
    delete model;
}

int model_nfaces(Model *model)
{
    return model->nfaces();
}

float model_vert(Model *model, int i, int j)
{
    return model->vert(i)[j];
}

int model_face(Model *model, int i, int j)
{
    return model->face(i)[j];
}

}
