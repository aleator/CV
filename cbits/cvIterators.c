#include "cvIterators.h"
#include <stdio.h>

F32_image_iterator *alloc_F32_image_iterator()
{
    return (F32_image_iterator *)malloc(sizeof(F32_image_iterator));
}

void free_F32_image_iterator(F32_image_iterator *i)
{
    free(i);
}

void F32_create_rowwise_iterator(F32_image_iterator *i, IplImage *image)
{
    i->image_data = (float *)image->imageData;
    i->pixel_offset = image->nChannels;
    i->line_offset = (image->widthStep  / sizeof(float)) - (image->width * image->nChannels) + 1;
    i->line_counter = image->width - 1;
    i->end_counter = image->height - 1;
    i->line_length = image->width;
    i->line_count = image->height;
}

F32_image_iterator *F32_next(F32_image_iterator *i)
{
    if (i->line_counter--) {
        i->image_data += i->pixel_offset;
    }
    else {
        if (i->end_counter--) {
            i->line_counter = i->line_length - 1;
            i->image_data += i->line_offset;
        }
        else {
            i->image_data = NULL;
            /* a bit clumsy, but needed to avoid trouble in case this is */
            /* called again after reaching the end */
            i->line_counter = 0;
            i->end_counter = 0;
        }
    }
    return i;
}

float *F32_val(F32_image_iterator *i)
{
    return i->image_data;
}

CvPoint *F32_rowwise_pos(F32_image_iterator *i)
{
    /* store the pos to struct member, so we can pass pointers to haskell.. */
    /* getting the pos like this is expensive, but assume it is used rarely */
    i->pos.x = i->line_length - i->line_counter - 1;
    i->pos.y = i->line_count - i->end_counter - 1;
/*
    if (i->image_data != NULL) {
      printf("P((%d,%d),%0.2f)", i->pos.x, i->pos.y, *(i->image_data));
    }
*/
    return &i->pos;
}
