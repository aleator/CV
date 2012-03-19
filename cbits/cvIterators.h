#ifndef __CVITERATORS__
#define __CVITERATORS__

#include <opencv2/core/core_c.h>

/*
Image Iterators.

Provides an 'image context' with
-a pointer to image data
-a scheme for updating the pointer in a manner to iterate over the image
-some iterators allow to change image content as well
-some iterators may provide access to a neighborhood
*/

typedef struct F32_image_iterator_t {
    /** pointer to current data position */
    float *image_data;
    /** offset to next pixel */
    int pixel_offset;
    /** offset to start of next col/row */
    int line_offset;
    /** counter to the end of current col/row */
    unsigned int line_counter;
    /** counter to the end of image */
    unsigned int end_counter;
    /** line length for resetting the line end counter */
    unsigned int line_length;
    /** total amount of lines */
    unsigned int line_count;
    /** current pixel position (updated only upon request) */
    CvPoint pos;
} F32_image_iterator;

F32_image_iterator *alloc_F32_image_iterator();

void free_F32_image_iterator(F32_image_iterator *i);

void F32_create_rowwise_iterator(F32_image_iterator *i, IplImage *image);

/**
 * Move the iterator to the next position
 */
F32_image_iterator *F32_next(F32_image_iterator *i);

/**
 * Acquire the pixel value
 */
float *F32_val(F32_image_iterator *i);

/**
 * Acquire the [x,y] position of this pixel
 */
CvPoint *F32_rowwise_pos(F32_image_iterator *i);

#endif
