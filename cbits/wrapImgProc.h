#ifndef __CVWRAPIMGPROC__
#define __CVWRAPIMGPROC__

#include <opencv2/imgproc/imgproc_c.h>

#define CV_THRESH_OTSU_BINARY     CV_THRESH_OTSU | CV_THRESH_BINARY
#define CV_THRESH_OTSU_BINARY_INV CV_THRESH_OTSU | CV_THRESH_BINARY_INV
#define CV_THRESH_OTSU_TRUNC      CV_THRESH_OTSU | CV_THRESH_TRUNC
#define CV_THRESH_OTSU_TOZERO     CV_THRESH_OTSU | CV_THRESH_TOZERO
#define CV_THRESH_OTSU_TOZERO_INV CV_THRESH_OTSU | CV_THRESH_TOZERO_INV

void wrapFilter2(const CvArr* src, CvArr* dst, const CvMat* kernel, CvPoint *anchor);

#endif
