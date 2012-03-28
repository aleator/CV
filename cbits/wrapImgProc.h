#ifndef __CVWRAPIMGPROC__
#define __CVWRAPIMGPROC__

#include <opencv2/imgproc/imgproc_c.h>

void wrapFilter2(const CvArr* src, CvArr* dst, const CvMat* kernel, CvPoint *anchor);

#endif
