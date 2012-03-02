#include "wrapImgProc.h"
#include <opencv2/imgproc/imgproc_c.h>

void wrapFilter2(const CvArr* src, CvArr* dst, const CvMat* kernel, CvPoint *anchor)
{cvFilter2D(src, dst, kernel, *anchor);};

