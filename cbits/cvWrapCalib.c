#include "cvWrapCalib.h"

int wrapStereoRectifyUncalibrated(const CvMat* points1, const CvMat* points2, const CvMat* F
                               ,CvSize* img_size, CvMat* H1, CvMat* H2, double threshold)
{
return cvStereoRectifyUncalibrated(points1, points2, F , *img_size, H1, H2, threshold);
};
