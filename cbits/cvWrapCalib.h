
#ifndef __CVWRAPCALIB__
#define __CVWRAPCALIB__

#include <opencv2/calib3d/calib3d.hpp>

int wrapStereoRectifyUncalibrated(const CvMat* points1, const CvMat* points2, const CvMat* F
                                 ,CvSize* img_size, CvMat* H1, CvMat* H2, double threshold);
#endif
