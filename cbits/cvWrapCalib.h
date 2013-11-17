
#ifndef __CVWRAPCALIB__
#define __CVWRAPCALIB__

#include <opencv2/calib3d/calib3d.hpp>

int wrapStereoRectifyUncalibrated(const CvMat* points1, const CvMat* points2, const CvMat* F
                                 ,CvSize* img_size, CvMat* H1, CvMat* H2, double threshold);

double wrapStereoCalibrate( const CvMat* object_points, const CvMat* image_points1,
                            const CvMat* image_points2, const CvMat* npoints,
                            CvMat* camera_matrix1, CvMat* dist_coeffs1,
                            CvMat* camera_matrix2, CvMat* dist_coeffs2,
                            CvSize* image_size, CvMat* R, CvMat* T,
                            CvMat* E, CvMat* F,
                            CvTermCriteria *term_crit,
                            int flags);


#endif
