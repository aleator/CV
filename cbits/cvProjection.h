#ifndef __CVITERATORS__
#define __CVITERATORS__

#include <opencv2/core/core_c.h>

IplImage *project_polar(IplImage *src);

double wrapStereoCalibrate( const CvMat* object_points, const CvMat* image_points1,
                            const CvMat* image_points2, const CvMat* npoints,
                            CvMat* camera_matrix1, CvMat* dist_coeffs1,
                            CvMat* camera_matrix2, CvMat* dist_coeffs2,
                            CvSize image_size, CvMat* R, CvMat* T,
                            CvMat* E, CvMat* F,
                            CvTermCriteria *term_crit,
                            int flags);

#endif
