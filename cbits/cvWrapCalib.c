#include "cvWrapCalib.h"

int wrapStereoRectifyUncalibrated(const CvMat* points1, const CvMat* points2, const CvMat* F
                               ,CvSize* img_size, CvMat* H1, CvMat* H2, double threshold)
{
return cvStereoRectifyUncalibrated(points1, points2, F , *img_size, H1, H2, threshold);
};

double wrapStereoCalibrate( const CvMat* object_points, const CvMat* image_points1,
                            const CvMat* image_points2, const CvMat* npoints,
                            CvMat* camera_matrix1, CvMat* dist_coeffs1,
                            CvMat* camera_matrix2, CvMat* dist_coeffs2,
                            CvSize *image_size, CvMat* R, CvMat* T,
                            CvMat* E, CvMat* F,
                            CvTermCriteria *term_crit,
                            int flags)
{
return cvStereoCalibrate( object_points, image_points1,
                            image_points2, npoints,
                            camera_matrix1, dist_coeffs1,
                            camera_matrix2, dist_coeffs2,
                            *image_size, R, T,
                            E, F,
                            *term_crit,
                            flags);
}
