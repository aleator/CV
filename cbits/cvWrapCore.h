#ifndef __CVWRAPCORE__
#define __CVWRAPCORE__

#include <opencv2/core/core_c.h>

/* add missing flag values */

#define CV_DXT_COMPLEX_OUTPUT 16
#define CV_DXT_REAL_OUTPUT 32
#define CV_DXT_INV_REAL (CV_DXT_INVERSE + CV_DXT_REAL_OUTPUT)

IplImage* wrapCreateImage(int width, int height, int depth, int channels);

/* wrap CvScalar as cvSet parameter */

void wrapSet(CvArr* arr, CvScalar *value, CvArr* mask);
void wrapSetAll(CvArr* arr, double value, CvArr* mask);
void wrapSet1(CvArr* arr, double value, CvArr* mask);
void wrapSet2(CvArr* arr, double value1, double value2, CvArr* mask);
void wrapSet3(CvArr* arr, double value1, double value2, double value3, CvArr* mask);
void wrapSet4(CvArr* arr, double value1, double value2, double value3, double value4, CvArr* mask);

/* swap the quadrants 1-3 and 2-4 in a DFT image */
void swapQuadrants(CvArr *src);

#endif
