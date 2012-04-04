#ifndef __CVFILES__
#define __CVFILES__

#include <opencv2/core/core_c.h>

#ifndef M_PI
#define M_PI           3.14159265358979323846
#endif

IplImage *read_from_tcr(const char *path, unsigned int **timestamp_array);
IplImage *read_from_tcr_rectified(const char *path);

#endif
