//@@language c
#include <opencv/cv.h>
#include <opencv/cxcore.h>
#include <opencv/highgui.h>
#include <complex.h>

/*
 * Average angular second moment calculated from four grey-level
 * co-occurrence matrices with degrees 0', 45', 90' and 135'
 * for given IplImage pointer.
 */
double calculate_asm_average(IplImage *im);

int get_color(IplImage *image, int x, int y);
