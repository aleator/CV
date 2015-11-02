#include "cvWrapCore.h"
#include <opencv2/core/core.hpp>

IplImage* wrapCreateImage(int width, int height, int depth, int channels)
{
    CvSize s;
    IplImage *r;
    s.width = width;
    s.height = height;
    r = cvCreateImage(s,depth,channels);
    cvSetZero(r);
    return r;
}

void wrapSet(CvArr* arr, CvScalar *value, CvArr* mask)
{
    cvSet(arr, *value, mask);
}

void wrapSetAll(CvArr* arr, double value, CvArr* mask)
{
    cvSet(arr, cvScalarAll(value), mask);
}

void wrapSet1(CvArr* arr, double value, CvArr* mask)
{
    cvSet(arr, cvRealScalar(value), mask);
}

void wrapSet2(CvArr* arr, double value1, double value2, CvArr* mask)
{
    cvSet(arr, cvScalar(value1, value2, 0, 0), mask);
}

void wrapSet3(CvArr* arr, double value1, double value2, double value3, CvArr* mask)
{
    cvSet(arr, cvScalar(value1, value2, value3, 0), mask);
}

void wrapSet4(CvArr* arr, double value1, double value2, double value3, double value4, CvArr* mask)
{
    cvSet(arr, cvScalar(value1, value2, value3, value4), mask);
}

void swapQuadrants(CvArr *src)
{
    int cx, cy;
    CvMat *tmp, q0, q1, q2, q3;

    cx = cvGetDimSize(src, 0) / 2;
    cy = cvGetDimSize(src, 1) / 2;

    tmp = cvCreateMat(cx,cy,cvGetElemType(src));

    cvGetSubRect(src, &q0, cvRect(0,0,cx,cy));
    cvGetSubRect(src, &q1, cvRect(cx,0,cx,cy));
    cvGetSubRect(src, &q2, cvRect(0,cy,cx,cy));
    cvGetSubRect(src, &q3, cvRect(cx,cy,cx,cy));

    cvCopy(&q0,tmp, NULL);
    cvCopy(&q3,&q0, NULL);
    cvCopy(tmp,&q3, NULL);

    cvCopy(&q1,tmp, NULL);
    cvCopy(&q2,&q1, NULL);
    cvCopy(tmp,&q2, NULL);

    cvReleaseMat(&tmp);
}
