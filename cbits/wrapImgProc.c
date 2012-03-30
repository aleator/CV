#include "wrapImgProc.h"
#include <opencv2/imgproc/imgproc_c.h>

void wrapFilter2(const CvArr* src, CvArr* dst, const CvMat* kernel, CvPoint *anchor)
{cvFilter2D(src, dst, kernel, *anchor);};

void fillConnectedComponents(IplImage* img, int *count)
{
  unsigned char *data, value;
  unsigned int x, y, w, h, pos, step, index;
  CvPoint point;
  CvSize size;
  
  size = cvGetSize(img);
  w = size.width;
  h = size.height;
  data = (unsigned char *)img->imageData;
  step = (unsigned int)(img->widthStep / sizeof(unsigned char));
  
  index = 0;
  for (y = 0; y < h; y++) {
    pos = y * step;
    for (x = 0; x < w; x++, pos++) {
      value = data[pos];
      if (value > index) {
        index++;
        point.x = x;
        point.y = y;
        cvFloodFill(img, point, cvRealScalar(index), cvScalarAll(0), cvScalarAll(0), NULL, 4, NULL);
      }
    }
  }
  *count = index;
}

void maskConnectedComponent(const IplImage *src, IplImage *mask, int id)
{
  unsigned char *data, value, index;
  unsigned int x, y, w, h, pos, step;
  CvPoint point;
  CvSize size;
  
  size = cvGetSize(mask);
  w = size.width;
  h = size.height;
  data = (unsigned char *)mask->imageData;
  step = (unsigned int)(mask->widthStep / sizeof(unsigned char));
  
  if (id > 0 && id < 256) {
    cvCopy(src, mask, NULL);
    index = (unsigned char)id;
    for (y = 0; y < h; y++) {
      pos = y * step;
      for (x = 0; x < w; x++, pos++) {
        value = data[pos];
        if (value > 0) {
          if (value == index) {
            data[pos] = 255;
          }
          else {
            data[pos] = 0;
          }
        }
      }
    }
  }
  else {
    cvSetZero(mask);
  }
}
