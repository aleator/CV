#include "cvProjection.h"

#include <stdio.h>
#include <math.h>

#ifndef M_PI
#define M_PI           3.14159265358979323846
#endif

float calc_sum_9(float *pos, size_t width)
{
  float *pos1, *pos2, *pos3, *pos4, *pos5;
  pos1 = pos - 1 * width - 1;
  pos2 = pos - 1;
  pos3 = pos + width - 1;
  return 
    *pos1 + *(pos1+1) + *(pos1+2) +
    *pos2 + *(pos2+1) + *(pos2+2) +
    *pos3 + *(pos3+1) + *(pos3+2); 
}

IplImage *project_polar(IplImage *src)
{
  IplImage *dst;
  unsigned int w, h, x, y, px, py, minx, maxx, miny, maxy, sumx, sumy, count;
  unsigned int new_width, new_height, src_stride, dst_stride;
  float value, a, r, rmin, rmax, cx, cy, ratio;
  float *src_data, *dst_data, *src_pos, *dst_pos;
  
  CvSize size = cvGetSize(src);
  src_data = (float*)src->imageData;
  src_stride = (int)(src->widthStep / sizeof(float));
  w = size.width;
  h = size.height;
  
  minx = 2000000000;
  maxx = 0;
  miny = 2000000000;
  maxy = 0;
  sumx = 0;
  sumy = 0;
  count = 0;
  
  /* find the extents of object */
  for (y = 0; y < h; y++) {
    src_pos = src_data + y * src_stride;
    for (x = 0; x < w; x++, src_pos++) {
      value = *src_pos;
      if (value > 0.0001) {
        if (x < minx) minx = x;
        if (x > maxx) maxx = x;
        if (y < miny) miny = y;
        if (y > maxy) maxy = y;
        sumx += x;
        sumy += y;
        count += 1;
      }
    }
  }
  
  cx = (float)sumx / count;
  cy = (float)sumy / count;
  ratio = (float)(maxy - miny) / (float)(maxx - minx);
  printf("w=%d h=%d cx=%f cy=%f ratio=%f\n", w, h, cx, cy, ratio);
  
  rmin = 2000000000;
  rmax = 0;
  for (y = 0; y < h; y++) {
    src_pos = src_data + y * src_stride;
    for (x = 0; x < w; x++, src_pos++) {
      value = *src_pos;
      if (value > 0.0001) {
        px = floor(x - cx);
        py = floor((y / ratio) - (cy / ratio));
        r = sqrt(py*py + px*px);
        if (r < rmin) rmin = r;
        if (r > rmax) rmax = r;
      }
    }
  }

  new_height = ceil(rmax - rmin);
  new_width = ceil(2 * M_PI * rmax);
  printf("w=%d h=%d rmin=%f rmax=%f\n", new_width, new_height, rmin, rmax);

  size.width = new_width;
  size.height = new_height;
  dst = cvCreateImage(size, IPL_DEPTH_32F, 1);
  dst_data = (float*)dst->imageData;
  dst_stride = (int)(dst->widthStep / sizeof(float));
  
  for (y = 0; y < new_height; y++) {
    dst_pos = dst_data + y * dst_stride;
    r = (float)(rmax - y);
    for (x = 0; x < new_width; x++, dst_pos++) {
      a = ((float)x / (float)new_width) * 2 * M_PI;
      px = floor(cx + (r * cos(a)));
      py = floor(((cy / ratio) + (r * sin(a))) * ratio);
      if (px < 0 || py < 0 || px >= w || py >= h) {
        /*printf(".");*/
      }
      else {
        src_pos = src_data + py * src_stride + px;
        value = *src_pos;
        if (value < 0.0001) {
          if (px > 0 && py > 0 && px < w-1 && py < h-1) {
            value = calc_sum_9(src_pos, src_stride) / 9;
            if (value < 0.0001) {
              value = 0.5;
            }
          }
          else {
            value = 0.5;
          }
        }
        *dst_pos = value;
      }
    }
  }
  return dst;
}

/*
  
  int w = (int)(2 * M_PI * r);
  tmp = ensure32F(src);
  dst = wrapCreateImage32F(w,h,1);
  if (r < cx && r < cy && h <= r) {
    float *src_data = (float *)tmp->imageData;
    float *dst_data = (float *)dst->imageData;
    int src_stride = (int)(tmp->widthStep / sizeof(float));
    int dst_stride = (int)(dst->widthStep / sizeof(float));
    int px, py;
    double angle;
    double radius;
    for (int i = 0; i < w; i++) {
      angle = ((double)i / (double)w) * (2 * M_PI);
      for (int j = 0; j < h; j++) {
        radius = (double)(r-j);
        px = cx + floor(radius * cos(angle));
        py = cy + floor(radius * sin(angle));
        *(dst_data + j * dst_stride + i) = *(src_data + py * src_stride + px);
      }
    }
  }
  else {
    printf("image too small\n");
  }
*/

