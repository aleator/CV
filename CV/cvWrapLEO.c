//@+leo-ver=4-thin
//@+node:aleator.20050908100314:@thin cvWrapLEO.c
//@@language c

//@+all
//@+node:aleator.20050908100314.1:Includes
#include "cvWrapLEO.h"
#include <stdio.h>
#include <complex.h>

//@-node:aleator.20050908100314.1:Includes
//@+node:aleator.20050908100314.2:Wrappers

const double M_PI=acos(-1.0);

size_t images;

void incrImageC(void)
{
 images++;
}

void wrapReleaseImage(IplImage *t)
{
 // printf("%d ",images);
 cvReleaseImage(&t);
 images--;
}

void wrapReleaseCapture(CvCapture *t)
{
 cvReleaseCapture(&t);
}

void wrapReleaseVideoWriter(CvCapture *t)
{
 cvReleaseCapture(&t);
}

void wrapReleaseStructuringElement(IplConvKernel *t)
{
 cvReleaseStructuringElement(&t);
}

IplImage* wrapLaplace(IplImage *src,int size)
{
IplImage *res;
IplImage *tmp;
tmp = cvCreateImage(cvGetSize(src),IPL_DEPTH_16S,1);
res = cvCreateImage(cvGetSize(src),IPL_DEPTH_8U,1);
cvLaplace(src,tmp,size);
cvConvertScale(tmp,res,1,0);
return res;
}

IplImage* wrapSobel(IplImage *src,int dx
                   ,int dy,int size)
{
IplImage *res;
IplImage *tmp;
tmp = cvCreateImage(cvGetSize(src),IPL_DEPTH_16S,1);
res = cvCreateImage(cvGetSize(src),IPL_DEPTH_8U,1);
cvSobel(src,tmp,dx,dy,size);
cvConvertScale(tmp,res,1,0);
cvReleaseImage(&tmp);
return res;
}

IplImage* wrapCreateImage32F(const int width
                         ,const int height
                         ,const int channels)
{
 CvSize s;
 IplImage *r;
 s.width = width; s.height = height;
 r = cvCreateImage(s,IPL_DEPTH_32F,channels);
 cvSetZero(r);
 return r;
}

IplImage* wrapCreateImage64F(const int width
                         ,const int height
                         ,const int channels)
{
 CvSize s;
 IplImage *r;
 s.width = width; s.height = height;
 r = cvCreateImage(s,IPL_DEPTH_64F,channels);
 cvSetZero(r);
 return r;
}


IplImage* wrapCreateImage8U(const int width
                         ,const int height
                         ,const int channels)
{
 CvSize s;
 IplImage *r;
 s.width = width; s.height = height;
 r = cvCreateImage(s,IPL_DEPTH_8U,channels);
 cvSetZero(r);
 return r;
}

IplImage* composeMultiChannel(IplImage* img0
                             ,IplImage* img1
                             ,IplImage* img2
                             ,IplImage* img3
                             ,const int channels)
{
 CvSize s;
 IplImage *r;
 s = cvGetSize(img0);
 r = cvCreateImage(s,img0->depth,channels);
 cvSetZero(r);
 cvMerge(img0,img1,img2,img3,r);
 return r;
}
void wrapSubRS(const CvArr *src, double s, CvArr *dst)
{
 cvSubRS(src,cvRealScalar(s),dst,0);
}

void wrapSubS(const CvArr *src, double s, CvArr *dst)
{
 cvSubS(src,cvRealScalar(s),dst,0);
}

void wrapAddS(const CvArr *src, double s, CvArr *dst)
{
 cvAddS(src,cvRealScalar(s),dst,0);
}

void wrapAbsDiffS(const CvArr *src, double s, CvArr *dst)
{
 cvAbsDiffS(src,dst,cvScalarAll(s));
}

double wrapAvg(const CvArr *src)
{
 CvScalar avg = cvAvg(src,0);
 return avg.val[0];
}

double wrapStdDev(const CvArr *src)
{
 CvScalar dev;
 cvAvgSdv(src,0,&dev,0);
 return dev.val[0];
}

double wrapStdDevMask(const CvArr *src,const CvArr *mask)
{
 CvScalar dev;
 IplImage *mask8 = ensure8U(mask);
 cvAvgSdv(src,0,&dev,mask8);
 cvReleaseImage(&mask8); 
 return dev.val[0];
}
double wrapMeanMask(const CvArr *src,const CvArr *mask)
{
 CvScalar mean;
 IplImage *mask8 = ensure8U(mask);
 cvAvgSdv(src,&mean,0,mask8);
 cvReleaseImage(&mask8); 
 return mean.val[0];
}

double wrapSum(const CvArr *src)
{
 CvScalar sum = cvSum(src);
 return sum.val[0];
}

void wrapMinMax(const CvArr *src,const CvArr *mask
               ,double *minVal, double *maxVal)
{
 //cvMinMaxLoc(src,minVal,maxVal,NULL,NULL,NULL);
 int i,j;
 int minx,miny,maxx,maxy;
 double pixel;
 double maskP;
 int t;
 double min=100000,max=-100000; // Some problem with DBL_MIN.

 CvSize s = cvGetSize(src);
 for(i=0; i<s.width; i++)
  for(j=0; j<s.height; j++)
   {
   pixel = cvGetReal2D(src,j,i);
   maskP = mask != 0 ? cvGetReal2D(mask,j,i) : 1;
   // TODO: Fix below.. 
   min   = (maskP >0.5 ) && (pixel < min) ? pixel : min; 
   max   = (maskP >0.5 ) && (pixel > max) ? pixel : max; 
   }
 (*minVal) = min; (*maxVal) = max; 
}

void wrapSetImageROI(IplImage *i,int x, int y, int w, int h)
{
 CvRect r = cvRect(x,y,w,h);
 cvSetImageROI(i,r);
}


// Return image that is IPL_DEPTH_8U version of 
// given src
IplImage* ensure8U(const IplImage *src)
{
 CvSize size;
 IplImage *result;
 int channels = src->nChannels;
 int dstDepth = IPL_DEPTH_8U;
 size = cvGetSize(src);
 result = cvCreateImage(size,dstDepth,channels);

 switch(src->depth) {
  case IPL_DEPTH_32F:
  case IPL_DEPTH_64F:
   cvConvertScale(src,result,255.0,0); // Scale the values to [0,255]
   return result;
  case IPL_DEPTH_8U:
   cvConvertScale(src,result,1,0);
   return result;
  default:
   printf("Cannot convert to floating image");
   abort();
   
 }
}

// Return image that is IPL_DEPTH_32F version of 
// given src
IplImage* ensure32F(const IplImage *src)
{
 CvSize size;
 IplImage *result;
 int channels = src->nChannels;
 int dstDepth = IPL_DEPTH_32F;
 size = cvGetSize(src);
 result = cvCreateImage(size,dstDepth,channels);

 switch(src->depth) {
  case IPL_DEPTH_32F:
  case IPL_DEPTH_64F:
   cvConvertScale(src,result,1,0); // Scale the values to [0,255]
   return result;
  case IPL_DEPTH_8U:
  case IPL_DEPTH_8S:
   cvConvertScale(src,result,1.0/255.0,0);
   return result;
  case IPL_DEPTH_16S:
   cvConvertScale(src,result,1.0/65535.0,0);
   return result;
  case IPL_DEPTH_32S:
   cvConvertScale(src,result,1.0/4294967295.0,0);
   return result;
  default:
   printf("Cannot convert to floating image");
   abort();
   
 }
}

void wrapSet32F2D(CvArr *arr, int x, int y, double value)
{ 
 cvSet2D(arr,x,y,cvRealScalar(value)); 
}

double wrapGet32F2D(CvArr *arr, int x, int y)
{ 
 CvScalar r;
 r = cvGet2D(arr,x,y); 
 return r.val[0];
}

double wrapGet32F2DC(CvArr *arr, int x, int y,int c)
{ 
 CvScalar r;
 r = cvGet2D(arr,x,y); 
 return r.val[c];
}


void wrapDrawCircle(CvArr *img, int x, int y, int radius, float r,float g,float b, int thickness)
{
 cvCircle(img,cvPoint(x,y),radius,CV_RGB(r,g,b),thickness,8,0);
}

void wrapDrawText(CvArr *img, char *text, float s, int x, int y,float r,float g,float b)
{
CvFont font; //?
cvInitFont(&font, CV_FONT_HERSHEY_PLAIN, s, s, 0, 2, 8);
cvPutText(img, text, cvPoint(x,y), &font, CV_RGB(r,g,b));
}

void wrapDrawRectangle(CvArr *img, int x1, int y1, 
                       int x2, int y2, float r, float g, float b,
                       int thickness)
{
 cvRectangle(img,cvPoint(x1,y1),cvPoint(x2,y2),CV_RGB(r,g,b),thickness,8,0);
}


void wrapDrawLine(CvArr *img, int x, int y, int x1, int y1, double r, double g, double b, int thickness)
{
 cvLine(img,cvPoint(x,y),cvPoint(x1,y1),CV_RGB(r,g,b),thickness,4,0);
}

void wrapFillPolygon(IplImage *img, int pc, int *xs, int *ys, float r, float g, float b)
{ 
 int i=0;
 int pSizes[] = {pc};
 CvPoint *pts = (CvPoint*)malloc(pc*sizeof(CvPoint));
 for (i=0; i<pc; ++i)
    {pts[i].x = xs[i]; 
     pts[i].y = ys[i]; 
     }
 cvFillPoly(img, &pts, pSizes, 1, CV_RGB(r,g,b), 8, 0 );
 free(pts);
}



int getImageWidth(IplImage *img)
{
 return cvGetSize(img).width;
}
int getImageHeight(IplImage *img)
{
 return cvGetSize(img).height;
}

IplImage* getSubImage(IplImage *img, int sx,int sy,int w,int h)
{
 CvRect r;
 CvSize s;
 IplImage *newImage;
 
 r.x = sx; r.y = sy;
 r.width = w; r.height = h;
 s.width = w; s.height = h;
 
 cvSetImageROI(img,r);
 newImage = cvCreateImage(s,img->depth,img->nChannels);
 cvCopy(img, newImage,0);
 cvResetImageROI(img);
 return newImage;
}

IplImage* simpleMergeImages(IplImage *a, IplImage *b,int offset_x, int offset_y)
{
 CvSize aSize = cvGetSize(a);
 CvSize bSize = cvGetSize(b);
 int startx = 0 < offset_x ? 0 : offset_x;
 int endx = aSize.width > bSize.width+offset_x ? aSize.width : bSize.width+offset_x ;

 int starty = 0 < offset_y ? 0 : offset_y;
 int endy = aSize.height > bSize.height+offset_y ? aSize.height : bSize.height+offset_y ;

 CvSize size;
 size.width  = endx-startx;
 size.height = endy-starty;

 CvRect aPos = cvRect(offset_x<0?-offset_x:0
                     ,offset_y<0?-offset_y:0
                     ,aSize.width
                     ,aSize.height);

 CvRect bPos = cvRect(offset_x<0?0:offset_x
                     ,offset_y<0?0:offset_y
                     ,bSize.width
                     ,bSize.height);

 IplImage *resultImage = cvCreateImage(size,a->depth,a->nChannels);

 // Blit the images into bigger result image using cvCopy
 cvSetImageROI(resultImage,aPos);
 cvCopy(a,resultImage,NULL);
 cvSetImageROI(resultImage,bPos);
 cvCopy(b,resultImage,NULL);
 cvResetImageROI(resultImage);
 return resultImage;
}

void blitImg(IplImage *a, IplImage *b,int offset_x, int offset_y)
{
 CvSize bSize = cvGetSize(b);
 CvRect pos = cvRect(offset_x
                    ,offset_y
                    ,bSize.width
                    ,bSize.height);

 // Blit the images b into a using cvCopy
 printf("Doing a blit\n"); fflush(stdout);
 cvSetImageROI(a,pos);
 cvCopy(b,a,NULL);
 cvResetImageROI(a);
 printf("Done!\n"); fflush(stdout);
}
#define FGET(img,x,y) (((float *)((img)->imageData + (y)*(img)->widthStep))[(x)])

IplImage* makeEvenDown(IplImage *src)
{
 CvSize size = cvGetSize(src);
 int w = size.width-(size.width % 2);
 int h = size.height-(size.height % 2);
 IplImage *result = wrapCreateImage32F(w,h,1);    
 CvRect pos = cvRect(0
                    ,0
                    ,size.width
                    ,size.height);
 // Blit the images b into a using cvCopy
 cvSetImageROI(src,pos);
 cvCopy(src,result,NULL);
 cvResetImageROI(result);
 return result;
}

IplImage* makeEvenUp(IplImage *src)
{
 CvSize size = cvGetSize(src);
 int w = size.width+(size.width % 2);
 int h = size.height+(size.height % 2);
 int j;
 IplImage *result = wrapCreateImage32F(w,h,1);    
 CvRect pos = cvRect(0
                    ,0
                    ,size.width
                    ,size.height);
 // Blit the images b into a using cvCopy
 cvSetImageROI(result,pos);
 cvCopy(src,result,NULL);
 cvResetImageROI(result);
 if (size.width % 2 == 1)
      {for (j=0; j<=size.height; j++) {
          FGET(result,size.width,j) = FGET(result,size.width-1,j); } }
 if (size.width % 2 == 1)
      {for (j=0; j<=size.width; j++) {
          FGET(result,j,(size.height)) = FGET(result,j,(size.height-1)); } }
 return result;
}

IplImage* padUp(IplImage *src,int right, int bottom)
{
 CvSize size = cvGetSize(src);
 int w = size.width + (right  ? 1 : 0);
 int h = size.height+ (bottom ? 1 : 0);
 int j;
 IplImage *result = wrapCreateImage32F(w,h,1);    
 CvRect pos = cvRect(0
                    ,0
                    ,size.width
                    ,size.height);
 // Blit the images b into a using cvCopy
 cvSetImageROI(result,pos);
 cvCopy(src,result,NULL);
 cvResetImageROI(result);
 if (right)
      {for (j=0; j<=size.height; j++) {
          FGET(result,size.width,j) = 2*FGET(result,size.width-1,j)
                                       -FGET(result,size.width-2,j); } }
 if (bottom)
      {for (j=0; j<=size.width; j++) {

          FGET(result,j,(size.height)) = 2*FGET(result,j,(size.height-1))
                                          -FGET(result,j,(size.height-2)); 
                                          } }
 return result;
}

void masked_merge(IplImage *src1, IplImage *mask, IplImage *src2, IplImage *dst)
{
 int i,j;
 CvSize size = cvGetSize(dst);
 for (i=0; i<size.width; i++)
    for (j=0; j<size.height; j++) {
       FGET(dst,i,j) =  FGET(src1,i,j)*FGET(mask,i,j) 
                        +FGET(src2,i,j)*(1-FGET(mask,i,j));
     }
}

void vertical_average(IplImage *src, IplImage *dst)
{
 int i,j;
 double avg;
 CvSize size = cvGetSize(dst);
 for (i=0; i<size.width; i++) {
    avg = 0;
    for (j=0; j<size.height; j++) { avg += FGET(src,i,j); }
    avg = avg / size.height;
    for (j=0; j<size.height; j++) { FGET(dst,i,j) = avg; }
    }
}


IplImage* fadedEdges(int w, int h, int edgeW) {
    IplImage *result;
    int i,j;
    result = wrapCreateImage32F(w,h,1);    
    for (i=0; i<h; i++)
       for (j=0; j<w; j++) {
           float dx = i < (h/2.0) ? i : h-i ;
           float dy = j < (w/2.0) ? j : w-j ;
           float x = dx > edgeW ? 1 : dx/edgeW;
           float y = dy > edgeW ? 1 : dy/edgeW;
           FGET(result,j,i) = x*y;
           }
    return result;
}

IplImage* rectangularDistance(int w, int h) {
    IplImage *result;
    int i,j;
    result = wrapCreateImage32F(w,h,1);    
    for (i=0; i<h; i++)
       for (j=0; j<w; j++) {
           float dx = i < (h/2.0) ? i/(h*1.0) : (h-i)/(h*1.0) ;
           float dy = j < (w/2.0) ? j/(w*1.0) : (w-j)/(w*1.0) ;
           FGET(result,j,i) = dx<dy?dx:dy;
           }
    return result;
}
IplImage* vignettingModelCos4(int w, int h) {
    IplImage *result;
    int i,j;
    double nx,ny;
    double r;
    const double x0 = w/2.0;
    const double y0 = h/2.0;
    result = wrapCreateImage32F(w,h,1);    
    for (i=0; i<h; i++)
       for (j=0; j<w; j++) {
            nx = (y0-i)/h;
            ny = (x0-j)/w;
            r = sqrt(nx*nx+ny*ny);
            FGET(result,j,i) = pow(cos (r),4);
           }
    return result;
}

IplImage* vignettingModelCos4XCyl(int w, int h) {
    IplImage *result;
    int i,j;
    double r;
    const double x0 = w/2.0;
    const double y0 = h/2.0;
    result = wrapCreateImage32F(w,h,1);    
    for (i=0; i<h; i++)
       for (j=0; j<w; j++) {
            r = fabs((i-y0)/y0) ;
            FGET(result,j,i) = pow(cos (r),4);
           }
    return result;
}

IplImage* vignettingModelX2Cyl(int w, int h,double m, double s, double c) {
    IplImage *result;
    int i,j;
    double r;
    result = wrapCreateImage32F(w,h,1);    
    for (i=0; i<h; i++)
       for (j=0; j<w; j++) {
            FGET(result,j,i) = -((i-c)*s)*((i-c)*s)-m;
           }
    return result;
}
inline double eucNorm(CvPoint2D64f p) {return (p.x*p.x+p.y*p.y);}

IplImage* vignettingModelB3(int w, int h,double b1, double b2, double b3) {
    IplImage *result;
    int i,j;
    double r;
    result = wrapCreateImage32F(w,h,1);    
    for (i=0; i<h; i++)
       for (j=0; j<w; j++) {
            CvPoint2D64f nor = toNormalizedCoords(cvSize(w,h),cvPoint(j,i));
            r = eucNorm(nor);
            FGET(result,j,i) = b3*pow(r,6)+b2*pow(r,4)+b3*pow(r,2)+1;
           }
    return result;
}
IplImage* vignettingModelP(int w, int h,double scalex, double scaley, double max) {
    IplImage *result;
    int i,j;
    double r;
    double mx = w/2.0;
    double my = w/2.0;
    result = wrapCreateImage32F(w,h,1);    
    for (i=0; i<h; i++)
       for (j=0; j<w; j++) {
            FGET(result,j,i) =-((i-my)*scaley)*((i-my)*scaley)*((j-mx)*scalex)*((j-mx)*scalex)-max ;
           }
    return result;
}

IplImage* simplePerspective(double k,IplImage *src) {
    IplImage *result;
    int i,j;
    double r;
    result = cvCloneImage(src);    
    int h = cvGetSize(src).height;
    int w = cvGetSize(src).width;
    CvPoint2D32f srcPts[4] = {{0,0},{w-1,0},{w-1,h-1},{0,h-1}};
    CvPoint2D32f dstPts[4] = {{-k,0},{w-1+k,0},{w-1,h-1},{0,h-1}};
    CvMat* M = cvCreateMat(3,3,CV_32FC1);
    cvGetPerspectiveTransform(srcPts, dstPts, M);
    cvWarpPerspective(src, result, M, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
    cvReleaseMat(&M);
    return result;
}

IplImage* wrapPerspective(IplImage* src, double a1, double a2, double a3
                                       , double a4, double a5, double a6
                                       , double a7, double a8, double a9)
{
    IplImage *res = cvCloneImage(src);
    double a[] = { a1,a2,a3,
                   a4,a5,a6,
                   a7,a8,a9};

    CvMat M = cvMat(3,3,CV_64FC1,a);
    cvWarpPerspective(src, res, &M, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
    return res;
}

void findHomography(double* srcPts, double *dstPts, int noPts, double *homography)
{
CvMat src = cvMat(noPts, 2, CV_64FC1, srcPts);
CvMat dst = cvMat(noPts, 2, CV_64FC1, dstPts);
CvMat *hmg = cvCreateMat(3,3,CV_32FC1);
int i;
cvFindHomography(&src, &dst, hmg, 0, 0, 0);
for (i=0;i<3*3;++i)
        homography[i] = cvmGet(hmg,i/3,i%3);
cvReleaseMat(&hmg);
}

inline CvPoint2D64f toNormalizedCoords(CvSize area, CvPoint from)
{
    CvPoint2D64f res;
    res.x = (from.x-area.width/2.0)/area.width;
    res.y = (from.y-area.height/2.0)/area.height;   
    return res;
}

inline CvPoint fromNormalizedCoords(CvSize area, CvPoint2D64f from)
{
    CvPoint res;
    res.x = (from.x+0.5)*area.width;
    res.y = (from.y+0.5)*area.height;   
    return res;
}

inline CvPoint2D64f fromNormalizedCoords64f(CvSize area, CvPoint2D64f from)
{
    CvPoint2D64f res;
    res.x = (from.x+0.5)*area.width;
    res.y = (from.y+0.5)*area.height;   
    return res;
}

void alphaBlit(IplImage *a, IplImage *aAlpha, IplImage *b, IplImage *bAlpha, int offset_y, int offset_x)
{
    // TODO: Add checks for image type and size
 int i,j;
 CvSize bSize = cvGetSize(b);
 CvSize aSize = cvGetSize(a);
 CvRect pos = cvRect(offset_x
                    ,offset_y
                    ,bSize.width
                    ,bSize.height);
 for (i=0; i<bSize.height; i++)
    for (j=0; j<bSize.width; j++) {
       float aA, bA,fV;
       if (j+offset_x>=aSize.width || i+offset_y>=aSize.height || i+offset_y < 0 || j+offset_x<0) continue;

       aA = FGET(aAlpha,j+offset_x,i+offset_y);
       bA = FGET(bAlpha,j,i);
       fV = aA+bA > 0 ? (FGET(b,j,i)*bA+FGET(a,j+offset_x,i+offset_y)*aA)/(aA+bA) : FGET(b,j,i) ;
       FGET(a,j+offset_x,i+offset_y) =fV;
       FGET(aAlpha,j+offset_x,i+offset_y) =aA+bA;
    }
}


void plainBlit(IplImage *a, IplImage *b, int offset_y, int offset_x)
{
    // TODO: Add checks for image type and size
 int i,j;
 CvSize aSize = cvGetSize(a);
 CvSize bSize = cvGetSize(b);
 for (i=0; i<bSize.height; i++) {
    for (j=0; j<bSize.width; j++) {
       if (j+offset_x<0 || j+offset_x>=aSize.width || i+offset_y<0 || i+offset_y>=aSize.height ) continue;
       if (a->nChannels == 1) 
            {FGET(a,j+offset_x,i+offset_y) =FGET(b,j,i);}
        else if (a->nChannels ==3) 
            {
             int dx = j+offset_x; int dy = i+offset_y;
             ((float *)(a->imageData + dy*a->widthStep))[dx*a->nChannels + 0] =
              ((float *)(b->imageData + i*b->widthStep))[j*b->nChannels + 0] ; // B
             ((float *)(a->imageData + dy*a->widthStep))[dx*a->nChannels + 1] =
              ((float *)(b->imageData + i*b->widthStep))[j*b->nChannels + 1] ; // G
             ((float *)(a->imageData + dy*a->widthStep))[dx*a->nChannels + 2] =
              ((float *)(b->imageData + i*b->widthStep))[j*b->nChannels + 2] ; // R
            }
             else {printf("Can't blit this - pic weird number of channels\n"); abort();}

    }}
}

void subpixel_blit(IplImage *a, IplImage *b, double offset_y, double offset_x)
{
    // TODO: Add checks for image type and size
 int i,j;
 CvSize aSize = cvGetSize(a);
 CvSize bSize = cvGetSize(b);
 for (i=0; i<aSize.height; i++)
    for (j=0; j<aSize.width; j++) {
       double x_at_b=j-offset_x;
       double y_at_b=i-offset_y;
       if (x_at_b <0 || x_at_b >= bSize.width
           || y_at_b <0 || y_at_b >= bSize.height) continue;
       FGET(a,j,i) =bilinearInterp(b,x_at_b,y_at_b);
        // TODO: Check boundaries! #SAFETY

    }
}


// Histograms.
void wrapReleaseHist(CvHistogram *hist)
{
 cvReleaseHist(&hist);
}

CvHistogram* calculateHistogram(IplImage *img,int bins)
{
 float st_range[] = {-1,1};
 float *ranges[]  = {st_range};
 int hist_size[] = {bins};
 CvHistogram *result = cvCreateHist(1,hist_size,CV_HIST_ARRAY,ranges,1);
 cvCalcHist(&img,result,0,0);
 return result;
}

void get_histogram(IplImage *img,IplImage *mask
                 ,float a, float b,int isCumulative
                 ,int binCount
                 ,double *values)
{
 int i=0;
 float st_range[] = {a,b};
 float *ranges[]  = {st_range};
 int hist_size[] = {binCount};
 CvHistogram *result = cvCreateHist(1,hist_size,CV_HIST_ARRAY
                                   ,ranges,1);
 cvCalcHist(&img,result,isCumulative,mask);
 for (i=0;i<binCount;++i)
    {
     *values = cvQueryHistValue_1D(result,i); values++;
    }
 cvReleaseHist(&result);
 return;
}

double getHistValue(CvHistogram *h,int bin)
{
 return *cvGetHistValue_1D(h,bin);
}

// Convolutions
IplImage* wrapFilter2D(IplImage *src, int ax,int ay, 
                    int w, int h, double *kernel){
int i,j;
IplImage *target = cvCloneImage(src);
CvMat *kernelMat = cvCreateMat(w,h,CV_32FC1);
for(i=0;i<w*h;i++)
  cvSetReal2D(kernelMat,i%w,i/w,kernel[i]); 
cvFilter2D(src,target,kernelMat,cvPoint(ay,ax));
cvReleaseMat(&kernelMat);
return target;
}

IplImage* wrapFilter2DImg(IplImage *src
                         ,IplImage *mask
                         ,int ax,int ay)
{
int i,j;
IplImage *target = cvCloneImage(src);
CvSize size = cvGetSize(mask);
CvMat *kernelMat = cvCreateMat(size.width,size.height,CV_32FC1);
for(i=0;i<size.width;i++)
 for(j=0;j<size.height;j++)
  cvSetReal2D(kernelMat,i,j,cvGetReal2D(mask,j,i)); 
cvFilter2D(src,target,kernelMat,cvPoint(ay,ax));
cvReleaseMat(&kernelMat);
return target;
}

// Connected components

void wrapFloodFill(IplImage *i, int x, int y, double c
                  ,double low, double high,int fixed)
{
 int flag = 8 | (fixed ? CV_FLOODFILL_FIXED_RANGE : 0);
 cvFloodFill(i,cvPoint(x,y),cvRealScalar(c),cvRealScalar(low)
            ,cvRealScalar(high),NULL,flag,NULL);
}
                  
// hough-lines

void wrapProbHoughLines(IplImage *img, double rho, double theta
                       , int threshold, double minLength
                       , double gapLength
                       , int *maxLines
                       , int *xs, int *ys
                       , int *xs1, int *ys1)
{
 IplImage *tmp;
 CvSeq *lines = 0;
 int i;
 CvMemStorage *storage = cvCreateMemStorage(0); 
 
 tmp = ensure8U(img);

 lines = cvHoughLines2(tmp,storage,CV_HOUGH_PROBABILISTIC
                     ,rho,theta,threshold,minLength,gapLength);
 for( i = 0; i < MIN(lines->total,*maxLines); i++ )
        {
            CvPoint* line = (CvPoint*)cvGetSeqElem(lines,i);
            xs[i] = line[0].x; xs1[i] = line[1].x;
            ys[i] = line[0].y; ys1[i] = line[1].y; 
        }
 *maxLines = MIN(lines->total,*maxLines);

 cvReleaseImage(&tmp);
 cvReleaseMemStorage(&storage);
 
}
                       


//@-node:aleator.20050908100314.2:Wrappers
//@+node:aleator.20050908100314.3:Utilities
/* These are utilities that operate on opencv primitives but
  are not really wrappers.. Due to the fact that I seem to
  be incapable to link multiple objects including openCV
  headers this seems to be the next best solution.

  Watch out for name collisions!

*/
//@+node:aleator.20070906153003:Trigonometric operations

void calculateAtan(IplImage *src, IplImage *dst)
{
  CvSize imageSize = cvGetSize(dst);
  double r=0; int i; int j;
  for(i=0; i<imageSize.width; ++i)
    for(j=0; j<imageSize.height; ++j) {
          r = cvGetReal2D(src,j,i);
          cvSet2D(dst,j,i,cvScalarAll(atan(r)));
    }
}
//@nonl
//@-node:aleator.20070906153003:Trigonometric operations
//@+node:aleator.20051109111547:Pixel accessors
// All these will work only on grayscale.
inline int imax(int x, int y) {return (x>y) ? x:y;}
inline int imin(int x, int y) {return (x<y) ? x:y;}
inline double blurGet2D(IplImage *img,int x, int y)
{
 CvSize size = cvGetSize(img);
 x = imax(0,imin(x,size.width-1));
 y = imax(0,imin(y,size.height-1));

 return cvGetReal2D(img,y,x);
}

//@-node:aleator.20051109111547:Pixel accessors
//@+node:aleator.20070827150608:Haar Filters

// Simple routines for calculating pixelwise
// haar responses

void haarFilter(IplImage *intImg, 
                int x1, int y1, int x2, int y2,
                IplImage *target)
{
  int i,j;
  double s = 0;
  double ratio = 1;
  double desArea = (x2-x1)*(y1-y2);
  double area = 0;
  int rx1,rx2,ry1,ry2;
  CvSize imageSize = cvGetSize(target);  
  for(i=0; i<imageSize.width; ++i)
    for(j=0; j<imageSize.height; ++j) {
         rx1 = imax(0,imin(i+x1,imageSize.width-1));  
         ry1 = imax(0,imin(j+y1,imageSize.height-1));
         rx2 = imax(0,imin(i+x2,imageSize.width-1)); 
         ry2 = imax(0,imin(j+y2,imageSize.height-1));
         area = (float)((rx2-rx1)*(ry2-ry1));
        // if (area > 0) ratio = fabs(desArea/area);
        // else ratio=1;
         //printf("Ratio(%d,%d) is %lf\n",rx1,ry1,ratio);
         s = blurGet2D(intImg,rx1,ry1)
             -blurGet2D(intImg,rx1,ry2)
             -blurGet2D(intImg,rx2,ry1)
             +blurGet2D(intImg,rx2,ry2);
          cvSet2D(target,j,i,cvScalarAll(s/area));
    }
}

double haar_at(IplImage *intImg, 
                int x1, int y1, int w, int h)
{
  int i,j;
  double s = 0;
  s = blurGet2D(intImg,x1,y1)
     -blurGet2D(intImg,x1,y1+h)
     -blurGet2D(intImg,x1+w,y1)
     +blurGet2D(intImg,x1+w,y1+h);
  return s;
}
                
//@nonl
//@-node:aleator.20070827150608:Haar Filters
//@+node:aleator.20070130144337:Statistics along a line
#define SWAP(a,b) { \
        int c = (a);    \
        (a) = (b);      \
        (b) = c;        \
    }


double average_of_line(int x0, int y0
                     ,int x1, int y1
                     ,IplImage *src) {
     int steep = abs(y1 - y0) > abs(x1 - x0);
     int deltax=0; int deltay=0;
     int error=0;
     int ystep=0;
     int x=0; int y=0;
     float sum=0; int len=0;

     if (steep)   { SWAP(x0, y0); SWAP(x1, y1); }
     if (x0 > x1) { SWAP(x0, x1); SWAP(y0, y1); }
     deltax = x1 - x0;
     deltay = abs(y1 - y0);
     error  = 0;
     y = y0;
     if (y0 < y1) {ystep = 1;} else {ystep = -1;}
     for (x=x0; x<x1; ++x) {
         if (steep) {sum+=blurGet2D(src,y,x);
                     ++len;} 
                    // _plot(y,x);} 
         else       {sum+=blurGet2D(src,x,y);
                     ++len; } 
                    //_plot(x,y);}
         error = error + deltay;
         if (2*error >= deltax) {
             y = y + ystep;
             error = error - deltax; }
         }
         return (sum/len);
}

//@-node:aleator.20070130144337:Statistics along a line
//@+node:aleator.20051130130836:Taking square roots of images

void sqrtImage(IplImage *src,IplImage *dst)
{
int i;int j;
double result;
CvSize size = cvGetSize(src);

for(i=0;i<size.width;++i)
 for(j=0;j<size.height;++j)
    {
     result = cvSqrt(cvGetReal2D(src,j,i));
     cvSetReal2D(dst,j,i,result);
    }
}
//@-node:aleator.20051130130836:Taking square roots of images
//@+node:aleator.20050930104348:Histogram Features
#define HISTOGRAMSIZE 10

double calculateMoment(int i,double arr[], int l)
{
int j=0;
double result = 0;
for(j=0; j<l; j++)
  {  result += pow((j*1.0)/HISTOGRAMSIZE,i)*arr[j]; }
return result;
}

double calculateAbsCentralMoment(int i,double arr[], int l)
{
int j=0;
double m1 = calculateMoment(1,arr,l);
double result = 0;
for(j=0; j<l; j++)
 {
  result += pow(fabs(((j*1.0)/HISTOGRAMSIZE)-m1),i)*arr[j];}
return result;
}

double calculateCentralMoment(int i,double arr[], int l)
{
int j=0;
double m1 = calculateMoment(1,arr, l);
double result = 0;
for(j=0; j<l; j++)
 {  
    result += pow(((j*1.0)/HISTOGRAMSIZE)-m1,i)*arr[j];

 }
return result;
}
//@+node:aleator.20050930104348.1:Central Moments

IplImage* getNthCentralMoment(IplImage *src,int n, int w, int h)
{

CvSize size = cvGetSize(src);
int iw = size.width-w;
int ih = size.height-h;
IplImage *target = wrapCreateImage32F(iw,ih,1);
int x = 0;
int y = 0;
int i = 0;
int j = 0;
double histogram[HISTOGRAMSIZE]; 
for (x=0; x<ih; x++)
 for (y=0; y<iw; y++)
 {
memset(histogram,0,HISTOGRAMSIZE*sizeof(double));
double result = 0;

// Calculate the local histogram
for (i=0; i<w; i++)
 for (j=0; j<h; j++)
    {
     int slot = HISTOGRAMSIZE*cvGet2D(src,x+i,y+j).val[0];
     histogram[slot] += 1.0/(w*h*1.0);
    }


result = calculateCentralMoment(n,histogram,HISTOGRAMSIZE); 
cvSet2D(target,x,y,cvScalarAll(result));
 }

return target;
}

IplImage* getNthAbsCentralMoment(IplImage *src,int n, int w, int h)
{

CvSize size = cvGetSize(src);
int iw = size.width-w;
int ih = size.height-h;
IplImage *target = wrapCreateImage32F(iw,ih,1);
int x = 0;
int y = 0;
int i = 0;
int j = 0;
double histogram[HISTOGRAMSIZE]; 
for (x=0; x<ih; x++)
 for (y=0; y<iw; y++)
 {
memset(histogram,0,HISTOGRAMSIZE*sizeof(double));
double result = 0;

// Calculate the local histogram
for (i=0; i<w; i++)
 for (j=0; j<h; j++)
    {
     int slot = HISTOGRAMSIZE*cvGet2D(src,x+i,y+j).val[0];
     histogram[slot] += 1.0/(w*h*1.0);
    }


result = calculateAbsCentralMoment(n,histogram,HISTOGRAMSIZE); 
cvSet2D(target,x,y,cvScalarAll(result));
 }

return target;
}

IplImage* getNthMoment(IplImage *src,int n, int w, int h)
{

CvSize size = cvGetSize(src);
int iw = size.width-w;
int ih = size.height-h;
IplImage *target = wrapCreateImage32F(iw,ih,1);
int x = 0;
int y = 0;
int i = 0;
int j = 0;
double histogram[HISTOGRAMSIZE]; 
for (x=0; x<ih; x++)
 for (y=0; y<iw; y++)
 {
memset(histogram,0,HISTOGRAMSIZE*sizeof(double));
double result = 0;

// Calculate the local histogram
for (i=0; i<w; i++)
 for (j=0; j<h; j++)
    {
     int slot = HISTOGRAMSIZE*cvGet2D(src,x+i,y+j).val[0];
     histogram[slot] += 1.0/(w*h*1.0);
    }

result = calculateMoment(n,histogram,HISTOGRAMSIZE); 
cvSet2D(target,x,y,cvScalarAll(result));
 }

return target;
}

//@-node:aleator.20050930104348.1:Central Moments
//@+node:aleator.20051103110155:SMAB
   
// Perform second moment adaptive binarization for a single pixel `x`
// using given histogram.
double max(double x,double y) {if (x<y) return y; else return x;}
double min(double x,double y) {if (x>y) return y; else return x;}
int SMABx(double x, CvHistogram *h,int binCount,double t)
{
 int binnedX;  double leftSM=0;
 double rightSM=0;
 int i=0;
 binnedX = round(min(1,max(x,0))*(binCount-1));

 // Calculate left second moment:
 for(i=0; i<binnedX; i++)
  { leftSM += pow(x - ((1.0*i)/(1.0*binCount)),2) * getHistValue(h,i); }
 for(i=binnedX; i<binCount; i++)
  { rightSM += pow(x - ((1.0*i)/(1.0*binCount)),2) * getHistValue(h,i); }
 return (leftSM - (rightSM * t));
}

// Perform SMAB for image
void smb(IplImage *image,double t)
{
int i;int j;
double result;
CvSize size = cvGetSize(image);
CvHistogram *h = calculateHistogram(image,255);
for(i=0;i<size.width;++i)
 for(j=0;j<size.height;++j)
 {
     result = SMABx(cvGet2D(image,j,i).val[0],h,255,t);
     cvSet2D(image,j,i,cvScalarAll(result));
 }
}

void smab(IplImage *image,int w, int h,double t)
{
int i;int j;
int wi;int wj;
double result;
CvHistogram *histogram;
CvRect roi;
CvSize size = cvGetSize(image);
roi.width = w;
roi.height = h;

for(i=0;i<size.width;++i)
 for(j=0;j<size.height;++j)
 {
     roi.x = i-(w/2);
     roi.y = j-(h/2);
     cvSetImageROI(image,roi);
     histogram = calculateHistogram(image,50);
     cvResetImageROI(image);
     result = SMABx(cvGetReal2D(image,j,i),histogram,50,t);
     cvReleaseHist(&histogram);
     cvSet2D(image,j,i,cvScalarAll(result));
 }
}
//@-node:aleator.20051103110155:SMAB
//@+node:aleator.20051108093248:Skewness

//@-node:aleator.20051108093248:Skewness
//@-node:aleator.20050930104348:Histogram Features
//@+node:aleator.20050926095227:Susan
/* 
 Susan (Smallest Univalue Segmenting Nucleus) is 
 family of image processing methods, including
 edge preserving noise reduction.
*/
//@+node:aleator.20050926095227.1:Susan Smoothing Function

/* 
 Calculate susan smoothing for `src` around `x`,`y` coordinates.
 `t` determines brightness treshold and sigma controls scale of
 spatial smoothing. `w` and `h` determine window size.
*/

inline double calcSusanSmooth(IplImage* src, int x, int y
                      ,double t,double sigma,int w, int h)
{

int i = 0;
int j = 0;

long double numerator = 0;
long double denominator = 0;
for (i = 0; i<w; i++)
 for (j = 0; j<h; j++)
    {
    if (i==w/2 && j==h/2) continue;
    double r2 = i*i+j*j;
    double expFrac = (cvGet2D(src,x+i,y+j).val[0] 
                     - cvGet2D(src,x,y).val[0]);
    expFrac *= expFrac;

    double exponential = exp(  (-r2/(2*sigma*sigma)) - (expFrac/(t*t))  );

    numerator   += cvGet2D(src,x+i,y+j).val[0] * exponential;
    denominator += exponential;
    }
return numerator/denominator;
}
//@-node:aleator.20050926095227.1:Susan Smoothing Function
//@+node:aleator.20050926100856:Susan Smoothing

IplImage* susanSmooth(IplImage *src, int w, int h
                     ,double t, double sigma)

{

CvSize size = cvGetSize(src);
int iw = size.width-w;
int ih = size.height-h;
IplImage *target = wrapCreateImage32F(iw,ih,1);
int x = 0;
int y = 0;
double result = 0;


for (x=0; x<iw; x++)
 for (y=0; y<ih; y++)
 {
  result = calcSusanSmooth(src,y,x,t,sigma,h,w);
  cvSet2D(target,y,x,cvScalarAll(result));
 }
return target;
}
//@-node:aleator.20050926100856:Susan Smoothing
//@+node:aleator.20050927083244:Susan Edge
/* 
 Susan Edge Detector.
*/

// susan threshold function
inline double susanC(double r, double r0,double t)
{
 return exp(-((r-r0)/t));
}

inline double susanValue(IplImage *src,int x, int y
                        ,int w, int h, double t)
{
int i; int j;
double geometricTreshold = (3*(w*h)) / 4;
double sum = 0;

for (i = 0; i<w; i++)
 for (j = 0; j<h; j++)
    {
     sum += susanC(cvGet2D(src,x+i,y+j).val[0]
                  ,cvGet2D(src,x,y).val[0]
                  ,t);
    }
if (sum < geometricTreshold)
    return geometricTreshold - sum;
else return 0;
}

IplImage* susanEdge(IplImage *src,int w,int h,double t)
{
CvSize size = cvGetSize(src);
int iw = size.width-w;
int ih = size.height-h;
IplImage *target = wrapCreateImage32F(iw,ih,1);
int x = 0;
int y = 0;
double result = 0;


for (x=0; x<iw; x++)
 for (y=0; y<ih; y++)
 {
  result = susanValue(src,y,x,h,w,t);
  cvSet2D(target,y,x,cvScalarAll(result));
 }
return target;

}
//@-node:aleator.20050927083244:Susan Edge
//@-node:aleator.20050926095227:Susan
//@+node:aleator.20050908112008:Gabors
/* 
 Gabor functions are modulated gaussians which bear some resemblance
 to human visual cortex neurons. */
//@+node:aleator.20050908104238:gabor function in C
/* This function calculates value of simple gabor function
  at given x,y coordinates. Parameters for the gabor are:
  
  stdX  - standard deviation in oscillation direction
  stdY  - standard deviation tangential to stdX
  theta - angle (in radians) of the gabor
  phase - phase of the gabor


*/
double calcGabor(double x, double y
                ,double stdX, double stdY
                ,double theta, double phase
                ,double cycles)
{
 double xth = x*cos(theta)  - y*sin(theta);
 double yth = x*sin(theta)  + y*cos(theta);
 double oscillationPart = cos(2*M_PI*xth/cycles+phase);
 double gaussianPart    = exp((-0.5*xth*xth)/(stdX*stdX))
                         *exp((-0.5*yth*yth)/(stdY*stdY));
 
 return gaussianPart * oscillationPart;
}

double calc1DGabor(double x
                ,double sigma
                ,double phase, double center
                ,double cycles)
{
 double oscillationPart = cos(2*M_PI*(x-center)/cycles+phase);
 double gaussianPart    = exp((-0.5*(x-center)*(x-center))
                              /(sigma*sigma));
 
 return gaussianPart * oscillationPart;
}


//@-node:aleator.20050908104238:gabor function in C
//@+node:aleator.20050908112116:rendering gabors to arrays
void renderGabor(CvArr *dst,int width, int height
                ,double dx, double dy
                ,double stdX, double stdY
                ,double theta, double phase
                ,double cycles)

{
 int i,j;
 int mx = width/2;
 int my = height/2;
 for (i=0; i<width; i++)
  for (j=0; j<height; j++) // TODO: This might be a bug
   cvSet2D(dst,i,j,cvScalarAll(calcGabor(i-dx,j-dy,stdX,stdY
                                        ,theta,phase,cycles)));
}

void render_gaussian(IplImage *dst
                   ,double stdX, double stdY)

{
 int i,j;
 double distX;
 double distY;
 CvSize size = cvGetSize(dst);
 double centerX = size.width/2.0;
 double centerY = size.height/2.0;
 for (i=0; i<size.width-1; i++)
  for (j=0; j<size.height-1; j++)
   { distX = ((centerX-i*1.0)*(centerX-i*1.0)) / (2*stdX*stdX);
     distY = ((centerY-j*1.0)*(centerY-j*1.0)) / (2*stdY*stdY);
  //   printf("w: %d, h: %d, i: %d, j:%d,dx: %e,dy: %e,exp:%e\n",size.width,size.height,i,j,distX,distY,exp(-distX-distY));
     fflush(stdout);
     cvSet2D(dst,j,i,cvScalarAll( exp(-distX-distY) ));
  }
}


void renderRadialGabor(CvArr *dst,int width, int height
                ,double sigma
                ,double phase, double center
                ,double cycles)

{
 int i,j;
 int mx = width/2;
 int my = height/2;
 double rad = 0;
 for (i=0; i<width; i++)
  for (j=0; j<width; j++)
   {
   rad = sqrt((i-mx)*(i-mx)+(j-my)*(j-my));
   cvSet2D(dst,i,j,cvScalarAll(calc1DGabor(rad,sigma
                                          ,phase,center,cycles)));
   }
}

void wrapMinMaxLoc(const IplImage* target, int* minx, int* miny, int* maxx, int* maxy, double *minval, double *maxval)
{
	CvPoint maxPoint;
	CvPoint minPoint;
	cvMinMaxLoc(target,minval,maxval,&minPoint, &maxPoint, NULL);
    *maxx = maxPoint.x ;
    *maxy = maxPoint.y ;
    *minx = minPoint.x ;
    *miny = minPoint.y ;
} 

void simpleMatchTemplate(const IplImage* target, const IplImage* template, int* x, int* y, double *val,int type)
{
	int rw = cvGetSize(target).width-cvGetSize(template).width+1;
	int rh = cvGetSize(target).height-cvGetSize(template).height+1;
	IplImage* result = wrapCreateImage32F(rw,rh,1);
	cvMatchTemplate(target,template,result,type);
	double min,max;
	CvPoint maxPoint;
	maxPoint.x=-1;
	maxPoint.y=-1;
	min =0;
	max =0;
	cvMinMaxLoc(result,&min,&max,NULL, &maxPoint, NULL);
	*x = maxPoint.x;+rw/2;
	*y = maxPoint.y;+rh/2;
	*val = max;
	cvReleaseImage(&result);
	} 

IplImage* templateImage(const IplImage* target, const IplImage* template)
{
	int rw = cvGetSize(target).width-cvGetSize(template).width+1;
	int rh = cvGetSize(target).height-cvGetSize(template).height+1;
	IplImage* result = wrapCreateImage32F(rw,rh,1);
	cvMatchTemplate(target,template,result,CV_TM_CCORR);
	return result;
	} 


//@-node:aleator.20050908112116:rendering gabors to arrays
//@+node:aleator.20050908101148:gabor filter using cvFilter2D
void gaborFilter(const CvArr *src, CvArr *dst
                ,int maskWidth, int maskHeight
                ,double stdX, double stdY
                ,double theta,double phase
                ,double cycles)

{
 int mx = maskWidth/2;
 int my = maskHeight/2;
 CvMat *kernel = cvCreateMat(maskWidth,maskHeight,CV_32F);
 renderGabor(kernel,maskWidth,maskHeight,mx,my,stdX,stdY
             ,theta,phase,cycles);
 cvFilter2D(src,dst,kernel,cvPoint(-1,-1));
}

void radialGaborFilter(const CvArr *src, CvArr *dst
                ,int maskWidth, int maskHeight
                ,double sigma
                ,double phase,double center
                ,double cycles)

{
 CvMat *kernel = cvCreateMat(maskWidth,maskHeight,CV_32F);
 renderRadialGabor(kernel,maskWidth,maskHeight,sigma
               ,phase,center,cycles);
 cvFilter2D(src,dst,kernel,cvPoint(-1,-1));
}


//@-node:aleator.20050908101148:gabor filter using cvFilter2D
//@-node:aleator.20050908112008:Gabors
//@+node:aleator.20070511142414:Adaboost Learning
// This doesn't really work properly yet.. No
// time to do anything about it really.
//@nonl
//@+node:aleator.20070511142414.1:Fitness
// In the following the class is encoded bit
// differently. 0 is one class and +1 is another.
// if target is gray it is considered null area.

double adaFitness1(IplImage *target
                 ,IplImage *weigths
                 ,IplImage *test)
{
CvSize size = cvGetSize(target);
int i,j;
int width  = size.width;
int height = size.height;
double result=0;
double tij=0,wij=0,testij=0,rij=0; 
for (i=0; i<width; i++)
 for (j=0; j<height; j++)
 { 
   tij = cvGetReal2D(target,j,i);
   wij = cvGetReal2D(weigths,j,i);
   testij = cvGetReal2D(test,j,i);
   rij=wij;
   if (((tij < 0.2) && (testij < 0.2)) || ((tij > 0.8) && (testij > 0.8))) 
    {rij=0;} 
   result += rij;
 }
 
return result;
}
//@-node:aleator.20070511142414.1:Fitness
//@+node:aleator.20070511145251:Updating distributions

// This function is used to update distribution.
// Notice that alpha_t must be calculated separately
// and normalization is not applied.
IplImage* adaUpdateDistrImage(IplImage *target
                          ,IplImage *weigths
                          ,IplImage *test
                          ,double at)
{
CvSize size = cvGetSize(target);
int i,j;
int width  = size.width;
int height = size.height;
double tij=0,wij=0,testij=0,rij=0; 
IplImage *result = wrapCreateImage32F(width,height,1);
for (i=0; i<width; i++)
 for (j=0; j<height; j++)
 { 
   tij = cvGetReal2D(target,j,i);
   wij = cvGetReal2D(weigths,j,i);
   testij = cvGetReal2D(test,j,i);
   if ( (tij>0.2) && (tij<0.8) ) continue;
   if (((tij < 0.2) && (testij < 0.2)) 
      || ((tij > 0.8) && (testij > 0.8))) 
    {rij = wij*exp(-at);
     cvSetReal2D(result,j,i,rij); }
   else 
    {rij = wij*exp(at);
     cvSetReal2D(result,j,i,rij); }
 }
 
return result;
}

//@-node:aleator.20070511145251:Updating distributions
//@-node:aleator.20070511142414:Adaboost Learning
//@+node:aleator.20051207074905:LBP

void get_weighted_histogram(IplImage *src, IplImage *weights, 
                       double start, double end, 
                       int bins, double *histo)
{
  int i,j,index;
  double value,weight;
  CvSize imageSize = cvGetSize(src);  
  for(i=0;i<bins;++i) histo[i]=0;
  for(i=0; i<imageSize.width-1; ++i)
    for(j=0; j<imageSize.height-1; ++j)
        {
         value = cvGetReal2D(src,j,i);
         weight = cvGetReal2D(weights,j,i);
         index = floor(bins*((value - start)/(end - start)));
         //printf("Adding weight %e to index %d\n",weight,index);
         if (index<0 || index>=bins) continue;
         histo[index] += weight;
        }
  
}

// Calculate local binary pattern for image. 
// LBP is outgoing array
// of (preallocated) 256 bytes that are assumed to be 0.
void localBinaryPattern(IplImage *src, int *LBP)
{
  int i,j;
  int pattern = 0;
  double center = 0;
  CvSize imageSize = cvGetSize(src);  
  for(i=1; i<imageSize.width-1; ++i)
    for(j=1; j<imageSize.height-1; ++j)
        {
         center = cvGetReal2D(src,j,i);

         pattern += (blurGet2D(src,i-1,j-1) > center) *1;
         pattern += (blurGet2D(src,i,j-1)   > center) *2;
         pattern += (blurGet2D(src,i+1,j-1) > center) *4;
         
         pattern += (blurGet2D(src,i-1,j)   > center) *8;
         pattern += (blurGet2D(src,i+1,j)   > center) *16;
         
         pattern += (blurGet2D(src,i-1,j+1) > center) *32;
         pattern += (blurGet2D(src,i,j+1)   > center) *64;
         pattern += (blurGet2D(src,i+1,j+1) > center) *128;
         LBP[pattern]++;
         pattern = 0;
        }
}

void localBinaryPattern3(IplImage *src, int *LBP)
{
  int i,j;
  int pattern = 0;
  double center = 0;
  CvSize imageSize = cvGetSize(src);  
  for(i=1; i<imageSize.width-1; ++i)
    for(j=1; j<imageSize.height-1; ++j)
        {
         center = cvGetReal2D(src,j,i);

         pattern += (blurGet2D(src,i-2,j-2) > center) *1;
         pattern += (blurGet2D(src,i,j-3)   > center) *2;
         pattern += (blurGet2D(src,i+2,j-2) > center) *4;
         
         pattern += (blurGet2D(src,i-3,j)   > center) *8;
         pattern += (blurGet2D(src,i+3,j)   > center) *16;
         
         pattern += (blurGet2D(src,i-2,j+2) > center) *32;
         pattern += (blurGet2D(src,i,j+3)   > center) *64;
         pattern += (blurGet2D(src,i+2,j+2) > center) *128;
         LBP[pattern]++;
         pattern = 0;
        }
}
void localBinaryPattern5(IplImage *src, int *LBP)
{
  int i,j;
  int pattern = 0;
  double center = 0;
  CvSize imageSize = cvGetSize(src);  
  for(i=1; i<imageSize.width-1; ++i)
    for(j=1; j<imageSize.height-1; ++j)
        {
         center = cvGetReal2D(src,j,i);

         pattern += (blurGet2D(src,i-4,j-4) > center) *1;
         pattern += (blurGet2D(src,i,j-5)   > center) *2;
         pattern += (blurGet2D(src,i+4,j-4) > center) *4;
         
         pattern += (blurGet2D(src,i-5,j)   > center) *8;
         pattern += (blurGet2D(src,i+5,j)   > center) *16;
         
         pattern += (blurGet2D(src,i-4,j+4) > center) *32;
         pattern += (blurGet2D(src,i,j+5)   > center) *64;
         pattern += (blurGet2D(src,i+4,j+4) > center) *128;
         LBP[pattern]++;
         pattern = 0;
        }
}

void weighted_localBinaryPattern(IplImage *src,int offsetX,int offsetXY
                                , IplImage* weights, double *LBP)
{
  int i,j;
  int pattern = 0;
  double center = 0;
  double weight = 0;
  CvSize imageSize = cvGetSize(src);  
  for(i=1; i<imageSize.width-1; ++i)
    for(j=1; j<imageSize.height-1; ++j)
        {
         center = cvGetReal2D(src,j,i);
         weight = cvGetReal2D(weights,j,i);

         pattern += (blurGet2D(src,i-offsetXY,j-offsetXY) > center) *1;
         pattern += (blurGet2D(src,i,j-offsetX)   > center) *2;
         pattern += (blurGet2D(src,i+offsetXY,j-offsetXY) > center) *4;
         
         pattern += (blurGet2D(src,i-offsetX,j)   > center) *8;
         pattern += (blurGet2D(src,i+offsetX,j)   > center) *16;
         
         pattern += (blurGet2D(src,i-offsetXY,j+offsetXY) > center) *32;
         pattern += (blurGet2D(src,i,j+offsetX)   > center) *64;
         pattern += (blurGet2D(src,i+offsetXY,j+offsetXY) > center) *128;
         LBP[pattern] += weight;
         pattern = 0;
        }
}

void localHorizontalBinaryPattern(IplImage *src, int *LBP)
{
  int i,j;
  int pattern = 0;
  double center = 0;
  CvSize imageSize = cvGetSize(src);  
  for(i=0; i<imageSize.width-1; ++i)
    for(j=0; j<imageSize.height-1; ++j)
        {
         center = cvGetReal2D(src,j,i);

         pattern += (blurGet2D(src,i-4,j) > center) *1;
         pattern += (blurGet2D(src,i-3,j) > center) *2;
         pattern += (blurGet2D(src,i-2,j) > center) *4;
         pattern += (blurGet2D(src,i-1,j) > center) *8;
         pattern += (blurGet2D(src,i+1,j) > center) *16;
         pattern += (blurGet2D(src,i+2,j) > center) *32;
         pattern += (blurGet2D(src,i+3,j) > center) *64;
         pattern += (blurGet2D(src,i+4,j) > center) *128;
         LBP[pattern]++;
         pattern = 0;
        }
}

void localVerticalBinaryPattern(IplImage *src, int *LBP)
{
  int i,j;
  int pattern = 0;
  double center = 0;
  CvSize imageSize = cvGetSize(src);  
  for(i=0; i<imageSize.width-1; ++i)
    for(j=0; j<imageSize.height-1; ++j)
        {
         center = cvGetReal2D(src,j,i);

         pattern += (blurGet2D(src,i,j-4) > center) *1;
         pattern += (blurGet2D(src,i,j-3) > center) *2;
         pattern += (blurGet2D(src,i,j-2) > center) *4;
         pattern += (blurGet2D(src,i,j-1) > center) *8;
         pattern += (blurGet2D(src,i,j+1) > center) *16;
         pattern += (blurGet2D(src,i,j+2) > center) *32;
         pattern += (blurGet2D(src,i,j+3) > center) *64;
         pattern += (blurGet2D(src,i,j+4) > center) *128;
         LBP[pattern]++;
         pattern = 0;
        }
}


//@-node:aleator.20051207074905:LBP
//@+node:aleator.20051109102750:Selective Average
// Assuming grayscale image calculate local selective average of point x y 
inline double calcSelectiveAvg(IplImage *img,double t
                                   ,int x, int y
                                   ,int wwidth, int wheight)
{
int i,j;
double accum=0; 
double count=0;
double centerValue; double processed=0;
CvSize size = cvGetSize(img);
centerValue = blurGet2D(img,x,y);

for (i=-wwidth; i<wwidth;++i)
 for (j=-wheight; j<wheight;++j)
  { 
   if (  x+i<0 || x+i>=size.width
      || y+j<0 || y+j>=size.height)
      continue;
      
   processed = blurGet2D(img,x+i,y+j);
   if (fabs(processed-centerValue)<t)
        {accum+=processed;++count;}
  }
return accum/count;
}


IplImage* selectiveAvgFilter(IplImage *src,double t
                            ,int wwidth, int wheight)
{
CvSize size = cvGetSize(src);
int i,j;
int width  = size.width;
int height = size.height;
double result;

IplImage *target = wrapCreateImage32F(width,height,1);
for (i=0; i<width; i++)
 for (j=0; j<height; j++)
 {
  result = calcSelectiveAvg(src,t,i,j,wwidth,wheight);
  cvSetReal2D(target,j,i,result);
 }
 
return target;
}

//@-node:aleator.20051109102750:Selective Average
//@+node:aleator.20060104154125:AcquireImage

// Copy array into single channel iplImage
IplImage *acquireImageSlow(int w, int h, double *d)
{
 IplImage *img;
 int i,j;
 img = cvCreateImage(cvSize(w,h), IPL_DEPTH_32F,1);
 for (i=0; i<h; i++) {
   for (j=0; j<w; j++) { 
         //printf("(%d,%d) => %d is %f\n",j,i,(i+j*h),d[i+j*h]);
         FGET(img,j,i) = d[j*h+i]; 
         }
    }
 return img;
}

IplImage *acquireImageSlowF(int w, int h, float *d)
{
 IplImage *img;
 int i,j;
 img = cvCreateImage(cvSize(w,h), IPL_DEPTH_32F,1);
 for (i=0; i<h; i++) {
   for (j=0; j<w; j++) { 
         //printf("(%d,%d) => %d is %f\n",j,i,(i+j*h),d[i+j*h]);
         FGET(img,j,i) = d[j*h+i]; 
         }
    }
 return img;
}

IplImage *acquireImageSlowComplex(int w, int h, complex double *d)
{
 IplImage *img;
 int i,j;
 img = cvCreateImage(cvSize(w,h), IPL_DEPTH_32F,1);
 for (i=0; i<h; i++) {
   for (j=0; j<w; j++) { 
         FGET(img,j,i) = (float)(creal(d[j*h+i])); 
         }
    }
 return img;
}

void exportImageSlowComplex(IplImage *img, complex double *d)
{
 int i,j;
 CvSize s= cvGetSize(img);
 for (i=0; i<s.height; i++) {
   for (j=0; j<s.width; j++) { 
         d[j*s.height+i] = (complex float)(FGET(img,j,i) + 0*I); 
         }
    }
}

void exportImageSlow(IplImage *img, double *d)
{
 int i,j;
 CvSize s= cvGetSize(img);
 for (i=0; i<s.height; i++) {
   for (j=0; j<s.width; j++) { 
         d[j*s.height+i] = FGET(img,j,i); 
         }
    }
}
//@-node:aleator.20060104154125:AcquireImage
//@-node:aleator.20050908100314.3:Utilities
//@+node:aleator.20060413093124:Connected components
//@+node:aleator.20071016114634:Contours


void free_found_contours(FoundContours *f)
{
 cvReleaseMemStorage(&(f->storage));
 free(f);
 
}

int reset_contour(FoundContours *f)
{ 
 f->contour = f->start;
}

int cur_contour_size(FoundContours *f)
{ 
 return f->contour->total;
}

double contour_area(FoundContours *f)
{ 
 return cvContourArea(f->contour,CV_WHOLE_SEQ,0);
}

CvMoments* contour_moments(FoundContours *f)
{ 
 CvMoments* moments = (CvMoments*) malloc(sizeof(CvMoments));
 cvMoments(f->contour,moments,0);
 return moments;
}

double contour_perimeter(FoundContours *f)
{ 
 return cvContourPerimeter(f->contour);
}

int more_contours(FoundContours *f)
{ 
 if (f->contour != 0)
  {return 1;}
  {return 0;} // no more contours
}

int next_contour(FoundContours *f)
{ 
 if (f->contour != 0)
  {f->contour = f->contour->h_next; return 1;}
  {return 0;} // no more contours
}

void contour_points(FoundContours *f, int *xs, int *ys)
{
 if (f->contour==0) {printf("unavailable contour\n"); exit(1);}
 
 CvPoint *pt=0;
 int total,i=0;
 total = f->contour->total;
 for (i=0; i<total;i++) 
  {
   pt = (CvPoint*)cvGetSeqElem(f->contour,i);
   if (pt==0) {printf("point out of contour\n"); exit(1);}
   xs[i] = pt->x;
   ys[i] = pt->y;
  } 
    
}

void print_contour(FoundContours *fc)
{
  int i=0;
  CvPoint *pt=0;
   for (i=0; i<fc->contour->total;++i) 
    {
     pt = (CvPoint*)cvGetSeqElem(fc->contour,i);
     printf("PT=%d,%d\n",pt->x,pt->y);
    }
}

/* void draw_contour(FoundContours *fc,double color
                 , IplImage *img, IplImage *dst)
{
 cvDrawContours( dst, fc->start, color, color, -1, 0, 8
               , cvPoint(0,0));
} */


FoundContours* get_contours(IplImage *src1)
{
 CvSize size;
 IplImage *src = ensure8U(src1);
 //int dstDepth = IPL_DEPTH_8U;
 //size = cvGetSize(src1);
 //src = cvCreateImage(size,dstDepth,1);
 //cvCopy(src1,src,NULL);
 
 
 CvPoint* pt=0;
 int i=0;
 
 CvMemStorage *storage=0;
 CvSeq *contour=0;
 FoundContours* result = (FoundContours*)malloc(sizeof(FoundContours));
 storage = cvCreateMemStorage(0);
       
 cvFindContours( src,storage
               , &contour
               , sizeof(CvContour) 
               ,CV_RETR_EXTERNAL 
            //,CV_RETR_CCOMP 
               ,CV_CHAIN_APPROX_NONE
               ,cvPoint(0,0) );

// result->contour = cvApproxPoly( result->contour, sizeof(CvContour)
//                                , result->storage, CV_POLY_APPROX_DP
//                                , 3, 1 );
 result->start = contour;
 result->contour = contour;
 result->storage = storage;

 cvReleaseImage(&src);
 return result;
    
 }
//@-node:aleator.20071016114634:Contours
//@+node:aleator.20070814123008:moments
CvMoments* getMoments(IplImage *src, int isBinary)
{
 CvMoments* moments = (CvMoments*) malloc(sizeof(CvMoments));
 cvMoments( src, moments, isBinary);
 return moments;
}

void freeCvMoments(CvMoments *x)
{
 free(x);
}


void getHuMoments(CvMoments *src,double *hu)
{
 CvHuMoments* hu_moments = (CvHuMoments*) malloc(sizeof(CvHuMoments));
 cvGetHuMoments( src, hu_moments);
 *hu = hu_moments->hu1; ++hu;
 *hu = hu_moments->hu2; ++hu;
 *hu = hu_moments->hu3; ++hu;
 *hu = hu_moments->hu4; ++hu;
 *hu = hu_moments->hu5; ++hu;
 *hu = hu_moments->hu6; ++hu;
 *hu = hu_moments->hu7; 
 return;
}

void freeCvHuMoments(CvHuMoments *x)
{
 free(x);
}
//@-node:aleator.20070814123008:moments
//@+node:aleator.20060727102514:blobCount
int blobCount(IplImage *src)
{
    int contourCount=0;
    CvMemStorage* storage = cvCreateMemStorage(0);
    CvSeq* contour = 0;

    contourCount = cvFindContours( src, storage, &contour, sizeof(CvContour), CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE, cvPoint(0,0) );

    cvReleaseMemStorage(&storage);
    return contourCount;
}

//@-node:aleator.20060727102514:blobCount
//@+node:aleator.20060413093124.1:sizeFilter
IplImage* sizeFilter(IplImage *src, double minSize, double maxSize)
{
    IplImage* dst = cvCreateImage( cvGetSize(src), IPL_DEPTH_32F, 1 );
    CvMemStorage* storage = cvCreateMemStorage(0);
    CvSeq* contour = 0;

    cvFindContours( src, storage, &contour, sizeof(CvContour), CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE, cvPoint(0,0) );
    cvZero( dst );

    for( ; contour != 0; contour = contour->h_next )
    {
        double area=fabs(cvContourArea(contour,CV_WHOLE_SEQ,0));
        if (area <=minSize || area >= maxSize) continue;
        CvScalar color = cvScalar(1,1,1,1);
        cvDrawContours( dst, contour, color, color, -1, CV_FILLED, 8,
            cvPoint(0,0));
    }
    cvReleaseMemStorage(&storage);
    return dst;
}
//@-node:aleator.20060413093124.1:sizeFilter
//@-node:aleator.20060413093124:Connected components
//@+node:aleator.20050908101148.1:function for rotating image
IplImage* rotateImage(IplImage* src,double scale,double angle)
{

  IplImage* dst = cvCloneImage( src );
  angle = angle * (180 / CV_PI);
  int w = src->width;
  int h = src->height;
  CvMat *M;
  M = cvCreateMat(2,3,CV_32FC1);
  CvPoint2D32f center = cvPoint2D32f(w/2.0,h/2.0);
  CvMat *N = cv2DRotationMatrix(center,angle,scale,M);
  cvWarpAffine( src, dst, N, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS
              , cvScalarAll(0)); 
  return dst;
  cvReleaseMat(&M);
}


inline double cubicInterpolate(
   double y0,double y1,
   double y2,double y3,
   double mu)
{
   double a0,a1,a2,a3,mu2;

   mu2 = mu*mu;
   a0 = y3 - y2 - y0 + y1;
   a1 = y0 - y1 - a0;
   a2 = y2 - y0;
   a3 = y1;
   return(a0*mu*mu2+a1*mu2+a2*mu+a3);
}

double bilinearInterp(IplImage *tex, double u, double v) {
   CvSize s = cvGetSize(tex);
   int x = floor(u);
   int y = floor(v);
   double u_ratio = u - x;
   double v_ratio = v - y;
   double u_opposite = 1 - u_ratio;
   double v_opposite = 1 - v_ratio;
   double result = ((x+1 >= s.width) || (y+1 >= s.height)) ? FGET(tex,x,y) :
                   (FGET(tex,x,y)   * u_opposite  + FGET(tex,x+1,y)   * u_ratio) * v_opposite + 
                   (FGET(tex,x,y+1) * u_opposite  + FGET(tex,x+1,y+1) * u_ratio) * v_ratio;
   return result;
 }

// TODO: Check boundaries! #SAFETY
double bicubicInterp(IplImage *tex, double u, double v) {
   CvSize s = cvGetSize(tex);
   int x = floor(u);
   int y = floor(v);
   double u_ratio = u - x;
   double v_ratio = v - y;
   double p[4][4] = {FGET(tex,x-1,y-1),  FGET(tex,x,y-1),  FGET(tex,x+1,y-1),  FGET(tex,x+2,y-1),
                     FGET(tex,x-1,y),    FGET(tex,x,y),    FGET(tex,x+1,y),    FGET(tex,x+2,y),
                     FGET(tex,x-1,y+1),  FGET(tex,x,y+1),  FGET(tex,x+1,y+1),  FGET(tex,x+2,y+1),
                     FGET(tex,x-1,y+2),  FGET(tex,x,y+2),  FGET(tex,x+1,y+2),  FGET(tex,x+2,y+2)
                     };
    double a00 = p[1][1];
	double a01 = -p[1][0] + p[1][2];
	double a02 = 2*p[1][0] - 2*p[1][1] + p[1][2] - p[1][3];
	double a03 = -p[1][0] + p[1][1] - p[1][2] + p[1][3];
	double a10 = -p[0][1] + p[2][1];
	double a11 = p[0][0] - p[0][2] - p[2][0] + p[2][2];
	double a12 = -2*p[0][0] + 2*p[0][1] - p[0][2] + p[0][3] + 2*p[2][0] - 2*p[2][1] 
                 + p[2][2] - p[2][3];
	double a13 = p[0][0] - p[0][1] + p[0][2] - p[0][3] - p[2][0] + p[2][1] - p[2][2] + p[2][3];
	double a20 = 2*p[0][1] - 2*p[1][1] + p[2][1] - p[3][1];
	double a21 = -2*p[0][0] + 2*p[0][2] + 2*p[1][0] - 2*p[1][2] - p[2][0] + p[2][2] 
                 + p[3][0] - p[3][2];
	double a22 = 4*p[0][0] - 4*p[0][1] + 2*p[0][2] - 2*p[0][3] - 4*p[1][0] + 4*p[1][1] 
                 - 2*p[1][2] + 2*p[1][3] + 2*p[2][0] - 2*p[2][1] + p[2][2] - p[2][3] 
                 - 2*p[3][0] + 2*p[3][1] - p[3][2] + p[3][3];
	double a23 = -2*p[0][0] + 2*p[0][1] - 2*p[0][2] + 2*p[0][3] + 2*p[1][0] - 2*p[1][1] 
                 + 2*p[1][2] - 2*p[1][3] - p[2][0] + p[2][1] - p[2][2] + p[2][3] + p[3][0] 
                 - p[3][1] + p[3][2] - p[3][3];
	double a30 = -p[0][1] + p[1][1] - p[2][1] + p[3][1];
	double a31 = p[0][0] - p[0][2] - p[1][0] + p[1][2] + p[2][0] - p[2][2] - p[3][0] + p[3][2];
	double a32 = -2*p[0][0] + 2*p[0][1] - p[0][2] + p[0][3] + 2*p[1][0] - 2*p[1][1] 
                 + p[1][2] - p[1][3] - 2*p[2][0] + 2*p[2][1] - p[2][2] + p[2][3] + 2*p[3][0] 
                 - 2*p[3][1] + p[3][2] - p[3][3];
	double a33 = p[0][0] - p[0][1] + p[0][2] - p[0][3] - p[1][0] + p[1][1] - p[1][2] 
                 + p[1][3] + p[2][0] - p[2][1] + p[2][2] - p[2][3] - p[3][0] + p[3][1] 
                 - p[3][2] + p[3][3];

	double x2 = u_ratio * u_ratio;
	double x3 = x2 * u_ratio;
	double y2 = v_ratio * v_ratio;
	double y3 = y2 * v_ratio;

	return a00 + a01 * v_ratio + a02 * y2 + a03 * y3 +
	       a10 * u_ratio + a11 * u_ratio * v_ratio + a12 * u_ratio * y2 + a13 * u_ratio * y3 +
	       a20 * x2 + a21 * x2 * v_ratio + a22 * x2 * y2 + a23 * x2 * y3 +
	       a30 * x3 + a31 * x3 * v_ratio + a32 * x3 * y2 + a33 * x3 * y3;
 }

void radialRemap(IplImage *source, IplImage *dest, double k)
{
    int i,j;
    CvSize s = cvGetSize(dest);
    double x,y,cx,cy,nx,ny,r2;
    cx = s.width/2.0;
    cy = s.height/2.0;
    for (i=0; i<s.height; i++)
       for (j=0; j<s.width; j++) {
           nx = (j-cx)/s.width;
           ny = (i-cy)/s.height;
           r2 = nx*nx+ny*ny;
           nx = nx*(1+k*r2);
           ny = ny*(1+k*r2);
           x = (nx+0.5)*s.width;
           y = (ny+0.5)*s.height;
           if (x<0 || x>=s.width || y<0 || y>=s.height) 
            { FGET(dest,j,i) = 0; 
             continue;}
           FGET(dest,j,i) = bilinearInterp(source,x,y);
           }


}


//@-node:aleator.20050908101148.1:function for rotating image
//@+node:aleator.20051220091717:Matrix multiplication

void wrapMatMul(int w, int h, double *mat
               , double *vec, double *t)
{

CvMat matrix;
CvMat vector;
CvMat target;
cvInitMatHeader(&matrix,w,h,CV_64FC1,mat,CV_AUTOSTEP);
cvInitMatHeader(&vector,h,1,CV_64FC1,vec,CV_AUTOSTEP);
cvInitMatHeader(&target,w,1,CV_64FC1,t,CV_AUTOSTEP);
cvMatMul(&matrix,&vector,&target);
}

void maximal_covering_circle(int ox,int oy, double or, IplImage *distmap
                            ,int *max_x, int *max_y, double *max_r)
{
 double distance,radius;

 *max_x = ox;
 *max_y = oy;
 *max_r  = or;

 CvSize s = cvGetSize(distmap);
 for(int i=0; i<s.width; i++) // TODO: Limit with max_r
  for(int j=0; j<s.height; j++)
   {
    distance = sqrt((i-ox)*(i-ox) + (j-oy)*(j-oy));
    radius   = FGET(distmap,i,j);
    if (radius > *max_r && radius >= or+distance )
         { *max_x=i; *max_y=j; *max_r=radius;}
   }
}

double juliaF(double a, double b,double x, double y) {
     int limit = 1000;
     double complex z;
     int i=0;
     double complex c;
     double cr,ci;
     c = a + b*I;
     z = x+y*I;
     for (i=0;i<limit;i++)
        {
         cr=creal(z); ci=cimag(i);
         if (cr*cr+ci*ci>4) return (i*1.0)/limit;
         z=z*z+c;
        }
    return 0;
    }

CvVideoWriter* wrapCreateVideoWriter(char *fn, int fourcc,
                                     double fps,int w, int h,
                                     int color) 
 {
   CvVideoWriter *res = cvCreateVideoWriter(fn,CV_FOURCC('M','P','G','4'),fps,cvSize(w,h), color);
   return res;
 }

//@-node:aleator.20051220091717:Matrix multiplication
//@-all
//@-node:aleator.20050908100314:@thin cvWrapLEO.c
//@-leo
