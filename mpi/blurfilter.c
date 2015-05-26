/*
  File: blurfilter.c

  Implementation of blurfilter function.
    
 */
#include <stdio.h>
#include "blurfilter.h"
#include "ppmio.h"


pixel* pix(pixel* image, const int xx, const int yy, const int xsize)
{
  register int off = xsize*yy + xx;

#ifdef DBG
  if(off >= MAX_PIXELS) {
    fprintf(stderr, "\n Terribly wrong: %d %d %d\n",xx,yy,xsize);
  }
#endif
  return (image + off);
}

//nikni337, lab2
void blurfilter(const int xsize, const int ysize, pixel* src, const int radius, const double *w){
  int x,y,x2,y2, wi;
  double r,g,b,n, wc;
  pixel dst[MAX_PIXELS];


  for (y=0; y<ysize; y++) {
    for (x=0; x<xsize; x++) {
      r = w[0] * pix(src, x, y, xsize)->r;
      g = w[0] * pix(src, x, y, xsize)->g;
      b = w[0] * pix(src, x, y, xsize)->b;
      n = w[0];
      for ( wi=1; wi <= radius; wi++) {
	wc = w[wi];
	x2 = x - wi;
	if(x2 >= 0) {
	  r += wc * pix(src, x2, y, xsize)->r;
	  g += wc * pix(src, x2, y, xsize)->g;
	  b += wc * pix(src, x2, y, xsize)->b;
	  n += wc;
	}
	x2 = x + wi;
	if(x2 < xsize) {
	  r += wc * pix(src, x2, y, xsize)->r;
	  g += wc * pix(src, x2, y, xsize)->g;
	  b += wc * pix(src, x2, y, xsize)->b;
	  n += wc;
	}
      }
      pix(dst,x,y, xsize)->r = r/n;
      pix(dst,x,y, xsize)->g = g/n;
      pix(dst,x,y, xsize)->b = b/n;
    }
  }

  for (y=0; y<ysize; y++) {
    for (x=0; x<xsize; x++) {
      r = w[0] * pix(dst, x, y, xsize)->r;
      g = w[0] * pix(dst, x, y, xsize)->g;
      b = w[0] * pix(dst, x, y, xsize)->b;
      n = w[0];
      for ( wi=1; wi <= radius; wi++) {
	wc = w[wi];
	y2 = y - wi;
	if(y2 >= 0) {
	  r += wc * pix(dst, x, y2, xsize)->r;
	  g += wc * pix(dst, x, y2, xsize)->g;
	  b += wc * pix(dst, x, y2, xsize)->b;
	  n += wc;
	}
	y2 = y + wi;
	if(y2 < ysize) {
	  r += wc * pix(dst, x, y2, xsize)->r;
	  g += wc * pix(dst, x, y2, xsize)->g;
	  b += wc * pix(dst, x, y2, xsize)->b;
	  n += wc;
	}
      }
      pix(src,x,y, xsize)->r = r/n;
      pix(src,x,y, xsize)->g = g/n;
      pix(src,x,y, xsize)->b = b/n;
    }
  }

}

#include <stdio.h>
#include <stdlib.h>
#include "mpiblurfilter.h"
#include "ppmio.h"

void blurfilter(char* start, int xsize, int ysize, 
    int ystart, int yend, double *w, int radius){

  int x,y, wi;
  double r,g,b,n, wc;
  pixel *dst = malloc(xsize*(yend-ystart)*3);
  pixel *src_pos = (pixel*)start, *dst_pos = dst, *tpos;

  for (y = ystart; y<yend; ++y) {
    for (x=0; x<xsize; ++x) {
      r = w[0] * src_pos->r;
      g = w[0] * src_pos->g;
      b = w[0] * src_pos->b;
      n = w[0];
      for ( wi=1; wi <= radius; wi++) {
        wc = w[wi];
        tpos = src_pos-wi*xsize;
        if(y-wi >= 0) {
          r += wc * tpos->r;
          g += wc * tpos->g;
          b += wc * tpos->b;
          n += wc;
        }
        tpos = src_pos+wi*xsize;
        if(y+wi < ysize) {
          r += wc * tpos->r;
          g += wc * tpos->g;
          b += wc * tpos->b;
          n += wc;
        }
      }
      dst_pos->r = r/n;
      dst_pos->g = g/n;
      dst_pos->b = b/n;
      ++src_pos;
      ++dst_pos;
    }
  }
  src_pos = dst;
  dst_pos = (pixel*)start;
  for (y = ystart; y<yend; ++y) {
    for (x=0; x<xsize; ++x) {
      r = w[0] * src_pos->r;
      g = w[0] * src_pos->g;
      b = w[0] * src_pos->b;
      n = w[0];
      for ( wi=1; wi <= radius; wi++) {
        wc = w[wi];
        tpos = src_pos-wi;
        if(x-wi >= 0) {
          r += wc * tpos->r;
          g += wc * tpos->g;
          b += wc * tpos->b;
          n += wc;
        }
        tpos = src_pos+wi;
        if(x+wi < xsize) {
          r += wc * tpos->r;
          g += wc * tpos->g;
          b += wc * tpos->b;
          n += wc;
        }
      }
      dst_pos->r = r/n;
      dst_pos->g = g/n;
      dst_pos->b = b/n;
      ++src_pos;
      ++dst_pos;
    }
  }
  return;
}



