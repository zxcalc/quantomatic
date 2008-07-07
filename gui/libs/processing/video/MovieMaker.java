/* -*- mode: java; c-basic-offset: 2; indent-tabs-mode: nil -*- */

/*
  Part of the Processing project - http://processing.org

  Copyright (c) 2006 Daniel Shiffman
  With minor modifications by Ben Fry for Processing 0125+

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General
  Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place, Suite 330,
  Boston, MA  02111-1307  USA
*/

package processing.video;
import processing.core.*;


/**
 * MoveMaker stub for linix; 
 * allows compilation with things that use MovieMaker. 
 */
public class MovieMaker {

  public static final int RAW = 0;
  public static final int ANIMATION = 0;
  public static final int BASE = 0;
  public static final int BMP = 0;
  public static final int CINEPAK = 0;
  public static final int COMPONENT = 0;
  public static final int CMYK = 0;
  public static final int GIF = 0;
  public static final int GRAPHICS = 0;
  public static final int H261 = 0;
  public static final int H263 = 0;
  // H.264 encoding, added because no constant is available in QTJava
  public static final int H264 = 0;
  public static final int JPEG = 0;
  public static final int MS_VIDEO = 0;
  public static final int MOTION_JPEG_A = 0;
  public static final int MOTION_JPEG_B = 0;
  public static final int SORENSON = 0;
  public static final int VIDEO = 0;

  public static final int WORST = 0;
  public static final int LOW = 0;
  public static final int MEDIUM = 0;
  public static final int HIGH = 0;
  public static final int BEST = 0;
  public static final int LOSSLESS = 0;

  /**
   * Create a movie with the specified width, height, and filename.
   * The movie will be created at 15 frames per second.
   * The codec will be set to RAW and quality set to HIGH.
   */
  public MovieMaker(PApplet p, int _w, int _h, String _filename) {
    this(p, _w, _h, _filename, 30, RAW, HIGH, 15);
  }


  /**
   * Create a movie with the specified width, height, filename, and frame rate.
   * The codec will be set to RAW and quality set to HIGH.
   */
  public MovieMaker(PApplet p, int _w, int _h, String _filename, int _rate) {
    this(p, _w, _h, _filename, _rate, RAW, HIGH, 15);
  }


  /**
   * Create a movie with the specified width, height, filename, frame rate,
   * and codec type and quality. Key frames will be set at 15 frames.
   */
  public MovieMaker(PApplet p, int _w, int _h, String _filename, int _rate,
                    int _codecType, int _codecQuality) {
    this(p, _w, _h, _filename, _rate, _codecType, _codecQuality, 15);
  }


  /**
   * Create a movie with the specified width, height, filename, frame rate,
   * codec type and quality, and key frame rate.
   */
  public MovieMaker(PApplet p, int _w, int _h, String _filename, int _rate,
                    int _codecType, int _codecQuality,
                    int _keyFrameRate) {
  }



  // A simple add function to just add whatever is in the parent window
  public void addFrame() {
  }


  public void addFrame(int[] _pixels, int w, int h) {
  }

  /**
   * Close out and finish the movie file.
   */
  public void finish() {
  }

  public void dispose() {
  }
}
