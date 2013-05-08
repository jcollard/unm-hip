<h2>The University of New Mexico's Haskell Image Processing Library</h2>
<p>To get started, import Data.Image or Data.Image.Boxed.</p>
<p>To use unm-hip interactively in ghci, import Data.Image.Interactive. This provides three useful functions: display, setDisplayProgram, and plotHistograms.</p>

<pre>setDisplayProgram :: String -> Bool -> IO ()</pre>
<p>Sets the program to use when making a call to display and specifies if the program can accept an image via stdin. If it cannot, then a temporary file will be created and passed as an argument instead. By default, ImageMagick (display) is the default program to use and it is read using stdin.</p>

<pre>
*Main> setDisplayProgram "gimp" False
*Main> setDisplayProgram "xv" False
*Main> setDisplayProgram "display" True
</pre>

<pre>display :: DisplayFormat df => -> IO (Handle, Handle, Handle, ProcessHandle)</pre>
<p>Makes a call to the current display program to be displayed. If the program cannot read from standard in, a file named .tmp-img is created and used as an argument to the program.</p>

<pre>makeImage :: Image i => Int -> Int -> PixelOp (Pixel i) -> i</pre>

<p>Given an Int m, Int n, and a PixelOp f, <b>makeImage</b> 
returns an Image with dimensions m x n and the Pixel value at 
each (i, j) is (f i j)</p>

<pre>
*Main> let grad = makeImage 128 128 (\ r c -> fromIntegral (r + c)) :: GrayImage
*Main> grad
&lt; Image 128x128 &gt;
*Main> display grad
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/gradient.jpg"/>
</p>

<pre>
pii :: Complex Double
pii = 0 :+ pi

harmonicSignal :: Double -> Double -> Int -> Int -> Complex Double
harmonicSignal u v m n = exp ((2*pii) * ((u*(fromIntegral m) + v*(fromIntegral n)) :+ 0))

*Main> let signal = makeImage 128 128 (harmonicSignal (3 / 128) (2 / 128)) :: ComplexImage
*Main> signal
*Main> signal
&lt; Image 128x128 &gt;
*Main> display signal
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/signal.jpg"/>
</p>
<pre>readImage :: FilePath -> IO GrayImage</pre>
Given the file path to a file containing an image
stored in ASCII <i>.pgm</i> format, <i>readImage</i> reads the file
and returns the <i>Image</i>. For example,

<pre>
*Main> frog &lt;- readImage "images/frog.pgm"
*Main> display frog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/frog.jpg" />
</p>
<pre>writeImage :: DisplayFormat df => FilePath -> df -> IO ()</pre>

Given a filename and an Image, 
<i>writeImage</i> creates a file representing the image in ASCII
<i>.pgm</i> format for <i>GrayImage</i>s and <i>.ppm</i> for <i>ColorImage</i> and <i>ComplexImage</i>.
Note: Images saved this way are normalized to integers in the range 0 to 255; 
this may result in loss of detail.

<pre>
*Main> writeImage "frog.pgm" frog
</pre>
creates a file which looks like this:
<pre>
P2
242 225
255
  151   151   151   151   151   150   150   149   148   147   146   145   145   142   142 
  143   145   148   152   156   158   159   159   159   159   157   155   152   150   153 
  152   151   149   149   149   149   150   149   149   149   149   149   149   149   149 
  149   146   144   141   138   136   133   132   136   136   136   136   136   136   136 
  136   139   138   138   138   137   136   136   136   135   135   136   136   137   137 
  138   138   138   137   138   137   138   137   138   137   135   134   134   134   138 
  141   147   150   149   147   143   138   134   132   131   130   129   129   130   132 
  134   136   137   137   137   137   138   139   142   145   147   149   145   146   150 
  153   156   159   161   163   156   158   161   163   167   170   174   175   181   183 
.
.
.
</pre>

<pre>ref :: Image i => i -> Int -> Int -> Pixel i</pre>

Given an image, a positive int i, and a positive  int j, 
<i>ref</i> returns the pixel value at location <i>(i, j)</i>. 

<pre>
*Main> ref frog 100 100
56.0
</pre>

<pre>rows :: Image i => i -> Int</pre>
Given an image, rows returns the number of rows of in the image.
For example,
<pre>
*Main> rows frog
225
</pre>
<pre>cols :: Image i => i -> Int</pre>

Given an image, cols returns the number of columns of in the image.
For example,

<pre>
*Main> cols frog
242
</pre>
<pre>transpose :: Image img => img -> img</pre>

Given an Image img, returns an image created by interchanging 
the rows and columns of the image, i.e., the value at location <i>(i, j)</i>
of the result image is the value of the img at location <i>(j, i)</i>.
For example,

<pre>
*Main> transpose frog
&lt; Image 242x225 &gt;
*Main> display . transpose $ frog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/transposefrog.jpg" />
</p>
<pre>convolveRows :: (Num (Pixel img), Image img) => [Pixel img] -> img -> img</pre>

Given a list consisting solely of pixel values representing a 1D 
convolution kernel and an image, convolveRows returns the 1D discrete 
periodic convolution of the rows of the image with the kernel.
For example,

<pre>
*Main> convolveRows [1, -1] frog
&lt; Image 225x242 &gt;
*Main> display . convolveRows [1, -1] $ frog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/convolverows.jpg"/>
</p>
<pre>convolveCols :: (Num (Pixel img), Image img) => [Pixel img] -> img -> img</pre>

Given a list consisting solely of pixel values representing a 1D 
convolution kernel and an image, convolveCols returns the 1D discrete 
periodic convolution of the columns of the image with the kernel.
For example,

<pre>
*Main> convolveCols [1, -1] frog
&lt; Image 225x242 &gt;
*Main> display . convolveCols [1, -1] $ frog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/convolvecols.jpg"/>
</p>
<pre>
*Main> let dx = convolveRows [1, -1] frog
*Main> let dy = convolveCols [1, -1] frog
*Main> let grad = imageMap sqrt ((dx .*. dx) .+. (dy .*. dy)) :: GrayImage
*Main> grad
&lt; Image 225x242 &gt;
*Main> display grad
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/convolvedxdy.jpg" />
</p>
<pre>convolve :: (Num (Pixel img), Image img) => [[Pixel img]] -> img -> img</pre>

Given a 2D list consisting solely of pixels representing a 2D 
convolution kernel and an image, convolve returns the 2D discrete 
periodic convolution of the image with the kernel.
For example,

<pre>
*Main> convolve [[1, 1, 1], [1, -8, 1], [1, 1, 1]] frog
&lt; Image 225x242 &gt;
*Main> display . convolve [[1, 1, 1], [1, -8, 1], [1, 1, 1]] $ frog>
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/convolve.jpg"/>
<p>
<pre>downsampleCols :: Image img => img -> img</pre>

Given img, downsampleCols returns the image created by discarding 
the odd numbered rows, i.e., the value at location (i, j) of the 
result image is the value of img at location (2i, j).  

For example,

<pre>
*Main> downsampleCols frog
&lt; Image 112x242 &gt;
*Main> display . downsampleCols $ frog
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/downsamplecolsfrog.jpg"/>
<p>

<pre>downsampleRows :: Image img => img -> img</pre>

Given img, downsampleRows returns the image created by discarding the odd 
    numbered columns, i.e., the value at location (i, j) is the value of img 
    at location (i, 2j).  

For example,

<pre>
*Main> downsampleRows frog
&lt; Image 225x121 &gt;
*Main> display . downsampleRows $ frog
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/downsamplerowsfrog.jpg"/>
<p>
<pre>downsample :: Image img => img -> img</pre>

<pre>
*Main> let tinyFrog = downsample frog
*Main> tinyFrog
&lt; Image 112x121 &gt;
*Main> display tinyFrog
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/downsamplefrog.jpg" />
<p>
<pre>upsampleCols :: (Monoid (Pixel img), Image img) => img -> img</pre>

Given img, upsampleCols returns an image with twice the number of 
rows where the value at location (i, j) of the result image is the 
value of img at location (i/2, j) if i is even and mempty otherwise.

For example,

<pre>
*Main> upsampleCols tinyFrog
&lt; Image 224x121 &gt;
*Main> display . upsampleCols $ tinyFrog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/upsamplecols.jpg" />
<p>
<pre>upsampleRows :: (Monoid (Pixel img), Image img) => img -> img</pre>

Given img, upsampleRows returns an image with twice the number of 
columns where the value at location (i, j) of the result image is 
the value of img at location (i, j/2) if j is even and 
mempty otherwise.

For example,

<pre>
*Main> upsampleRows tinyFrog
&lt; Image 112x242 &gt;
*Main> display . upsampleRows $ tinyFrog
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/upsamplerows.jpg"/>
<p>
<pre>upsample :: (Monoid (Pixel img), Image img) => img -> img</pre>

Given img, upsample returns an image with twice the number of
    rows and columns where the value at location (i, j) of the resulting
    image is the value of img at location (i/2, j/2) if i and jare are even
    and mempty otherwise.

For example,

<pre>
*Main> upsample tinyFrog
&lt; Image 224x242 &gt;
*Main> display . upsample $ tinyFrog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/upsample.jpg"/>
<p>

<pre>pad :: (Monoid (Pixel img), Image img) => Int -> Int -> img -> img</pre>

Given m, n, and img, pad returns an Image with m rows and n columns 
    where the value at location (i, j) of the result image is the value 
    of img at location (i, j) if i is less than m and j is less than n 
    and mempty otherwise.

For example,

<pre>
*Main> pad 200 200 tinyFrog
&lt; Image 200x200 &gt;
*Main> display . pad 200 200 $ tinyFrog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/padfrog.jpg"/>
</p>
<pre>crop :: Image img => Int -> Int -> Int -> Int -> img -> img</pre>

Given a i0, j0, m, n, and img, crop returns an image with m rows 
and n columns where the value at location (i, j) of the result 
image is the value of img at location (i0 + i, j0 + j).  

For example,

<pre>
*Main> let frogPart = crop 64 64 128 128 frog
*Main> frogPart
&lt; Image 128x128 &gt;
*Main> display frogPart
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/cropfrog.jpg"/>
<p>
<pre>leftToRight :: Image img => img -> img -> img</pre>

Given two images with the same number of rows X and Y,  returns an
image that is the concatenation of the two images from left to right.
There is a convenience function, <b>leftToRight'</b> that takes a pair,
triple, or list of images and displays them left to right.

For example,

<pre>
*Main> leftToRight tinyFrog tinyFrog
&lt; Image 112x242 &gt;
*Main> display . leftToRight tinyFrog $ tinyFrog
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/lefttoright.jpg"/>
<p>
<pre>topToBottom :: Image img => img -> img -> img</pre>

Given two images with the same number of columns X and Y, returns an
image that is the concatenation of the two images from top to bottom.
There is a convenience function, <b>topToBottom'</b> that takes a pair,
triple, or list of images and displays them top to bottom.

For example,

<pre>
*Main> topToBottom tinyFrog tinyFrog
&lt; Image 224x121 &gt;
*Main> display . topToBottom tinyFrog $ tinyFrog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/toptobottom.jpg">
</p>

<pre>makeFilter :: Image img => Int -> Int -> PixelOp (Pixel img) -> img</pre>

Given a positive integer m, a positive integer n, and a function 
returning a pixel value, makeFilter returns an image with m rows 
and n columns. Let x equal i if i is less than m/2 and i - m otherwise 
and let y equal j if j is less than n/2 and j - n otherwise. To match 
the periodicity of the 2D discrete Fourier spectrum, the value of the 
result image at location (i, j) is computed by applying the function to x 
and y, e.g., the value at location (0, 0) is the result of applying the 
function to 0 and 0, the value at (m-1, n-1) is the result of applying 
function to -1 and -1.

For example,

<pre>
*Main Data.Complex> let filter = makeFilter 128 128 (\ i j -> fromIntegral (i + j)) :: GrayImage
*Main Data.Complex> filter
&lt; Image 128x128 &gt;
*Main Data.Complex> display filter
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/makefilter.jpg" />
<p>
<pre>
laplacianOfGaussian stddev i j =
  let r = fromIntegral (i*i + j*j)
      x = (r / 2.0) / stddev 
  in (((-pi) / stddev) / stddev) * (1 - x) * (exp (-x))

*Main Data.Complex> let d2g = makeFilter 128 128 (laplacianOfGaussian 8) :: ComplexImage
*Main Data.Complex> d2g
&lt; Image 128x128 &gt;
*Main Data.Complex> display d2g
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/d2g.jpg" />
<p>
<pre>fft :: (Image img, Image img', ComplexPixel (Pixel img), Pixel img' ~ Complex (Value (Pixel img))) => img -> img'</pre>

Given an image whose pixels can be converted to a complex value, 
fft returns an image with complex pixels representing its 2D discrete 
Fourier transform (DFT). Because the DFT is computed using the Fast Fourier 
Transform (FFT) algorithm, the number of rows and columns of the image 
must both be powers of two, i.e., 2K where K is an integer.

For example,

<pre>
*Main> let logFrog = magnitude . imageMap log . fft $ frogpart
*Main> logFrog
&lt; Image 128x128 &gt;
*Main> display logFrog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/fft.jpg"/>
<p>
<pre>
*Main> fft d2g
&lt; Image 128x128 &gt;
*Main> display . fft $ d2g
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/fftd2g.jpg" />
<pre>
gaussian variance i j =          
  let r = fromIntegral (i*i + j*j)
      x = (r / (2*pi)) / variance
  in exp (-x)
*Main> let g = makeFilter 128 128 (gaussian 8) :: GrayImage
*Main> display g
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/g.jpg"/>
<pre>
*Main> fft g 
&lt; Image 128x128 &gt;
*Main> display . fft $ g
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/fftg.jpg"/>
<p>
<pre>ifft :: (Image img, Image img', ComplexPixel (Pixel img), Pixel img' ~ Complex (Value (Pixel img))) => img -> img'</pre>

Given an image, ifft returns a complex image representing its 2D 
inverse discrete Fourier transform (DFT). Because the inverse DFT is 
computed using the Fast Fourier Transform (FFT) algorithm, the number 
of rows and columns of <image> must both be powers of two, i.e., 2K 
where K is an integer. 

For example,

<pre>
*Main> ifft ((fft frogPart) * (fft d2g))

&lt; Image 128x128 &gt;
*Main> display $ ifft ((fft frogPart) * (fft d2g))
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/ifft.jpg"/>
<p>
<pre>
*Main> ifft ((fft frogPart) * (fft g))
&lt; Image 128x128 &gt;
*Main> display $ ifft ((fft frogPart) * (fft g))
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/ifft2.jpg" />
<p>
<pre>realPart :: (Image img, Image img', ComplexPixel (Pixel img), Pixel img' ~ Value (Pixel img)) => img -> img'</pre>

Given a complex image, returns a real image representing
    the real part of the image.

For example,

<pre>
*Main> let cosine = realPart signal :: GrayImage
*Main> cosine
&lt; Image 128x128 &gt;
*Main> display cosine
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/cosine.jpg"/>
<p>
<pre>
*Main> display . realPart realPart . ifft $ (fft frogpart) * (fft d2g)
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/realpart.jpg"/>
<p>
<pre>
*Main> display . realPart . ifft $ (fft frogpart) * (fft g)
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/realpart2.jpg" />
<p>
<pre>imagPart :: (Image img, Image img', ComplexPixel (Pixel img), Pixel img' ~ Value (Pixel img)) => img -> img'</pre>

Given a complex image, returns a real image representing
   the imaginary part of the image

For example,

<pre>
*Main> let sine = imagPart signal :: GrayImage
*Main> sine
&lt; Image 128x128 &gt;
*Main> display sine
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/sine.jpg" />
<p>
<pre>complex :: (Image img, Image img', Pixel img' ~ C.Complex (Pixel img)) => img -> img -> img'</pre>

Given an image representing the real part of a complex image, and
an image representing the imaginary part of a complex image, returns
a complex image.

For example,

<pre>
*Main> complex cosine sine :: ComplexImage
&lt; Image 128x128 &gt;
*Main> display (complex cosine sine :: ComplexImage)
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/signal.jpg" />
<p>
<pre>complexImageToRectangular :: (Image img, Image img', ComplexPixel (Pixel img), Pixel img' ~ Value (Pixel img)) => img -> (img', img')</pre>

Given a complex image, return a pair of real images each representing
a component of the complex image (real, imaginary).

For example,

<pre>
*Main> leftToRight' . complexImageToRectangular $ signal
&lt; Image 128x256 &gt;
*Main> display . leftToRight' . complexImageToRectangular $ signal
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/complexsignaltorectangular.jpg"/>
<p>
<pre>magnitude :: (Image img, Image img', ComplexPixel (Pixel img), Pixel img' ~ Value (Pixel img)) => img -> img'</pre>

Given a complex image, returns a real image representing
the magnitude of the image.

<p>
<pre>angle :: (Image img, ComplexPixel (Pixel img), Image img', Pixel img' ~ Value (Pixel img)) => img -> img'</pre>

Given a complex image, returns a real image representing
the angle of the image.

For example,

<pre>
*Main> angle signal
&lt; Image 128x128 &gt;
*Main> display (angle signal :: GrayImage)
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/angle.jpg"/>
<p>
<pre>complexImageToPolar :: (Image img, ComplexPixel (Pixel img), Image img', Pixel img' ~ Value (Pixel img)) => img -> (img', img')</pre>

Given a complex image, returns a pair of real images each
representing the component (magnitude, phase) of the image

<pre>
*Main> complexImageToPolar signal
(&lt; Image 128x128 &gt;,&lt; Image 128x128 &gt;)
*Main> display . leftToRight' . complexImageToPolar $ signal
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/compleximagetopolar.jpg"/>
<p>
<pre>(+) :: Num a => a -> a -> a</pre>

Any two images installed in the Num type class (any two Boxed images) may be added if their dimensions match.
For each <i>(i, j)</i> the resulting pixel will be the sum of the pixels from the given images.
For example,

<pre>
*Main> callisto &lt;- readImage "images/callisto.pgm"
*Main> display callisto
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/callisto.gif">
<p>
<pre>
*Main> ganymede &lt;- readImage "images/ganymede.pgm"
*Main> display ganymede
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/ganymede.gif">
<p>
<pre>
*Main> callisto + ganymede
&lt; Image 128x128 &gt;
*Main> display $ callisto + ganymede
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/sum.gif">
<p>
<pre>(-) :: Num a => a -> a -> a</pre>

Any two images installed in the Num type class (any two Boxed images) may be subtracted if their dimensions match.
For each <i>(i, j)</i> the resulting pixel will be the difference of the two pixels from the given images.
For example,

<pre>
*Main> display $ callisto - ganymede
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/difference.gif">
<p>
<pre>(*) :: Num a => a -> a -> a</pre>

Any two images installed in the Num type class (any two Boxed images) may be multiplied if their dimensions match.
For each <i>(i, j)</i> the resulting pixel will be the product of the two pixels from the given images.
For example,

<pre>
*Main> display (callisto * ganymede)
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/product.gif">
<p>
<pre>(/) :: Fractional a => a -> a -> a</pre>

Any two images installed in the Num type class (any two Boxed images) may be divided if their dimensions match.
For each <i>(i, j)</i> the resulting pixel will be the quotient of the two pixels from the given images.
For example,

<pre>
*Main> display (callisto / ganymede)
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/quotient.gif">
<p>

<pre>arrayToImage :: Image img => Array (Int, Int) (Pixel img) -> img</pre>

 Given a two dimensional array of Pixel values indexed by
pairs of Ints where the fst is the row and snd is the column, returns
an Image.

For example,

<pre>
*Main> let array = listArray ((0,0),(127,127)) [0..] :: Array (Int,Int) Double
*Main> arrayToImage array :: GrayImage
&lt; Image 128x128 &gt;
*Main> display (arrayToImage array :: GrayImage)
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/matrix2image.gif"/>
<p>
<pre>imageToArray :: Image img => img -> Array (Int, Int) (Pixel img)</pre>

Given img, returns an two dimensional array of Pixel values 
indexed by pairs of Ints where the fst is the row and snd is the column.

<pre>
*Main> let arr = listArray ((0,0),(2,2)) [0..] :: Array (Int, Int) Double
*Main> imageToArray (arrayToImage arr :: GrayImage)
array ((0,0),(2,2)) [((0,0),0.0),((0,1),1.0),((0,2),2.0),((1,0),3.0),((1,1),4.0),((1,2),5.0),((2,0),6.0),((2,1),7.0),((2,2),8.0)]
</pre>

<pre>(>.) :: (Ord (Pixel img), Image img, BinaryPixel (Pixel img)) => Pixel img -> img -> img</pre>

Given a Pixel p and an image img, return a Binary image where the
pixel at (i, j) is on if p is greater than the corresponding pixel in 
img at (i,j) and off otherwise.

Note: there is a variation of <i>(.<)</i> named <i>(>.)</i> where the arguments are flipped.

<pre>
*Main> stop &lt;- readColorImage "images/stop.ppm"
*Main> display stop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/stop.jpg"/>
<p>
<pre>
*Main> let (r,g,b) = colorImageToRGB stop
*Main> let binaryStop = (r + g + b) .> 400
*Main> display binaryStop
</pre>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/binarystop.jpg"/>
<p>

<pre>(<.) :: (Ord (Pixel img), Image img, BinaryPixel (Pixel img)) => Pixel img -> img -> img</pre>
Given a Pixel p and an image img, return a Binary image where the
    pixel at (i, j) is on if p is less than the corresponding pixel in 
    img at (i,j) and off otherwise.

Note: there is a variation of <i>(<.)</i> named <i>(.<)</i> where the arguments are flipped.

<pre>
*Main> let binaryStop = (r + g + b) .< 400
*Main> display binaryStop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/binarystop.jpg" />
<p>

<pre>(.==.) :: (Eq (Pixel img), Image img, BinaryPixel (Pixel img)) => img -> img -> img</pre>
Given an image with pixels, <i>p</i>, and a pixel, <i>c</i>, returns an image where
each pixel has the value 1 iff <i>p = c</i> and 0 otherwise.
Note: there is a variation of <i>(==.)</i> named <i>(.==)</i> where the arguments are flipped.

<pre>
shiftRight :: Image img => Int -> img -> img
shiftRight s img = makeImage (rows img) (cols img) shift where
  shift r c = ref img r c' where
      c' = let sum = c + s 
           in if sum < (cols img) then sum else sum - (cols img)
*Main> let binaryStop = (r + g + b) .> 400
*Main> display $ (shiftRight 100 binaryStop)
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/shiftBinaryStop.gif">
<pre>
*Main> display $ (shiftRight 100 binaryStop) .==. binaryStop
</pre>
<img src="https://raw.github.com/jcollard/unm-hip/master/examples/binaryEqual.gif">

<pre>normalize :: (Fractional (Pixel img), MaxMin (Pixel img), Image img) => img -> img</pre>

Given img, normalize returns an image with the same dimensions 
where the values have been normalized to lie in the interval [0, 1].

<p>

<pre>shrink :: (Num a, Image img, ComplexPixel (Pixel img), Image img', Pixel img' ~ C.Complex (Value (Pixel img))) => a -> img -> img'</pre>

Given a complex image and a real positive number x, shrink returns 
a complex image with the same dimensions. Let z be the value of the 
image at location (i, j). The value of the complex result image at 
location (i, j) is zero if |z| < x, otherwise the result has the 
same phase as z but the amplitude is decreased by x.

<p>

<pre>medianFilter :: (Ord (Pixel img), Image img) => Int -> Int -> img -> img</pre>

Given two positive integers, m and n and a an image, 
medianFilter returns an image with the same dimensions where each 
pixel (i, j) in <image> is replaced by the pixel with median value 
in the neighborhood of size m times n centered on (i, j).

<p>
<pre>
*Main> let medianFilteredFrog = medianFilter 5 5 frog
*Main> display medianFilteredFrog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/medianfilter.jpg"/>
<p>

<pre>imageFold :: Image img => (Pixel img -> b -> b) -> b -> img -> b</pre>

Given a function of a pixel to a value of type <i>b</i> which
returns a value of type <i>b</i>, <i>imageFold</i> 
returns the value of type <i>b</i> which results
from repeatedly applying the function to: 1) the result
accumulated to this point (initially the value of the first pixel);
and 2) the value of the next pixel.

<p>

<pre>matrixProduct :: (Num (Pixel img), Image img) => img -> img -> img</pre>

Given an image X1 and an image X2, where the number of columns of X1 
equals the number of rows of X2, matrixProduct returns an image 
representing the matrix product of X1 and X2. 

<pre>
*Main> display (matrixProduct frogPart frogPart)
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/matrixproduct.jpg" />
<p>

<pre>imageMap :: (Image a, Image b) => (Pixel a -> Pixel b) -> a -> b</pre>

Given a function of a pixel value of type <i>a</i> to a pixel value of type 
<i>b</i>, and an image containing pixel values of type <i>a</i>, 
imageMap returns an image of type <i>b</i> where each pixel in the result
image is the result of appyling the function to each pixel in the given image.

Note: Boxed images are in typeclass <i>Functor</i> and <i>Applicative</i> it
is recommended you use <i>fmap</i> instead of imageMap for Boxed images.

<pre>
*Main> let img = imageMap ((-1) *) frog :: GrayImage
*Main> display img
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/invertfrog.jpg"/>
<p>
<pre>readColorImage :: FilePath -> IO ColorImage</pre>

Given the file path to a file containing an image
stored in ASCII <i>.ppm</i> format, <i>readColorImage</i> reads the
file and returns the ColorImage

For example,

<pre>
*Main> cacti &lt;- readColorImage "images/cactii.ppm"
*Main> display cacti
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/cactii.jpg"/>
<p>

<pre>colorImageRed :: ColorImage -> GrayImage</pre>

Given a ColorImage, returns a GrayImage representing the Red color component

For example,

<pre>
*Main> let red = colorImageRed cacti
*Main> display red
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/colorimagered.jpg" />
<p>
<pre>colorImageGreen :: ColorImage -> GrayImage</pre>

Given a ColorImage, returns a GrayImage representing the Green color component

For example,

<pre>
*Main> let green = colorImageGreen cacti
*Main> display green
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/colorimagegreen.jpg"/>
<p>
<pre>colorImageBlue :: ColorImage -> GrayImage</pre>

Given a ColorImage, returns a GrayImage representing the Blue color component

For example,

<pre>
*Main> let blue = colorImageBlue cacti
*Main> display blue
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/colorimageblue.jpg" />
<p>
<pre>rgbToColorImage :: (GrayImage, GrayImage, GrayImage) -> ColorImage</pre>

Given a triple containing three GrayImages each containing one of the
    color components (red, green, blue), returns a ColorImage

<pre>
*Main> display . rgbToColorImage $ (red,green,blue)
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/cactii.jpg" />
<p>
<pre>colorImageToRGB :: ColorImage -> (GrayImage, GrayImage, GrayImage)</pre>

Given a ColorImage, returns a triple containing three GrayImages each
    containing one of the color components (red, green, blue)

For example,

<pre>
*Main> display . leftToRight' $ colorImageToRGB cacti
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/colorimagetorgb.jpg"/>
<p>
<pre>colorImageToHSI :: ColorImage -> (GrayImage, GrayImage, GrayImage)</pre>

Given a ColorImage, returns a triple containing three GrayImages each
    containing one of the components (hue, saturation, intensity)

For example,

<pre>
*Main> let (h,s,i) = colorImageToHSI cacti
*Main> display h
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/colorimagehue.jpg" />
<p>
<pre>
*Main> display s
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/colorimagesaturation.jpg" />
<p>
<pre>
*Main> display i
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/colorimageintensity.jpg" />
<p>
<pre>hsiToColorImage :: (GrayImage, GrayImage, GrayImage) -> ColorImage</pre>

Given a triple containing three GrayImages each containing one of the
color components (hue, saturation, intensity), returns a ColorImage

For example,

<pre>
*Main> display . hsiToColorImage $ (h, s, i)
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/cactii.jpg" />
<p>

<pre>makeHotImage :: GrayImage -> ColorImage</pre>

Given a GrayImage, makeHotImage returns a ColorImage with the same 
dimensions. The R, G, B values of the result image at (i, j) are 
determined by using the value of the ColorImage at (i, j) to index 
three lookup tables. These lookup tables implement a false coloring 
scheme which maps small values to black, large values to white, and 
intermediate values to shades of red, orange, and yellow (in that order).

<pre>
*Main> display . makeHotImage $ frog
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/makehotimage.jpg" />
<p>
<pre>dilate :: (Eq (Pixel img), Num (Pixel img), Image img, BinaryPixel (Pixel img)) => [[Pixel img]] -> img -> img</pre>

Given a 2D list consisting solely of pixels representing a structuring 
    element, and a binary image, dilate returns the morphological dilation of 
    the <image> with the structuring element. 

Note: There is a <i>dilate'</i> function that uses a default
structuring element of [[1,1], [1,1]]. For example,

<pre>
structure = [[0, 0, 1, 0, 0],
             [0, 1, 1, 1, 0],
             [1, 1, 1, 1, 1],
             [0, 1, 1, 1, 0],
             [0, 0, 1, 0, 0]] 

*Main> display . dilate structure $ binaryStop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/dilate-stop.gif">
<p>
<pre>erode :: (Eq (Pixel img), Num (Pixel img), Image img, BinaryPixel (Pixel img)) => [[Pixel img]] -> img -> img</pre>

Given a 2D list consisting solely of pixels representing a structuring 
element, and a binary image, erode returns the morphological erosion of 
the <image> with the structuring element. 

Note: There is a <i>erode'</i> function that uses a default structuring element of [[1,1], [1,1]].
For example,

<pre>
*Main> display . erode structure $ binaryStop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/erode-stop.gif">
<p>
<pre>outline :: (Image img, BinaryPixel (Pixel img), Eq (Pixel img)) => img -> img</pre>

Given an image, outline returns an image where edge pixels are 
set to the value on and non-edge pixels are set to the value off. 
Pixel (i, j) is an edge pixel iff its value is different than the value 
of either pixel (i, j+1) or pixel (i+1, j).

Note: There is an <i>outline'</i> that allows the for the non-edge and edge pixel values to be specified.

<pre>
*Main> display . outline $ binaryStop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/outline.jpg"/>
<p>
<pre>label :: (Image img, BinaryPixel (Pixel img)) => img -> GrayImage</pre>

Given a binary image, label returns an image where pixels in 
distinct connected components (based on 4-neighbor connectivity) 
have distinct integer values. These values range from 1 to n where 
n is the number of connected components in image.

<pre>
*Main> display . makeHotImage . label $ binaryStop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/label-stop.gif">
<p>

<pre>distanceTransform :: (Image img, BinaryPixel (Pixel img)) => img -> GrayImage</pre>

Given a binary image, distanceTransform returns an image 
representing the 2D distance transform of the image.
The distance transform is accurate to within a 2% error for euclidean
distance.

<pre>
*Main> display . distanceTransform . dilate $ binaryStop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/distancetransform.jpg"/>
<p>
<pre> open :: (Eq (Pixel img), Num (Pixel img), Image img, BinaryPixel (Pixel img)) => [[Pixel img]] -> img -> img</pre>

Given a 2D list consisting solely of pixels representing a structuring 
element, and a binary image, dilate returns the morphological opening of 
the image with the structuring element. 

Note: There is a version <i>open'</i> that uses the default structuring element [[1,1],[1,1]].

<pre>
Main*> noise &lt;- readColorImage "images/noise.ppm"
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/noise.jpg"/>
<p>
<pre>
Main*> let noisyStop = binaryStop ./=. noise
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/noisystop.jpg"/>
<p>
<pre>
Main*> display . open $ noisyStop
</pre>
<p>
<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/open.jpg" />
<p>
<pre>close :: (Eq (Pixel img), Num (Pixel img), Image img, BinaryPixel (Pixel img)) => [[Pixel img]] -> img -> img</pre>

Given a 2D list consisting solely of pixels representing a structuring 
element, and a binary image, dilate returns the morphological closing of 
the image with the structuring element. 

Note: There is a version <i>close'</i> that uses the default structuring element [[1,1],[1,1]].

<pre>
Main*>close [[1,1],[1,1]] noisyStop
</pre>

<IMG SRC="https://raw.github.com/jcollard/unm-hip/master/examples/close.jpg" />

<pre>areas :: (Image img, MaxMin (Pixel img), RealFrac (Pixel img)) => img -> V.Vector Double</pre>

Given an image, areas returns a vector where the n-th component equals 
the number of pixels with value n. If image is the result of applying 
label to a binary image, then the vector represents the areas of the 
connected-components of the binary-image. If not, areas returns 
the histogram of the image.

For example,

<pre>
*Main> areas . label $ binaryStop
fromList [9676.0,1033.0,1201.0,6.0,718.0,4.0,0.0,1036.0]
</pre>

<pre>perimeters :: (Image img, MaxMin (Pixel img), Pixel img ~ Double) => img -> V.Vector Double</pre>

Given an image, perimeters returns a vector where the n-th component 
equals the number of pixels with value n which are adjacent to pixels 
of value 0 and the 0-th component equals the sum of the other components. 
If image is the result of applying label to a binary image, then the 
vector represents the perimeters of the connected-components of the 
binary-image.

For example,

<pre>
*Main> perimeters . label $ binaryStop
fromList [1072.0,304.0,322.0,6.0,184.0,4.0,0.0,252.0]
</pre>

<pre>centersOfMass :: (Image img, MaxMin (Pixel img), Pixel img ~ Double) => img -> [(Double, Double)]</pre>

Given an image, the result of applying label to a binary-image, 
centersOfMass returns a vector where the n-th component is a tuple 
representing the average row and column indices of pixels of the 
n-th connected-component of the image.

For example,

<pre>
*Main> centersOfMass . label $ binaryStop
[(42.373668925459825,24.764762826718297),(41.76935886761032,92.25978351373855),(14.5,14.5),(35.139275766016716,57.46239554317549),(14.5,81.5),(NaN,NaN),(35.69015444015444,130.08590733590734)]
</pre>

<pre>boundingBoxes :: (Image img, MaxMin (Pixel img), Pixel img ~ Double) => img -> [(Int, Int, Int, Int)]</pre>

Given an image, the result of applying label to a binary-image, 
boundingBoxes returns a vector where the n-th component is a four 
element tuple representing the minimum and maximum row and column 
indices of pixels of the n-th connected-component of the image.

For example,

<pre>
*Main> boundingBoxes . label $ binaryStop
[(10,8,73,40),(10,75,73,109),(12,12,17,17),(11,43,72,72),(13,80,16,83),(86,159,0,0),(12,118,72,150)]
</pre>
