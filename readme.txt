:[:: vbDABL - visualbasic Directx AlphaBlending Library ::]:
:[:: Copyright (C) 2000 David Goodlad ::]:

:[:: beta release 0.200 ::]:

* THIS SOFTWARE IS SUBJECT TO THE TERMS OF THE GNU LGPL LICENSE INCLUDED AS LICENSE.TXT *

:[:: changelog ::]:
??-01-2000
	First alpha release (0.001).  Contained basic alphablend and halfalpha functions;
	colorblend was broken completely.  Colorkeying I don't think was implemented.
18-01-2000
	Second alpha release (0.002).  Colorblending is no longer broken, though the way I fixed it
	should be considered a quick hack, not a true fix (for now at least).  Weird things are bound
	to happen with it.  Colorkeying has been implemented with all of the basic functions, though
	there is no way of setting the transparent color yet - it is always pure black.
12-02-2000
	Third alpha release (0.003).  Rewrote all the main alphablending routines.  I also removed
	the pointless halfalpha routines.  Added a couple sections to this readme describing how
	the algorithm works, and what asm registers are used for what, when...  The alphablend***ck
	routines were removed as well, because I don't believe non-colorkeyed versions are very
	necessary.

	The sample was modified to reflect the changes to the library.  In addition, I used Glib's
	snow dll just to add a little extra effect to it.  It hardly touches the framerate, and it's
	easy to comment out the three lines of code that make it do it's thing.  Glib's page is at:
	http://go.to/HomeSoft/
09-03-2000
	First public beta (0.100)!  Basically the same as 0.003, but with a few added things including
	some semi-broken code in alphablend555 to blend pics with any width, not just multiples of 4.
	I forget everything else that was added - not too much really.
30-06-2000
	Second beta!!  The alphablend16555 and alphablend16565 routines have been combined into one
	called vbDABLalphablend16, which re-introduces non-colorkeyed versions of the routines as
	well.  The two original functions are still there, but unmaintained, and not up-to-date.  The
	code to blend any width of image has been fixed and implemented completely in the alphablend16
	function.  Also, colorkeying has been fixed; before the first 4 pixels were *always* skipped.
	The problem with colorblend not working on 565 mode has also been ratified as far as I can tell,
	it was a problem with vc++'s optimization, so that whenever you used the 'Win32-Release' version
	of the dll, 565 mode colorblending would crash.  I've removed optimization, and simply left the
	'Release' version the same as 'Debug' but without debugging symbols.  Colorblending still needs
	to be formatted better and have its comments rewritten, but it still works.

:[:: contents :]::

The zip contains the following:

--library - this dir contains the main c++ library files
    \-precompiled - contains precompiled dll's
           \-Release - contains the 'release' dll. this is the one you should copy to your windows\system dir.
	   \-Debug - contains the dll compiled with debug symbols. most users shouldn't need this.
--sample - a simple sample app... not commented too well yet, but you should get it. most stuff is in the modDirectX
            module.


:[:: usage instructions ::]:

Make sure that the vbdabl.dll file is in either your windows\system directory, or the exe's
directory!  In the vb IDE, you won't be able to use it if the dll is in the project's dir,
only as an exe will this work, so the windows\system directory is best.

Use these declare statements in your VB project (in a module):

Public Declare Function vbDABLalphablend16 Lib "vbDABL" (ByVal iMode As Integer, ByVal bColorKey As Integer, ByRef sPtr As Any, ByRef dPtr As Any, ByVal iAlphaVal As Integer, ByVal iWidth As Integer, ByVal iHeight As Integer, ByVal isPitch As Integer, ByVal idPitch As Integer, ByVal iColorKey As Integer) As Integer
Public Declare Function vbDABLcolorblend16555 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
Public Declare Function vbDABLcolorblend16565 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
Public Declare Function vbDABLcolorblend16555ck Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
Public Declare Function vbDABLcolorblend16565ck Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long

Do not use the old vbDABLalphablend555 and 565 functions any more!


:[:: parameter descriptions ::]:

function vbDABLalphablend16:
 iMode		: one of either 555 or 565; it's the 16bpp mode used the the video card.
 bColorKey	: 0 disables colorkeying, anything else enables it
 sPtr		: a pointer to the source surface
 dPtr		: a pointer to the destination surface
 iAlphaVal	: alpha value (0-255), where 0 leaves destination unchanged.
 iWidth		: width of the area you wish to blend (in pixels)
 iHeight	: height of the area you wish to blend (in pixels)
 isPitch	: the pitch of the source surface as given by it's DDSURFACEDESC2
 idPitch	: the pitch of the destination surface as given by it's DDSURFACEDESC2
 iColorKey	: the color value you wish to use as transparent

function vbDABLcolorblend165*5(ck):
 sPtr		: a pointer to the source surface
 dPtr		: a pointer to the destination surface
			note!  these two pointers can be to the same surface!  read below to find out
				why.
 alpha_val	: alpha value (0-255), where 0 leaves destination unchanged.
 Width		: width of the area you wish to blend (in pixels)
 Height		: height of the area you wish to blend (in pixels)
 sPitch		: the pitch of the source surface as given by it's DDSURFACEDESC2
 dPitch		: the pitch of the destination surface as given by it's DDSURFACEDESC2
 rVal		: red value of the color you wish to blend with (0-255)
 gVal		: green value of the color you wish to blend with (0-255)
 bVal		: blue value of the color you wish to blend with (0-255)

 Many people don't understand what the colorblend functions actually do, so here it is: basically,
  they take the source, alphablend it with the specified color values (NOT the destination pixels),
  then put the resulting pixels onto the destination, overwriting whatever was there before.  This
  is useful for things like tinting a sprite green when it's poisoned, and fading between this green
  and the original colors (like in Final Fantasy).  It's basically a filter placed in the middle of
  a regular blit.  BUT, you can also use it where the two surface pointers are to the same surface,
  at the same place.  This would let you just blend a color on top of that surface, as if you had
  a second surface filled with that one color and did a regular alphablend of the second surface as
  the source and the original as the destination.  Look at the sample to see how this is done.


:[:: algorithm description ::]:

The algorithm that I use is basically the standard alphablending formula:

finalcolor = (source * alpha) + (dest * (1 - alpha))

But, a few things are changed.  This formula assumes an alpha value between 0 and 1 (ie: floating point).
This is BAD :P  Integer multiplication is much faster, and easier.  So, modifying the formula to this:

finalcolor = ((source * alpha) / 256) + ((dest * (256-alpha)) / 256)

we eliminate the floating point values and make it a range of 0 to 255.  Now, you'll notice quite a
few multiplications (2) and divisions (2) in this formula.  These are VERY slow.  The divisions,
though, can easily be replaced by bitshifting to the right by 8 (which is division by 2^8=256).
But, we are still left with 2 multiplications... UGLY!  If we use some simple algebraic simplification
of the formula, we get this:

finalcolor = (alpha(source - dest)) / 256 + dest

This is WAY better!  Now we're getting somewhere.  But, there's still one last problem...  If you think
about it for a minute, you'll realize "What if the source is smaller than the dest??"  We end up having
to deal with signed numbers, which can be an annoyance in assembly language, especially dealing with
"packed" data.  So, to compensate, we add 256 to the source value on the inside...

finalcolor = (alpha((source + 256) - dest)) / 256 + dest

Hey, what the hell, we can't just arbitrarily add numbers into the middle of the formula though!  We
have to make an equivalent subtraction outside of the multiplication, taking into account 'FOIL'

finalcolor = (alpha((source + 256) - dest)) / 256 + dest - (alpha(256)) / 256

We basically took the addition of 256 and put it into it's own little mult/div set to compensate.
But, there is ONE last thing to do which should be obvious:

finalcolor = (alpha((source + 256) - dest)) / 256 + dest - alpha

In the previous formula we were multiplying by 256, then dividing by 256... This is pointless and can
simply be removed.  So now, we have our final algorithm/formula or whatever you want to call it.


:[:: register usage ::]:

I figure that if *I* even get confused sometimes as to what register I'm using for what, I decided
to write a little table showing what each register is used for...

Register | Usage

. esi    | Stores the source surface pointer
. edi    | Stores the destination surface pointer
. cx     | Stores the width of the area to be blended;
            Also acts as a counter to determine when to jump to the next line
. dx     | Stores the height of the area to be blended;
            Also acts as a counter to determine when we've finished blending
. mm0    | Is commonly used to store the source pixel values...
            This register is also the main register used for the calculations of the algorithm
            At the end it is also used in the colorkey-checking routine as it is no longer needed
. mm1    | Used to store the destination pixel values
. mm2    | Stores the new pixel values as the function progresses
. mm3    | Stores the green bitmask 'permanently'
. mm4    | Stores the blue bitmask, again 'permanently'
. mm5    | Stores the alpha values 'permanently'
. mm6    | Stores the current source pixels in their original form
. mm7    | Stores the current dest pixels in their original form

A note on the usage of registers mm3, mm4, and mm5:
  I used these registers to replace memory... This allowed me to only reference memory *3* times
   throughout the entire inner loop!!  Not bad if I do say so myself :)

Another note - on the usage of mm0, mm1, mm6 and mm7:
  You may notice some redundancy between the pairs mm0/mm6 and mm1/mm7.  This is because mm0 and mm1
   are modified during the loop... I need to be able to reset them to treat the other colors later.
   If I didn't use mm6/mm7 to do this, I would end up reading the pixels from memory (possibly video
   memory - HURTIN!) 3 times for both source and dest every loop, whereas the way I've got it setup 
   I only have to read from mem twice (once for source, once for dest).


:[:: final notes ::]:

If you need some help with the use of the library, either email me at dgoodlad@junction.net, or
post a message on my website's message board (the board itself is at 
http://blackhole.thenexus.bc.ca/phorum/ ).  I also check the VBGaming Central and VoodooVB 
message boards often, so if you post there I will most likely see it as well.  I am always very
willing to help anyone figure out how this works, and answer any questions you may have :)

If you know your c++/asm, help out!  That's why this library is open-source, I'm only one guy!  I
can't do it all.  So, if you find something that you can fix/add, I'd appreciate it if you sent
me an email with the updated .cpp file, as well as a description of what you did.  That way
development of the library can continue at a much more rapid pace.  

Enjoy!
David