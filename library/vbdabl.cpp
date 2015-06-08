/*  ::[: vbDABL (c) 2000 David Goodlad :]::  */

/* 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*  the file readme.txt contains important comments 
     about how this all works and assumptions about
     how things will be called etc.  */

/*  the function vbDABLalphablend16 replaces both
     of the old mode-specific functions (16555 and
	 16565).  the old ones remain intact for
	 backwards-compatibility, but are no longer
	 maintained, and neither are up-to-date.  please
	 use vbDABLalphablend16 from now on.  */

/*  comments for the 'colorblend*' routines are 
     inaccurate... need to be rewritten... so ignore
	 them for now.  */


#include <windows.h>
#include <stdio.h>

#define CCONV _stdcall

short CCONV vbDABLalphablend16(unsigned short iMode, unsigned short bColorKey, unsigned __int16* sPtr, unsigned __int16* dPtr, unsigned short iAlphaVal, unsigned short iWidth, unsigned short iHeight, unsigned short isPitch, unsigned short idPitch, unsigned short ColorKey)
{

	if (iAlphaVal == 0)
		return 0; //there is no need to alphablend when alpha value is 0, as this means the destination is to remain unchanged.

	unsigned __int64 ColorKey64=(unsigned __int64)(ColorKey)+((unsigned __int64)(ColorKey)<<16)+((unsigned __int64)(ColorKey)<<32)+((unsigned __int64)(ColorKey)<<48);
	unsigned __int32 ColorKey32=(unsigned __int32)(ColorKey)+((unsigned __int32)(ColorKey)<<16);
	
	__int16 WidthQuad		=iWidth/4;
	__int16 RemainingWidth	=iWidth-(WidthQuad*4);

	unsigned __int64 Alpha64=(unsigned __int64)(iAlphaVal)+((unsigned __int64)(iAlphaVal)<<16)+((unsigned __int64)(iAlphaVal)<<32)+((unsigned __int64)(iAlphaVal)<<48);

	unsigned __int64 Add256=0x0100010001000100;

	unsigned __int32 AddS=isPitch-WidthQuad*8;
	unsigned __int32 AddD=idPitch-WidthQuad*8;

	unsigned __int64 RedMask;
	unsigned __int64 GreenMask;
	unsigned __int64 BlueMask;

	switch (iMode)
	{
	case 555:
		RedMask		=0x7C007C007C007C00;
		GreenMask	=0x03E003E003E003E0;
		BlueMask	=0x001F001F001F001F;

		if (bColorKey != 0) {
			__asm
			{
				emms							//Prepare mmx registers for use
				push	esi						//Save just in case
				push	edi						//Save just in case
				xor		edx,edx					//Clear the edx register
				xor		ecx,ecx					//Clear the ecx register

                mov     esi,sPtr				//Move the source pointer to esi - avoid reading AGI stalls, reading from mem, etc
                mov     edi,dPtr				//Move the dest pointer to edi - same reasons
				
				movq	mm3,GreenMask			//Copy the green mask to mm3	
				movq	mm4,BlueMask			//Copy the blue mask to mm4		>- all to avoid reading from mem during inner loop
				movq	mm5,Alpha64				//Copy the alpha values to mm5	

                mov     dx,iHeight				//Copy the height of the area we want to blit to the dx register

NextLine555ck:										
                mov     cx,WidthQuad			//Copy the width of the area to blit divided by 4 to the cx register

NextPixels555ck:
				//This next 6 lines does initial colorkey checking.  It simply sees whether
				//ALL 4 pixels are equal to the transparent color; if so, we can skip doing
				//any work at all on these pixels, and go to the IncPointers label.
				mov		eax,[esi]					//Get the first 2 source pixels
				cmp		eax,dword ptr ColorKey32	//Compare these with the colorkey
				jne		BlendPixels555ck			//If the pixels weren't all the same as the colorkey, we know that we have to do alphablending
				mov		ebx,[esi+4]					//Get the second 2 source pixels
				cmp		ebx,dword ptr ColorKey32	//Compare these with the colorkey
				je		IncPointers555ck			//If the pixels were the same, therefore all 4 were the same, and we can skip the blend on them and leave the destination unchanged.

BlendPixels555ck:
                movq	mm6,[esi]				//Get the current set of 4 source pixels to mm6
				movq	mm7,[edi]				//Get the current set of 4 dest pixels to mm7

				movq	mm0,mm6					//Copy the current set of 4 source pixels to mm0
				movq	mm1,mm7					//Copy the current set of 4 dest pixels to mm1

				psrlw	mm0,10					//Shift the source pixels right - no need for the redmask, cause the other colors get shifted out
				psrlw	mm1,10					//Shift the dest pixels right - same reason for no redmask
				paddw	mm0,Add256				//Add 256 to the source pixels - this ensures that we never get a negative number in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication by the alpha values
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values
				psllw	mm0,10					//Shift the new pixel values into their correct bit-positions
				movq	mm2,mm0					//Copy the new red values to mm2 for storage

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm3					//AND the source pixels with the greenmask, to isolate the green values
				pand	mm1,mm3					//AND the dest pixels with the greenmask
				psrlw	mm0,5					//Shift the source pixels right
				psrlw	mm1,5					//Shift the dest pixels right
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				psllw	mm0,5					//Shift the new pixel values into their correct bit-positions
				pand	mm0,mm3					//AND the pixel values with the greenmask - just in case
				por		mm2,mm0					//OR the new green values with the red ones already in mm2

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm4					//AND the source pixels with the bluemask
				pand	mm1,mm4					//AND the source pixels with the bluemask
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				pand	mm0,mm4					//AND the pixel values with the bluemask - just in case
				por		mm2,mm0					//OR the new blue values with the red and green ones - we now have fully blended pixels!

				pcmpeqw	mm6,ColorKey64			//Compare the original 4 source pixels with the colorkey.  Puts all 1's in the pixel if it was equal, all 0's in the pixel if it was not.
				pand	mm7,mm6					//Leave destination pixels that correspond to transparent source pixels unchanged, but 0-out the ones that correspond with non-transparent source pixels
				pandn	mm6,mm2					//Copies only newly blended pixels that are not transparent to mm6
				por		mm6,mm7					//Fill in the spaces left by transparent pixels in mm6 with destination pixels from mm7

                movq    [edi],mm6				//Copy the final set of 4 pixels to the destination surface

IncPointers555ck:
                add     esi,8					//Increase the source pointer to go to the next pixels
                add     edi,8					//Increase the dest pointer to go the the next pixels

                dec     cx						//Decrement the width value
                jnz     NextPixels555ck			//If it's not zero, then head back up and treat the next pixels in the line

                add     esi,AddS				//Add the AddS value to set the source pointer to the first pixel in the next line
                add     edi,AddD				//Add the AddD value to set the dest pointer to the first pixel in the next line
                dec     dx						//Decrement the remaining height value
                jnz     NextLine555ck			//If height is not zero, then head back up to the very top to the NextLine label

												//We're done blending all the requested pixels.

                emms							//Clean up the mmx registers

				pop		edi						//Return edi to what it was when we started
				pop		esi						//Return esi to what it was when we started
			}

			if(RemainingWidth>0)
			{
				unsigned int i=iHeight;
				unsigned int j=RemainingWidth;
				unsigned __int16 sTemp=0;
				unsigned __int16 dTemp=0;

				sPtr += iWidth - RemainingWidth;
				dPtr += iWidth - RemainingWidth;

				do
				{
					j=RemainingWidth;
					do
					{
						sTemp = *sPtr++;
						dTemp = *dPtr;
						if (sTemp != ColorKey) {
							*dPtr++ =(((iAlphaVal * (((sTemp>>10) & 0x1F) - ((dTemp>>10) & 0x1F)) >> 8) + ((dTemp>>10) & 0x1F))<<10) |
									 (((iAlphaVal * (((sTemp>>5) & 0x1F) - ((dTemp>>5) & 0x1F)) >> 8) + ((dTemp>>5) & 0x1F))<<5) |
									 ((iAlphaVal * ((sTemp & 0x1F) - (dTemp & 0x1F)) >> 8) + (dTemp & 0x1F));
						} else {
							dPtr++;
						}
					} while (--j > 0);
					sPtr += (isPitch >> 1) - RemainingWidth;
					dPtr += (idPitch >> 1) - RemainingWidth;
				} while (--i > 0);	
			}

		} else {
			__asm
			{
				emms							//Prepare mmx registers for use
				push	esi						//Save just in case
				push	edi						//Save just in case
				xor		edx,edx					//Clear the edx register
				xor		ecx,ecx					//Clear the ecx register

                mov     esi,sPtr				//Move the source pointer to esi - avoid reading AGI stalls, reading from mem, etc
                mov     edi,dPtr				//Move the dest pointer to edi - same reasons
				
				movq	mm3,GreenMask			//Copy the green mask to mm3	
				movq	mm4,BlueMask			//Copy the blue mask to mm4		>- all to avoid reading from mem during inner loop
				movq	mm5,Alpha64				//Copy the alpha values to mm5	

                mov     dx,iHeight				//Copy the height of the area we want to blit to the dx register

NextLine555nock:										
                mov     cx,WidthQuad			//Copy the width of the area to blit divided by 4 to the cx register

NextPixels555nock:
                movq	mm6,[esi]				//Get the current set of 4 source pixels to mm6
				movq	mm7,[edi]				//Get the current set of 4 dest pixels to mm7

				movq	mm0,mm6					//Copy the current set of 4 source pixels to mm0
				movq	mm1,mm7					//Copy the current set of 4 dest pixels to mm1

				psrlw	mm0,10					//Shift the source pixels right - no need for the redmask, cause the other colors get shifted out
				psrlw	mm1,10					//Shift the dest pixels right - same reason for no redmask
				paddw	mm0,Add256				//Add 256 to the source pixels - this ensures that we never get a negative number in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication by the alpha values
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values
				psllw	mm0,10					//Shift the new pixel values into their correct bit-positions
				movq	mm2,mm0					//Copy the new red values to mm2 for storage

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm3					//AND the source pixels with the greenmask, to isolate the green values
				pand	mm1,mm3					//AND the dest pixels with the greenmask
				psrlw	mm0,5					//Shift the source pixels right
				psrlw	mm1,5					//Shift the dest pixels right
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				psllw	mm0,5					//Shift the new pixel values into their correct bit-positions
				pand	mm0,mm3					//AND the pixel values with the greenmask - just in case
				por		mm2,mm0					//OR the new green values with the red ones already in mm2

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm4					//AND the source pixels with the bluemask
				pand	mm1,mm4					//AND the source pixels with the bluemask
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				pand	mm0,mm4					//AND the pixel values with the bluemask - just in case
				por		mm2,mm0					//OR the new blue values with the red and green ones - we now have fully blended pixels!

                movq    [edi],mm2				//Copy the final set of 4 pixels to the destination surface

IncPointers555nock:
                add     esi,8					//Increase the source pointer to go to the next pixels
                add     edi,8					//Increase the dest pointer to go the the next pixels

                dec     cx						//Decrement the width value
                jnz     NextPixels555nock			//If it's not zero, then head back up and treat the next pixels in the line

                add     esi,AddS				//Add the AddS value to set the source pointer to the first pixel in the next line
                add     edi,AddD				//Add the AddD value to set the dest pointer to the first pixel in the next line
                dec     dx						//Decrement the remaining height value
                jnz     NextLine555nock			//If height is not zero, then head back up to the very top to the NextLine label

												//We're done blending all the requested pixels.

                emms							//Clean up the mmx registers

				pop		edi						//Return edi to what it was when we started
				pop		esi						//Return esi to what it was when we started
			}

			if(RemainingWidth>0)
			{
				unsigned int i=iHeight;
				unsigned int j=RemainingWidth;
				unsigned __int16 sTemp=0;
				unsigned __int16 dTemp=0;

				sPtr += iWidth - RemainingWidth;
				dPtr += iWidth - RemainingWidth;

				do
				{
					j=RemainingWidth;
					do
					{
						sTemp = *sPtr++;
						dTemp = *dPtr;

						*dPtr++ =(((iAlphaVal * (((sTemp>>10) & 0x1F) - ((dTemp>>10) & 0x1F)) >> 8) + ((dTemp>>10) & 0x1F))<<10) |
									 (((iAlphaVal * (((sTemp>>5) & 0x1F) - ((dTemp>>5) & 0x1F)) >> 8) + ((dTemp>>5) & 0x1F))<<5) |
									 ((iAlphaVal * ((sTemp & 0x1F) - (dTemp & 0x1F)) >> 8) + (dTemp & 0x1F));
					} while (--j > 0);
					sPtr += (isPitch >> 1) - RemainingWidth;
					dPtr += (idPitch >> 1) - RemainingWidth;
				} while (--i > 0);	
			}
		}
		break;
	case 565:
        RedMask		=0xF800F800F800F800;		
        GreenMask	=0x07E007E007E007E0;		
        BlueMask	=0x001F001F001F001F;	

		if (bColorKey != 0) {
			__asm
			{
				emms							//Prepare mmx registers for use
				push	esi						//Save just in case
				push	edi						//Save just in case
				xor		edx,edx					//Clear the edx register
				xor		ecx,ecx					//Clear the ecx register

                mov     esi,sPtr				//Move the source pointer to esi - avoid reading AGI stalls, reading from mem, etc
                mov     edi,dPtr				//Move the dest pointer to edi - same reasons
				
				movq	mm3,GreenMask			//Copy the green mask to mm3	
				movq	mm4,BlueMask			//Copy the blue mask to mm4		>- all to avoid reading from mem during inner loop
				movq	mm5,Alpha64				//Copy the alpha values to mm5	

                mov     dx,iHeight				//Copy the height of the area we want to blit to the dx register

NextLine565ck:										
                mov     cx,WidthQuad			//Copy the width of the area to blit divided by 4 to the cx register

NextPixels565ck:
				//This next 6 lines does initial colorkey checking.  It simply sees whether
				//ALL 4 pixels are equal to the transparent color; if so, we can skip doing
				//any work at all on these pixels, and go to the IncPointers label.
				mov		eax,[esi]					//Get the first 2 source pixels
				cmp		eax,dword ptr ColorKey32	//Compare these with the colorkey
				jne		BlendPixels565ck			//If the pixels weren't all the same as the colorkey, we know that we have to do alphablending
				mov		ebx,[esi+4]					//Get the second 2 source pixels
				cmp		ebx,dword ptr ColorKey32	//Compare these with the colorkey
				je		IncPointers565ck			//If the pixels were the same, therefore all 4 were the same, and we can skip the blend on them and leave the destination unchanged.

BlendPixels565ck:
                movq	mm6,[esi]				//Get the current set of 4 source pixels to mm6
				movq	mm7,[edi]				//Get the current set of 4 dest pixels to mm7

				movq	mm0,mm6					//Copy the current set of 4 source pixels to mm0
				movq	mm1,mm7					//Copy the current set of 4 dest pixels to mm1

				psrlw	mm0,11					//Shift the source pixels right - no need for the redmask, cause the other colors get shifted out
				psrlw	mm1,11					//Shift the dest pixels right - same reason for no redmask
				paddw	mm0,Add256				//Add 256 to the source pixels - this ensures that we never get a negative number in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication by the alpha values
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values
				psllw	mm0,11					//Shift the new pixel values into their correct bit-positions
				movq	mm2,mm0					//Copy the new red values to mm2 for storage

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm3					//AND the source pixels with the greenmask, to isolate the green values
				pand	mm1,mm3					//AND the dest pixels with the greenmask
				psrlw	mm0,5					//Shift the source pixels right
				psrlw	mm1,5					//Shift the dest pixels right
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				psllw	mm0,5					//Shift the new pixel values into their correct bit-positions
				pand	mm0,mm3					//AND the pixel values with the greenmask - just in case
				por		mm2,mm0					//OR the new green values with the red ones already in mm2

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm4					//AND the source pixels with the bluemask
				pand	mm1,mm4					//AND the source pixels with the bluemask
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				pand	mm0,mm4					//AND the pixel values with the bluemask - just in case
				por		mm2,mm0					//OR the new blue values with the red and green ones - we now have fully blended pixels!

				pcmpeqw	mm6,ColorKey64			//Compare the original 4 source pixels with the colorkey.  Puts all 1's in the pixel if it was equal, all 0's in the pixel if it was not.
				pand	mm7,mm6					//Leave destination pixels that correspond to transparent source pixels unchanged, but 0-out the ones that correspond with non-transparent source pixels
				pandn	mm6,mm2					//Copies only newly blended pixels that are not transparent to mm6
				por		mm6,mm7					//Fill in the spaces left by transparent pixels in mm6 with destination pixels from mm7

                movq    [edi],mm6				//Copy the final set of 4 pixels to the destination surface

IncPointers565ck:
                add     esi,8					//Increase the source pointer to go to the next pixels
                add     edi,8					//Increase the dest pointer to go the the next pixels

                dec     cx						//Decrement the width value
                jnz     NextPixels565ck			//If it's not zero, then head back up and treat the next pixels in the line

                add     esi,AddS				//Add the AddS value to set the source pointer to the first pixel in the next line
                add     edi,AddD				//Add the AddD value to set the dest pointer to the first pixel in the next line
                dec     dx						//Decrement the remaining height value
                jnz     NextLine565ck			//If height is not zero, then head back up to the very top to the NextLine label

												//We're done blending all the requested pixels.

                emms							//Clean up the mmx registers

				pop		edi						//Return edi to what it was when we started
				pop		esi						//Return esi to what it was when we started
			}

			if(RemainingWidth>0)
			{
				unsigned int i=iHeight;
				unsigned int j=RemainingWidth;
				unsigned __int16 sTemp=0;
				unsigned __int16 dTemp=0;

				sPtr += iWidth - RemainingWidth;
				dPtr += iWidth - RemainingWidth;

				do
				{
					j=RemainingWidth;
					do
					{
						sTemp = *sPtr++;
						dTemp = *dPtr;
						if (sTemp != ColorKey) {
							*dPtr++ =(((iAlphaVal * (((sTemp>>11) & 0x1F) - ((dTemp>>11) & 0x1F)) >> 8) + ((dTemp>>11) & 0x1F))<<11) |
									 (((iAlphaVal * (((sTemp>>5) & 0x1F) - ((dTemp>>5) & 0x1F)) >> 8) + ((dTemp>>5) & 0x1F))<<5) |
									 ((iAlphaVal * ((sTemp & 0x1F) - (dTemp & 0x1F)) >> 8) + (dTemp & 0x1F));
						} else {
							dPtr++;
						}
					} while (--j > 0);
					sPtr += (isPitch >> 1) - RemainingWidth;
					dPtr += (idPitch >> 1) - RemainingWidth;
				} while (--i > 0);	
			}
		} else {
			__asm
			{
				emms							//Prepare mmx registers for use
				push	esi						//Save just in case
				push	edi						//Save just in case
				xor		edx,edx					//Clear the edx register
				xor		ecx,ecx					//Clear the ecx register

                mov     esi,sPtr				//Move the source pointer to esi - avoid reading AGI stalls, reading from mem, etc
                mov     edi,dPtr				//Move the dest pointer to edi - same reasons
				
				movq	mm3,GreenMask			//Copy the green mask to mm3	
				movq	mm4,BlueMask			//Copy the blue mask to mm4		>- all to avoid reading from mem during inner loop
				movq	mm5,Alpha64				//Copy the alpha values to mm5	

                mov     dx,iHeight				//Copy the height of the area we want to blit to the dx register

NextLine565nock:										
                mov     cx,WidthQuad			//Copy the width of the area to blit divided by 4 to the cx register

NextPixels565nock:
                movq	mm6,[esi]				//Get the current set of 4 source pixels to mm6
				movq	mm7,[edi]				//Get the current set of 4 dest pixels to mm7

				movq	mm0,mm6					//Copy the current set of 4 source pixels to mm0
				movq	mm1,mm7					//Copy the current set of 4 dest pixels to mm1

				psrlw	mm0,11					//Shift the source pixels right - no need for the redmask, cause the other colors get shifted out
				psrlw	mm1,11					//Shift the dest pixels right - same reason for no redmask
				paddw	mm0,Add256				//Add 256 to the source pixels - this ensures that we never get a negative number in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication by the alpha values
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values
				psllw	mm0,11					//Shift the new pixel values into their correct bit-positions
				movq	mm2,mm0					//Copy the new red values to mm2 for storage

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm3					//AND the source pixels with the greenmask, to isolate the green values
				pand	mm1,mm3					//AND the dest pixels with the greenmask
				psrlw	mm0,5					//Shift the source pixels right
				psrlw	mm1,5					//Shift the dest pixels right
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				psllw	mm0,5					//Shift the new pixel values into their correct bit-positions
				pand	mm0,mm3					//AND the pixel values with the greenmask - just in case
				por		mm2,mm0					//OR the new green values with the red ones already in mm2

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm4					//AND the source pixels with the bluemask
				pand	mm1,mm4					//AND the source pixels with the bluemask
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				pand	mm0,mm4					//AND the pixel values with the bluemask - just in case
				por		mm2,mm0					//OR the new blue values with the red and green ones - we now have fully blended pixels!

                movq    [edi],mm2				//Copy the final set of 4 pixels to the destination surface

IncPointers565nock:
                add     esi,8					//Increase the source pointer to go to the next pixels
                add     edi,8					//Increase the dest pointer to go the the next pixels

                dec     cx						//Decrement the width value
                jnz     NextPixels565nock			//If it's not zero, then head back up and treat the next pixels in the line

                add     esi,AddS				//Add the AddS value to set the source pointer to the first pixel in the next line
                add     edi,AddD				//Add the AddD value to set the dest pointer to the first pixel in the next line
                dec     dx						//Decrement the remaining height value
                jnz     NextLine565nock			//If height is not zero, then head back up to the very top to the NextLine label

												//We're done blending all the requested pixels.

                emms							//Clean up the mmx registers

				pop		edi						//Return edi to what it was when we started
				pop		esi						//Return esi to what it was when we started
			}

			if(RemainingWidth>0)
			{
				unsigned int i=iHeight;
				unsigned int j=RemainingWidth;
				unsigned __int16 sTemp=0;
				unsigned __int16 dTemp=0;

				sPtr += iWidth - RemainingWidth;
				dPtr += iWidth - RemainingWidth;

				do
				{
					j=RemainingWidth;
					do
					{
						sTemp = *sPtr++;
						dTemp = *dPtr;

						*dPtr++ =(((iAlphaVal * (((sTemp>>11) & 0x1F) - ((dTemp>>11) & 0x1F)) >> 8) + ((dTemp>>11) & 0x1F))<<11) |
									 (((iAlphaVal * (((sTemp>>5) & 0x1F) - ((dTemp>>5) & 0x1F)) >> 8) + ((dTemp>>5) & 0x1F))<<5) |
									 ((iAlphaVal * ((sTemp & 0x1F) - (dTemp & 0x1F)) >> 8) + (dTemp & 0x1F));
					} while (--j > 0);
					sPtr += (isPitch >> 1) - RemainingWidth;
					dPtr += (idPitch >> 1) - RemainingWidth;
				} while (--i > 0);	
			}
		}
		break;		
	}
	return 1;
}

int     CCONV vbDABLcolorblend16555(__int32 sPtr, __int32 dPtr, __int32 alpha_val, __int32 width, __int32 height, __int32 sPitch, __int32 dPitch, __int32 rVal, __int32 gVal, __int32 bVal)
{
		__int64 RedMask=8935278002225314816;	// 7C007C007C007C00 in decimal - don't ask why :P
		__int64 GreenMask=279227437569541088;	// 03E003E003E003E0 in decimal
		__int64 BlueMask=8725857424048159;		// 001F001F001F001F in decimal

		__int16 alpha=255-alpha_val;			// The inverse of the alpha_val - for use with the source surface

		__int16 widthquad=width/4;				// How many sets of 4 pixels wide we can do

		__int64 Alphad=__int64(alpha_val)+(__int64(alpha_val)<<16)+(__int64(alpha_val)<<32)+(__int64(alpha_val)<<48);		// Create a 64-bit integer from the alpha value for 4 pixels (16bits*4pixels=64bits)
        __int64 Alphas=__int64(alpha)+(__int64(alpha)<<16)+(__int64(alpha)<<32)+(__int64(alpha)<<48);				// Again but with the inverse alpha value

		__int32 addspos=sPitch-widthquad*8;		// How many bytes to add to the memory address of the src surface to get to the first pixel to be treated in the next line
		__int32 adddpos=dPitch-widthquad*8;		// Again, but for the dest surface

		//__int64 rVal64=__int64(rVal>>3)+(__int64(rVal)<<16)+(__int64(rVal)<<32)+(__int64(rVal)<<48);
		//__int64 gVal64=__int64((gVal>>3)*alpha_val)+(__int64(((gVal>>3)*alpha_val)<<16))+(__int64(((gVal>>3)*alpha_val)<<32))+(__int64(((gVal>>3)*alpha_val))<<48);
		//__int64 bVal64=__int64((bVal>>3)*alpha_val)+(__int64(((bVal>>3)*alpha_val)<<16))+(__int64(((bVal>>3)*alpha_val)<<32))+(__int64(((bVal>>3)*alpha_val))<<48);

		//rVal=0;
		//gVal=0;
		//bVal=0;

		__int64 rVal64=__int64(rVal>>3)+(__int64(rVal>>3)<<16)+(__int64(rVal>>3)<<32)+(__int64(rVal>>3)<<48);
		__int64 gVal64=__int64(gVal>>3)+(__int64(gVal>>3)<<16)+(__int64(gVal>>3)<<32)+(__int64(gVal>>3)<<48);
		__int64 bVal64=__int64(bVal>>3)+(__int64(bVal>>3)<<16)+(__int64(bVal>>3)<<32)+(__int64(bVal>>3)<<48);

		//__int64 colorVal64=(rVal64<<10)+(gVal64<<5)+bVal64

		__asm
		{
                mov     esi,sPtr			// So we don't have to refer to memory, copy the pointer to the src surface to a register
                mov     edi,dPtr			// Again for destination surface ptr
				movq	mm7,Alphad
                movq    mm4,bVal64			// Copy the inverse alpha values to the mm4 register
				pmullw	mm4,mm7
				movq	mm1,rVal64
				pmullw	mm1,mm7
				movq	mm3,gVal64
				pmullw	mm3,mm7
				movq    mm7,Alphas			// Copy the source alphavalues to the mm7 register
                mov     edx,height			// Copy the height of the region to be blitted to the dx register
NextLine:
                mov     cx,widthquad		// Every time we start a new line, reset the cx register to how many times we need to treat 4 pixels in a line
NextPixels:
                movq    mm0,[esi]			// Get 4 source pixel values

                movq    mm2,mm0				// Make a copy of the source pixel value

                pand    mm2,RedMask			// Isolate the red values in the source pixels
                psrlw   mm2,10				// Shift the src pixels right to align them to the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm1				// Now add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 essentially - we have multiplied in total by 255 in the src and dest pixels, so now we've gotta reverse that process
                psllw   mm2,10				// Move the values back to their original bit positions
                movq    mm6,mm2				// Copy the blended red colors to mm6 where the new pixels will be built

                movq    mm2,mm0				// Reset the src pixel value so we get all the colors back

                pand    mm2,GreenMask		// Isolate the green values in the src pixels
                psrlw   mm2,5				// Shift the src pixels right to align them with the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm3				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                psllw   mm2,5				// Return the green values to their original bit positions
                por     mm6,mm2				// Copy the blended green colors to mm6 - we're 2/3 of the way there!

                movq    mm2,mm0				// Reset the src pixel value again

                pand    mm2,BlueMask		// Isolate the blue values in the src pixels
                pmullw  mm2,mm7				// Blue pixels are already aligned with alpha values so we skip to multiplying them by the alpha value
                paddw   mm2,mm4				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                por     mm6,mm2				// Copy the blended blue colors to mm6 - the pixels have now been completely blended!

                movq    [edi],mm6			// Copy the new pixels to the destination

                add     esi,8				// Set the src pointer to point at the next set of 4 pixels
                add     edi,8				// Set the dest pointer to point at the next set of 4 pixels

                dec     ecx					// Decrement the width counter
                jnz     NextPixels			// If we've still got more pixels to treat, jump back to the beginning to treat more, otherwise continue on......

                add     esi,addspos			// Set the src pointer to the next line
                add     edi,adddpos			// Set the dest pointer to the next line
                dec     edx					// Decrement the height counter
                jnz     NextLine			// If we've still got lines to treat, jump back to the beginning and get going!  otherwise.......

                emms						// Clean up our mmx registers		
		}
		return 1;
}
			

int     CCONV vbDABLcolorblend16565(__int32 sPtr, __int32 dPtr, __int32 alpha_val, __int32 width, __int32 height, __int32 sPitch, __int32 dPitch, __int32 rVal, __int32 gVal, __int32 bVal)
{
		__int64 RedMask=0xF800F800F800F800;	
		__int64 GreenMask=0x07E007E007E007E0;	
		__int64 BlueMask=8725857424048159;		// 001F001F001F001F in decimal

		__int16 alpha=255-alpha_val;			// The inverse of the alpha_val - for use with the source surface

		__int16 widthquad=width/4;				// How many sets of 4 pixels wide we can do

		__int64 Alphad=__int64(alpha_val)+(__int64(alpha_val)<<16)+(__int64(alpha_val)<<32)+(__int64(alpha_val)<<48);		// Create a 64-bit integer from the alpha value for 4 pixels (16bits*4pixels=64bits)
        __int64 Alphas=__int64(alpha)+(__int64(alpha)<<16)+(__int64(alpha)<<32)+(__int64(alpha)<<48);				// Again but with the inverse alpha value

		__int32 addspos=sPitch-widthquad*8;		// How many bytes to add to the memory address of the src surface to get to the first pixel to be treated in the next line
		__int32 adddpos=dPitch-widthquad*8;		// Again, but for the dest surface

		//__int64 rVal64=__int64(rVal>>3)+(__int64(rVal)<<16)+(__int64(rVal)<<32)+(__int64(rVal)<<48);
		//__int64 gVal64=__int64((gVal>>3)*alpha_val)+(__int64(((gVal>>3)*alpha_val)<<16))+(__int64(((gVal>>3)*alpha_val)<<32))+(__int64(((gVal>>3)*alpha_val))<<48);
		//__int64 bVal64=__int64((bVal>>3)*alpha_val)+(__int64(((bVal>>3)*alpha_val)<<16))+(__int64(((bVal>>3)*alpha_val)<<32))+(__int64(((bVal>>3)*alpha_val))<<48);

		//rVal=0;
		//gVal=0;
		//bVal=0;

		__int64 rVal64=__int64(rVal>>3)+(__int64(rVal>>3)<<16)+(__int64(rVal>>3)<<32)+(__int64(rVal>>3)<<48);
		__int64 gVal64=__int64(gVal>>2)+(__int64(gVal>>2)<<16)+(__int64(gVal>>2)<<32)+(__int64(gVal>>2)<<48);
		__int64 bVal64=__int64(bVal>>3)+(__int64(bVal>>3)<<16)+(__int64(bVal>>3)<<32)+(__int64(bVal>>3)<<48);

		__asm
		{
                mov     esi,sPtr			// So we don't have to refer to memory, copy the pointer to the src surface to a register
                mov     edi,dPtr			// Again for destination surface ptr
				movq	mm7,Alphad
                movq    mm4,bVal64			// Copy the inverse alpha values to the mm4 register
				pmullw	mm4,mm7
				movq	mm1,rVal64
				pmullw	mm1,mm7
				movq	mm3,gVal64
				pmullw	mm3,mm7
				movq    mm7,Alphas			// Copy the source alphavalues to the mm7 register
                mov     edx,height			// Copy the height of the region to be blitted to the dx register
NextLine:
                mov     cx,widthquad		// Every time we start a new line, reset the cx register to how many times we need to treat 4 pixels in a line
NextPixels:
                movq    mm0,[esi]			// Get 4 source pixel values

                movq    mm2,mm0				// Make a copy of the source pixel value

                pand    mm2,RedMask			// Isolate the red values in the source pixels
                psrlw   mm2,11				// Shift the src pixels right to align them to the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm1				// Now add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 essentially - we have multiplied in total by 255 in the src and dest pixels, so now we've gotta reverse that process
                psllw   mm2,11				// Move the values back to their original bit positions
                movq    mm6,mm2				// Copy the blended red colors to mm6 where the new pixels will be built

                movq    mm2,mm0				// Reset the src pixel value so we get all the colors back

                pand    mm2,GreenMask		// Isolate the green values in the src pixels
                psrlw   mm2,5				// Shift the src pixels right to align them with the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm3				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                psllw   mm2,5				// Return the green values to their original bit positions
                por     mm6,mm2				// Copy the blended green colors to mm6 - we're 2/3 of the way there!

                movq    mm2,mm0				// Reset the src pixel value again

                pand    mm2,BlueMask		// Isolate the blue values in the src pixels
                pmullw  mm2,mm7				// Blue pixels are already aligned with alpha values so we skip to multiplying them by the alpha value
                paddw   mm2,mm4				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                por     mm6,mm2				// Copy the blended blue colors to mm6 - the pixels have now been completely blended!

                movq    [edi],mm6			// Copy the new pixels to the destination

                add     esi,8				// Set the src pointer to point at the next set of 4 pixels
                add     edi,8				// Set the dest pointer to point at the next set of 4 pixels

                dec     ecx					// Decrement the width counter
                jnz     NextPixels			// If we've still got more pixels to treat, jump back to the beginning to treat more, otherwise continue on......

                add     esi,addspos			// Set the src pointer to the next line
                add     edi,adddpos			// Set the dest pointer to the next line
                dec     edx					// Decrement the height counter
                jnz     NextLine			// If we've still got lines to treat, jump back to the beginning and get going!  otherwise.......

                emms						// Clean up our mmx registers		
		}
		return 1;
}







int     CCONV vbDABLcolorblend16555ck(__int32 sPtr, __int32 dPtr, __int32 alpha_val, __int32 width, __int32 height, __int32 sPitch, __int32 dPitch, __int32 rVal, __int32 gVal, __int32 bVal)
{
		__int64 RedMask=8935278002225314816;	// 7C007C007C007C00 in decimal - don't ask why :P
		__int64 GreenMask=279227437569541088;	// 03E003E003E003E0 in decimal
		__int64 BlueMask=8725857424048159;		// 001F001F001F001F in decimal

		__int16 alpha=255-alpha_val;			// The inverse of the alpha_val - for use with the source surface

		__int16 widthquad=width/4;				// How many sets of 4 pixels wide we can do

		__int64 Alphad=__int64(alpha_val)+(__int64(alpha_val)<<16)+(__int64(alpha_val)<<32)+(__int64(alpha_val)<<48);		// Create a 64-bit integer from the alpha value for 4 pixels (16bits*4pixels=64bits)
        __int64 Alphas=__int64(alpha)+(__int64(alpha)<<16)+(__int64(alpha)<<32)+(__int64(alpha)<<48);				// Again but with the inverse alpha value

		__int32 addspos=sPitch-widthquad*8;		// How many bytes to add to the memory address of the src surface to get to the first pixel to be treated in the next line
		__int32 adddpos=dPitch-widthquad*8;		// Again, but for the dest surface

		__int64 colkey=0;

		__int64 rVal64=__int64(rVal>>3)+(__int64(rVal>>3)<<16)+(__int64(rVal>>3)<<32)+(__int64(rVal>>3)<<48);
		__int64 gVal64=__int64(gVal>>3)+(__int64(gVal>>3)<<16)+(__int64(gVal>>3)<<32)+(__int64(gVal>>3)<<48);
		__int64 bVal64=__int64(bVal>>3)+(__int64(bVal>>3)<<16)+(__int64(bVal>>3)<<32)+(__int64(bVal>>3)<<48);

		__asm
		{
                mov     esi,sPtr			// So we don't have to refer to memory, copy the pointer to the src surface to a register
                mov     edi,dPtr			// Again for destination surface ptr
				movq	mm7,Alphad
                movq    mm4,bVal64			// Copy the inverse alpha values to the mm4 register
				pmullw	mm4,mm7
				movq	mm1,rVal64
				pmullw	mm1,mm7
				movq	mm3,gVal64
				pmullw	mm3,mm7
				movq    mm7,Alphas			// Copy the source alphavalues to the mm7 register
                mov     edx,height			// Copy the height of the region to be blitted to the dx register
NextLine:
                mov     cx,widthquad		// Every time we start a new line, reset the cx register to how many times we need to treat 4 pixels in a line
NextPixels:
				mov		eax,[esi]
				cmp		eax,dword ptr colkey
				jne		BlendPixels
				mov		ebx,[esi+4]
				cmp		ebx,dword ptr colkey
				je		SkipPixels
BlendPixels:
                movq    mm0,[esi]			// Get 4 source pixel values
				movq	mm5,[edi]

                movq    mm2,mm0				// Make a copy of the source pixel value

                pand    mm2,RedMask			// Isolate the red values in the source pixels
                psrlw   mm2,10				// Shift the src pixels right to align them to the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm1				// Now add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 essentially - we have multiplied in total by 255 in the src and dest pixels, so now we've gotta reverse that process
                psllw   mm2,10				// Move the values back to their original bit positions
                movq    mm6,mm2				// Copy the blended red colors to mm6 where the new pixels will be built

                movq    mm2,mm0				// Reset the src pixel value so we get all the colors back

                pand    mm2,GreenMask		// Isolate the green values in the src pixels
                psrlw   mm2,5				// Shift the src pixels right to align them with the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm3				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                psllw   mm2,5				// Return the green values to their original bit positions
                por     mm6,mm2				// Copy the blended green colors to mm6 - we're 2/3 of the way there!

                movq    mm2,mm0				// Reset the src pixel value again

                pand    mm2,BlueMask		// Isolate the blue values in the src pixels
                pmullw  mm2,mm7				// Blue pixels are already aligned with alpha values so we skip to multiplying them by the alpha value
                paddw   mm2,mm4				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                por     mm6,mm2				// Copy the blended blue colors to mm6 - the pixels have now been completely blended!

				pcmpeqw	mm0,colkey
				pand	mm5,mm0
				pandn	mm0,mm6
				por		mm0,mm5

                movq    [edi],mm0			// Copy the new pixels to the destination

                add     esi,8				// Set the src pointer to point at the next set of 4 pixels
                add     edi,8				// Set the dest pointer to point at the next set of 4 pixels

                dec     ecx					// Decrement the width counter
                jnz     NextPixels			// If we've still got more pixels to treat, jump back to the beginning to treat more, otherwise continue on......

                add     esi,addspos			// Set the src pointer to the next line
                add     edi,adddpos			// Set the dest pointer to the next line
                dec     edx					// Decrement the height counter
                jnz     NextLine			// If we've still got lines to treat, jump back to the beginning and get going!  otherwise.......
				jmp		Done

SkipPixels:
                add     esi,8				// Set the src pointer to point at the next set of 4 pixels
                add     edi,8				// Set the dest pointer to point at the next set of 4 pixels

                dec     ecx					// Decrement the width counter
                jnz     NextPixels			// If we've still got more pixels to treat, jump back to the beginning to treat more, otherwise continue on......

                add     esi,addspos			// Set the src pointer to the next line
                add     edi,adddpos			// Set the dest pointer to the next line
                dec     edx					// Decrement the height counter
                jnz     NextLine			// If we've still got lines to treat, jump back to the beginning and get going!  otherwise.......

Done:
                emms						// Clean up our mmx registers		
		}
		return 1;
}

int     CCONV vbDABLcolorblend16565ck(__int32 sPtr, __int32 dPtr, __int32 alpha_val, __int32 width, __int32 height, __int32 sPitch, __int32 dPitch, __int32 rVal, __int32 gVal, __int32 bVal)
{
		__int64 RedMask=0xF800F800F800F800;	
		__int64 GreenMask=0x07E007E007E007E0;	
		__int64 BlueMask=8725857424048159;		// 001F001F001F001F in decimal

		__int16 alpha=255-alpha_val;			// The inverse of the alpha_val - for use with the source surface

		__int16 widthquad=width/4;				// How many sets of 4 pixels wide we can do

		__int64 Alphad=__int64(alpha_val)+(__int64(alpha_val)<<16)+(__int64(alpha_val)<<32)+(__int64(alpha_val)<<48);		// Create a 64-bit integer from the alpha value for 4 pixels (16bits*4pixels=64bits)
        __int64 Alphas=__int64(alpha)+(__int64(alpha)<<16)+(__int64(alpha)<<32)+(__int64(alpha)<<48);				// Again but with the inverse alpha value

		__int32 addspos=sPitch-widthquad*8;		// How many bytes to add to the memory address of the src surface to get to the first pixel to be treated in the next line
		__int32 adddpos=dPitch-widthquad*8;		// Again, but for the dest surface

		__int64 colkey=0;

		__int64 rVal64=__int64(rVal>>3)+(__int64(rVal>>3)<<16)+(__int64(rVal>>3)<<32)+(__int64(rVal>>3)<<48);
		__int64 gVal64=__int64(gVal>>2)+(__int64(gVal>>2)<<16)+(__int64(gVal>>2)<<32)+(__int64(gVal>>2)<<48);
		__int64 bVal64=__int64(bVal>>3)+(__int64(bVal>>3)<<16)+(__int64(bVal>>3)<<32)+(__int64(bVal>>3)<<48);

		__asm
		{
                mov     esi,sPtr			// So we don't have to refer to memory, copy the pointer to the src surface to a register
                mov     edi,dPtr			// Again for destination surface ptr
				movq	mm7,Alphad
                movq    mm4,bVal64			// Copy the inverse alpha values to the mm4 register
				pmullw	mm4,mm7
				movq	mm1,rVal64
				pmullw	mm1,mm7
				movq	mm3,gVal64
				pmullw	mm3,mm7
				movq    mm7,Alphas			// Copy the source alphavalues to the mm7 register
                mov     edx,height			// Copy the height of the region to be blitted to the dx register
NextLine:
                mov     cx,widthquad		// Every time we start a new line, reset the cx register to how many times we need to treat 4 pixels in a line
NextPixels:
				mov		eax,[esi]
				cmp		eax,dword ptr colkey
				jne		BlendPixels
				mov		ebx,[esi+4]
				cmp		ebx,dword ptr colkey
				je		SkipPixels
BlendPixels:
                movq    mm0,[esi]			// Get 4 source pixel values
				movq	mm5,[edi]

                movq    mm2,mm0				// Make a copy of the source pixel value

                pand    mm2,RedMask			// Isolate the red values in the source pixels
                psrlw   mm2,11				// Shift the src pixels right to align them to the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm1				// Now add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 essentially - we have multiplied in total by 255 in the src and dest pixels, so now we've gotta reverse that process
                psllw   mm2,11				// Move the values back to their original bit positions
                movq    mm6,mm2				// Copy the blended red colors to mm6 where the new pixels will be built

                movq    mm2,mm0				// Reset the src pixel value so we get all the colors back

                pand    mm2,GreenMask		// Isolate the green values in the src pixels
                psrlw   mm2,5				// Shift the src pixels right to align them with the alpha values
                pmullw  mm2,mm7				// Multiply the src pixels by the alpha value
                paddw   mm2,mm3				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                psllw   mm2,5				// Return the green values to their original bit positions
                por     mm6,mm2				// Copy the blended green colors to mm6 - we're 2/3 of the way there!

                movq    mm2,mm0				// Reset the src pixel value again

                pand    mm2,BlueMask		// Isolate the blue values in the src pixels
                pmullw  mm2,mm7				// Blue pixels are already aligned with alpha values so we skip to multiplying them by the alpha value
                paddw   mm2,mm4				// Add the src and dest pixels together
                psrlw   mm2,8				// Divide by 256 - why? explained for the red values
                por     mm6,mm2				// Copy the blended blue colors to mm6 - the pixels have now been completely blended!

				pcmpeqw	mm0,colkey
				pand	mm5,mm0
				pandn	mm0,mm6
				por		mm0,mm5

                movq    [edi],mm0			// Copy the new pixels to the destination

                add     esi,8				// Set the src pointer to point at the next set of 4 pixels
                add     edi,8				// Set the dest pointer to point at the next set of 4 pixels

                dec     ecx					// Decrement the width counter
                jnz     NextPixels			// If we've still got more pixels to treat, jump back to the beginning to treat more, otherwise continue on......

                add     esi,addspos			// Set the src pointer to the next line
                add     edi,adddpos			// Set the dest pointer to the next line
                dec     edx					// Decrement the height counter
                jnz     NextLine			// If we've still got lines to treat, jump back to the beginning and get going!  otherwise.......
				jmp		Done

SkipPixels:
                add     esi,8				// Set the src pointer to point at the next set of 4 pixels
                add     edi,8				// Set the dest pointer to point at the next set of 4 pixels

                dec     ecx					// Decrement the width counter
                jnz     NextPixels			// If we've still got more pixels to treat, jump back to the beginning to treat more, otherwise continue on......

                add     esi,addspos			// Set the src pointer to the next line
                add     edi,adddpos			// Set the dest pointer to the next line
                dec     edx					// Decrement the height counter
                jnz     NextLine			// If we've still got lines to treat, jump back to the beginning and get going!  otherwise.......

Done:
                emms						// Clean up our mmx registers		
		}
		return 1;
}

short CCONV vbDABLalphablend16555(unsigned __int16* sPtr, unsigned __int16* dPtr, unsigned short alpha_val, unsigned short width, unsigned short height, unsigned short sPitch, unsigned short dPitch, unsigned short ColorKey)
{

        unsigned __int64 RedMask=0x7C007C007C007C00;
        unsigned __int64 GreenMask=0x3E003E003E003E0;
        unsigned __int64 BlueMask=0x1F001F001F001F;

		unsigned __int64 ColorKey64=(unsigned __int64)(ColorKey)+((unsigned __int64)(ColorKey)<<16)+((unsigned __int64)(ColorKey)<<32)+((unsigned __int64)(ColorKey)<<48);
		unsigned __int32 ColorKey32=(unsigned __int32)(ColorKey)+((unsigned __int32)(ColorKey)<<16);
		unsigned __int64 Full64=0xFFFFFFFFFFFFFFFF;

        __int16 WidthQuad=width/4;			

		__int16 RemainingWidth=width-(WidthQuad*4);
        
        unsigned __int64 Alpha=(unsigned __int64)(alpha_val)+((unsigned __int64)(alpha_val)<<16)+((unsigned __int64)(alpha_val)<<32)+((unsigned __int64)(alpha_val)<<48);
		
		unsigned __int64 Add256=0x0100010001000100;

        unsigned __int32 AddS=sPitch-WidthQuad*8;			
        unsigned __int32 AddD=dPitch-WidthQuad*8;			

		if (alpha_val==0||alpha_val==255) { return 0; }

        __asm
        {
				push	esi						//Save just in case
				push	edi						//Save just in case

                mov     esi,sPtr				//Move the source pointer to esi - avoid reading AGI stalls, reading from mem, etc
                mov     edi,dPtr				//Move the dest pointer to edi - same reasons
				
				movq	mm3,GreenMask			//Copy the green mask to mm3	
				movq	mm4,BlueMask			//Copy the blue mask to mm4		>- all to avoid reading from mem during inner loop
				movq	mm5,Alpha				//Copy the alpha values to mm5	

                mov     dx,height				//Copy the height of the area we want to blit to the dx register

NextLine:										
                mov     cx,WidthQuad			//Copy the width of the area to blit divided by 4 to the cx register

NextPixels:
//				movq	mm0,ColorKey64			//This block does initial colorkey checking - if the entire area is equal to the colorkey then we can skip all these pixels
//				pcmpeqw	mm0,mm6					//Compare the colorkey to the source pixels
//				pandn	mm0,Full64				//AND the result of the comparison with all 1's, then invert it
//				jz		IncPointers				//If we find that the source IS equal to the colorkey, jump down to increment pointers

				mov		eax,[esi]
				cmp		eax,dword ptr ColorKey32
				jne		BlendPixels
				mov		ebx,[esi+4]
				cmp		ebx,dword ptr ColorKey32
				je		IncPointers

BlendPixels:
                movq	mm6,[esi]				//Get the current set of 4 source pixels to mm6
				movq	mm7,[edi]				//Get the current set of 4 dest pixels to mm7

				movq	mm0,mm6					//Copy the current set of 4 source pixels to mm0
				movq	mm1,mm7					//Copy the current set of 4 dest pixels to mm1

				psrlw	mm0,10					//Shift the source pixels right - no need for the redmask, cause the other colors get shifted out
				psrlw	mm1,10					//Shift the dest pixels right - same reason for no redmask
				paddw	mm0,Add256				//ADD 256 to the source pixels - this ensures that we never get a negative number in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication by the alpha values
				paddw	mm0,mm1					//AND the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values
				psllw	mm0,10					//Shift the new pixel values into their correct bit-positions
				movq	mm2,mm0					//Copy the new red values to mm2 for storage

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm3					//AND the source pixels with the greenmask, to isolate the green values
				pand	mm1,mm3					//AND the dest pixels with the greenmask
				psrlw	mm0,5					//Shift the source pixels right
				psrlw	mm1,5					//Shift the dest pixels right
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				psllw	mm0,5					//Shift the new pixel values into their correct bit-positions
				pand	mm0,mm3					//AND the pixel values with the greenmask - just in case
				por		mm2,mm0					//Or the new green values with the red ones already in mm2

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm4					//AND the source pixels with the bluemask
				pand	mm1,mm4					//AND the source pixels with the bluemask
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				pand	mm0,mm4					//AND the pixel values with the bluemask - just in case
				por		mm2,mm0					//Or the new blue values with the red and green ones - we now have full pixels!

//				movq	mm0,ColorKey64			//Another more comprehensive colorkey check - isolates the pixels that we really need to copy
//				pcmpeqw	mm0,mm6					//Compare the colorkey with the original source pixels
//				pand	mm7,mm0					//AND the original destination pixels with the result of the comparison
//				pandn	mm0,mm2					//Invert this result, then AND with the newly created pixels
//				por		mm0,mm7					//OR the destination pixels into it

				pcmpeqw	mm6,ColorKey64
				pand	mm7,mm6
				pandn	mm6,mm2
				por		mm6,mm7

//                movq    [edi],mm0				//Copy the final set of 4 pixels to the destination
                movq    [edi],mm6				//Copy the final set of 4 pixels to the destination

IncPointers:
                add     esi,8					//Increase the source pointer to go to the next pixels
                add     edi,8					//Increase the dest pointer to go the the next pixels

                dec     cx						//Decrement the width value
                jnz     NextPixels				//If it's not zero, then head back up and treat the pixels; otherwise, keep going down.....

                add     esi,AddS				//Add the AddS value to set the source pointer to the first pixel in the next line
                add     edi,AddD				//Add the AddD value to set the dest pointer to the first pixel in the next line
                dec     dx						//Decrement the height value
                jnz     NextLine				//If height is not zero, then head back up to the very top to 'NextLine:'

                emms							//Clean up the mmx registers

				pop		edi						//Return edi to what it was when we started
				pop		esi						//Return esi to what it was when we started
        }

		if(RemainingWidth>0)
		{
			unsigned int i=height;
			unsigned int j=RemainingWidth;
			unsigned __int16 sTemp=0;
			unsigned __int16 dTemp=0;

			sPtr += width;
			dPtr += width;

			do
			{
				j=RemainingWidth;
				do
				{
					sTemp = *sPtr++;
					dTemp = *dPtr;
					*dPtr++ = (((alpha_val * (((sTemp>>10) & 0x1F) - ((dTemp>>10) & 0x1F)) >> 8) + ((dTemp>>10) & 0x1F))<<10) |
							  (((alpha_val * (((sTemp>>5) & 0x1F) - ((dTemp>>5) & 0x1F)) >> 8) + ((dTemp>>5) & 0x1F))<<5) |
							  ((alpha_val * ((sTemp & 0x1F) - (dTemp & 0x1F)) >> 8) + (dTemp & 0x1F));
				} while (--j > 0);
				sPtr += (sPitch>>1) - RemainingWidth - 1;
				dPtr += (dPitch>>1) - RemainingWidth - 1;
			} while (--i > 0);	
		}

        return 1;
}

int     CCONV vbDABLalphablend16565(__int16 *sPtr, __int16 *dPtr, __int16 alpha_val, __int16 width, __int16 height, __int16 sPitch, __int16 dPitch, __int16 ColorKey)
{

        __int64 RedMask=0xF800F800F800F800;		
        __int64 GreenMask=0x07E007E007E007E0;		
        __int64 BlueMask=0x001F001F001F001F;		

		//__int64 ColorKey64=__int64(RedMask & BlueMask)+(__int64(RedMask & BlueMask)<<16)+(__int64(RedMask & BlueMask)<<32)+(__int64(RedMask & BlueMask)<<48);
		//__int64 ColorKey64=0;
		//__int16 ColorKey=0;
		__int64 ColorKey64=__int64(ColorKey)+(__int64(ColorKey)<<16)+(__int64(ColorKey)<<32)+(__int64(ColorKey)<<48);
		__int64 Full64=0xFFFFFFFFFFFFFFFF;

        __int16 WidthQuad=width/4;			

		__int16 RemainingWidth=width-(WidthQuad*4);
        
        __int64 Alpha=__int64(alpha_val)+(__int64(alpha_val)<<16)+(__int64(alpha_val)<<32)+(__int64(alpha_val)<<48);
		
		__int64 Add256=0x0100010001000100;

        __int32 AddS=sPitch-WidthQuad*8;			
        __int32 AddD=dPitch-WidthQuad*8;			

		if (alpha_val==0||alpha_val==255) { return 0; }

        __asm
        {
				push	esi						//Save just in case
				push	edi						//Save just in case

                mov     esi,sPtr				//Move the source pointer to esi - avoid reading AGI stalls, reading from mem, etc
                mov     edi,dPtr				//Move the dest pointer to edi - same reasons
				
				movq	mm3,GreenMask			//Copy the green mask to mm3	
				movq	mm4,BlueMask			//Copy the blue mask to mm4		>- all to avoid reading from mem during inner loop
				movq	mm5,Alpha				//Copy the alpha values to mm5	

                mov     dx,height				//Copy the height of the area we want to blit to the dx register

NextLine:										
                mov     cx,WidthQuad			//Copy the width of the area to blit divided by 4 to the cx register

NextPixels:
                movq	mm6,[esi]				//Get the current set of 4 source pixels to mm6
				movq	mm7,[edi]				//Get the current set of 4 dest pixels to mm7

				movq	mm0,ColorKey64			//This block does initial colorkey checking - if the entire area is equal to the colorkey then we can skip all these pixels
				pcmpeqw	mm0,mm6					//Compare the colorkey to the source pixels
				pandn	mm0,Full64				//AND the result of the comparison with all 1's, then invert it
				jna		IncPointers				//If we find that the source IS equal to the colorkey, jump down to increment pointers

				movq	mm0,mm6					//Copy the current set of 4 source pixels to mm0
				movq	mm1,mm7					//Copy the current set of 4 dest pixels to mm1

				psrlw	mm0,11					//Shift the source pixels right - no need for the redmask, cause the other colors get shifted out
				psrlw	mm1,11					//Shift the dest pixels right - same reason for no redmask
				paddw	mm0,Add256				//AND 256 to the source pixels - this ensures that we never get a negative number in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication by the alpha values
				paddw	mm0,mm1					//AND the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values
				psllw	mm0,11					//Shift the new pixel values into their correct bit-positions
				movq	mm2,mm0					//Copy the new red values to mm2 for storage

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm3					//AND the source pixels with the greenmask, to isolate the green values
				pand	mm1,mm3					//AND the dest pixels with the greenmask
				psrlw	mm0,5					//Shift the source pixels right
				psrlw	mm1,5					//Shift the dest pixels right
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				psllw	mm0,5					//Shift the new pixel values into their correct bit-positions
				pand	mm0,mm3					//AND the pixel values with the greenmask - just in case
				por		mm2,mm0					//Or the new green values with the red ones already in mm2

				movq	mm0,mm6					//Reset mm0 to the original 4 source pixels again
				movq	mm1,mm7					//Reset mm1 to the original 4 dest pixels again

				pand	mm0,mm4					//AND the source pixels with the bluemask
				pand	mm1,mm4					//AND the source pixels with the bluemask
				paddw	mm0,Add256				//Add 256 to the source pixels to ensure no negative numbers in the subtraction
				psubsw	mm0,mm1					//Subtract the dest pixels from the source
				pmullw	mm0,mm5					//Multiply by the alpha values
				psrlw	mm0,8					//Divide by 256 to compensate for the multiplication
				paddw	mm0,mm1					//Add the dest pixels into the mixture
				psubw	mm0,mm5					//Subtract the alpha values to compensate for adding of 256 near the beginning
				pand	mm0,mm4					//AND the pixel values with the bluemask - just in case
				por		mm2,mm0					//Or the new blue values with the red and green ones - we now have full pixels!

				movq	mm0,ColorKey64			//Another more comprehensive colorkey check - isolates the pixels that we really need to copy
				pcmpeqw	mm0,mm6					//Compare the colorkey with the original source pixels
				pand	mm7,mm0					//AND the original destination pixels with the result of the comparison
				pandn	mm0,mm2					//Invert this result, then AND with the newly created pixels
				por		mm0,mm7					//OR the destination pixels into it

                movq    [edi],mm0				//Copy the final set of 4 pixels to the destination

IncPointers:
                add     esi,8					//Increase the source pointer to go to the next pixels
                add     edi,8					//Increase the dest pointer to go the the next pixels

                dec     cx						//Decrement the width value
                jnz     NextPixels				//If it's not zero, then head back up and treat the pixels; otherwise, keep going down.....

                add     esi,AddS				//Add the AddS value to set the source pointer to the first pixel in the next line
                add     edi,AddD				//Add the AddD value to set the dest pointer to the first pixel in the next line
                dec     dx						//Decrement the height value
                jnz     NextLine				//If height is not zero, then head back up to the very top to 'NextLine:'

                emms							//Clean up the mmx registers

				pop		edi						//Return edi to what it was when we started
				pop		esi						//Return esi to what it was when we started
        }
        return 1;
}


