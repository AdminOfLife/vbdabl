Attribute VB_Name = "modDirectX"
Option Explicit

Dim dx As New DirectX7                      'the main directx object
Global dd As DirectDraw7                    'the main directdraw object

Global ddsPrimary As DirectDrawSurface7     'the primary directdraw surface
Dim ddsdPrimary As DDSURFACEDESC2           'the primary surface's description
Global ddsBack As DirectDrawSurface7        'the backbuffer surface
Dim ddsdBack As DDSURFACEDESC2              'the backbuffer surface's description

Global ddsBackGround As DirectDrawSurface7  'the surface to hold the background image
Dim ddsdBackGround As DDSURFACEDESC2        'description of that surface
Global ddsForeGround As DirectDrawSurface7  'the surface to hole the foreground image
Dim ddsdForeGround As DDSURFACEDESC2        'description of that surface

Global ddsBuffer As DirectDrawSurface7      'a place to 'build' the backbuffer so we're not reading from video mem
Dim ddsdBuffer As DDSURFACEDESC2

Dim r As RECT                               'rect variable - useful to have around :P

Global whattodo As Byte                     'This controls what function the render loop will do
Global Const QUITME = 0                     'When to quit
Global Const NORMALALPHA = 1                'Draw using a random alpha value forground onto background
Global Const COLORBLEND = 2                  'Fade the alpha value up and down to make a kewl effect :)
Global Const HALFALPHA = 3                  'Use the 'optimized' half-alpha routine
Global Const NEWALPHA = 4                   'Use the new alphablending routine
Global Const NORMALBLIT = 127               'NO alpha blitting at all

Global fadealpha As Boolean
Global movefg As Boolean
Global moveoffsetx As Integer
Global moveoffsety As Integer
Global fgleft As Integer
Global fgtop As Integer

Global fadealpha2 As Boolean
Global movefg2 As Boolean
Global moveoffsetx2 As Integer
Global moveoffsety2 As Integer
Global fgleft2 As Integer
Global fgtop2 As Integer

Global myfont As New StdFont

Dim FirstTime As Long
Dim SecondTime As Long
Dim NumLoops As Integer
Dim FPS As Long

Public Declare Function timeGetTime Lib "winmm.dll" () As Long

Public Declare Function asmShl Lib "Shift" (ByVal PassedVar As Long, ByVal NumBits As Long)
Public Declare Function asmShr Lib "Shift" (ByVal PassedVar As Long, ByVal NumBits As Long)

'Public Declare Function vbDABLalphablend16555 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val As Integer, ByVal Width As Integer, ByVal Height As Integer, ByVal sPitch As Integer, ByVal dPitch As Integer, ColorKey As Integer) As Integer
'Public Declare Function vbDABLalphablend16565 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val As Long, ByVal Width As Long, ByVal Height As Long, ByVal sPitch As Long, ByVal dPitch As Long, ColorKey As Long) As Integer
'Public Declare Function vbDABLcolorblend16555 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
'Public Declare Function vbDABLcolorblend16565 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
'Public Declare Function vbDABLcolorblend16555ck Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
'Public Declare Function vbDABLcolorblend16565ck Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
Public Declare Function vbDABLalphablend16 Lib "vbDABL" (ByVal iMode As Integer, ByVal bColorKey As Integer, ByRef sPtr As Any, ByRef dPtr As Any, ByVal iAlphaVal As Integer, ByVal iWidth As Integer, ByVal iHeight As Integer, ByVal isPitch As Integer, ByVal idPitch As Integer, ByVal iColorKey As Integer) As Integer
Public Declare Function vbDABLalphablend16555 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val As Integer, ByVal Width As Integer, ByVal Height As Long, ByVal sPitch As Long, ByVal dPitch As Long, ByVal ColorKey As Integer) As Integer
Public Declare Function vbDABLalphablend16565 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val As Long, ByVal Width As Long, ByVal Height As Long, ByVal sPitch As Long, ByVal dPitch As Long, ColorKey As Long) As Integer
Public Declare Function vbDABLcolorblend16555 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
Public Declare Function vbDABLcolorblend16565 Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
Public Declare Function vbDABLcolorblend16555ck Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long
Public Declare Function vbDABLcolorblend16565ck Lib "vbDABL" (ByRef sPtr As Any, ByRef dPtr As Any, ByVal alpha_val%, ByVal Width%, ByVal Height%, ByVal sPitch%, ByVal dPitch%, ByVal rVal%, ByVal gVal%, ByVal bVal%) As Long


Public Sub InitDirectX()
  
  ' ::[: DirectDraw Initialization :]::
  ' if you don't know what all this does, i'm not sure you're ready for alphablitting :P
  
  Set dd = dx.DirectDrawCreate("")
  Call dd.SetCooperativeLevel(frmMain.hWnd, DDSCL_FULLSCREEN Or DDSCL_EXCLUSIVE)
  dd.SetDisplayMode 640, 480, 16, 0, DDSDM_DEFAULT

  ddsdPrimary.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
  ddsdPrimary.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX
  ddsdPrimary.lBackBufferCount = 1
  Set ddsPrimary = dd.CreateSurface(ddsdPrimary)

  Dim caps As DDSCAPS2
  caps.lCaps = DDSCAPS_BACKBUFFER
  Set ddsBack = ddsPrimary.GetAttachedSurface(caps)

  ddsdBackGround.lFlags = DDSD_CAPS
  ddsdBackGround.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
  Set ddsBackGround = dd.CreateSurfaceFromFile(App.Path & "\background.bmp", ddsdBackGround)

  ddsdForeGround.lFlags = DDSD_CAPS
  ddsdForeGround.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
  Set ddsForeGround = dd.CreateSurfaceFromFile(App.Path & "\foreground.bmp", ddsdForeGround)
  'Dim rtemp As RECT
  'With rtemp
  '.Left = 0
  '.Right = 160
  '.Top = 0
  '.Bottom = 120
  'End With
  'ddsForeGround.BltColorFill rtemp, 0

  ddsdBuffer.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
  ddsdBuffer.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
  ddsdBuffer.lHeight = 480
  ddsdBuffer.lWidth = 640
  Set ddsBuffer = dd.CreateSurface(ddsdBuffer)

End Sub



Public Sub RenderLoop()

Dim emptyrect As RECT
Dim ddsBufferArray() As Byte, ddsForeGroundArray() As Byte
Dim alphaval As Byte, alphainc As Boolean
Dim alphaval2 As Byte, alphainc2 As Boolean
Dim bbhDC As Long

r.Left = 0
r.Top = 0
r.Right = 640
r.Bottom = 480

ddsBack.BltColorFill r, 0

whattodo = NORMALBLIT

fadealpha = False
alphainc = True
movefg = False
moveoffsetx = 1
moveoffsety = 1
fgleft = 80
fgtop = 0
alphaval = 170

fadealpha2 = False
alphainc2 = True
movefg2 = False
moveoffsetx2 = -2
moveoffsety2 = -2
fgleft2 = 370
fgtop2 = 300
alphaval2 = 70

ddsBack.SetForeColor RGB(255, 255, 255)

myfont.Name = "Terminal"
myfont.Size = 9
myfont.Bold = False

ddsBack.SetFont myfont

FPS = 0

'Call SnowMod2.InitSnow(639, 479)

'Dim PA As POINTAPI

Do
  FirstTime = timeGetTime
  
  If fadealpha Then
    If alphainc Then
      If alphaval = 254 Then
        alphaval = 253
        alphainc = False
      Else
        alphaval = alphaval + 1
      End If
    Else
      If alphaval = 1 Then
        alphaval = 2
        alphainc = True
      Else
        alphaval = alphaval - 1
      End If
    End If
  End If
  
  If fadealpha2 Then
    If alphainc2 Then
      If alphaval2 = 254 Then
        alphaval2 = 253
        alphainc2 = False
      Else
        alphaval2 = alphaval2 + 1
      End If
    Else
      If alphaval2 = 1 Then
        alphaval2 = 2
        alphainc2 = True
      Else
        alphaval2 = alphaval2 - 1
      End If
    End If
  End If
  
  If movefg Then
    If moveoffsetx > 0 Then
      If fgleft = 479 Then moveoffsetx = (-1)
    Else
      If fgleft = 0 Then moveoffsetx = 1
    End If
    If moveoffsety > 0 Then
      If fgtop = 319 Then moveoffsety = (-1)
    Else
      If fgtop = 0 Then moveoffsety = 1
    End If
    fgleft = fgleft + moveoffsetx
    fgtop = fgtop + moveoffsety
  End If

  If movefg2 Then
    If moveoffsetx2 > 0 Then
      If fgleft2 >= 478 Then moveoffsetx2 = (-2)
    Else
      If fgleft2 = 0 Then moveoffsetx2 = 2
    End If
    If moveoffsety2 > 0 Then
      If fgtop2 >= 318 Then moveoffsety2 = (-2)
    Else
      If fgtop2 = 0 Then moveoffsety2 = 2
    End If
    fgleft2 = fgleft2 + moveoffsetx2
    fgtop2 = fgtop2 + moveoffsety2
  End If

  Select Case whattodo
    Case NORMALBLIT
      ddsBack.BltFast 0, 0, ddsBackGround, r, DDBLTFAST_WAIT
    Case NORMALALPHA
      'alphaval = 170
      ddsBuffer.BltFast 0, 0, ddsBackGround, r, DDBLTFAST_WAIT
      
      ddsBuffer.Lock emptyrect, ddsdBuffer, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      ddsForeGround.Lock emptyrect, ddsdForeGround, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      
      ddsBuffer.GetLockedArray ddsBufferArray()
      ddsForeGround.GetLockedArray ddsForeGroundArray()

      Select Case ddsdBuffer.ddpfPixelFormat.lGBitMask
        Case &H3E0 '555 mode
          Call vbDABLalphablend16555(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
          Call vbDABLalphablend16555(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft2 + fgleft2, fgtop2), alphaval2, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
        Case &H7E0 '565 mode
          Call vbDABLalphablend16565(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
          Call vbDABLalphablend16565(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft2 + fgleft2, fgtop2), alphaval2, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
      End Select
      
      ddsForeGround.Unlock emptyrect
      ddsBuffer.Unlock emptyrect
      
      ddsBack.BltFast 0, 0, ddsBuffer, r, DDBLTFAST_WAIT
    Case NEWALPHA
      ddsBuffer.BltFast 0, 0, ddsBackGround, r, DDBLTFAST_WAIT
      
      ddsBuffer.Lock emptyrect, ddsdBuffer, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      ddsForeGround.Lock emptyrect, ddsdForeGround, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      
      ddsBuffer.GetLockedArray ddsBufferArray()
      ddsForeGround.GetLockedArray ddsForeGroundArray()

      Select Case ddsdBuffer.ddpfPixelFormat.lGBitMask
        Case &H3E0 '555 mode
'          Call vbDABLalphablend16(555, 1, ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 125, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, ddsdBuffer.ddpfPixelFormat.lRBitMask)
          Call vbDABLalphablend16(555, 1, ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 115, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
          Call vbDABLalphablend16(555, 0, ddsForeGroundArray(0, 0), ddsBufferArray(fgleft2 + fgleft2, fgtop2), alphaval2, 115, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
        Case &H7E0 '565 mode
          Call vbDABLalphablend16(565, 1, ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
          Call vbDABLalphablend16(565, 0, ddsForeGroundArray(0, 0), ddsBufferArray(fgleft2 + fgleft2, fgtop2), alphaval2, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0)
      End Select
      
      ddsForeGround.Unlock emptyrect
      ddsBuffer.Unlock emptyrect
      
      ddsBack.BltFast 0, 0, ddsBuffer, r, DDBLTFAST_WAIT
    Case COLORBLEND
      ddsBuffer.BltFast 0, 0, ddsBackGround, r, DDBLTFAST_WAIT
      
      ddsBuffer.Lock emptyrect, ddsdBuffer, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      ddsForeGround.Lock emptyrect, ddsdForeGround, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      
      ddsBuffer.GetLockedArray ddsBufferArray()
      ddsForeGround.GetLockedArray ddsForeGroundArray()
      
      Select Case ddsdBuffer.ddpfPixelFormat.lGBitMask
        Case &H3E0 '555 mode
          Call vbDABLcolorblend16555ck(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0, 128, 255)
          Call vbDABLcolorblend16555(ddsBufferArray(fgleft2 + fgleft2, fgtop2), ddsBufferArray(fgleft2 + fgleft2, fgtop2), alphaval2, 160, 160, ddsdBuffer.lPitch, ddsdBuffer.lPitch, 255, 0, 0)
          'Call vbDABLcolorblend16565ck(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0, 128, 255)
          'Call vbDABLcolorblend16565(ddsBufferArray(fgleft2 + fgleft2, fgtop2), ddsBufferArray(fgleft2 + fgleft2, fgtop2), alphaval2, 160, 160, ddsdBuffer.lPitch, ddsdBuffer.lPitch, 255, 0, 0)
        Case &H7E0 '565 mode
          Call vbDABLcolorblend16565ck(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), alphaval, 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch, 0, 128, 255)
          Call vbDABLcolorblend16565(ddsBufferArray(fgleft2 + fgleft2, fgtop2), ddsBufferArray(fgleft2 + fgleft2, fgtop2), alphaval2, 160, 160, ddsdBuffer.lPitch, ddsdBuffer.lPitch, 255, 0, 0)
      End Select
      
      ddsForeGround.Unlock emptyrect
      ddsBuffer.Unlock emptyrect
      
      ddsBack.BltFast 0, 0, ddsBuffer, r, DDBLTFAST_WAIT
      
    Case HALFALPHA
      ddsBuffer.BltFast 0, 0, ddsBackGround, r, DDBLTFAST_WAIT
      
      'ddsBuffer.Lock emptyrect, ddsdBuffer, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      'ddsForeGround.Lock emptyrect, ddsdForeGround, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT, 0
      
      'ddsBuffer.GetLockedArray ddsBufferArray()
      'ddsForeGround.GetLockedArray ddsForeGroundArray()

      'Select Case ddsdBuffer.ddpfPixelFormat.lGBitMask
      '  Case &H3E0 '555 mode
      '    Call vbDABLhalfalpha16555ck(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch)
      '    Call vbDABLhalfalpha16555(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft2 + fgleft2, fgtop2), 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch)
      '  Case &H7E0 '565 mode
      '    Call vbDABLhalfalpha16565ck(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft + fgleft, fgtop), 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch)
      '    Call vbDABLhalfalpha16565(ddsForeGroundArray(0, 0), ddsBufferArray(fgleft2 + fgleft2, fgtop2), 160, 160, ddsdForeGround.lPitch, ddsdBuffer.lPitch)
      'End Select
      
      'ddsForeGround.Unlock emptyrect
      'ddsBuffer.Unlock emptyrect
      
      ddsBuffer.SetForeColor RGB(255, 255, 255)
      ddsBuffer.DrawText 100, 220, "Half-alpha is no longer available - refer to readme.txt", False
      ddsBack.BltFast 0, 0, ddsBuffer, r, DDBLTFAST_WAIT
  End Select

  'Do the snow stuff :)
'  bbhDC = ddsBack.GetDC()
'  SnowMod2.MoveSnow bbhDC
'  ddsBack.ReleaseDC bbhDC

  'Show instructions
  ddsBack.DrawText 5, 5, "Draw modes:", False
  ddsBack.DrawText 5, 15, "1 - Normal Blit", False
  ddsBack.DrawText 5, 25, "2 - Normal Alpha Blend", False
  ddsBack.DrawText 5, 35, "4 - Color Blend", False
  ddsBack.DrawText 5, 55, "Box #1:", False
  ddsBack.DrawText 5, 65, "F - toggle alphavalue fading", False
  ddsBack.DrawText 5, 75, "M - toggle moving", False
  ddsBack.DrawText 5, 85, "Box #2:", False
  ddsBack.DrawText 5, 95, "D - toggle alphavalue fading", False
  ddsBack.DrawText 5, 105, "N - toggle moving", False

  ddsBack.DrawText 550, 5, "FPS: " & CStr(FPS), False

  ddsPrimary.Flip Nothing, DDFLIP_WAIT
  
  NumLoops = NumLoops + 1
  If NumLoops = 9 Then
    SecondTime = timeGetTime
    FPS = (1000 / (SecondTime - FirstTime))
    FirstTime = timeGetTime
    NumLoops = 0
  End If
  
  DoEvents
Loop Until whattodo = QUITME

End Sub


