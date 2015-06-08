VERSION 5.00
Begin VB.Form frmMain 
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   3195
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  If KeyCode = vbKeyEscape Then whattodo = QUITME
  If KeyCode = vbKey1 Then whattodo = NORMALBLIT
  If KeyCode = vbKey2 Then whattodo = NORMALALPHA
  If KeyCode = vbKey3 Then whattodo = HALFALPHA
  If KeyCode = vbKey4 Then whattodo = COLORBLEND
  If KeyCode = vbKey5 Then whattodo = NEWALPHA
  
  If KeyCode = vbKeyM Then movefg = Not movefg
  If KeyCode = vbKeyN Then movefg2 = Not movefg2
  If KeyCode = vbKeyF Then fadealpha = Not fadealpha
  If KeyCode = vbKeyD Then fadealpha2 = Not fadealpha2
End Sub

Private Sub Form_Load()

  InitDirectX
  RenderLoop
  End

End Sub
