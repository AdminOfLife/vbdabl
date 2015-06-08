Attribute VB_Name = "SnowMod2"
Public Declare Sub InitSnow Lib "SnowDLL.DLL" Alias "Init" (ByVal Width As Long, ByVal Height As Long)
Public Declare Sub MoveSnow Lib "SnowDLL.DLL" Alias "Move" (ByVal hDC As Long)
