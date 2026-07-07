REM  *****  BASIC  *****

'Scene~Draft a framework for rendering 3D scenes
'Copyright 2026 Robert W. Schuler
'
'This program is free software: you can redistribute it and/or modify
'it under the terms of the GNU General Public License as published by
'the Free Software Foundation, either version 3 of the License.
'
'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.
'
'You should have received a copy of the GNU General Public License
'along with this program.  If not, see <https://www.gnu.org/licenses/>.

'''                       Scene~Draft
  ' entity =  1 Cone                       color =  0 black
  '        =  2 Cylinder                         =  1 red
  '        =  3 Torus                            =  2 green
  '        =  4 Arc (centered)                   =  3 blue
  '        =  5 Arc (endpoints)                  =  4 orange
  '        =  6 Sphere80                         =  5 yellow
  '        =  7 Hemisphere                       =  6 cyan
  '        =  8 Extrusion                        =  7 purple
  '                                              =  8 magenta
  '                                              =  9 white
  '                                              = 10 forrest green
'''

  '
  '
  '  Use the CMD subst command to define b: as your working directory.
  '   For example, my working path is c:\Users\Schuler\Documents\MagicStuff
  '   and in this directory I have a file called scene.csv.
  '
  '    Prior to running sceneDraft(), I open a command shell and type:
  '     subst b: c:\Users\Schuler\Documents\MagicStuff
  '
  '   sceneDraft() can now refer to c:\Users\Schuler\Documents\MagicStuff\scene.csv as simply,
  '      b:\scene.csv
  '
  

  '''''''''''''''''''''''''' GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
  ' GLOBALS
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' Set path to a legal place to write on your disk & modify the CopyIt.bat file'
  Dim theTempFile As String                                                     '
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  Dim theOBJfile As String 'Wavefron *.obj format file.
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  Dim theSceneFile As String 'Path to SceneDesciptionFile
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  'Adaptation of Leen Ammeraal's Cable.cpp tool to work in VBA Excel
  ' Rotation matrix is made global
  ' Call initRotate with a base point, direction and an angle in degrees
  ' subsequent calls to rotate will rotate the X,Y,Z input by angle
  '  about the axis defined through the base point in the given direction
  Dim r11, r12, r13 As Double
  Dim r21, r22, r23 As Double
  Dim r31, r32, r33 As Double
  Dim r41, r42, r43 As Double
 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
  Dim xmlOffset As Integer 'used to track output row on Sheets("XML")
  Dim xmlObjectID As Integer
  Dim objOffset As Integer
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
  Dim objVertexCount As Integer 'used to offset vertix numbers for multiple objects
    '''''''''''''''''''''''''''''''''''''''''' String Buffers and thier indexes
  Dim Vertices(65000) As String
  Dim vNext As Long
  Dim Faces(65000) As String
  Dim fNext As Long
  Dim WaveFront(130000) As String
  Dim wNext As Long
  Dim XML3MF(130000) As String
  Dim xNext As Long
  '--------------------------------------------------------------------------------
  Dim global_radius As String  'default cylinder diameter     pen-width
  Dim global_steps As String   'default circle steps          smoothness
  '--------------------------------------------------------------------------------
  ' End of Globals
  '''''''' GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG

  
Sub Main
ff = FreeFile()
Dim a As String
Dim B() As String
Dim S As String
Dim objColor As Integer
Dim myMsg as String
Dim HL as integer

  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  '  full path to 3dmodel.model  temporary file
  theTempFile = "b:\3dmodel.model"
  theOBJfile = "b:\scene.obj"
  theSceneFile = "b:\scene.csv"
  '  use SUBST B: <your path> at the cmd prompt--see the help for SUBST
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

vNext = 0
fNext = 0
wNext = 0
xNext = 0
objOffset = 0
objVertexCount = 0
xmlOffset = 0
xmlObjectID = 1

Call writeOBJheader

'Open "b:\scene.csv" For Input As #1
Open theSceneFile For Input As ff

myMsg = ""
Line Input #ff, a
B = Split(a, "D") ' get the character before Delimiter
c = Right(B(0), 1)
'Debug.Print "Delimiter is '" & c & "'"
myMsg = myMsg + "Delimiter is '" & c & "'"
Line Input #ff, a
B = Split(a, c)
HL = B(0)
'Debug.Print "There are " & HL & " header lines"
myMsg = myMsg + "There are " & HL & " header lines\n"
Line Input #ff, a
B = Split(a, c)
global_radius = B(0)
'Debug.Print "The global_radius is:" & global_radius
myMsg = myMsg + "The global_radius is:" & global_radius
Line Input #ff, a
B = Split(a, c)
global_steps = B(0)
'Debug.Print "global_steps:" & global_steps
myMsg = myMsg + "global_steps:" & global_steps
print myMsg

For i = 5 To HL
  Line Input #ff, a 'skip to end of header
Next i

' RWS B is an array of STRINGs.
'     VBA does not always convert to numbers as expected.
'     Test, test, test
bail = False
Do Until EOF(1) Or bail = True
Line Input #ff, a
objColor = 0
If Len(a) > 0 Then  'stop parsing at blank line
B = Split(a, c)
'Debug.Print B(0), UBound(B)
'1 Cone
'2 Cylinder
'3 Torus
'4 Arc (centered)
'5 Chord
'
'6 Sphere80
'7 Hemisphere
'8 Extrusion

'#-myCone,C, x1,y1,z1, x2,y2,z2, <R>,<N>,<phi>
'#-myCylinder,C,x1,y1,z1, x2,y2,z2, <R1>, <R2>, <N>
'#-myTorus,C, Cx,Cy,Cz,ax,ay,az,R1,N1,<R2>,<N2>
'#-myArcC,C, Cx,Cy,Cz, ax,ay,az, Sx,Sy,Sz, alpha,N1, <R2>, <N2>
'#-myChord, C, Sx,Sy,Sz, Ex, Ey,Ez, ax,ay,az, H, N, <R1>, <N1>

'#-sphere80,C, Cx,Cy,Cz,R
'#-hemisphere,C, Cx,Cy,Cz, Px,Py,Pz, R, N
'#-extrusion,C, dx,dy,dz, N, 1x,1y,1z, 2x,2y,2z, ... Nx,Ny,Nz

S = Trim(B(0))
If S = "1" Then
 'Debug.Print "cone"
 print "cone"
 objColor = B(1)
 Call myCone(B)
 Call writeOBJobject(objColor)
ElseIf S = "2" Then
 'Debug.Print "cylinder"
 Print "cylinder"
 objColor = B(1)
 Call myCylinder(B)
 Call writeOBJobject(objColor)
ElseIf S = "3" Then
 'Debug.Print "Torus"
 Print "Torus"
 objColor = B(1)
 Call myTorus(B)
 Call writeOBJobject(objColor)
ElseIf S = "4" Then
 'Debug.Print "arc center"
 print "arc center"
 objColor = B(1)
 Call myArcC(B)
 Call writeOBJobject(objColor)
ElseIf S = "5" Then
 'Debug.Print "arc chord"
 print "arc chord"
 objColor = B(1)
 Call myChord(B)
 Call writeOBJobject(objColor)

ElseIf S = "6" Then
 'Debug.Print "sphere"
 print "sphere"
 objColor = B(1)
 Call mySphere80(B)
 Call writeOBJobject(objColor)
ElseIf S = "7" Then
 'Debug.Print "Hemisphere"
 print "Hemisphere"
 objColor = B(1)
 Call myHemisphere(B)
 Call writeOBJobject(objColor)
 ElseIf S = "8" Then
 'Debug.Print "Extrusion"
 print "Extrusion"
 objColor = B(1)
 Call myExtrusion(B)
 Call writeOBJobject(objColor)
End If

Else
 bail = True
End If

Loop
Close 1
Call dumpOBJfile

End Sub

Sub initRotate(ByVal a1 As Double, ByVal a2 As Double, ByVal a3 As Double, ByVal v1 As Double, ByVal v2 As Double, ByVal v3 As Double, ByVal alpha As Double)
  ' Computation of rotation matrix (stored as 12 globals):
  '
  '                | r11 r12 r13 r14 0 |
  '           R =  | r21 r22 r23 r24 0 |
  '                | r31 r32 r33 r34 0 |
  '                | r41 r42 r43 r44 1 |
  '
  ' to be used as [x1 y1 z1 1] = [X Y Z 1] R
  '    see sub rotate
  '  [x1 y1 z1] is the image of [X Y Z].
  '
  ' compute rotation matrix for rotation by alpha degrees around
  '  axis (a1,a2,a3)+lambda(v1,v2,v3)
  '  a is the point, v is the direction
  '
  Dim cosAlpha, sinAlpha, cosAlpha1, rho As Double
  Dim cosphi, sinPhi, cosPhi2, sinPhi2 As Double
  Dim cosTheta, sinTheta As Double
  ' pi = 4.0 * Atn(1)
   myAlpha = alpha * pi / 180
  cosAlpha = Cos(myAlpha)
  sinAlpha = Sin(myAlpha)
  cosAlpha1 = 1 - cosAlpha
  rho = Sqr(v1 * v1 + v2 * v2 + v3 * v3)
  'Debug.Print ("RRRRRRRRRRRR>>>>>>>>>>>>>>>>>>>>>>>>> myAlpha = " & myAlpha & ", alpha = " & alpha & ", rho = " & rho)
  If rho = 0 Then
    theta = 0
    cosphi = 1
    sinPhi = 0
  Else
    If v1 = 0 Then
      If v2 >= 0 Then
        theta = 0.5 * pi
  '      Debug.Print ("RRRRRRRRRRRR>>>>>>>>>>>>>>>>>>>>>>>>> V1==0 and v2>0")
      Else
        theta = 1.5 * pi
    '    Debug.Print ("RRRRRRRRRRRR>>>>>>>>>>>>>>>>>>>>>>>>> V1==0 and v2<=0")
      End If
    Else
      theta = Atn(v2 / v1)
      If (v1 < 0) Then
        theta = theta + pi
      End If
    '     Debug.Print ("RRRRRRRRRRRR>>>>>>>>>>>>>>>>>>>>>>>>> v1 = " & v1 & ", " & "v2 = " & v2 & ", theta = " & theta)
    End If
    cosphi = v3 / rho
    sinPhi = Sqr(1 - cosphi * cosphi)
    ' cosPhi = cos(phi) sinPhi = sin(phi)
  End If
  cosTheta = Cos(theta)
  sinTheta = Sin(theta)
  cosPhi2 = cosphi * cosphi
  sinPhi2 = 1 - cosPhi2
  cosTheta2 = cosTheta * cosTheta
  sinTheta2 = 1 - cosTheta2
  r11 = (cosAlpha * cosPhi2 + sinPhi2) * cosTheta2 + cosAlpha * sinTheta2
  r12 = sinAlpha * cosphi + cosAlpha1 * sinPhi2 * cosTheta * sinTheta
  r13 = sinPhi * (cosphi * cosTheta * cosAlpha1 - sinAlpha * sinTheta)
  r21 = sinPhi2 * cosTheta * sinTheta * cosAlpha1 - sinAlpha * cosphi
  r22 = sinTheta2 * (cosAlpha * cosPhi2 + sinPhi2) + cosAlpha * cosTheta2
  r23 = sinPhi * (cosphi * sinTheta * cosAlpha1 + sinAlpha * cosTheta)
  r31 = sinPhi * (cosphi * cosTheta * cosAlpha1 + sinAlpha * sinTheta)
  r32 = sinPhi * (cosphi * sinTheta * cosAlpha1 - sinAlpha * cosTheta)
  r33 = cosAlpha * sinPhi2 + cosPhi2
  r41 = a1 - a1 * r11 - a2 * r21 - a3 * r31
  r42 = a2 - a1 * r12 - a2 * r22 - a3 * r32
  r43 = a3 - a1 * r13 - a2 * r23 - a3 * r33
End Sub
Sub rotate(ByRef x As Variant, ByRef y As Variant, ByRef z As Variant)
  Dim mx, my, mz As Double
  mx = x * r11 + y * r21 + z * r31 + r41
  my = x * r12 + y * r22 + z * r32 + r42
  mz = x * r13 + y * r23 + z * r33 + r43
  x = mx
  y = my
  z = mz
  End Sub

Sub printRotate()
  Debug.Print ("================================================")
  Debug.Print ("[")
  Debug.Print (" " & r11 & "," & r12 & "," & r13)
  Debug.Print (" " & r21 & "," & r22 & "," & r23)
  Debug.Print (" " & r31 & "," & r32 & "," & r33)
  Debug.Print (" " & r41 & "," & r42 & "," & r43)
  Debug.Print ("]")
End Sub

Sub dumpOBJfile()
fff = FreeFile()
  If Len(theOBJfile) > 1 Then
    Open theOBJfile For Output Shared As fff
  Else
      Open "c:\users\schulerrw\Documents\scene.obj" For Output Shared As fff
  End If
  'Open myPath For Output Shared As #1
  For i = 0 To wNext - 1
    Print #fff, WaveFront(i)
  Next i
  print "Thats all folks"
  Close #fff
End Sub

Sub writeOBJheader()
  wNext = 0
  WaveFront(wNext) = "mtllib ./scenedraft.mtl"
  wNext = wNext + 1
End Sub

Sub writeOBJobject(Optional colorID As Integer)
  'NOTE: objOffset 'used to track output row on Sheets("OBJ") and
  '      objVertexCount 'used to offset vertexIDs
  '      are GLOBAL and must be managed in the calling environment
  maxVertex = vNext
  maxFace = fNext
  'Debug.Print ("In writeOBJobject " & maxVertex & "vertices and faces = " & maxFace)
 ' WaveFront(wNext) = "o ent_" & format(objOffset, "000000")
  WaveFront(wNext) = "o ent_" & Format(objOffset, "000000")
  wNext = wNext + 1
  objOffset = objOffset + 1

  For i = 0 To maxVertex - 1
    VV = Trim(Vertices(i))
    SS = Split(VV, " ")
    xx = CDbl(SS(0))
    yy = CDbl(SS(1))
    zz = CDbl(SS(2))
    myStr = "v " & Format(xx, "####.00000000000") & " " & Format(yy, "####.00000000000") & " " & Format(zz, "####.00000000000") & "    #" & i + objVertexCount
    'Debug.Print VV & "x" & xx & "y" & yy & "z" & zz & ":"
    'Debug.Print myStr
    WaveFront(wNext) = myStr
    wNext = wNext + 1
  Next i
    WaveFront(wNext) = "S 0"
  wNext = wNext + 1
  WaveFront(wNext) = "usemtl m" & Format(colorID, "0000")
  wNext = wNext + 1
  For i = 0 To maxFace - 1
    FF = Trim(Faces(i))
    'Debug.Print FF
    SS = Split(FF, " ")
    xx = CInt(Trim(SS(0))) + objVertexCount + 1
    yy = CInt(Trim(SS(1))) + objVertexCount + 1
    zz = CInt(Trim(SS(2))) + objVertexCount + 1
    myStr = "f " & Format(xx, "#####0") & " " & Format(yy, "#####0") & " " & Format(zz, "#####0")
    WaveFront(wNext) = myStr
    wNext = wNext + 1
  Next i
  WaveFront(wNext) = ""
  wNext = wNext + 1
  objVertexCount = objVertexCount + maxVertex
End Sub


Sub myCone(B() As String)
  Dim r, Phi as Double
  Phi = 0.0# 'optional rotation about axis to align base-points
  x1 = CDbl(Trim(B(2))) 
  y1 = CDbl(Trim(B(3))) 
  z1 = CDbl(Trim(B(4)))
  x2 = CDbl(Trim(B(5)))
  y2 = CDbl(Trim(B(6)))
  z2 = CDbl(Trim(B(7)))
  r = CDbl(global_radius) * 2.0
  N = CInt(global_steps)
  PPPL = UBound(B)
  If PPPL > 7 Then
    If Len(B(8)) > 0 Then
      R1 = CDbl(Trim(B(8))) 'optional overide of R for 1rst cap
      'Debug.Print "overide R1 = " & R1
    End If
  End If
  If PPPL > 8 Then
    If Len(B(9)) > 0 Then
      N = CInt(Trim(B(9)))  ' optional overide of N, number of steps
      'Debug.Print "overide N = " & N
    End If
  End If
  If PPPL > 9 Then
    If Len(B(10)) > 0 Then
      Phi = CDbl(Trim(B(10)))  ' optional overide of phi, orientation
      'Debug.Print "overide phi = " & Phi
    End If
  End If
  '#-myCone,C, x1,y1,z1, x2,y2,z2, <R>,<N>,<phi>
  ' ttt = sqrt(-1)
    Dim xx(30) As Double
    Dim yy(30) As Double  ' dimension must match n
    Dim zz(30) As Double
    
    Dim myXX, myYY, myZZ As Double
    Dim aa, bb, cc As Double
    Dim rx, ry, rz As Double
    Dim myLen As Double
    
  vertCount = 0
       'save center of first point as index zero
      Vertices(vertCount) = x1 & " " & y1 & " " & z1
      vertCount = vertCount + 1
      
      '''''''''''''''''''''''''' Vector from P1 to P2
      aa = x2 - x1
      bb = y2 - y1
      cc = z2 - z1
      'dd = a * xC1 + b * yC1 + c * zC1
    

    ''''''''''''''''''''''  Orthogonal Vector
    If Abs(aa) < 0.000001 And Abs(bb) < 0.000001 Then
      rx = 0
      ry = cc
      rz = -1 * bb
    Else
      rx = bb
      ry = -1 * aa
      rz = 0
    End If
    ''''''''''''''''''''''''''''''''''' Unit Length
    myLen = Sqr(rx * rx + ry * ry + rz * rz)
    rx = rx / myLen
    ry = ry / myLen
    rz = rz / myLen
    
    ' stride in degrees around circle
    theta = 360 / N
    
    'First Face's Vertices ''''''''''''''''''''''''''''''''''''''
    If Len(R1) > 0 Then r = R1 'optional overide of radius
    xx(0) = x1 + rx * r
    yy(0) = y1 + ry * r
    zz(0) = z1 + rz * r
    If Phi <> 0 Then 'extra rotation for orientation
      myXX = xx(0)
      myYY = yy(0)
      myZZ = zz(0)
      Call initRotate(x1, y1, z1, aa, bb, cc, Phi)
      Debug.Print "Phi fix: " & Phi, myXX, myYY, myZZ
      Call rotate(myXX, myYY, myZZ)
      Debug.Print "Phi fixed           : " & Phi, myXX, myYY, myZZ
      xx(0) = myXX
      yy(0) = myYY
      zz(0) = myZZ
    End If
    
    Call initRotate(x1, y1, z1, aa, bb, cc, theta)
    For i = 1 To N - 1
      myXX = xx(i - 1)
      myYY = yy(i - 1)
      myZZ = zz(i - 1)
        Call rotate(myXX, myYY, myZZ)
      xx(i) = myXX
      yy(i) = myYY
      zz(i) = myZZ
    Next i
    For i = 0 To N - 1
      Vertices(vertCount) = xx(i) & " " & yy(i) & " " & zz(i)
      vertCount = vertCount + 1
    Next i
    
  
    'save center of 2nd point as index 2n+1
    Vertices(vertCount) = x2 & " " & y2 & " " & z2
    ' Debug.Print "In cone z2 = >" & z2 & "<"
    vertCount = vertCount + 1

''''''' FACES
  vNext = vertCount ' update global pointer
  '

   faceCount = 0
    vertCount = 0
    '2n + 2 vertices per cylinder

    'first end cap

    'Sheets("Faces").Cells(faceCount, 1) = faceCount - 1
    v1 = N + j * (2 * N + 2)
    v2 = 1 + j * (2 * N + 2)
    v3 = 0 + j * (2 * N + 2)
    Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
        
    For i = 1 To N - 1
      v1 = i + j * (2 * N + 2)
      v2 = i + 1 + j * (2 * N + 2)
      v3 = 0 + j * (2 * N + 2)
      Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
    Next i
    
 'point
    v1 = N + j * (2 * N + 2)
    v2 = 1 + j * (2 * N + 2)
    v3 = N + 1 + j * (2 * N + 2)
      Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
  
    For i = 1 To N - 1
    v1 = i + j * (2 * N + 2)
    v2 = i + 1 + j * (2 * N + 2)
    v3 = N + 1 + j * (2 * N + 2)
      Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
    Next i
  ' Next j
      'For i = 0 To vNext - 1
           'ebug.Print "v " & Vertices(i)
     'ext i
     For i = 0 To faceCount - 1
       FF = Faces(i)
       GG = Split(FF, " ")
       
       
       '   Debug.Print "f " & GG(0) + 1 & " " & GG(1) + 1 & " " & GG(2) + 1
     Next i
         Faces(faceCount) = ""
    fNext = faceCount
End Sub



Sub myCylinder(B() As String)
' #,C,x1,y1,z1, x2,y2,z2, R, N
  x1 = CDbl(Trim(B(2)))
  y1 = CDbl(Trim(B(3)))
  z1 = CDbl(Trim(B(4)))
  x2 = CDbl(Trim(B(5)))
  y2 = CDbl(Trim(B(6)))
  z2 = CDbl(Trim(B(7)))
  r = global_radius
  N = CInt(global_steps)
  PPPL = UBound(B)
  If PPPL > 7 Then
    If Len(B(8)) > 0 Then
      R1 = CDbl(Trim(B(8))) 'optional overide of R for 1rst cap
    End If
  End If
  If PPPL > 8 Then
    If Len(B(9)) > 0 Then
      R2 = CDbl(Trim(B(9)))' optional overid of R for 2nd cap
    End If
  End If
  If PPPL > 9 Then
    If Len(B(10)) > 0 Then
      N = CInt(Trim(B(10))) ' optional overide of N, number of steps
      'Debug.Print "overide N"
    End If
  End If
  'm = Sheets("Cyl").Cells(2, 11) 'number of cylinders
  ' ttt = sqrt(-1)
    Dim xx(30) As Double
    Dim yy(30) As Double  ' dimension must match n
    Dim zz(30) As Double
    
    Dim myXX, myYY, myZZ As Double
    Dim aa, bb, cc As Double
    Dim rx, ry, rz As Double
    Dim myLen As Double
    
  vertCount = 0
       'save center of first point as index zero
      Vertices(vertCount) = Trim(x1) & " " & Trim(y1) & " " & Trim(z1)
      vertCount = vertCount + 1
      
      '''''''''''''''''''''''''' Vector from P1 to P2
      aa = x2 - x1
      bb = y2 - y1
      cc = z2 - z1
      'dd = a * xC1 + b * yC1 + c * zC1
    

    ''''''''''''''''''''''  Orthogonal Vector
    If Abs(aa) < 0.000001 And Abs(bb) < 0.000001 Then
      rx = 0
      ry = cc
      rz = -1 * bb
    Else
      rx = bb
      ry = -1 * aa
      rz = 0
    End If
    ''''''''''''''''''''''''''''''''''' Unit Length
    myLen = Sqr(rx * rx + ry * ry + rz * rz)
    rx = rx / myLen
    ry = ry / myLen
    rz = rz / myLen
    
    ' stride in degrees around circle
    theta = 360 / N
    
    'First Face's Vertices ''''''''''''''''''''''''''''''''''''''
    If Len(R1) > 0 Then r = R1 'optional overide of radius
    xx(0) = x1 + rx * r
    yy(0) = y1 + ry * r
    zz(0) = z1 + rz * r
    Call initRotate(x1, y1, z1, aa, bb, cc, theta)
    For i = 1 To N - 1
      myXX = xx(i - 1)
      myYY = yy(i - 1)
      myZZ = zz(i - 1)
        Call rotate(myXX, myYY, myZZ)
      xx(i) = myXX
      yy(i) = myYY
      zz(i) = myZZ
    Next i
    For i = 0 To N - 1
      Vertices(vertCount) = xx(i) & " " & yy(i) & " " & zz(i)
      vertCount = vertCount + 1
    Next i
    
    'Second Faces's Vertices '''''''''''''''''''''''''''''''''''''
    If Len(R2) > 0 Then r = R2 'optional overide of radius
    xx(0) = x2 + rx * r
    yy(0) = y2 + ry * r
    zz(0) = z2 + rz * r
    For i = 1 To N - 1
      myXX = xx(i - 1)
      myYY = yy(i - 1)
      myZZ = zz(i - 1)
        Call rotate(myXX, myYY, myZZ)
      xx(i) = myXX
      yy(i) = myYY
      zz(i) = myZZ
    Next i
    For i = 0 To N - 1
      Vertices(vertCount) = xx(i) & " " & yy(i) & " " & zz(i)
      vertCount = vertCount + 1
    Next i
  
    'save center of 2nd point as index 2n+1
    Vertices(vertCount) = Trim(x2) & " " & Trim(y2) & " " & Trim(z2)
    vertCount = vertCount + 1

''''''' FACES
  vNext = vertCount ' update global pointer
  '

   faceCount = 0
    vertCount = 0
    '2n + 2 vertices per cylinder

    'first end cap

    'Sheets("Faces").Cells(faceCount, 1) = faceCount - 1
    v1 = N + j * (2 * N + 2)
    v2 = 1 + j * (2 * N + 2)
    v3 = 0 + j * (2 * N + 2)
    Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
        
    For i = 1 To N - 1
      v1 = i + j * (2 * N + 2)
      v2 = i + 1 + j * (2 * N + 2)
      v3 = 0 + j * (2 * N + 2)
      Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
    Next i
    
      'Sheets("Faces").Cells(faceCount, 6) = "last"
      'faceCount = faceCount + 1
      'Sheets("Faces").Cells(faceCount, 1) = faceCount
      v1 = j * (2 * N + 2) + 2 * N
      v2 = 1 + j * (2 * N + 2)
      v3 = N + j * (2 * N + 2)
            Faces(faceCount) = v1 & " " & v2 & " " & v3
      faceCount = faceCount + 1
      'Sheets("Faces").Cells(faceCount, 1) = faceCount
      v1 = j * (2 * N + 2) + 2 * N
      v2 = j * (2 * N + 2) + 1 + N
      v3 = 1 + j * (2 * N + 2)
      Faces(faceCount) = v1 & " " & v2 & " " & v3
          faceCount = faceCount + 1

      For i = 1 To N - 1
        v1 = i + j * (2 * N + 2)
        v2 = j * (2 * N + 2) + i + N
        v3 = j * (2 * N + 2) + i + N + 1
        Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
        v1 = i + j * (2 * N + 2) + 1
        v2 = i + j * (2 * N + 2)
        v3 = j * (2 * N + 2) + i + N + 1
        Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
      Next i
  'last end cap
    For i = N To 2 Step -1
      v1 = N + i + j * (2 * N + 2)
      v2 = N + i - 1 + j * (2 * N + 2)
      v3 = 2 * N + 1 + j * (2 * N + 2)
        Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
    Next i

    v1 = N + 1 + j * (2 * N + 2)
    v2 = 2 * N + j * (2 * N + 2)
    v3 = 2 * N + 1 + j * (2 * N + 2)
        Faces(faceCount) = v1 & " " & v2 & " " & v3
        faceCount = faceCount + 1
    Faces(faceCount) = ""
    fNext = faceCount

End Sub


Sub myTorus(B() As String)
  Dim Cx, Cy, Cz As Double ' center point
  Dim ax, ay, az As Double ' direction=axis of rotation
  Dim R1,R2 As Double 'Radii and Steps 1 is torus 2 is tube
  Dim i,j, N1, N2 as Integer
  PPPL = UBound(B)
  Cx = CDbl(Trim(B(2)))
  Cy = CDbl(Trim(B(3)))
  Cz = CDbl(Trim(B(4)))
  ax = CDbl(Trim(B(5)))
  ay = CDbl(Trim(B(6)))
  az = CDbl(Trim(B(7)))
  R1 = 10.0
  N1 = CInt(6)
  R2 = global_radius
  N2 = global_steps
  If PPPL > 7 Then
    If Len(B(8)) > 0 Then
      R1 = CDbl(Trim(B(8))) 'optional overide of R for 1rst cap
    End If
  End If
  If PPPL > 8 Then
    If Len(B(9)) > 0 Then
      N1 = CDbl(Trim(B(9))) ' optional overide of N, number of steps
    End If
  End If
  If PPPL > 9 Then
    If Len(B(10)) > 0 Then
      R2 = CDbl(Trim(B(10)))  'optional overide of R for 1rst cap
    End If
  End If
  If PPPL > 10 Then
    If Len(B(11)) > 0 Then
      N2 = CDbl(Trim(B(11))) ' optional overide of N, number of steps
      Debug.Print "overide N"
    End If
  End If
  '#-myTorus,C, Cx,Cy,Cz,ax,ay,az,R1,N1,<R2>,<N2>
  ' ttt = sqrt(-1)
    Dim xx(30) As Double
    Dim yy(30) As Double  ' dimension must match n
    Dim zz(30) As Double
    
    Dim myXX, myYY, myZZ As Double
    Dim aa, bb, cc As Double
    Dim rx, ry, rz As Double
    Dim myLen As Double
    

      
      '''''''''''''''''''''''''' Vector direction of central axis
      aa = ax
      bb = ay
      cc = az
      'dd = a * xC1 + b * yC1 + c * zC1
    

    ''''''''''''''''''''''  Orthogonal Vector
    If Abs(aa) < 0.000001 And Abs(bb) < 0.000001 Then
      rx = 0.0
      ry = cc
      rz = -1.0 * bb
    Else
      rx = bb
      ry = -1.0 * aa
      rz = 0.0
    End If
    rx = cdbl(rx)
    ry = cdbl(ry)
    rz = cdbl(rz)
    ''''''''''''''''''''''''''''''''''' Unit Length
    myLen = Sqr(rx * rx + ry * ry + rz * rz)
    rx = rx / myLen
    ry = ry / myLen
    rz = rz / myLen
    
    ' stride in degrees around cenerline circle
    theta1 = 360.0 / N1
    theta2 = 360.0 / N2 'stride in degrees around profile ''pencil width
    
    CLx = Cx + rx * R1
    CLy = Cy + ry * R1  'point on center line
    CLz = Cz + rz * R1
    
    px = Cx + rx * (R1 + R2)
    py = Cy + ry * (R1 + R2) ' point on surface
    pz = Cz + rz * (R1 + R2)
    
    APx = ry * az - rz * ay
    APy = rz * ax - rx * az  ' cross product vector normal to profile
    APz = rx * ay - ry * ax
    LLL = Sqr(APx * APx + APy * APy + APz * APz)
    APx = APx / LLL
    APy = APy / LLL
    APz = APz / LLL
    
   vertCount = 0
   '' First Profile ''''''''''''''''''''''''''''''''''''''''
   Call initRotate(CLx, CLy, CLz, APx, APy, APz, theta2)
   xx(0) = px
   yy(0) = py   'first point on first profile
   zz(0) = pz
     For i = 1 To N2 - 1
      myXX = xx(i - 1)
      myYY = yy(i - 1)
      myZZ = zz(i - 1)
        Call rotate(myXX, myYY, myZZ)
      xx(i) = myXX
      yy(i) = myYY
      zz(i) = myZZ
    Next i
    For i = 0 To N2 - 1
      Vertices(vertCount) = xx(i) & " " & yy(i) & " " & zz(i)
      vertCount = vertCount + 1
    Next i
    
    '' Rotate Profile N1 times '''''''''''''''''''''''''''''''
    Call initRotate(Cx, Cy, Cz, ax, ay, az, theta1)
    For j = CInt(1) To CInt(N1) - 1
      For i = 0 To N2 - 1
        myXX = xx(i)
        myYY = yy(i)
        myZZ = zz(i)
          Call rotate(myXX, myYY, myZZ)
        xx(i) = myXX
        yy(i) = myYY
        zz(i) = myZZ
      Next i
      For i = 0 To N2 - 1  ' save new profile
      Vertices(vertCount) = xx(i) & " " & yy(i) & " " & zz(i)
      vertCount = vertCount + 1
      Next i
    Next j ''End of rotate profile outer loop
    
    vNext = vertCount ' update global pointer
    
    '' Faces '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
       faceCount = 0
       vertCount = 0
    'N2 vertices per profile
    'N1 profiles
    For j = 0 To CInt(N1) - 2
      For i = 0 To N2 - 2
       v1 = j * N2 + i
       v2 = v1 + 1
       v3 = (j + 1) * N2 + i + 1
       v4 = v3 - 1
       Faces(faceCount) = v1 & " " & v2 & " " & v4
          faceCount = faceCount + 1
       Faces(faceCount) = v2 & " " & v3 & " " & v4
          faceCount = faceCount + 1
      Next i
       v1 = j * N2 + i
       v2 = j * N2
       v3 = (j + 1) * N2 + i
       v4 = v1 + 1
       Faces(faceCount) = v1 & " " & v2 & " " & v3
          faceCount = faceCount + 1
       Faces(faceCount) = v2 & " " & v4 & " " & v3
          faceCount = faceCount + 1

    Next j  ' end of loop through profiles
    For i = 0 To N2 - 2
       v1 = (Cint(N1) - 1) * N2 + i
       v2 = v1 + 1
       v3 = i + 1
       v4 = i
       Faces(faceCount) = v1 & " " & v2 & " " & v4
          faceCount = faceCount + 1
       Faces(faceCount) = v2 & " " & v3 & " " & v4
          faceCount = faceCount + 1
    Next i
       v1 = CInt(N1) * N2 - 1
       v2 = (CInt(N1) - 1) * N2
       v3 = N2 - 1
       v4 = 0
       Faces(faceCount) = v1 & " " & v2 & " " & v3
          faceCount = faceCount + 1
       Faces(faceCount) = v2 & " " & v4 & " " & v3
          faceCount = faceCount + 1
   
    fNext = faceCount
'    myStr =  "in torus: center is (" & Cx & "," & Cy & "," & Cz & ") CL is (" & CLx & "," & CLy & "," & CLz & ")" +chr(13)
'    myStr = myStr + "        : vertex is (" & px & "," & py & "," & pz & ") and AP is (" & APx & "," & APy & "," & APz & ")"+chr(13)
'    myStr = myStr + "        : R1 = " & R1 & ", N1 = " & N1 & "  R2 = " & R2 & ", N2 = " & N2 & chr(13)
'    myStr = myStr + "        : rx = " & rx & ", ry = " & ry & ", rz = " & rz & " check " & (R2 + R1) & chr(13)
'    myStr = myStr + "        : Theta1 = " & theta1 & ", Theta2 = " & theta2 & chr(13)
'    print myStr
    
End Sub



Sub myArcC(B() As String)
' #-myArcC,C, Cx,Cy,Cz, ax,ay,az, Sx,Sy,Sz, alpha,N1, <R2>, <N2>
  Dim Cx, Cy, Cz As Double ' center point
  Dim ax, ay, az As Double ' direction=axis of rotation
  Dim R1, N1, R2, N2 As Double 'Radii and Steps
  PPPL = UBound(B)  ' How many optional parameters are set? Subscript out of range errors otherwise
  Cx = CDbl(Trim(B(2)))
  Cy = CDbl(Trim(B(3)))
  Cz = CDbl(Trim(B(4)))
  ax = CDbl(Trim(B(5)))
  ay = CDbl(Trim(B(6)))
  az = CDbl(Trim(B(7)))
  Sx = CDbl(Trim(B(8)))
  Sy = CDbl(Trim(B(9)))
  Sz = CDbl(Trim(B(10)))
  alpha = CDbl(Trim(B(11)))
  N1 = CInt(6)
  R2 = CDbl(global_radius)
  N2 = CInt(global_steps)
  If PPPL > 11 Then
	  If Len(B(12)) > 0 Then
	    N1 = CInt(Trim(B(12))) ' optional overide of N, number of steps
	    'Debug.Print "overide N1 = " & N1
	  End If
  End If
  If PPPL > 12 Then
     If Len(B(13)) > 0 Then
       R2 = CDbl(Trim(B(13)))  'optional overide of R for 1rst cap
     End If
  End If
  If PPPL > 13 Then
    If Len(B(14)) > 0 Then
      N2 = CInt(Trim(B(14))) ' optional overide of N, number of steps
      'Debug.Print "overide N2"
    End If
   End If
  'm = Sheets("Cyl").Cells(2, 11) 'number of cylinders
  ' ttt = sqrt(-1)
    Dim xx(30) As Double
    Dim yy(30) As Double  ' dimension must match n
    Dim zz(30) As Double
    
    Dim myXX, myYY, myZZ As Double
    Dim aa, bb, cc As Double
    Dim rx, ry, rz As Double
    Dim myLen As Double
    
    CLx = Sx
    CLy = Sy    'point on center line
    CLz = Sz
      
      '''''''''''''''''''''''''' Vector direction of central axis
      aa = ax
      bb = ay
      cc = az
      'dd = a * xC1 + b * yC1 + c * zC1
    
    rx = Sx - Cx
    ry = Sy - Cy
    rz = Sz - Cz
     
    
    ''''''''''''''''''''''''''''''''''' Unit Length
    myLen = Sqr(rx * rx + ry * ry + rz * rz)
    rx = rx / myLen
    ry = ry / myLen
    rz = rz / myLen
    
    ' stride in degrees around cenerline circle
    theta1 = alpha / N1
    theta2 = 360 / N2 'stride in degrees around profile ''pencil width
    

    
    px = rx * (myLen + R2) + Cx
    py = ry * (myLen + R2) + Cy ' point on surface
    pz = rz * (myLen + R2) + Cz
    
    '  i     j     k   '
    '  ax   ay     az  '
    ' CLx   CLy   CLz  '
    
    APx = ay * rz - az * ry
    APy = az * rx - ax * rz ' cross product vector normal to profile
    APz = ax * ry - ay * rx
    
   vertCount = 0
    'Vertices(vertCount) = Sx & " " & Sy & " " & Sz
    'vertCount = vertCount + 1

   '' First Profile ''''''''''''''''''''''''''''''''''''''''
   Call initRotate(CLx, CLy, CLz, APx, APy, APz, theta2)
   xx(0) = px
   yy(0) = py   'first point on first profile
   zz(0) = pz
     For i = 1 To N2 - 1
      myXX = xx(i - 1)
      myYY = yy(i - 1)
      myZZ = zz(i - 1)
        Call rotate(myXX, myYY, myZZ)
      xx(i) = myXX
      yy(i) = myYY
      zz(i) = myZZ
    Next i
    For i = 0 To N2 - 1
      Vertices(vertCount) = xx(i) & " " & yy(i) & " " & zz(i)
      vertCount = vertCount + 1
    Next i
    
    '' Rotate Profile N1 times '''''''''''''''''''''''''''''''
    Call initRotate(Cx, Cy, Cz, ax, ay, az, theta1)
    For j = 1 To N1
      For i = 0 To N2 - 1
        myXX = xx(i)
        myYY = yy(i)
        myZZ = zz(i)
          Call rotate(myXX, myYY, myZZ)
        xx(i) = myXX
        yy(i) = myYY
        zz(i) = myZZ
      Next i
      For i = 0 To N2 - 1  ' save new profile
      Vertices(vertCount) = xx(i) & " " & yy(i) & " " & zz(i)
      vertCount = vertCount + 1
      Next i
    Next j ''End of rotate profile outer loop
    
    Vertices(vertCount) = Sx & " " & Sy & " " & Sz
    vertCount = vertCount + 1
    Call initRotate(Cx, Cy, Cz, ax, ay, az, alpha)
    myXX = Sx
    myYY = Sy
    myZZ = Sz
        Call rotate(myXX, myYY, myZZ)
    Vertices(vertCount) = myXX & " " & myYY & " " & myZZ
    vertCount = vertCount + 1

    
    vNext = vertCount ' update global pointer
    
    '' Faces '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
       faceCount = 0
       vertCount = 0
    'N2 vertices per profile
    'N1 profiles
    For j = 0 To N1 - 1
      For i = 0 To N2 - 2
       v1 = j * N2 + i
       v2 = v1 + 1
       v3 = (j + 1) * N2 + i + 1
       v4 = v3 - 1
       Faces(faceCount) = v1 & " " & v2 & " " & v4
          faceCount = faceCount + 1
       Faces(faceCount) = v2 & " " & v3 & " " & v4
          faceCount = faceCount + 1
      Next i
       v1 = j * N2 + i
       v2 = j * N2
       v3 = (j + 1) * N2 + i
       v4 = v1 + 1
       Faces(faceCount) = v1 & " " & v2 & " " & v3
          faceCount = faceCount + 1
       Faces(faceCount) = v2 & " " & v4 & " " & v3
          faceCount = faceCount + 1

    Next j  ' end of loop through profiles

    
    ''' End Cap Start
    ' Center point is N1*N2
    ' 0 to N2 points on circumfrence
    For i = 0 To N2 - 2
    Faces(faceCount) = (N1 + 1) * N2 & " " & i + 1 & " " & i
    faceCount = faceCount + 1
    Next i
    Faces(faceCount) = (N1 + 1) * N2 & " " & 0 & " " & i
    faceCount = faceCount + 1
    
    ''' End Cap End
    ' Center point is N1*N2 + 1
    ' 0 to N2 points on circumfrence
    For i = 0 To N2 - 2
    Faces(faceCount) = 1 + (N1 + 1) * N2 & " " & (N1) * N2 + i + 1 & " " & i + (N1) * N2
    faceCount = faceCount + 1
    Next i
    Faces(faceCount) = 1 + (N1 + 1) * N2 & " " & (N1) * N2 & " " & i + (N1) * N2
    faceCount = faceCount + 1
 
    fNext = faceCount
   ' Debug.Print "in   arc: center is (" & Cx & "," & Cy & "," & Cz & ") CL is (" & CLx & "," & CLy & "," & CLz & ")"
   ' Debug.Print "        : start is (" & Sx & "," & Sy & "," & Sz & ")"
   ' Debug.Print "        : vertex is (" & px & "," & py & "," & pz & ") and AP is (" & APx & "," & APy & "," & APz & ")"
   ' Debug.Print "        : N1 = " & N1 & "; R2 = " & R2 & ", N2 = " & N2 & ";"
   ' Debug.Print "        : rx = " & rx & ", ry = " & ry & ", rz = " & rz & " check " & N1 * N2
   ' Debug.Print "        : Theta1 = " & theta1 & ", Theta2 = " & theta2
    
End Sub
Sub myChord(B() As String)
'#-myChord, C, Sx,Sy,Sz, Ex, Ey,Ez, ax,ay,az, H, N, <R1>, <N1>
  Dim Cx, Cy, Cz As Double ' center point
  Dim ax, ay, az As Double ' direction=axis of rotation
  Dim R1, N1, R2, N2 As Double 'Radii and Steps 1
  PPPL = UBound(B)  ' How many optional parameters are set? Subscript out of range errors otherwise
  Sx = CDbl(Trim(B(2)))
  Sy = CDbl(Trim(B(3)))
  Sz = CDbl(Trim(B(4)))
  Ex = CDbl(Trim(B(5)))
  Ey = CDbl(Trim(B(6)))
  Ez = CDbl(Trim(B(7)))
  ax = CDbl(Trim(B(8)))
  ay = CDbl(Trim(B(9)))
  az = CDbl(Trim(B(10)))
  H = CDbl(Trim(B(11)))
  N = CInt(Trim(B(12)))
  
  R1 = global_radius
  N1 = global_steps
  If PPPL > 12 Then
    If Len(B(13)) > 0 Then
      R1 = CDbl(Trim(B(13))) 'optional overide
    End If
  End If
  If PPPL > 13 Then
    If Len(B(14)) > 0 Then
      N1 = CInt(Trim(B(14))) ' optional overide of N, number of steps
      Debug.Print "overide N1"
    End If
  End If

    Dim xx(30) As Double
    Dim yy(30) As Double  ' dimension must match n
    Dim zz(30) As Double
    
    Dim myXX, myYY, myZZ As Double
    Dim aa, bb, cc As Double
    Dim rx, ry, rz As Double
    Dim myLen As Double
    
    'pi = 4# * Atn(1)
    
    If H = 0 Then
      B(8) = R1    'If H is zero, we have a straight line
      B(9) = R1
      B(10) = N1
      B(11) = ""
      B(12) = ""
      If PPPL > 12 Then
      B(13) = ""
      End If
      If PPPL > 13 Then
      B(14) = ""
      End If
    Call myCylinder(B())
    Exit Sub
    End If
    If H < 0 Then 'switch end points
      temp = Ex: Ex = Sx: Sx = temp
      temp = Ey: Ey = Sy: Sy = temp
      temp = Ez: Ez = Sz: Sz = temp
      H = H * -1
    End If
    
     
    '' Chord
    chordX = Ex - Sx
    chordY = Ey - Sy
    chordZ = Ez - Sz
    
    chordMidX = Sx + chordX / 2
    chordMidY = Sy + chordY / 2
    chordMidZ = Sz + chordZ / 2
    
    
    chordL = Sqr(chordX * chordX + chordY * chordY + chordZ * chordZ)
    chordXhat = chordX / chordL
    chordYhat = chordY / chordL
    chordZhat = chordZ / chordL
    
    'jumping to torus is not 'smooth' need more study
   ' If H > chordL And Trim(B(11)) < 0 Then
   '   '#-myTorus,C, Cx,Cy,Cz,ax,ay,az,R1,N1,<R2>,<N2>
   '    B(2) = chordMidX
   '    B(3) = chordMidY
   '    B(4) = chordMidZ
   '    B(5) = B(8)
   '    B(6) = B(9)   'direction
   '    B(7) = B(10)
   '    B(8) = chordL / 2
   '    B(9) = N
   '    B(10) = R1
   '    B(11) = N1
   '    Call myTorus(B())
   ' Exit Sub
   ' End If
    
    '' Radius
    circleR = chordL * chordL / (8 * H) + H / 2
    
    '' direction to circle center is axis cross cord
    '    i     j     k
    '   ax    ay     az
    '  crdX  crdY   crdZ
    ' i(ay*crdZ-az*crdY) + j(az*crdX-ax*crdZ) + k(ax*crdY-ay*crdX)
    dircenX = ay * chordZ - az * chordY
    dircenY = az * chordX - ax * chordZ
    dircenZ = ax * chordY - ay * chordX
    dircenL = Sqr(dircenX * dircenX + dircenY * dircenY + dircenZ * dircenZ)
    dircenX = dircenX / dircenL
    dircenY = dircenY / dircenL
    dircenZ = dircenZ / dircenL
    
    centerX = dircenX * (circleR - H) + chordMidX
    centerY = dircenY * (circleR - H) + chordMidY
    centerZ = dircenZ * (circleR - H) + chordMidZ
    
    'arcsin(x) = atn(x/sqr(-x*x+1)
    x = chordL / (2 * circleR)
    If x = 1 Then
      alpha = 180
    Else
      alpha = 2 * Atn(x / Sqr(-x * x + 1))
      alpha = alpha * 180 / pi
    End If
    If H > circleR Then
      alpha = 360 - alpha
      'Debug.Print "adding to alpha " & alpha
    End If
    
   ' Debug.Print "in chord: center is (" & centerX & "," & centerY & "," & centerZ & ") and r = " & circleR
   ' Debug.Print "        : start is (" & Sx & "," & Sy & "," & Sz & ")  and N = " & N
   ' Debug.Print "        : MidChord is (" & chordMidX & "," & chordMidY & "," & chordMidZ & ") "
   ' Debug.Print "        : dirCenter is (" & dircenX & "," & dircenY & "," & dircenZ & ")  and H = " & H
   ' Debug.Print "        : end is (" & Ex & "," & Ey & "," & Ez & ") and  L = " & chordL
   ' Debug.Print "        : Central Angle = " & alpha & "   , x = " & x
   ' Debug.Print "        : R1 = " & R1 & ", N1 = " & N1
    
    ' #-myArcC,C, Cx,Cy,Cz, ax,ay,az, Sx,Sy,Sz, alpha,N1, <R2>, <N2>
    B(2) = centerX
    B(3) = centerY
    B(4) = centerZ
    B(5) = ax
    B(6) = ay
    B(7) = az
    B(8) = Sx
    B(9) = Sy
    B(10) = Sz
    B(11) = alpha
    B(12) = N
    If PPPL > 12 Then
    B(13) = R2
    End If
    If PPPL > 13 Then
    B(14) = N2
    End If
    Call myArcC(B())
End Sub


Sub mySphere80(B() As String)
' 2 | Color | Cx | Cy | Cz | Radius
'#-sphere80,C, Cx,Cy,Cz,R
  ' DATA FOR INDEXED FACE SET OF AN 80 TRIANGLE SPHERE OF RADIUS 1
  '  PP(x,y,z) are the points--scale these to desired radius; then add offset
  '  FF(1,2,3) are the three indexes defining the triangular face in counterclockwise orientation
  PPX = Array(0, 0.723607, 0.723607, -0.276393, -0.894427, -0.276393, 0.894427, 0.276393, -0.723607, -0.723607, 0.276393, 0, 0.850651, 0.425325, 0.425325, 0.262866, -0.16246, -0.688191, -0.525731, -0.688191, -0.16246, 0.262866, 0.951056, 0.951056, 0, 0.587785, -0.951056, -0.587785, -0.587785, -0.951056, 0.587785, 0, 0.688191, -0.262866, -0.850651, -0.262866, 0.688191, 0.16246, 0.525731, -0.425325, -0.425325, 0.16246)
  PPY = Array(0, -0.525731, 0.525731, 0.850651, 0, -0.850651, 0, 0.850651, 0.525731, -0.525731, -0.850651, 0, 0, 0.309017, -0.309017, 0.809017, 0.5, 0.5, 0, -0.5, -0.5, -0.809017, 0.309017, -0.309017, 1, 0.809017, 0.309017, 0.809017, -0.809017, -0.309017, -0.809017, -1, 0.5, 0.809017, 0, -0.809017, -0.5, 0.5, 0, 0.309017, -0.309017, -0.5)
  PPZ = Array(1, 0.447214, 0.447214, 0.447214, 0.447214, 0.447214, -0.447214, -0.447214, -0.447214, -0.447214, -0.447214, -1, 0.525731, 0.850651, 0.850651, 0.525731, 0.850651, 0.525731, 0.850651, 0.525731, 0.850651, 0.525731, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.525731, -0.525731, -0.525731, -0.525731, -0.525731, -0.850651, -0.850651, -0.850651, -0.850651, -0.850651)
  FF1 = Array(1, 15, 14, 14, 1, 14, 17, 17, 1, 17, 19, 19, 1, 19, 21, 21, 1, 21, 15, 15, 2, 24, 13, 13, 3, 26, 16, 16, 4, 28, 18, 18, 5, 30, 20, 20, 6, 32, 22, 22, 7, 33, 23, 23, 8, 34, 25, 25, 9, 35, 27, 27, 10, 36, 29, 29, 11, 37, 31, 31, 7, 39, 33, 33, 8, 38, 34, 34, 9, 40, 35, 35, 10, 41, 36, 36, 11, 42, 37, 37)
  FF2 = Array(15, 2, 13, 15, 14, 3, 16, 14, 17, 4, 18, 17, 19, 5, 20, 19, 21, 6, 22, 21, 24, 7, 23, 24, 26, 8, 25, 26, 28, 9, 27, 28, 30, 10, 29, 30, 32, 11, 31, 32, 33, 8, 26, 33, 34, 9, 28, 34, 35, 10, 30, 35, 36, 11, 32, 36, 37, 7, 24, 37, 39, 12, 38, 39, 38, 12, 40, 38, 40, 12, 41, 40, 41, 12, 42, 41, 42, 12, 39, 42)
  FF3 = Array(14, 13, 3, 13, 17, 16, 4, 16, 19, 18, 5, 18, 21, 20, 6, 20, 15, 22, 2, 22, 13, 23, 3, 23, 16, 25, 4, 25, 18, 27, 5, 27, 20, 29, 6, 29, 22, 31, 2, 31, 23, 26, 3, 26, 25, 28, 4, 28, 27, 30, 5, 30, 29, 32, 6, 32, 31, 24, 2, 24, 33, 38, 8, 38, 34, 40, 9, 40, 35, 41, 10, 41, 36, 42, 11, 42, 37, 39, 7, 39)

    r = CDbl(Trim(B(5))) 'Radius of Sphere
  xxx = CDbl(Trim(B(2)))
  yyy = CDbl(Trim(B(3)))
  zzz = CDbl(Trim(B(4)))
  vNext = 0
  fNext = 0
  
  For j = 0 To 41
  
    myStr = PPX(j) * r + xxx & " " & PPY(j) * r + yyy & " " & PPZ(j) * r + zzz
    Vertices(vNext) = myStr
    ' bug.Print myStr
    vNext = vNext + 1
  Next j

  faceCount = 0
  For j = 0 To 79

    myStr = FF1(j) - 1 & " " & FF2(j) - 1 & " " & FF3(j) - 1
    Faces(fNext) = myStr
    ' Debug.Print myStr
    fNext = fNext + 1
  Next j

End Sub


Sub myHemisphere(B() As String)
' 5 | Color | Cx | Cy | Cz | Px | Py | Pz | Radius | Steps
  Dim Cx, Cy, Cz As Double ' center point
  Dim ax, ay, az As Double ' direction=axis of rotation
  Dim R1, N1, R2, N2 As Double 'Radii and Steps
  Dim theta, phi as Double
  PPPL = UBound(B)  ' How many optional parameters are set? Subscript out of range errors otherwise
  Cx = CDbl(Trim(B(2)))
  Cy = CDbl(Trim(B(3)))
  Cz = CDbl(Trim(B(4)))
  ax = CDbl(Trim(B(5)))
  ay = CDbl(Trim(B(6)))
  az = CDbl(Trim(B(7)))
  R1 = CDbl(Trim(B(8)))
  stepsLat = CDbl(Trim(B(9)))
  R2 = global_radius
  N2 = global_steps
  If PPPL > 9 Then
     If Len(B(10)) > 0 Then
       R2 = CDbl(Trim(B(10)))  'optional overide of R for 1rst cap
     End If
  End If
  If PPPL > 10 Then
    If Len(B(11)) > 0 Then
      N2 = CDbl(Trim(B(11))) ' optional overide of N, number of steps
     ' Debug.Print "overide N"
    End If
   End If

    Dim xx(30) As Double
    Dim yy(30) As Double  ' dimension must match n
    Dim zz(30) As Double
    
    Dim myXX, myYY, myZZ As Double
    Dim aa, bb, bc As Double
    Dim rx, ry, rz As Double
    Dim myLen As Double
  
  vNext = 0
  fNext = 0
  

       
      
 ''' computer vector Head - Tail from base to Pole     
      aa = ax - Cx
      bb = ay - Cy
      cc = az - Cz
      'd = a * xC1 + b * yC1 + c * zC1
      
 ''' save center of first profile at index zero
    Vertices(vNext) = Cx & " " & " " & Cy & " " & Cz
    vNext = vNext + 1
      
''' get direction to point on first profile
           If Abs(aa) < 0.000001 And Abs(bb) < 0.000001 Then
              rx = 0.0
              ry = cc
              rz = -1.0 * bb
            Else
              rx = bb
              ry = -1.0 * aa
              rz = 0.0
            End If

     ''' normalize to unit length
        LLL = Sqr(rx*rx + ry*ry + rz*rz)
        rx = rx/LLL
        ry = ry/LLL
        rz = rz/LLL
        
     ''' normalize to unit length
     ''' vector from base center to pole
        LLL = Sqr(aa*aa + bb*bb + cc*cc)
        aa = aa/LLL
        bb = bb/LLL
        cc = cc/LLL
      
      phi = pi/(2*stepsLat)
      theta = 360.0/N2
      
      ''' set first point on fist profile
        xx(0) = Cx + rx * R1
        yy(0) = Cy + ry * R1
        zz(0) = Cz + rz * R1
        Vertices(vNext) = xx(0) & " " & yy(0) & " " & zz(0)
        vNext = vNext + 1
        
        ''' generate rest of points on first profile
        Call initRotate(Cx, Cy, Cz, aa, bb, cc, theta)
        For i = 1 To N2 - 1
            myXX = xx(i - 1)
            myYY = yy(i - 1)
            myZZ = zz(i - 1)
            Call rotate(myXX, myYY, myZZ)
            xx(i) = myXX
            yy(i) = myYY
            zz(i) = myZZ
            Vertices(vNext) = xx(i) & " " & yy(i) & " " & zz(i)
            vNext = vNext + 1
        Next i    
        
      
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    For myLat = 2 To stepsLat - 1
      R = R1*cos((myLat-1)*phi)  ' jReduce each lattitude radius
      x2 = Cx + aa *sin((myLat-1)*phi)
      y2 = Cy + bb *sin((myLat-1)*phi)
      z2 = Cz + cc *sin((myLat-1)*phi)
      xx(0) = x2 + rx*R
      yy(0) = y2 + ry*R
      zz(0) = z2 + rz*R
      Vertices(vNext) = xx(0) & " " & yy(0) & " " & zz(0)
      vNext = vNext + 1
      ''' rotation stays the same: same axis same angle  
      For i = 1 To N2 -1
            myXX = xx(i - 1)
            myYY = yy(i - 1)
            myZZ = zz(i - 1)
            Call rotate(myXX, myYY, myZZ)
            xx(i) = myXX
            yy(i) = myYY
            zz(i) = myZZ
            Vertices(vNext) = xx(i) & " " & yy(i) & " " & zz(i)
            vNext = vNext + 1
      Next i    
    Next myLat
    ''' save pole at index 2n+1
    Vertices(vNext) = ax & " " & ay & " " & az
    vNext = vNext + 1   

   

     ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  '
    'Equitorial base
     v1 = N2
     v2 = 1
     v3 = 0
     Faces(fNext) = v1 & " " & v2 & " " & v3
     fNext = fNext + 1
     For i = 1 To N2 - 1
       v1 = i
       v2 = i + 1
       v3 = 0
       Faces(fNext) = v1 & " " & v2 & " " & v3
       fNext = fNext + 1
     Next i
  
  For j = 0 To stepsLat - 3 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        v0 = j*N2+N2
        v1 = (j + 1) * N2 + N2
        v2 = j*N2+1
        v3 = (j + 1)*N2 + 1
        Faces(fNext) = v0 & " " & v1 & " " & v2
        fNext = fNext + 1
        Faces(fNext) = v1 & " " & v3 & " " & v2
        fNext = fNext + 1

      For i = 1 To N2 - 1
        v0 = j*N2 + i
        v1 = (j + 1) * N2 + i
        v2 = v1 + 1
        v3 = v0 + 1
        Faces(fNext) = v0 & " " & v1 & " " & v2
        fNext = fNext + 1
        Faces(fNext) = v3 & " " & v0 & " " & v2
        fNext = fNext + 1
       Next i
      
  Next j ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  'Polar region
    
    v3 = vNext - 1
    For i = N2 to 2 step -1
      v1 = (stepsLat -1)*(N2-1) + i +1
      v2 = v1 - 1
      Faces(fNext) = v1 & " " & v2 & " " & v3
      fNext = fNext + 1
    Next i
    v1 = v2
    v2 = v3 - 1
    Faces(fNext) = v1 & " " & v2 & " " & v3
    fNext = fNext + 1

End Sub

sub doit
 call Main
end sub

Sub myExtrusion(B() As String)
  'R = Sheets("Scene").Cells(2, 9) ' Radius of hemisphere base
  'N = Sheets("Scene").Cells(m, 6) ' number of polygon vertices
         r = global_radius
    deltaX = CDbl(Trim(B(2)))
    deltaY = CDbl(Trim(B(3)))
    deltaZ = CDbl(Trim(B(4)))
         N = CDbl(Trim(B(5)))
  Dim xx(30) As Double
  Dim yy(30) As Double  ' dimension must match N 30 seems safe for now
  Dim zz(30) As Double
  
  Dim nn As Integer 'used to force N/2 to be integer
  
 '#-extrusion,C, dx,dy,dz, N, 1x,1y,1z, 2x,2y,2z, ... Nx,Ny,Nz
  vNext = 0
  
  For i = 1 To N
    xx(i - 1) = CDbl(Trim(B(6 + 3 * (i - 1))))
    yy(i - 1) = CDbl(Trim(B(7 + 3 * (i - 1))))
    zz(i - 1) = CDbl(Trim(B(8 + 3 * (i - 1))))
    Debug.Print i, xx(i), yy(i), zz(i)
  Next i
  
  'assume bottom face is listed pointing into center of volume
  For i = 0 To N - 1
    Vertices(vNext) = xx(i) & " " & yy(i) & " " & zz(i)
    vNext = vNext + 1
  Next i
  
  'top face
  For i = 0 To N - 1
    Vertices(vNext) = xx(i) + deltaX & " " & yy(i) + deltaY & " " & zz(i) + deltaZ
    vNext = vNext + 1
   Next i

    ''''''''''''''''''''''''''''''''''''''''''''''' Extrusion Faces ''''''''''''''''''''
    fNext = 0
    nn = Int(N / 2)
    
    ''given face
    Debug.Print nn
    If N Mod 2 = 0 Then
     Debug.Print "even N"
     For i = 1 To nn
      v1 = i - 1
      v2 = v1 + 1
      v3 = N - i - 1
      v4 = v3 + 1
      If v4 - v2 > 1 Then
        Faces(fNext) = v4 & " " & v2 & " " & v1
        fNext = fNext + 1
        Faces(fNext) = v2 & " " & v4 & " " & v3
        fNext = fNext + 1
      End If
      Next i
    Else
      Debug.Print "odd N " & nn
      For i = 1 To nn
      Debug.Print "         i = " & i
      v1 = i - 1
      v2 = v1 + 1
      v3 = N - i - 1
      v4 = v3 + 1
       Faces(fNext) = v4 & " " & v2 & " " & v1
       fNext = fNext + 1
      If v4 - v2 > 1 Then
       Faces(fNext) = v2 & " " & v4 & " " & v3
       fNext = fNext + 1
      End If
      Next i
    End If

''extruded face
    Debug.Print nn
    If N Mod 2 = 0 Then
     Debug.Print "even N"
     For i = 1 To nn
      v1 = N + i - 1
      v2 = v1 + 1
      v3 = N + N - i - 1
      v4 = v3 + 1
      If v4 - v2 > 1 Then
        Faces(fNext) = v1 & " " & v2 & " " & v4
        fNext = fNext + 1
        Faces(fNext) = v3 & " " & v4 & " " & v2
        fNext = fNext + 1
       End If
      Next i
    Else
      Debug.Print "odd N"
      For i = 1 To nn
      v1 = N + i - 1
      v2 = v1 + 1
      v3 = N + N - i - 1
      v4 = v3 + 1
      Faces(fNext) = v1 & " " & v2 & " " & v4
      fNext = fNext + 1
      
     If v4 - v2 > 1 Then
       Faces(fNext) = v3 & " " & v4 & " " & v2
       fNext = fNext + 1
     End If
      
      Next i
    End If

    '''''' side faces next
    For i = 1 To N - 1
      v1 = i - 1
      v2 = v1 + 1
      v3 = i + N - 1
      v4 = v3 + 1
      Faces(fNext) = v1 & " " & v2 & " " & v3
      fNext = fNext + 1
      Faces(fNext) = v4 & " " & v2 & " " & v3
      fNext = fNext + 1
    Next i
      v1 = N - 1
      v2 = 0
      v3 = N + N - 1
      v4 = v1 + 1
      Faces(fNext) = v1 & " " & v2 & " " & v3
      fNext = fNext + 1
      Faces(fNext) = v4 & " " & v2 & " " & v3
      fNext = fNext + 1
End Sub
