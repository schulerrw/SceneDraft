#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone                                                        #
import cylinder                                                    #
#      torus                                                       #
#      arc               # This file.                              #
#      sphere                                                      #
#      hemisphere                                                  #
#      extrusion                                                   #
import math              # Native Python--not part of Scen~Draft   #
import transform3d                                                 #
####################################################################
'''
4 <color> Cx Cy Cz Ax Ay Az Sx Sy Sz phi n1 Radius Steps
5 <color> Sx Sy Sz Ex Ey Ez Ax Ay Az H  n1 Radius Steps
'''
'''
+------------------------------------+  +------------------------------------+
|  Arc                               |  |  Arc2                              |
+------------------------------------+  +------------------------------------+
| Type = 4                           |  | Type = 5                           |
| C color                            |  | C color                            |
| Cx                                 |  | Sx                                 |
| Cy  center                         |  | Sy  start                          |
| Cz                                 |  | Sz                                 |
| Ax                                 |  | Ex                                 |
| Ay  axis                           |  | Ey  end                            |
| Az                                 |  | Ez                                 |
| Sx                                 |  | Ax                                 |
| Sy  start                          |  | Ay  axis                           |
| Sz                                 |  | Az                                 |
| phi angle in degrees               |  | H   chord height                   |
| n1 smoothness of arc (#steps)      |  | N1 smoothness of arc (#steps)      |
| <R2> optional radius override      |  | <R2> optional radius override      |
| <N2> optional steps override       |  | <N2> optional steps override       |
| Ex                                 |  +------------------------------------+
| Ey  computed End point             |  | __init__(self, line, gRad, gStep)  |
| Ez                                 |  | describe(self)                     |
+------------------------------------+  | vertices(self)                     |
| __init__(self, line, gRad, gStep)  |  | faces(self)                        |
| describe(self)                     |  | __main__()                         |
| vertices(self)                     |  +------------------------------------+
| faces(self)                        |  
| __main__()                         |  
+------------------------------------+  
'''
"""
Scene~Draft a framework for rendering 3D scenes
Copyright 2026 Robert W. Schuler

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
"""

class arc:
    def __init__(self, line, globalRadius, globalSteps):
        self.globalRadius = globalRadius
        self.globalSteps = globalSteps
        self.verts = []  # used for analytic arc method
        self.profile = [] # used for analytic arc method (one 'circle')
        try:
            self.color = int(line[1])
            self.Cx = float(line[2])
            self.Cy = float(line[3])
            self.Cz = float(line[4])
            self.Ax = float(line[5])
            self.Ay = float(line[6])
            self.Az = float(line[7])
            self.Sx = float(line[8])
            self.Sy = float(line[9])
            self.Sz = float(line[10])
            self.phi= float(line[11])
            self.n1 = int(line[12])
        except Exception:
            print(f"Error parsing {line} in arc.__init__()")
            self.color = 0
            self.Cx = self.Cy = self.Cz = 0
            self.Sx = 1
            self.Sy = self.Sz = 0
            self.Ax = self.Ay = 0
            self.Az = 1
            self.phi = 90
            self.n1 = 6
        try:
            self.r2 = float(line[13])
            if self.r2 < 0:
                self.r2 = -1.0
        except Exception:
            self.r2 = globalRadius

        try:
            self.n2 = int(line[14])
            if self.n2 < 3:
                self.n2 = -1    
        except Exception:
            self.n2 = globalSteps

        rx = self.Sx - self.Cx
        ry = self.Sy - self.Cy
        rz = self.Sz - self.Cz
        self.R1 = math.sqrt(rx*rx + ry*ry + rz*rz)
        rx = rx/self.R1
        ry = ry/self.R1  # now its a unit direction from centerline
        rz = rz/self.R1

        xxx = math.sqrt(self.Ax*self.Ax + self.Ay*self.Ay + self.Az*self.Az)
        self.Ax = self.Ax/xxx
        self.Ay = self.Ay/xxx
        self.Az = self.Az/xxx

        '''
        Start is the first point on the centerline 'trace' of
        the Torus-section.
           theta is the angle-step around circle of centerline
           Cxyz is center of torus
           Axyz is direction of axis of torus
           [rx,ry,rz] is direction from center to first centerline point.
           cross product of Ax X [rx,ry,rz] will give axis of rotation for first profile
           [ i   j   k ]
           [ Ax  Ay  Az] -->  i*Ay*rz + j*Az*rx + k*Ax*ry -i*Az*ry -j*Ax*rz -k*Ay*rx
           [ rx  ry  rz] ---> i(Ay*rz -Az*ry) + j(Az*rx -Ax*rz) + k(Ax*ry -Ay*rx)
        '''
        axx = self.Ay*rz - self.Az*ry
        ayy = self.Az*rx - self.Ax*rz
        azz = self.Ax*ry - self.Ay*rx

        # point on first profile
        xxx = self.Sx + self.r2*rx
        yyy = self.Sy + self.r2*ry
        zzz = self.Sz + self.r2*rz

        self.verts.append([self.Sx, self.Sy, self.Sz]) # center of first profile
        self.verts.append([xxx, yyy, zzz])
        self.profile.append([xxx, yyy, zzz])

        phi = 360 / self.n2
        T = transform3d.transform3d(self.Sx, self.Sy, self.Sz, axx, ayy, azz, phi)
        for i in range(1, self.n2):
            [xxx, yyy, zzz] = T.rotate(xxx, yyy, zzz)
            self.verts.append([xxx, yyy, zzz])
            self.profile.append([xxx, yyy, zzz])

        '''
        sweep trace to feed to cable.
        '''
        theta = 360 / self.n1
        T = transform3d.transform3d(self.Cx, self.Cy, self.Cz, self.Ax, self.Ay, self.Az, theta)
        for i in range(1, self.n1):
            for j in self.profile:
                [j[0] , j[1], j[2]] = T.rotate(j[0], j[1], j[2])
                self.verts.append([j[0] , j[1], j[2]])
        
        '''
        Center of last profile
        '''
        T = transform3d.transform3d(self.Cx, self.Cy, self.Cz, self.Ax, self.Ay, self.Az, self.phi)
        [xx, yy, zz] = T.rotate(self.Sx, self.Sy, self.Sz)
        self.verts.append([xx, yy, zz])
        self.Ex = xx
        self.Ey = yy  # save end point for fun and profit
        self.Ez = zz

        return 
        



    def describe(self):
        # https://qpsllc.com/Perpendiculars/Orthogonal%20to%20a%203d%20Line.html
        print(f'arc:   center ({self.Cx},{self.Cy},{self.Cz}) start ({self.Sx}, {self.Sy}, {self.Sz})  end: ({self.Ex}, {self.Ey}, {self.Ez})')
        print(f'         R  = {self.R1}  and N  = {self.n1}')
        print(f'         R2 = {self.r2} and N1 = {self.n2}')
        print(f'         globalRadius = {self.globalRadius}  and  globalSteps = {self.globalSteps}')

    def vertices(self, cableIt=False):
        '''
        Class is initialized. No need to store vertices as local/self
        Compute and return a list of 0-indexed vertices. Caller is
        responsible for keeping track of global offset.
        '''
     
        return self.verts
 
  
    def faces(self):
        '''
        Class is initialized. No need to store face vertices as local/self
        Compute and return a list of 0-indexed faces. Caller is
        responsible for keeping track of global offset to first vertex and
        next face
        '''
        faces = []
        # '2n + 2 vertices per cylinder
        N = int(self.globalSteps)
        if self.n1 > 0:
            N = self.n1   
        
        m = N + 1
        print(f'in faces, there are {m} profiles to connect')
        
        # first end cap
        faces.append([N, 1, 0])
        for i in range(1,N):
            faces.append([i, i+1, 0])

        # faces between profiles
        for j in range(0, m-1):
            v1 = j*N+N
            v2 = (j+1)*N + N
            v3 = j*N + 1
            v4 = (j+1)*N + 1
            faces.append([v1, v2, v3])
            faces.append([v2, v4, v3])

            for i in range(1, N):
                v1 = j*N + i
                v2 = (j+1)*N + i
                v3 = v2+1
                v4 = v1 + 1
                faces.append([v1, v2, v3])
                faces.append([v4, v1, v3])

        # last end cap
        for i in range(N, 1, -1):
            v1 = (m - 1)*N + i
            v2 = v1 - 1
            v3 = m * N + 1
            faces.append([v1, v2, v3])
        v1 = (m - 1) * N + 1
        v2 = m * N
        v3 = v2 + 1
        faces.append([v1, v2, v3])

        return faces
    
class arc2:
    def __init__(self, line, globalRadius, globalSteps):
        self.globalRadius = globalRadius
        self.globalSteps = globalSteps
        self.verts = []  # used for analytic arc method
        self.profile = [] # used for analytic arc method (one 'circle')
        # -myChord, C, Sx,Sy,Sz, Ex, Ey,Ez, ax,ay,az, H, N, <R1>, <N1>
        print(line)
        try:
            self.color = int(line[1])
            self.Sx = float(line[2])
            self.Sy = float(line[3])
            self.Sz = float(line[4])
            self.Ex = float(line[5])
            self.Ey = float(line[6])
            self.Ez = float(line[7])
            ax = float(line[8])
            ay = float(line[9])
            az = float(line[10])
            HH  = float(line[11])
            self.N  = int(line[12])
        
        except Exception:
            print(f"Error parsing {line} in arc2.__init__() az={self.az}")
            self.color = 0
            self.Cx = self.Cy = self.Cz = 0
            self.Sx = 1
            self.Sy = self.Sz = 0
            self.Ex = self.Ez = 0
            self.Ey = 1
        try:
            self.r1 = float(line[13])
            if self.r1 < 0:
                self.r1 = -1.0
            self.n1 = int(line[14])
            if self.n1 < 3:
                self.n1 = -1
        except Exception:
            self.r1 = -1.0
            self.n1 = -1
            # no options on line

        if HH == 0: # we have a straight line
            myLine = []
            myLine.append(self.color)
            myLine.append(self.Sx)
            myLine.append(self.Sy)
            myLine.append(self.Sz)
            myLine.append(self.Ex)
            myLine.append(self.Ey)
            myLine.append(self.Ez)
            if self.r1 > 0 and self.n1 > 0:
                myLine.append(self.r1)
                myLine.append(self.n1)
                myLine.append(self.r1)
                myLine.append(self.n1)
            self.E = cylinder.cylinder(myLine, globalRadius, globalSteps)
            self.Cx = 0.5*(self.Ex - self.Sx)
            self.Cy = 0.5*(self.Ey - self.Sy)
            self.Cz = 0.5*(self.Ez - self.Sz)
            return

        if HH < 0: #'switch end points
            temp = self.Ex; self.Ex = self.Sx; self.Sx = temp
            temp = self.Ey; self.Ey = self.Sy; self.Sy = temp
            temp = self.Ez; self.Ez = self.Sz; self.Sz = temp
            HH = HH * -1
    
     
        # Chord
        chordX = self.Ex - self.Sx
        chordY = self.Ey - self.Sy
        chordZ = self.Ez - self.Sz
        
        chordMidX = self.Sx + chordX / 2
        chordMidY = self.Sy + chordY / 2
        chordMidZ = self.Sz + chordZ / 2
        
        
        chordL = math.sqrt(chordX * chordX + chordY * chordY + chordZ * chordZ)
        # chordXhat = chordX / chordL
        # chordYhat = chordY / chordL
        # chordZhat = chordZ / chordL

        #'' Radius given chord-length and chord-height
        circleR = chordL * chordL / (8 * HH) + HH / 2
    
        #'' direction to circle center is axis cross cord
        # '    i     j     k
        # '   ax    ay     az
        # '  crdX  crdY   crdZ
        # ' i(ay*crdZ-az*crdY) + j(az*crdX-ax*crdZ) + k(ax*crdY-ay*crdX)
        dircenX = ay * chordZ - az * chordY
        dircenY = az * chordX - ax * chordZ
        dircenZ = ax * chordY - ay * chordX
        dircenL = math.sqrt(dircenX * dircenX + dircenY * dircenY + dircenZ * dircenZ)
        dircenX = dircenX / dircenL
        dircenY = dircenY / dircenL
        dircenZ = dircenZ / dircenL
        
        centerX = dircenX * (circleR - HH) + chordMidX
        centerY = dircenY * (circleR - HH) + chordMidY
        centerZ = dircenZ * (circleR - HH) + chordMidZ
        
        # 'arcsin(x) = atn(x/sqr(-x*x+1)
        x = chordL / (2 * circleR)
        if x == 1:
            alpha = 180.0
        else:
            alpha = 2 * math.atan(x / math.sqrt(-x * x + 1))
            alpha = alpha * 180 / math.pi

        if HH > circleR:
            alpha = 360.0 - alpha

        myLine = []
        myLine.append(4)
        myLine.append(self.color)
        myLine.append(centerX)
        myLine.append(centerY)
        myLine.append(centerZ)
        myLine.append(ax)
        myLine.append(ay)
        myLine.append(az)
        myLine.append(self.Sx)
        myLine.append(self.Sy)
        myLine.append(self.Sz)
        myLine.append(alpha)
        myLine.append(self.N)
        # If PPPL > 12 Then
        # B(13) = R2
        # End If
        # If PPPL > 13 Then
        # B(14) = N2
        # End If
        # Call myArcC(B())
        self.E = arc(myLine,globalRadius,globalSteps)
        self.Cx = centerX
        self.Cy = centerY
        self.Cz = centerZ
        return 
        



    def describe(self):
        # https://qpsllc.com/Perpendiculars/Orthogonal%20to%20a%203d%20Line.html
        print(f'arc2:  center ({self.Cx},{self.Cy},{self.Cz}) start ({self.Sx}, {self.Sy}, {self.Sz})  end: ({self.Ex}, {self.Ey}, {self.Ez})')
        # print(f'actual center (angle {self.angle}  myStep {self.myStep}')
        # print(f'         R  = {self.R}  and N  = {self.N}  t = {self.t}')
        print(f'         R1 = {self.r1} and N1 = {self.n1}')
        print(f'         globalRadius = {self.globalRadius}  and  globalSteps = {self.globalSteps}')
        # print(f'         mid point ({self.MIDx}, {self.MIDy}, {self.MIDz})')
        # print(f'         V1        ({self.V1x}, {self.V1y}, {self.V1z})')
        # print(f'         DC {self.DC}  angle = {self.angle}')
           
    def vertices(self):
        EE = self.E.vertices()
        return EE
 
  
    def faces(self):
        return self.E.faces()
    
if __name__ == "__main__":
    print("arc")
    E = arc([6,0, 0,0,0, 1,0,0, 0,1,0, 1.0, 5], .3, 6)
    E.describe()
    EE = E.vertices()
    print(f"vertices {len(EE)}:")
    print(EE)
    FF = E.faces()
    print(f"FACES {len(FF)}:")
    print(FF)

    print("arc2 H=0")
    E = arc2([5,0, 0,0,0, 0,1,0, 0,0,1, 0.0, 6], .3, 6)
    E.describe()
    EE = E.vertices()
    print(f"vertices {len(EE)}:")
    print(EE)
    FF = E.faces()
    print(f"FACES {len(FF)}:")
    print(FF)

    print("arc2 H=0.1")
    E = arc2([5,0, 0,0,0, 0,1,0, 0,0,1, -0.1, 6], .3, 6)
    E.describe()
    EE = E.vertices()
    print(f"vertices {len(EE)}:")
    print(EE)
    FF = E.faces()
    print(f"FACES {len(FF)}:")
    print(FF)
