#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone                                                        #
#      cylinder                                                    #
#      torus             # This file.                              #
#      arc                                                         #
#      sphere                                                      #
#      hemisphere                                                  #
#      extrusion                                                   #
import math              # Native Python--not part of Scen~Draft   #
import transform3d                                                 #
####################################################################
'''
3 <color> Cx Cy Cz Ax Ay Az R1 N1 Radius Steps
'''
'''
+-------------------------------------+
|  Torus                              |
+-------------------------------------+
| Type = 3                            |
| C color                             |
| Cx                                  |
| Cy  center                          |
| Cz                                  |
| Ax                                  | 
| Ay  axis                            |
| Az                                  |
| R1 radius                           |
| N1 smoothness of circle (#steps)    |
| <R2> optional radius override       |
| <N2> optional steps override        |
+-------------------------------------+
| __init__(self, line, gRad, gStep)   |
| describe(self)                      |
| vertices(self)                      | 
| faces(self)                         |
| __main__()                          |
+-------------------------------------+
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

class torus:
    def __init__(self, line, globalRadius, globalSteps):
        self.verts = []  # used for analytic torus method
        self.profile = [] # used for analytic torus method (one 'circle')
        try:
            self.color = int(line[1])
            self.Cx = float(line[2])
            self.Cy = float(line[3])
            self.Cz = float(line[4])
            self.Ax = float(line[5])
            self.Ay = float(line[6])
            self.Az = float(line[7])
            self.r1 = float(line[8])
            self.n1 = int(line[9])
        except Exception:
            print("Error parsing {line} in cylinder.__init__()")
            self.color = 0
            self.Cx = self.Cy = self.Cz = 0
            self.Ax = self.Ay = self.Az = 1.0
            self.r1 = 1
            self.n1 = 6
        try:
            self.r2 = float(line[10])
            if self.r2 < 0:
                self.r2 = -1.0
        except Exception:
            self.r2 = -1.0
        try:
            self.n2 = int(line[11])
            if self.n2 < 3:
                self.n2 = -1
        except Exception:
            self.n2 = -1
        
        if self.r2 < 0:
            self.r2 = globalRadius
        if self.n2 < 0:
            self.n2 = globalSteps

        xxx = math.sqrt(self.Ax*self.Ax + self.Ay*self.Ay + self.Az*self.Az)
        self.Ax = self.Ax/xxx
        self.Ay = self.Ay/xxx
        self.Az = self.Az/xxx

        a = self.r1 * self.Ax - self.Cx
        b = self.r1 * self.Ay - self.Cy
        c = self.r1 * self.Az - self.Cz
        if abs(a) < 0.000001 and abs(b) < 0.000001:
            rx = 0
            ry = c
            rz = -1 * b
        else:
            rx = b
            ry = -1 * a
            rz = 0
        
        xxx = math.sqrt(rx * rx + ry * ry + rz * rz) # denom
        rx = rx / xxx
        ry = ry / xxx
        rz = rz / xxx
        xx = self.r1*rx+self.Cx
        yy = self.r1*ry+self.Cy
        zz = self.r1*rz + self.Cz

        '''
        At this point, we've found the first point on the centerline 'trace' of
        the Torus.
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
        xxx = xx + self.r2*rx
        yyy = yy + self.r2*ry
        zzz = zz + self.r2*rz

        self.verts.append([xxx, yyy, zzz])
        self.profile.append([xxx, yyy, zzz])

        phi = 360 / self.n2
        T = transform3d.transform3d(xx, yy, zz, axx, ayy, azz, phi)
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
            [xx, yy, zz] = T.rotate(xx, yy, zz)
            for j in self.profile:
                [j[0] , j[1], j[2]] = T.rotate(j[0], j[1], j[2])
                self.verts.append([j[0] , j[1], j[2]])


    def describe(self):
        print(f'Torus: center ({self.Cx},{self.Cy},{self.Cz}) axis ({self.Ax}, {self.Ay}, {self.Az})')
        print(f'         R1 = {self.r1} and N1 = {self.n1}')
        print(f'         r2 = {self.r2}  and  n2 = {self.n2}')
        print(f'{len(self.verts)} VERTS  each with {len(self.profile)} PROFILE POINTS')
        for i in self.verts:
            print(f' [{i[0]}, {i[1]}, {i[2]}]')
    
    def vertices(self, cableIt=False):
        '''
        Class is initialized. No need to store vertices as local/self
        Compute and return a list of 0-indexed vertices. Caller is
        responsible for keeping track of global offset.
        '''
        return self.verts
    
    def faces(self, cableIt = False):
        '''
        Class is initialized. No need to store face vertices as local/self
        Compute and return a list of 0-indexed faces. Caller is
        responsible for keeping track of global offset to first vertex and
        next face
        '''
        faces = []
        # '2n + 2 vertices per cylinder
        N = int(self.n2)
        for i in range(0,self.n1-1):
            v1 = (i + 1)*N-1
            v2 = i*N
            v3 = v1 + N
            v4 = v2 + N
            faces.append([v1, v3, v2])
            faces.append([v4, v3, v2])
            for j in range(0,N-1):
                v1 = i*N + j
                v2 = v1 + 1
                v3 = v1 + N
                v4 = v2 + N
                faces.append([v1, v2, v3])
                faces.append([v4, v3, v2])
        # last and first profiles
        v1 = self.n1*N -1
        v2 = 0
        v3 = N -1
        v4 = v1 - N + 1
        # print(f'***************************>>>> {v1}, {v2}, {v3}, {v4}')
        faces.append([v1, v3, v2])
        faces.append([v2, v4, v1])
        for j in range(0, N-1):
            v1 = N*(self.n1 -1) +j
            v2 = j
            v3 = v2 + 1
            v4 = v1 + 1
           # print(f'***************************>>>> {v1}, {v2}, {v3}, {v4}')
            faces.append([v2, v1, v4])
            faces.append([v4, v3, v2])
        return faces
        
if __name__ == "__main__":
    print("torus")
    E = torus([4,0,0,0,0,1,1,0,1,5], .3, 6)
    E.describe()
    print("vertices:")
    print(E.vertices())
    print("FACES:")
    print(E.faces())