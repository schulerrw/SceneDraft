#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone                                                        #
#      cylinder                                                    #
#      torus                                                       #
#      arc                                                         #
#      sphere                                                      #
#      hemisphere        # This file.                              #
#      extrusion                                                   #
import math              # Native Python--not part of Scen~Draft   #
import transform3d                                                 #
####################################################################
'''
7 <color> Cx Cy Cz Px Py Pz Radius Steps
'''
'''
+-------------------------------------+
|  Hemisphere                         |
+-------------------------------------+
| Type = 7                            |
| C color                             |
| Cx                                  |
| Cy  center                          |
| Cz                                  |
| Px                                  | 
| Py  pole                            |
| Pz                                  |
| R   base radius                     |
| N   number of latitude layers       |
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

class hemisphere:
    def __init__(self, line, globalRadius, globalSteps):
        # for i, f in enumerate(line):
        #     print(f'{i}: 
        #           in hemisphere.__init__ : {f}')
        self.globalRadius = globalRadius
        self.globalSteps = globalSteps
        try:
            self.color = int(line[1])
            self.Cx = float(line[2])
            self.Cy = float(line[3])
            self.Cz = float(line[4])
            self.Px = float(line[5])
            self.Py = float(line[6])
            self.Pz = float(line[7])
            self.BaseRadius = float(line[8])
            self.Steps = int(line[9])
        except Exception:
            print("Error parsing {line} in hemisphere.__init__()")
            self.color = 0

    def describe(self):
        print(f'hemisphere from ({self.Cx},{self.Cy},{self.Cz}) to ({self.Px}, {self.Py}, {self.Pz})')
        print(f'         R1 = {self.BaseRadius} and N = {self.Steps} latitudes')
        print(f'         globalRadius = {self.globalRadius}  and  globalSteps = {self.globalSteps}')
    
    def vertices(self):
        '''
        Class is initialized. No need to store vertices as local/self
        Compute and return a list of 0-indexed vertices. Caller is
        responsible for keeping track of global offset.
        '''

        ''' process overides and initialize theta '''
        R = self.globalRadius
        N = int(self.globalSteps)
        verts = []

        # if self.r1 >= 0:
        #     R = self.r1
        #     # NOTE: R2 overides R after we are done with first profile
        # if self.n1 > 0:
        #     N = self.n1
        
        theta = 360 / N
    
        ''' compute vector Head - Tail'''
        # Defines plane of first profile
        a = self.Px - self.Cx #xB - xA
        b = self.Py - self.Cy #yB - yA
        c = self.Pz - self.Cz #zB - zA
        
        ''' save center of first profile at index zero '''
        verts.append([self.Cx, self.Cy, self.Cz])

    
        ''' get direction to point on first profile'''
        if abs(a) < 0.000001 and abs(b) < 0.000001:
            rx = 0
            ry = c
            rz = -1 * b
        else:
            rx = b
            ry = -1 * a
            rz = 0

    
        ''' normalize to unit length'''
        myLen = math.sqrt(rx * rx + ry * ry + rz * rz)
        rx = rx / myLen
        ry = ry / myLen
        rz = rz / myLen

        ''' normalize to unit length
            vector from base center to pole
        '''
        myLen = math.sqrt(a*a + b*b + c*c)
        a = a / myLen
        b = b / myLen
        c = c / myLen

        phi = math.pi/(2*self.Steps)
        print(phi)

        

        ''' set first point on first profile'''
        xx = self.Cx + rx * self.BaseRadius
        yy = self.Cy + ry * self.BaseRadius
        zz = self.Cz + rz * self.BaseRadius
        verts.append([xx, yy, zz])

        ''' generate rest of points on first profile'''
        T = transform3d.transform3d(self.Cx, self.Cy, self.Cz, a, b, c, theta)
        for i in range(1, N):
            [xx, yy, zz] = T.rotate(xx, yy, zz)
            verts.append([xx, yy, zz])

        print(f'R = {R}, phi={phi} cos(phi)={math.cos(phi)}')
        for j in range(2,self.Steps):
            R = self.BaseRadius*math.cos((j-1)*phi) # reduce each lattitude radius
            ''' set first point on 2nd profile'''
            x2 = self.Cx + a*math.sin((j-1)*phi)
            y2 = self.Cy+ b*math.sin((j-1)*phi)
            z2 = self.Cz + c*math.sin((j-1)*phi)
            xx = x2 + rx * R
            yy = y2 + ry * R
            zz = z2 + rz * R
            verts.append([xx, yy, zz])
            for i in range(1,N):
                [xx, yy, zz] = T.rotate(xx, yy, zz)
                verts.append([xx, yy, zz])

  
        ''' save pole at index 2n+1'''
        verts.append([self.Px, self.Py, self.Pz])
        self.noLattitutes = int((len(verts)-2)/self.globalSteps)
        return verts
    
    def faces(self):
        '''
        Class is initialized. No need to store face vertices as local/self
        Compute and return a list of 0-indexed faces. Caller is
        responsible for keeping track of global offset to first vertex and
        next face
        '''
        faces = []
        # '2n + 2 vertices per cylinder
        N = self.globalSteps
        
        m = self.noLattitutes
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
        

if __name__ == "__main__":
    print("hemisphere")
    myLine = [5, 0, 0,0,0, 0,0,1,1,10]
    H = hemisphere(myLine, 0.3, 6)
    H.describe()
    verts = H.vertices()
    fac = H.faces()