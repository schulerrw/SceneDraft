#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone                                                        #
#      cylinder          # This file.                              #
#      torus                                                       #
#      arc                                                         #
#      sphere                                                      #
#      hemisphere                                                  #
#      extrusion                                                   #
import math              # Native Python--not part of Scen~Draft   #
import transform3d                                                 #
####################################################################
'''
2 <color> Sx Sy Sz Ex Ey Ez  R1 R2 N1
'''
'''
+-------------------------------------+
|  Cylinder                           |
+-------------------------------------+
| Type = 2                            |
| C color                             |
| x1                                  |
| y1  start                           |
| z1                                  |
| x2                                  | 
| y2  end                             |
| z2                                  |
| <R1> optional radius override start |
| <R2> optional radius override end   |
| <N1> optional steps override        |
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
class cylinder:
    def __init__(self, line, globalRadius, globalSteps):
        # for i, f in enumerate(line):
        #     print(f'{i}: 
        #           in cylinder.__init__ : {f}')
        self.globalRadius = globalRadius
        self.globalSteps = globalSteps

        try:
            self.color = int(line[1])
            self.x1 = float(line[2])
            self.y1 = float(line[3])
            self.z1 = float(line[4])
            self.x2 = float(line[5])
            self.y2 = float(line[6])
            self.z2 = float(line[7])
        except Exception as e:
            print(f"Error parsing {line} in cylinder.__init__()")
            print(e)
            self.color = 0
            self.x1 = self.y1 = self.z1 = 0
            self.x2 = self.y2 = self.z2 = 1.0
        try:
            self.r1 = float(line[8])
            if self.r1 < 0:
                self.r1 = -1.0
        except Exception:
            self.r1 = -1.0
        try:
            self.r2 = float(line[9])
            if self.r2 < 0:
                self.r2 = -1.0
        except Exception:
            self.r2 = -1.0
        try:
            self.n1 = int(line[10])
            if self.n1 < 3:
                self.n1 = -1
        except Exception:
            self.n1 = -1
            # no options on line

    def describe(self):
        print(f'cylinder from ({self.x1},{self.y1},{self.z1}) to ({self.x2}, {self.y2}, {self.z2})')
        print(f'         R1 = {self.r1} and R2 = {self.r2} and  N1 = {self.n1}')
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

        if self.r1 >= 0:
            R = self.r1
            # NOTE: R2 overides R after we are done with first profile
        if self.n1 > 0:
            N = self.n1
        
        theta = 360 / N
    
        ''' compute vector Head - Tail'''
        # Defines plane of first profile
        a = self.x2 - self.x1 #xB - xA
        b = self.y2 - self.y1 #yB - yA
        c = self.z2 - self.z1 #zB - zA
        
        ''' save center of first profile at index zero '''
        verts.append([self.x1, self.y1, self.z1])

    
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
        

        ''' set first point on first profile'''
        xx = self.x1 + rx * R
        yy = self.y1 + ry * R
        zz = self.z1 + rz * R
        verts.append([xx, yy, zz])

        ''' generate rest of points on first profile'''
        T = transform3d.transform3d(self.x1, self.y1, self.z1, a, b, c, theta)
        for i in range(1, N):
            [xx, yy, zz] = T.rotate(xx, yy, zz)
            verts.append([xx, yy, zz])
    
        
        ''' Optionally update radius of 2nd profile'''
        if self.r2 >= 0:
            R = self.r2
    
        ''' set first point on 2nd profile'''
        xx = self.x2 + rx * R
        yy = self.y2 + ry * R
        zz = self.z2 + rz * R
        verts.append([xx, yy, zz])

        ''' generate rest of points on 2nd profile'''
        # T = transform3d.transform3d(self.x1, self.y1, self.z1, a, b, c, theta)
        for i in range(1, N):
            [xx, yy, zz] = T.rotate(xx, yy, zz)
            verts.append([xx, yy, zz])
    
        ''' save center of 2nd profile at index 2n+1'''
        verts.append([self.x2, self.y2, self.z2])
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
        N = int(self.globalSteps)
        if self.n1 > 0:
            N = self.n1   
        '''first end cap'''
        j = 0

        v3 = 0
        v2 = v3 + 1
        v1 = v3 + N
        faces.append([v1, v2, v3])
        for i in range (1, N):
            v3 = 0
            v1 = v3 + i
            v2 = v1 + 1
            faces.append([v1, v2, v3])
        
    
        ''' first side account for 'rolling over' index N'''
        faces.append([j * (2 * N + 2) + 2 * N, 1 + j * (2 * N + 2), N + j * (2 * N + 2) ])
        faces.append([j * (2 * N + 2) + 2 * N, j * (2 * N + 2) + 1 + N , 1 + j * (2 * N + 2)])
        
        ''' rest of sides'''
        for i in range(1, N):
            v1 = i
            v2 = i + N
            v3 = v2 + 1
            v4 = v1 + 1
            faces.append([v1, v2, v3])
            faces.append([v4, v1, v3])
 
        ''' last end cap'''
        for i in range(N, 1, -1): # NOTE: last cap is traversed backwards to face out
            v1 = N + i
            v2 = v1 - 1
            v3 = 2 * N + 1
            faces.append([v1, v2, v3])
        faces.append([N + 1, 2 * N, 2 * N + 1 ])

        return faces
    

if __name__ == "__main__":
   print("cylinder")
   E = cylinder([2,3,0,0,0,0,1,2.0], 0.3, 12)
   E.describe()
   print(E.vertices())
   print(E.faces())