#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone              # This file.                              #
#      cylinder                                                    #
#      torus                                                       #
#      arc                                                         #
#      sphere                                                      #
#      hemisphere                                                  #
#      extrusion                                                   #
import math              # Native Python--not part of Scen~Draft   #
import transform3d                                                 #
####################################################################
'''
1 <color> Bx By Bz Ax Ay Az  R N phi
'''
'''
+-----------------------------------+
|  Cone                             |
+-----------------------------------+
| Type = 1                          |
| C color                           |
| Bx                                |
| By  base                          |
| Bz                                |
| Ax                                | 
| Ay  Apex                          |
| Az                                |
| <R> optional radius override      |
| <N> optional steps override       |
| <phi> optional initial rotation   |
+-----------------------------------+
| __init__(self, line, gRad, gStep) |
| describe(self)                    |
| vertices(self)                    | 
| faces(self)                       |
| __main__()                        |
+-----------------------------------+
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
class cone:
    def __init__(self, line, globalRadius, globalSteps):
        self.globalRadius = globalRadius
        self.globalSteps = globalSteps

        try:
            self.color = int(line[1])
            self.Bx = float(line[2])
            self.By = float(line[3])
            self.Bz = float(line[4])
            self.Ax = float(line[5])
            self.Ay = float(line[6])
            self.Az = float(line[7])
        except Exception:
            print("Error parsing {line} in cone.__init__()")
            self.color = 0
            self.Bx = self.By = self.Bz = 0
            self.Ax = self.Ay = self.Az = 1.0
        try:
            self.r1 = float(line[8])
            if self.r1 <= 0:
                self.r1 = -2
        except Exception as e:
            print(e)
            self.r1 = -5

        try:
            self.n1 = int(line[9])
            if self.n1 < 3:
                self.n1 = -2
        except Exception:
            self.n1 = -3
            print(f' line = {line}')
        
        try:
            self.phi = float(line[10])
            if self.phi < -360.0:
                self.phi = 0
        except Exception:
            self.phi = 0

    def describe(self):
        print(f'cone from ({self.Bx},{self.By},{self.Bz}) to ({self.Ax}, {self.Ay}, {self.Az})')
        print(f'         R1 = {self.r1}     N1={self.n1}     phi = {self.phi}')
        print(f'         globalRadius = {self.globalRadius}  and  globalSteps = {self.globalSteps}')
    
    def vertices(self):
        '''
        Class is initialized. No need to store vertices as local/self
        Compute and return a list of 0-indexed vertices. Caller is
        responsible for keeping track of global offset.
        '''

        ''' process overides and initialize theta '''

        R = self.globalRadius * 2.0
        N = int(self.globalSteps)
        verts = []

        if self.r1 >= 0:
            R = self.r1
        if self.n1 > 0:
            N = self.n1
        
        theta = 360 / N
    
        ''' compute vector Head - Tail'''
        # Defines plane of first profile
        a = self.Ax - self.Bx #xB - xA
        b = self.Ay - self.By #yB - yA
        c = self.Az - self.Bz #zB - zA
        
        ''' save center of first profile at index zero '''
        verts.append([self.Bx, self.By, self.Bz])

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
        

        ''' set first point on base profile'''
        xx = self.Bx + rx * R
        yy = self.By + ry * R
        zz = self.Bz + rz * R
   
        ''' optionally rotate first point by <phi>'''
        if self.phi != 0:
            T = transform3d.transform3d(self.Bx, self.By, self.Bz, a, b, c, self.phi)
            [xx, yy, zz] = T.rotate(xx, yy, zz)

        verts.append([xx, yy, zz])

        ''' generate rest of points on base profile'''
        T = transform3d.transform3d(self.Bx, self.By, self.Bz, a, b, c, theta)
        for i in range(1, N):
            [xx, yy, zz] = T.rotate(xx, yy, zz)
            verts.append([xx, yy, zz])
    
        ''' save apex at index 2n+1'''
        verts.append([self.Ax, self.Ay, self.Az])
        return verts
    
    def faces(self):
        '''
        Class is initialized. No need to store face vertices as local/self
        Compute and return a list of 0-indexed faces. Caller is
        responsible for keeping track of global offset to first vertex and
        next face
        '''
        faces = []
        # 'n + 2 vertices per cone
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
        v1 = N+1
        v2 = N
        v3 = 1
        faces.append([v1, v2, v3])
        
        ''' rest of sides'''
        for i in range(1, N):
            v1 = i
            v2 = i + 1
            v3 = N+1
            faces.append([v1, v2, v3])
 
        return faces
    

if __name__ == "__main__":
   print("cone")
   E = cone([1,0,0,0,0,1,2.0,3.0, 0.6, 12, 30], 0.3, 12)
   E.describe()
   print(E.vertices())
   print(E.faces())