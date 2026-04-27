#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone                                                        #
#      cylinder                                                    #
#      torus                                                       #
#      arc                                                         #
#      sphere                                                      #
#      hemisphere                                                  #
#      extrusion         # This file.                              #
import math              # Native Python--not part of Scen~Draft   #
import transform3d                                                 #
####################################################################
'''
8 <color> dx dy dz N P1x P1y P1z, ... PNx, PNy, PNz
'''
'''
+-------------------------------------+
|  Extrusion                          |
+-------------------------------------+
| Type = 8                            |
| C color                             |
| dx                                  |
| dy  extrusion vector                |
| dz                                  |
| N   number of polygon points        | 
| verts[N][3] polygon vertices        |
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

class extrusion:
    def __init__(self, line, globalRadius, globalSteps):
        # for i, f in enumerate(line):
        #     print(f'{i}: 
        #           in extrusion.__init__ : {f}')
        self.globalRadius = globalRadius
        self.globalSteps = globalSteps
        try:
            self.color = int(line[1])
            self.dx = float(line[2])
            self.dy = float(line[3])
            self.dz = float(line[4])
            self.N = int(line[5])
        except Exception:
            print("Error parsing {line} in extrusion.__init__()")
            self.color = 0

        try:
            self.verts = []
            for i in range(self.N):
                j = i * 3
                x = float(line[6+j])
                y = float(line[7+j])
                z = float(line[8+j])
                self.verts.append([x, y, z])

        except Exception:
            self.verts = [0,0,0, 10,0,0, 0,10,0]
            print(f'Puking')
            # no options on line

    def describe(self):
        print(f'extrusion ({self.dx}, {self.dy}, {self.dz})')
        print(f'         globalRadius = {self.globalRadius}  and  globalSteps = {self.globalSteps}')
        print(f' verts {self.verts}')
        print(f'of: \n')
        for p in self.verts:
            print(f'     ({p[0]}, {p[1]}, {p[2]})')
    
    def verticies(self):
        '''
        Class is initialized. No need to store verticies as local/self
        Compute and return a list of 0-indexed verticies. Caller is
        responsible for keeping track of global offset.
        '''

        ''' process overides and initialize theta '''
        R = self.globalRadius
        N = int(self.globalSteps)
        verts = []
        # copy profile as-is
        for p in self.verts:
            verts.append([p[0], p[1], p[2]])
        
        #offset profile by dx,dy,dz
        for p in self.verts:
            verts.append([p[0]+self.dx, p[1]+self.dy, p[2]+self.dz])
        return verts
    
    def faces(self):
        '''
        Class is initialized. No need to store face verticies as local/self
        Compute and return a list of 0-indexed faces. Caller is
        responsible for keeping track of global offset to first vertex and
        next face
        '''
        faces = []

        # given profile
        if self.N % 2 == 0:
            print(f'in extrusion faces, found even')
            for i in range(1,self.N):
                v1 = i -1
                v2 = v1 +1
                v3 = self.N - i -1
                v4 = v3 + 1
                if v4 - v2 > 1:
                    faces.append([v4, v2, v1])
                    faces.append([v2,v4,v3])
        else:
            print(f'in extrusion faces found odd')
            for i in range(1,self.N):
                v1 = i -1
                v2 = v1 +1
                v3 = self.N - i -1
                v4 = v3 + 1
                faces.append([v4, v2, v1])
                if v4 - v2 > 1:
                    faces.append([v2,v4,v3])    

        # Extruded profile
        if self.N % 2 == 0:
            print(f'in extrusion faces, found even')
            for i in range(1,self.N):
                v1 = self.N + i -1
                v2 = v1 +1
                v3 = self.N +self.N - i -1
                v4 = v3 + 1
                if v4 - v2 > 1:
                    faces.append([v1, v2, v4])
                    faces.append([v3,v4,v2])
        else:
            print(f'in extrusion faces found odd')
            for i in range(1,self.N):
                v1 = self.N + i -1
                v2 = v1 +1
                v3 = self.N + self.N - i -1
                v4 = v3 + 1
                faces.append([v1, v2, v4])
                if v4 - v2 > 1:
                    faces.append([v3,v4,v2]) 

        # side faces
        for i in range (1,self.N):
            v1 = i - 1
            v2 = v1+1
            v3 = i + self.N -1
            v4 = v3 + 1
            faces.append([v1, v2, v3]) 
            faces.append([v4, v2, v3])    

        #last side
        v1 = self.N -1 
        v2 = 0
        v3 = self.N + self.N -1
        v4 = v1 + 1
        faces.append([v1, v2, v3])
        faces.append([v4, v2, v3])
        return faces
    

if __name__ == "__main__":
    print("extrusion")
    E = extrusion([7,0, 0,0,0.5, 4, 0,0,0, 10,0,0, 10,10,0, 0,10,0], .3, 6)
    E.describe()
    V = E.verticies()
    print(V)
    F = E.faces()
    print(F)