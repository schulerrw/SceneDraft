#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone                                                        #
#      cylinder                                                    #
#      torus                                                       #
#      arc                                                         #
#      sphere            # This file.                              #
#      hemisphere                                                  #
#      extrusion                                                   #
#      math              # Native Python--not part of Scen~Draft   #
#      transform3d                                                 #
####################################################################
'''
6 <color> Cx Cy Cz Radius
'''
'''
+-------------------------------------+
|  Sphere                             |
+-------------------------------------+
| Type = 6                            |
| C color                             |
| Cx                                  |
| Cy  center                          |
| Cz                                  |
| R radius                            |
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
the Free Software Foundation, version 3of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
"""

class sphere:
    def __init__(self, line, globalRadius, globalSteps):
        # for i, f in enumerate(line):
        #     print(f'{i}: 
        #           in cylinder.__init__ : {f}')
        self.globalRadius = globalRadius
        self.globalSteps = globalSteps
        try:
            self.color = int(line[1])
            self.Cx = float(line[2])
            self.Cy = float(line[3])
            self.Cz = float(line[4])
            self.R = float(line[5])
        except Exception:
            print("Error parsing {line} in sphere.__init__()")
            self.color = 0
            self.Cx = self.Cy = self.Cz = 0.0
            self.R = 1.0
            # no options on line

    def describe(self):
        print(f'sphere with center ({self.Cx},{self.Cy},{self.Cz}) and Radius {self.R}.')

    '''
    ' DATA FOR INDEXED FACE SET OF AN 80 TRIANGLE SPHERE OF RADIUS 1
    '  PP(x,y,z) are the points--scale these to desired radius; then add offset
    '  FF(1,2,3) are the three indexes defining the triangular face in counterclockwise orientation
    '''
    def vertices(self):
        '''
        Class is initialized. No need to store vertices as local/self
        Compute and return a list of 0-indexed vertices. Caller is
        responsible for keeping track of global offset.
        '''
        verts = []
        PPX = (0.0, 0.723607, 0.723607, -0.276393, -0.894427, -0.276393, 0.894427, 0.276393,
                -0.723607, -0.723607, 0.276393, 0, 0.850651, 0.425325, 0.425325, 0.262866, 
                -0.16246, -0.688191, -0.525731, -0.688191, -0.16246, 0.262866, 0.951056, 0.951056,
                  0, 0.587785, -0.951056, -0.587785, -0.587785, -0.951056, 0.587785, 0, 0.688191, 
                  -0.262866, -0.850651, -0.262866, 0.688191, 0.16246, 0.525731, -0.425325, -0.425325, 0.16246)
        PPY = (0.0, -0.525731, 0.525731, 0.850651, 0, -0.850651, 0, 0.850651, 0.525731, -0.525731, 
               -0.850651, 0, 0, 0.309017, -0.309017, 0.809017, 0.5, 0.5, 0, -0.5, -0.5, -0.809017, 0.309017, 
               -0.309017, 1, 0.809017, 0.309017, 0.809017, -0.809017, -0.309017, -0.809017, -1, 0.5, 0.809017, 
               0, -0.809017, -0.5, 0.5, 0, 0.309017, -0.309017, -0.5)
        PPZ = (1.0, 0.447214, 0.447214, 0.447214, 0.447214, 0.447214, -0.447214, -0.447214, -0.447214, 
               -0.447214, -0.447214, -1, 0.525731, 0.850651, 0.850651, 0.525731, 0.850651, 0.525731, 
               0.850651, 0.525731, 0.850651, 0.525731, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.525731, -0.525731, 
               -0.525731, -0.525731, -0.525731, -0.850651, -0.850651, -0.850651, -0.850651, -0.850651)
        for i in range(len(PPX)):
            verts.append([self.R*PPX[i]+self.Cx, self.R*PPY[i]+self.Cy, self.R*PPZ[i]+self.Cz])
        return verts
  
    def faces(self):
        '''
        Class is initialized. No need to store face vertices as local/self
        Compute and return a list of 0-indexed faces. Caller is
        responsible for keeping track of global offset to first vertex and
        next face
        '''

        FF1 = [1, 15, 14, 14, 1, 14, 17, 17, 1, 17, 19, 19, 1, 19, 21, 21, 1, 21, 15, 15, 2, 
               24, 13, 13, 3, 26, 16, 16, 4, 28, 18, 18, 5, 30, 20, 20, 6, 32, 22, 22, 7, 33, 
               23, 23, 8, 34, 25, 25, 9, 35, 27, 27, 10, 36, 29, 29, 11, 37, 31, 31, 7, 39, 33, 
               33, 8, 38, 34, 34, 9, 40, 35, 35, 10, 41, 36, 36, 11, 42, 37, 37]
        FF2 = [15, 2, 13, 15, 14, 3, 16, 14, 17, 4, 18, 17, 19, 5, 20, 19, 21, 6, 22, 21, 24, 
               7, 23, 24, 26, 8, 25, 26, 28, 9, 27, 28, 30, 10, 29, 30, 32, 11, 31, 32, 33, 8, 
               26, 33, 34, 9, 28, 34, 35, 10, 30, 35, 36, 11, 32, 36, 37, 7, 24, 37, 39, 12, 38, 
               39, 38, 12, 40, 38, 40, 12, 41, 40, 41, 12, 42, 41, 42, 12, 39, 42]
        FF3 = [14, 13, 3, 13, 17, 16, 4, 16, 19, 18, 5, 18, 21, 20, 6, 20, 15, 22, 2, 22, 13, 
               23, 3, 23, 16, 25, 4, 25, 18, 27, 5, 27, 20, 29, 6, 29, 22, 31, 2, 31, 23, 26, 3, 
               26, 25, 28, 4, 28, 27, 30, 5, 30, 29, 32, 6, 32, 31, 24, 2, 24, 33, 38, 8, 38, 34, 
               40, 9, 40, 35, 41, 10, 41, 36, 42, 11, 42, 37, 39, 7, 39]
        faces = []
        for i in range(len(FF1)):
            faces.append([FF1[i]-1, FF2[i]-1, FF3[i]-1])

        return faces


if __name__ == "__main__":
    print("sphere")
    E = sphere([2,0,1,0,0,1],0,0)
    E.describe()
    print(E.vertices())
    print(E.faces())
