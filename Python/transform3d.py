#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
#      cone                                                        #
#      cylinder                                                    #
#      torus                                                       #
#      arc                                                         #
#      sphere                                                      #
#      hemisphere                                                  #
#      extrusion                                                   #
import math              # Native Python--not part of Scen~Draft   #
#import transform3d      # This file.                              #
####################################################################
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

class transform3d:
    def __init__(self, a1, a2, a3, v1, v2, v3, alpha):
        pi = 4 * math.atan(1)
        myAlpha = alpha * pi / 180
        cosAlpha = math.cos(myAlpha)
        sinAlpha = math.sin(myAlpha)
        cosAlpha1 = 1 - cosAlpha
        rho = math.sqrt(v1 * v1 + v2 * v2 + v3 * v3)
        # print (f'RRRRRRRRRRRR>>>>>>>>>>>>>>>>>>>>>>>>> myAlpha = {myAlpha}, alpha = {alpha}, rho = {rho}')
        if rho == 0:
            theta = 0
            cosphi = 1
            sinPhi = 0
        else:
            if v1 == 0:
                if v2 >= 0:
                    theta = 0.5 * pi
                else:
                    theta = 1.5 * pi
            else:
                theta = math.atan(v2 / v1)
                if (v1 < 0):
                    theta = theta + pi
        cosphi = v3 / rho
        sinPhi = math.sqrt(1 - cosphi * cosphi)

        cosTheta = math.cos(theta)
        sinTheta = math.sin(theta)
        cosPhi2 = cosphi * cosphi
        sinPhi2 = 1 - cosPhi2
        cosTheta2 = cosTheta * cosTheta
        sinTheta2 = 1 - cosTheta2
        self.r11 = (cosAlpha * cosPhi2 + sinPhi2) * cosTheta2 + cosAlpha * sinTheta2
        self.r12 = sinAlpha * cosphi + cosAlpha1 * sinPhi2 * cosTheta * sinTheta
        self.r13 = sinPhi * (cosphi * cosTheta * cosAlpha1 - sinAlpha * sinTheta)
        self.r21 = sinPhi2 * cosTheta * sinTheta * cosAlpha1 - sinAlpha * cosphi
        self.r22 = sinTheta2 * (cosAlpha * cosPhi2 + sinPhi2) + cosAlpha * cosTheta2
        self.r23 = sinPhi * (cosphi * sinTheta * cosAlpha1 + sinAlpha * cosTheta)
        self.r31 = sinPhi * (cosphi * cosTheta * cosAlpha1 + sinAlpha * sinTheta)
        self.r32 = sinPhi * (cosphi * sinTheta * cosAlpha1 - sinAlpha * cosTheta)
        self.r33 = cosAlpha * sinPhi2 + cosPhi2
        self.r41 = a1 - a1 * self.r11 - a2 * self.r21 - a3 * self.r31
        self.r42 = a2 - a1 * self.r12 - a2 * self.r22 - a3 * self.r32
        self.r43 = a3 - a1 * self.r13 - a2 * self.r23 - a3 * self.r33

    def dump(self):
        print(f'[{self.r11} {self.r12} {self.r13} 0.0')
        print(f'[{self.r21} {self.r22} {self.r13} 0.0')
        print(f'[{self.r31} {self.r32} {self.r33} 0.0')
        print(f'[{self.r41} {self.r42} {self.r43} 1.0')

    def rotate(self, x, y, z):
        mx = x * self.r11 + y * self.r21 + z * self.r31 + self.r41
        my = x * self.r12 + y * self.r22 + z * self.r32 + self.r42
        mz = x * self.r13 + y * self.r23 + z * self.r33 + self.r43
        return [mx, my, mz] 


if __name__ == "__main__":
    print("transform3d")