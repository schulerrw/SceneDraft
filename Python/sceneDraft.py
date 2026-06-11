#####  python files that must be available to run Scene~Draft ######
#      sceneDraft                                                  #
import cone                                                        #
import cylinder                                                    #
import torus                                                       #
import arc                                                         #
import sphere                                                      #
import hemisphere                                                  #
import extrusion                                                   #
#      transform3d                                                 #
import sys                                                         #
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
#####  User Input ######### File Name and Path #####################
ScenePath = 'b:\\'          #<----------- User Edit or Action in OS# 
SceneFamilyName = 'scene'   #<----------- User Edit                # 
#                                                                  #
if len(sys.argv) > 1:                                              #
    SceneFamilyName = sys.argv[1]                                  #
##### computed file paths from User Input                          #
theSceneFile = ScenePath + SceneFamilyName + '.csv'                #
filePathOutput = ScenePath + SceneFamilyName + '.obj'              #
materialFileName = './'+SceneFamilyName + '.mtl'                   #
if len(sys.argv) > 2:                                              #
    materialFileName = './scenedraft.mtl'                          #
# Code puts <family>.obj file in same directory as <family>.csv    #
# User puts <family>.mtl file in same directory as <family>.obj    # <-+
####################################################################   |
#                           # User Action in OS -----------------------+

####################################################################
# 1) load theSceneFile                                             #
# 2) parse the header                                              #
# 3) parse each object/entity                                      #
#      a) based on entityID, instantiate the object                #
#      b) append to entityList[]                                   #
# 4) open filePathOutput                                           #
# 5) for each entity in entityList[]:                              #
#      a) write each vertex (keeping count)                        #
#      b) write each face (offset by vertex count)                 #
# 6) Close filePathOutput and bask in the glory of a new scene.obj #
####################################################################

######  Header of Scene Description  ###############################
#                   #  ,,Delimiter                                 #
#                   #  4, Header Lines. Scene parsing starts at +1 #
globalRadius = 0.3  #  0.05, Global Radius = Pencil Width          #
globalSteps = 6     #  6, Global Steps = smoothness                #
####################################################################

globalVertCount = 0
globalFaceCount = 0

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
  '                                              = 10 forrest green
'''
    

if __name__ == "__main__":
    entityList = []    # list of python objects (see imports above)
    
    #1### Load the File  ###########################################
    ################################################################  
    try:
        with open(theSceneFile, "r") as file:
            content = file.read()  # read whole file into content
            scene = content.split('\n')  #  split into lines
    except Exception:
        print(f'pyhthon s:\SceneDraft.py "familyName"')
        print(f'        to execute from the prompt in the directory with')
        print(f'        familyName.csv')
        print(f'\n I cannot find {theSceneFile}')
        sys.exit()
    #2### Parse the Header #########################################
    ################################################################
    ''' get Delimiter from first line.
        Split on D in Delimiter allows working with spreadsheets like Excel,
        that add a space for the first empty cell
    '''
    s = scene[0].split('D')                # Parse Delimieter
    d = s[0][-1]
    print(f'Delimiter is "{d}". Use keyword Delimiter on first line.')

    s = scene[1].split(d)                  # Parse Header Length
    try:
        headerLength = int(s[0])
    except Exception:
        headerLength = 5
        print(f'Warning: Second line of scene description should be the' + 
                         f' number of header lines to skip.')
    print(f'Header length is "{headerLength}". 2nd line first field. Parsing' + 
             f' starts on line {headerLength + 1}')

    s = scene[2].split(d)                 # Parse globalRadius
    try:
        globalRadius = float(s[0])
    except Exception:
        globalRadius = 0.3
        print(f'Warning: Third line of scene description should be'  +
                          f' glboalRadius: line width')
    print(f'globalRadius is {globalRadius}.')

    s = scene[3].split(d)                 # Parse globalSteps  
    try:
        globalSteps = int(s[0])
    except Exception:
        globalSteps = 6
        print(f'Warning: Fourth line of scene description should be' + 
                          f' globalSteps: smoothness')
    print(f'globalSteps is {globalSteps}.')
 
    #3### Parse Scene Objects ######################################
    ################################################################
    '''
    Build list of Entities (E) by parsing each shape.
    If a 'legal' shape is found, then instantiate its class from
    the correct module. Inheritance is skipped, but each class
    is expected to have __init__, vertices, faces, and describe
    methods.
    '''
    flag = 1 # lowers to zero upon parsing a blank line
    for s in scene[headerLength + 1:]: #skip header lines
        if flag == 0:
            print(f'Stop Parsing, a blank line was found.')
            break
        line = s.split(d)               # parse each scene object
        try:
            flag = len(line[0])
            entityID = int(line[0])
        except Exception:
            entityID = 0
        if entityID > 0:
            if entityID == 1:
                E = cone.cone(line, globalRadius, globalSteps)    
                entityList.append(E)
            elif entityID == 2:
                E = cylinder.cylinder(line, globalRadius, globalSteps)
                entityList.append(E)
            elif entityID == 3:
               E = torus.torus(line, globalRadius, globalSteps)    
               entityList.append(E)
            elif entityID == 4:
               E = arc.arc(line, globalRadius, globalSteps)    
               entityList.append(E)
            elif entityID == 5:
                E = arc.arc2(line, globalRadius, globalSteps)
                entityList.append(E)
            elif entityID == 6:
                E = sphere.sphere(line, globalRadius, globalSteps)    
                entityList.append(E)
            elif entityID == 7:
                E = hemisphere.hemisphere(line, globalRadius, globalSteps)    
                entityList.append(E)
            elif entityID == 8:
                E = extrusion.extrusion(line, globalRadius, globalSteps)    
                entityList.append(E)
            else:
                print(f'Cannot process {entityID} yet.')

    #4### Open filePathOutput: Scene.obj file ######################
    ################################################################
    fout = open(filePathOutput, 'w')
    print(     'mtllib ' + materialFileName)
    fout.write('mtllib ' + materialFileName + '\n')

    #5### Write objects into file ##################################
    ################################################################
    for E in entityList:  # for each Entity
        E.describe()
        fout.write(f'\nusemtl m{E.color:04d}\n')
        V = E.vertices()  
        F = E.faces()       #RWS write these with offset
        fOffset = globalVertCount + 1
        for vv in V:  # write each vertex
            fout.write(f'v {vv[0]} {vv[1]} {vv[2]}   #{globalVertCount}\n')
            globalVertCount += 1
        for ff in F:  # write each face
            fout.write(f'f {ff[0]+fOffset} {ff[1]+fOffset} {ff[2]+fOffset}\n')
            globalFaceCount += 1

    print(f'# dumped {globalVertCount} vertices and {globalFaceCount} faces.')
    print(f'{filePathOutput}')

    #6### Close filePathOutput and bask in glory of scene.obj file #
    ################################################################
    fout.close()



