rem Batch File to run Scene~Draft Python Version
rem Put this file in the directory with your scene.csv file
rem change <your path to sceneDraft.py> to what you actuall have
rem from the prompt >sd <scene family name> <optional override of MTL filename>
rem
rem Scene~Draft assumes three members in a family of files all starting
rem   with the same name, but with a different extension. E.g.
rem      scene.csv
rem      scene.obj
rem      scene.mtl
rem
rem sceneDraft.py is looking for ONLY the family name. E.g. scene
rem
rem %1 is the first command line input and should be your scene.csv file
rem %2 is the second command line input and can be skipped.
rem        if %2 has a value, the python uses sceneDraft.mtl
rem        otherwise python uses <scene family name>.mtl
rem
rem 
rem python <your path to sceneDraft.py>\sceneDraft.py %1 %2
python C:\Users\Schulerrw\ONEDRI~1\Documents\3DOBJE~1\SceneDraft\Python\sceneDraft.py %1 %2
