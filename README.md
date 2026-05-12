Scene\~Draft🄯 is an extensible framework used to translate a list of parametric shape
descriptions into files that define three-dimensional scenes. The formats of these files are
based on indexed-face-sets of triangles. The goal of Scene\~Draft🄯 is to allow users to draw 
figures in 3D with as little overhead and non-accumulating knowledge as possible. 

Scene~Draft🄯 reads a scene description file and generates indexed face sets for each 
parametric object listed. Output files are based on indexed-face-sets of triangles and can 
be read by tools Like Word or Blender.

In the most fundamental workflow, the drafter creates a comma seperated value (CSV) scene description file; runs Scene~Draft🄯; and inputs the resulting Wavefront object file (OBJ) into a visualization tool like Microsft Word™ or Blender.

<img width="707" height="459" alt="workflow" src="https://github.com/user-attachments/assets/939a31f3-7ae3-4012-8ca1-ff38975d6a47" />


The framework is intended to be run from source code that drafters modify as needed. It is 
presented in Visual Basic for Applications (VBA) and Python but can be easily ported to
other languages. Screen shots of output files may be added to modern word processors 
like Microsoft Word or Libre Office enabling figure annotation. Microsoft Word™ allows 
direct importation of Scene~Draft🄯 output files, which has motivated this software.
