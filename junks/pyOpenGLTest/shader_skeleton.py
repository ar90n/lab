#! /usr/bin/env python
'''Tests rendering using shader objects from core GL or extensions

Uses the:
    Lighthouse 3D Tutorial toon shader
        http://www.lighthouse3d.com/opengl/glsl/index.php?toon2

By way of:
    http://www.pygame.org/wiki/GLSLExample
'''
import numpy as np
import sys
import time
import Image
import OpenGL
OpenGL.ERROR_ON_COPY = True
from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *

# PyOpenGL 3.0.1 introduces this convenience module...
from OpenGL.GL.shaders import *

vertices = None
indices = None

def InitGL( vertex_shade_code, fragment_shader_code, texture_image ):
    glClearColor(0.0, 0.0, 0.0, 0.0)

    texture_id = glGenTextures( 1 )
    glPixelStorei( GL_UNPACK_ALIGNMENT, 1 )
    glActiveTexture( GL_TEXTURE0 )
    glBindTexture( GL_TEXTURE_2D, texture_id )

    if texture_image.mode == 'RGB':
        glTexImage2D( GL_TEXTURE_2D,
                      0,
                      4,
                      texture_image.size[0],
                      texture_image.size[1],
                      0,
                      GL_RGB,
                      GL_UNSIGNED_BYTE,
                      texture_image.tostring() )
    else:
        glTexImage2D( GL_TEXTURE_2D,
                      0,
                      4,
                      texture_image.size[0],
                      texture_image.size[1],
                      0,
                      GL_RGBA,
                      GL_UNSIGNED_BYTE,
                      texture_image.tostring() )
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR )
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR )
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE )
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE )

    program = compileProgram(
        compileShader( vertex_shade_code,GL_VERTEX_SHADER),
        compileShader( fragment_shader_code,GL_FRAGMENT_SHADER),)

    glUseProgram(program)
    glUniform1i( glGetUniformLocation( program, "s_texture" ), 0 );
    glUniform1f( glGetUniformLocation( program, "texture_width" ), float( texture_image.size[ 0 ] ) )
    glUniform1f( glGetUniformLocation( program, "texture_height" ), float( texture_image.size[ 1 ] ) )


    global vertices
    global indices
    position_vertices = [ -1.0,  1.0, 0.0,
                          -1.0, -1.0, 0.0,
                           1.0, -1.0, 0.0,
                           1.0,  1.0, 0.0, ]
    texture_vertices = [ 0.0, 0.0,
                         0.0, texture_image.size[ 1 ],
                         texture_image.size[ 0 ], texture_image.size[ 1 ],
                         texture_image.size[ 0 ], 0.0 ]

    indices = [ 0, 1, 2, 0, 2, 3 ]

    position_loc = glGetAttribLocation( program, 'a_position' )
    glVertexAttribPointer( position_loc,
                           3,
                           GL_FLOAT,
                           GL_FALSE,
                           3 * 4,
                           np.array( position_vertices, np.float32 ) )

    tex_loc = glGetAttribLocation( program, 'a_texCoord' )
    glVertexAttribPointer( tex_loc,
                           2,
                           GL_FLOAT,
                           GL_FALSE,
                           2 * 4,
                           np.array( texture_vertices, np.float32 ) )

    glEnableVertexAttribArray( position_loc )
    glEnableVertexAttribArray( tex_loc )


def ReSizeGLScene(Width, Height):
    glViewport(0, 0, Width, Height)

# The main drawing function.
def DrawGLScene():
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glEnable( GL_TEXTURE_2D )

    glDrawElements( GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, np.array( indices, np.uint16 ) )
    glDisable(GL_TEXTURE_2D)

    glutSwapBuffers()

def keyPressed(*args):
    # If escape is pressed, kill everything.
    if args[0] == '\x1b':
        sys.exit()

def usage():
    print "usage:%s vertex_shader_file fragment_shader_file texture_file" % sys.argv[ 0 ]

def main():
    try:
        vertex_shader_file = sys.argv[ 1 ]
        fragment_shader_file = sys.argv[ 2 ]
        texture_file = sys.argv[ 3 ]
    except IndexError:
        usage()
        sys.exit( -1 )

    vertex_shade_code = '\n'.join( open( vertex_shader_file, 'r' ).readlines() )
    fragment_shader_code = '\n'.join( open( fragment_shader_file, 'r' ).readlines() )
    texture_image = Image.open( texture_file )
    print texture_image.mode
    assert texture_image.mode == 'RGBA' or texture_image.mode == 'RGB'

    glutInit(sys.argv)

    if texture_image.mode == 'RGBA':
        glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH)
    else:
        glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH)

    window_width,window_height = texture_image.size
    glutInitWindowSize( window_width, window_height )

    # the window starts at the upper left corner of the screen
    glutInitWindowPosition(0, 0)

    glutCreateWindow( sys.argv[ 0 ] )

    glutDisplayFunc(DrawGLScene)

    # Uncomment this line to get full screen.
    #glutFullScreen()

    # When we are doing nothing, redraw the scene.
    glutIdleFunc(DrawGLScene)

    # Register the function called when our window is resized.
    glutReshapeFunc(ReSizeGLScene)

    # Register the function called when the keyboard is pressed.
    glutKeyboardFunc(keyPressed)

    # Initialize our window.
    InitGL( vertex_shade_code, fragment_shader_code, texture_image )

    # Start Event Processing Engine
    glutMainLoop()

# Print message to console, and kick off the main to get it rolling.

if __name__ == "__main__":
    print "Hit ESC key to quit."
    main()

