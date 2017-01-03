attribute vec4 a_position;
attribute vec2 a_texCoord;
varying vec2 v_texCoord;
uniform float texture_width;
uniform float texture_height;

void main() {
    /*
    mat4 m4 = mat4( cos( 0.5 ), -sin( 0.5 ), 0.0, 0.0,
                    sin( 0.5 ),  cos( 0.5 ), 0.0, 0.0,
                           0.0,         0.0, 1.0, 0.0,
                           0.0,         0.0, 0.0, 1.0 );
    */
/*
    mat4 m4 = mat4( cos(0.5),  0.0, 0.0, -sin(0.5),
                    0.0,  1.0, 0.0, 0.0,
                    sin(0.5),  0.0, 1.0, cos(0.5),
                    0.0,  0.0, 0.0, 1.0 );
*/
/*
    mat4 m4 = mat4( cos(0.2),  0.0, sin(0.2), 0.0,
                         0.0,  1.0,      0.0, 0.0,
                         0.0,  0.0,      1.0, 0.0,
                   -sin(0.2),  0.0, cos(0.2), 1.0 );
*/
    vec3 n = vec3( 1.0,0.0,0.0 );
    float theta = ( 120.0 ) / 180.0 * 3.1415;
    float c = cos( theta );
    float s = sin( theta );
    mat4 m4 = mat4( n.x * n.x * ( 1.0 - c ) + c,  n.x * n.y * ( 1.0 - c ) + n.z * s, n.x * n.z * ( 1.0 - c ) - n.y * s, 0.0,
                    n.x * n.y * ( 1.0 - c ) - n.z * s, n.y * n.y * ( 1.0 - c ) + c, n.y * n.z * ( 1.0 - c ) + n.x * s, 0.0,
                         0.0,  0.0,      1.0, 0.0,
                   n.x * n.z * ( 1.0 - c ) + n.y * s, n.y * n.z * ( 1.0 - c ) - n.x * s, n.x * n.x * ( 1.0 - c ) + c, 2.0 );
    v_texCoord = a_texCoord;
    gl_Position = a_position * m4;
}
