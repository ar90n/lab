varying vec2 v_texCoord;
uniform sampler2D s_texture;
uniform float texture_width;
uniform float texture_height;

void main() {
    float t_x = v_texCoord.x / texture_width;
    float t_y = v_texCoord.y / texture_height;
    float d_x = 1.0 / texture_width;
    float d_y = 1.0 / texture_height;

    vec4 v1 = texture2D( s_texture, vec2( t_x, t_y ) + vec2( d_x, 0.0 ) );
    vec4 v2 = texture2D( s_texture, vec2( t_x, t_y ) - vec2( d_x, 0.0 ) );
    vec4 v3 = texture2D( s_texture, vec2( t_x, t_y ) + vec2( 0.0, d_y ) );
    vec4 v4 = texture2D( s_texture, vec2( t_x, t_y ) - vec2( 0.0, d_y ) );
    vec4 vvv = texture2D( s_texture, vec2( t_x, t_y ) );
    //gl_FragColor = ( abs( v1 - v2 ) + abs( v3 - v4 ) ) / 2.0;
    gl_FragColor = vvv;
}
