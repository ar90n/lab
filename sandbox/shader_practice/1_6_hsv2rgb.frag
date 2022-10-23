#version 300 es
precision highp float;

out vec4 fragColor;
uniform vec2 u_resolution;

vec3 hsv2frag(vec3 c) {
    vec3 rgb = clamp(abs(mod(c.x * 6.0 + vec3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return c.z * mix(vec3(1.0), rgb, c.y);
}

const float PI = 3.1415926;
float atan2(float y, float x) {
    return x == 0.0 ? sign(y) * PI / 2.0 : atan(y, x);
}

vec2 xy2pol(vec2 xy) {
    return vec2(atan2(xy.y, xy.x), length(xy));
}

void main() {
    vec2 coord = gl_FragCoord.xy / u_resolution.xy;
    coord = coord * 2.0 - 1.0;
    vec2 pol = xy2pol(coord);
    pol.x = 0.5 * pol.x / PI;
    vec3 color = hsv2frag(vec3(pol.x, pol.y, 1.0));
    fragColor = vec4(color, 1.0);
}