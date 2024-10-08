#version 300 es
precision lowp float;

out vec4 fragColor;
uniform vec2 u_resolution;
void main() {
    vec2 pos = gl_FragCoord.xy / u_resolution.xy;
    fragColor = vec4(1.0, pos, 1.0);
}