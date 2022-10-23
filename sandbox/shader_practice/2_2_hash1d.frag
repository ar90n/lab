#version 300 es
precision highp float;

out vec4 fragColor;

uint k = 0x456789abu;
const uint UINT_MAX = 0xffffffffu;
uint uhash11(uint n) {
    n ^= (n << 1);
    n ^= (n >> 1);
    n *= k;
    n ^= (n << 1);
    return n * k;
}

float hash11(float p) {
    uint n = floatBitsToUint(p);
    return float(uhash11(n)) / float(UINT_MAX);
}

uniform vec2 u_resolution;
void main() {
    vec2 st = gl_FragCoord.xy / u_resolution;
    float h = hash11(st.x);
    fragColor = vec4(vec3(h), 1.0);
}