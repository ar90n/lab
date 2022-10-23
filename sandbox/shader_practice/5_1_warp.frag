#version 300 es
precision mediump float;

out vec4 fragColor;
uniform vec2 u_resolution;
uniform float u_time;

uvec3 k = uvec3(0x456789abu, 0x6789ab45u, 0x90ab4567u);
uvec3 u = uvec3(1, 2, 3);
const uint UINT_MAX = 0xffffffffu;

uvec2 uhash22(uvec2 n) {
    n ^= (n.yx << u.xy);
    n ^= (n.yx >> u.xy);
    n *= k.xy;
    n ^= (n.yx << u.xy);
    return n * k.xy;
}

vec2 hash22(vec2 p) {
    uvec2 n = floatBitsToUint(p);
    return float(uhash22(n)) / vec2(UINT_MAX);
}

float hash21(vec2 p) {
    return hash22(p).x;
}

float gtable2(vec2 lattice, vec2 p) {
    uvec2 n = floatBitsToUint(lattice);
    uint ind = uhash22(n).x >> 29;
    float u = 0.92387953 * (ind < 4u ? p.x : p.y);
    float v = 0.38268343 * (ind < 4u ? p.y : p.x);
    return ((ind & 1u) == 0u ? u : -u) + ((ind & 2u) == 0u ? v : -v);
}

float pnoise21(vec2 p) {
    vec2 n = floor(p);
    vec2 f = fract(p);
    float[4] v;
    for( int j = 0; j < 2; j++) {
        for(int i = 0; i < 2 ; i++) {
            v[i + 2 * j] = gtable2(n + vec2(i, j), f - vec2(i,j));
        }
    }

    f = f * f * f * (10.0 - 15.0 * f + 6.0 * f * f);
    return 0.5 * mix(mix(v[0], v[1], f[0]), mix(v[2], v[3], f[0]), f[1]) + 0.5;
}

float warp21(vec2 p, float g) {
    float val = 0.0;
    for(int i = 0; i < 4; i++) {
        val = pnoise21(p + g * val);
    }

    return val;
}

void main() {
    vec2 p = gl_FragCoord.xy / u_resolution;
    p = 10.0 * p;
    float v = warp21(p, 4.0);
    fragColor = vec4(v, v, v, 1.0);
}