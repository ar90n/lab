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

uvec3 uhash33(uvec3 n) {
    n ^= (n.yzx << u);
    n ^= (n.yzx >> u);
    n *= k;
    n ^= (n.yzx << u);
    return n * k;
}

vec2 hash22(vec2 p) {
    uvec2 n = floatBitsToUint(p);
    return float(uhash22(n)) / vec2(UINT_MAX);
}

vec3 hash33(vec3 p) {
    uvec3 n = floatBitsToUint(p);;
    return float(uhash33(n)) / vec3(UINT_MAX);
}

float hash21(vec2 p) {
    return hash22(p).x;
}

float hash31(vec3 p) {
    return hash33(p).x;
}

float vnoise21(vec2 p) {
    vec2 n = floor(p);
    float[4] v;
    for( int j = 0; j < 2; j++) {
        for(int i = 0; i < 2 ; i++) {
            v[i + 2 * j] = hash21(n + vec2(i, j));
        }
    }

    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    return mix(mix(v[0], v[1], f[0]), mix(v[2], v[3], f[0]), f[1]);
}

float vnoise31(vec3 p) {
    vec3 n = floor(p);
    float[8] v;
    for (int k = 0; k < 2; k++ ) {
        for( int j = 0; j < 2; j++) {
            for(int i = 0; i < 2 ; i++) {
                v[i + 2 * (j + 2 * k)] = hash31(n + vec3(i, j, k));
            }
        }
    }

    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    return mix(
        mix(mix(v[0], v[1], f[0]), mix(v[2], v[3], f[0]), f[1]),
        mix(mix(v[4], v[5], f[0]), mix(v[6], v[7], f[0]), f[1]),
        f[2]       
    );
}

void main() {
    vec2 p = gl_FragCoord.xy / min(u_resolution.x, u_resolution.y);
    p = 10.0 * p + u_time;
    float v = vnoise21(p);
    fragColor = vec4(v, v, v, 1.0);
}