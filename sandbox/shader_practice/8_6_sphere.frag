#version 300 es
precision mediump float;

out vec4 fragColor;
uniform vec2 u_resolution;

float sphereSDF(vec3 p, vec3 c, float r) {
    return length(p - c) - r;
}

float sceneSDF(vec3 p) {
    vec3 cent = vec3(0.0, 0.0, -2);
    float scale = 0.3;
    return sphereSDF(p, cent, scale);
}

vec3 gradSDF(vec3 p) {
    float eps = 0.001;
    return normalize(vec3(
        sceneSDF(p + vec3(eps, 0.0, 0.0)) - sceneSDF(p - vec3(eps, 0.0, 0.0)),
        sceneSDF(p + vec3(0.0, eps, 0.0)) - sceneSDF(p - vec3(0.0, eps, 0.0)),
        sceneSDF(p + vec3(0.0, 0.0, eps)) - sceneSDF(p - vec3(0.0, 0.0, eps))
    ));
}

void main() {
    vec2 p = (gl_FragCoord.xy * 2.0 - u_resolution) / min(u_resolution.x, u_resolution.y);
    vec3 lPos = vec3(8.0, 8.0, 1.0);
    vec3 cPos = vec3(0.0, 0.0, 0.0);
    vec3 cUp = vec3(0.0, 1.0, 0.0);
    vec3 cDir = vec3(0.0, 0.0, -1.0);
    vec3 cSide = cross(cDir, cUp);
    vec3 ray = cSide * p.x + cUp * p.y + cDir; 
    vec3 rPos = ray + cPos;

    ray = normalize(ray);
    fragColor = vec4(0.0, 0.0, 0.0, 1.0);
    for(int i = 0; i < 50; i++) {
        if(0.00001 < sceneSDF(rPos)) {
            rPos += sceneSDF(rPos) * ray;
        } else {
            float amb = 0.1;
            float diff = 0.9 * max(dot(normalize(lPos - rPos), gradSDF(rPos)), 0.0);
            vec3 col = vec3(0.0, 1.0, 1.0);
            col *= diff + amb;
            fragColor.rgb = col;
            break;
        }
    }
}