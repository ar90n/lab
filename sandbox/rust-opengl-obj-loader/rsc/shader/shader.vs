#version 100
precision mediump float;

attribute vec3 Position;

uniform mat4 Model;
uniform mat4 View;
uniform mat4 Projection;

void main() {
    gl_Position = Projection * View * vec4(vec3(Model * vec4(Position, 1.0)), 1.0);
}