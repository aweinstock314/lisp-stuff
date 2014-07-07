#version 120

//in vec3 position;
attribute vec3 position;
vec4 tmp;

void main()
{
    gl_FrontColor = gl_Color;
    tmp = vec4(position, 1.0);
    gl_Position = gl_ModelViewProjectionMatrix * tmp;
}
