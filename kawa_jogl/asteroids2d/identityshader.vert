#version 120

//in vec3 position;
attribute vec3 position;
attribute vec3 color;
vec4 tmp;

void main()
{
    //gl_FrontColor = gl_Color;
    gl_FrontColor = vec4(color, 1.0);
    //gl_FrontColor = (.5 * gl_Color) + (.5 * vec4(color, 1.0));
    tmp = vec4(position, 1.0);
    gl_Position = gl_ModelViewProjectionMatrix * tmp;
}
