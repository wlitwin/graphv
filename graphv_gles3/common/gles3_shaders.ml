let fill_frag = {|#version 300 es
precision highp float;

#define EDGE_AA 1
layout(std140) uniform frag {
    mat3 scissorMat;
    mat3 paintMat;
    vec4 innerCol;
    vec4 outerCol;
    vec2 scissorExt;
    vec2 scissorScale;
    vec2 extent;
    float radius;
    float feather;
    float strokeMult;
    float strokeThr;
    float texType;
    float type;
};

uniform sampler2D tex;
in vec2 ftcoord;
in vec2 fpos;
out vec4 fragColor;

float sdroundrect(vec2 pt, vec2 ext, float rad) {
	vec2 ext2 = ext - vec2(rad,rad);
	vec2 d = abs(pt) - ext2;
	return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;
}

#ifdef EDGE_AA
float strokeMask() {
	return min(1.0, (1.0-abs(ftcoord.x*2.0-1.0))*strokeMult) * min(1.0, ftcoord.y);
}
#endif

// Scissoring
float scissorMask(vec2 p) {
	vec2 sc = (abs((scissorMat * vec3(p,1.0)).xy) - scissorExt);
	sc = vec2(0.5,0.5) - sc * scissorScale;
	return clamp(sc.x,0.0,1.0) * clamp(sc.y,0.0,1.0);
}

void main(void) {
   vec4 result = vec4(1, 1, 0, 1);
	float scissor = scissorMask(fpos);
#ifdef EDGE_AA
	float strokeAlpha = strokeMask();
	if (strokeAlpha < strokeThr) discard;
#else
	float strokeAlpha = 1.0;
#endif
	if (type == 0.) {			// Gradient
		// Calculate gradient color using box gradient
		vec2 pt = (paintMat * vec3(fpos,1.0)).xy;
		float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5) / feather, 0.0, 1.0);
		vec4 color = mix(innerCol,outerCol,d);
		// Combine alpha
		color *= strokeAlpha * scissor;
		result = color;
	} else if (type == 1.) {		// Image
		// Calculate color fron texture

		vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;
		vec4 color = texture(tex, pt);

		if (texType == 1.) color = vec4(color.xyz*color.w,color.w);
		if (texType == 2.) color = vec4(color.x);
		// Apply color tint and alpha.
		color *= innerCol;
		// Combine alpha
		color *= strokeAlpha * scissor;
		result = color;
	} else if (type == 2.) {		// Stencil fill
		result = vec4(1,1,1,1);
	} else if (type == 3.) {		// Textured tris
		vec4 color = texture(tex, ftcoord);
		if (texType == 1.) color = vec4(color.xyz*color.w,color.w);
		if (texType == 2.) color = vec4(color.x);
		color *= scissor;
		result = color * innerCol;
	}
	fragColor = result;
}
|}

let fill_vert = {|#version 300 es

uniform vec2 viewSize;
in vec2 vertex;
in vec2 tcoord;
out vec2 ftcoord;
out vec2 fpos;

void main(void) {
	ftcoord = tcoord;
	fpos = vertex;
	gl_Position = vec4(2.0*vertex.x/viewSize.x - 1.0, 1.0 - 2.0*vertex.y/viewSize.y, 0, 1);
}
|}
