#include <math.h>
#include "acfloat.h"

#define RETURN_FLOAT(x) return (longfloat){.f = (x)}
#define INIT_SCRIPT_FLOAT(x) float x = fl_##x.f

extern void quit_c(char *);

enum RoundDirections {
	eRoundDown = 0,
	eRoundNearest = 1,
	eRoundUp = 2
};

long FloatToInt(longfloat fl_value, long roundDirection) {
	INIT_SCRIPT_FLOAT(value);

	long longval;

	if (value >= 0.0) {
		if (roundDirection == eRoundDown)
			longval = (long)value;
		else if (roundDirection == eRoundNearest)
			longval = (long)(value + 0.5);
		else if (roundDirection == eRoundUp)
			longval = (long)(value + 0.999999);
		else
			quit_c("!FloatToInt: invalid round direction");
	} else {
		// negative number
		if (roundDirection == eRoundUp)
			longval = (long)value; // this just truncates
		else if (roundDirection == eRoundNearest)
			longval = (long)(value - 0.5);
		else if (roundDirection == eRoundDown)
			longval = (long)(value - 0.999999);
		else
			quit_c("!FloatToInt: invalid round direction");
	}

	return longval;
}

longfloat IntToFloat(long value) {
	float fval = value;

	RETURN_FLOAT(fval);
}

longfloat StringToFloat(const char *theString) {
	float fval = atof(theString);

	RETURN_FLOAT(fval);
}

longfloat Math_Cos(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = cos(value);

	RETURN_FLOAT(value);
}

longfloat Math_Sin(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = sin(value);

	RETURN_FLOAT(value);
}

longfloat Math_Tan(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = tan(value);

	RETURN_FLOAT(value);
}

longfloat Math_ArcCos(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = acos(value);

	RETURN_FLOAT(value);
}

longfloat Math_ArcSin(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = asin(value);

	RETURN_FLOAT(value);
}

longfloat Math_ArcTan(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = atan(value);

	RETURN_FLOAT(value);
}

longfloat Math_ArcTan2(longfloat fl_yval, longfloat fl_xval) {
	INIT_SCRIPT_FLOAT(yval);
	INIT_SCRIPT_FLOAT(xval);

	float value = atan2(yval, xval);

	RETURN_FLOAT(value);
}

longfloat Math_Log(longfloat fl_num) {
	INIT_SCRIPT_FLOAT(num);

	float value = log(num);

	RETURN_FLOAT(value);
}

longfloat Math_Log10(longfloat fl_num) {
	INIT_SCRIPT_FLOAT(num);

	float value = log10(num);

	RETURN_FLOAT(value);
}

longfloat Math_Exp(longfloat fl_num) {
	INIT_SCRIPT_FLOAT(num);

	float value = exp(num);

	RETURN_FLOAT(value);
}

longfloat Math_Cosh(longfloat fl_num) {
	INIT_SCRIPT_FLOAT(num);

	float value = cosh(num);

	RETURN_FLOAT(value);
}

longfloat Math_Sinh(longfloat fl_num) {
	INIT_SCRIPT_FLOAT(num);

	float value = sinh(num);

	RETURN_FLOAT(value);
}

longfloat Math_Tanh(longfloat fl_num) {
	INIT_SCRIPT_FLOAT(num);

	float value = tanh(num);

	RETURN_FLOAT(value);
}

longfloat Math_RaiseToPower(longfloat fl_base, longfloat fl_exp) {
	INIT_SCRIPT_FLOAT(base);
	INIT_SCRIPT_FLOAT(exp);

	float value = pow(base, exp);

	RETURN_FLOAT(value);
}

longfloat Math_DegreesToRadians(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = value * (M_PI / 180.0);

	RETURN_FLOAT(value);
}

longfloat Math_RadiansToDegrees(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	value = value * (180.0 / M_PI);

	RETURN_FLOAT(value);
}

longfloat Math_GetPi(void) {
	float pi = M_PI;

	RETURN_FLOAT(pi);
}

longfloat Math_Sqrt(longfloat fl_value) {
	INIT_SCRIPT_FLOAT(value);

	if (value < 0.0)
	quit_c("!Sqrt: cannot perform square root of negative number");

	value = sqrt(value);

	RETURN_FLOAT(value);
}
